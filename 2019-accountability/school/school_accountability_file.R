library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

amo_ach <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv") %>%
    select(system, school, subgroup, metric_prior = success_rate_prior, AMO_target, AMO_target_double)

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
    filter(
        residential_facility == 0,
        enrolled_50_pct_school == "Y",
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, T1234 = t1234) %>%
    mutate_at(vars(BHN, ED, SWD, EL, T1234), as.logical) %>%
    mutate(
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        All = TRUE,
        Asian = reported_race == "Asian",
        Black = reported_race == "Black or African American",
        Hispanic = reported_race == "Hispanic/Latino",
        Hawaiian = reported_race == "Native Hawaiian/Pac. Islander",
        Native = reported_race == "American Indian/Alaska Native",
        White = reported_race == "White",
        EL_T1234 = EL | T1234,
        Super = BHN | ED | SWD | EL_T1234
    )

# int_math_systems <- student_level %>%
#     filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
#     count(system, original_subject) %>%
#     group_by(system) %>%
#     mutate(temp = max(n)) %>%
#     filter(n == temp, original_subject == "Integrated Math I") %>%
#     magrittr::extract2("system")

# ACT_substitution <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv") %>%
#     transmute(
#         system, school,
#         subject = case_when(
#             subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
#             subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
#         ),
#         grade = 11,
#         subgroup = "All",
#         valid_tests, 
#         ot_m = n_met_benchmark
#     )

collapse <- function(g) {

    g_quo <- enquo(g)

    student_level %>%
        filter(!!g_quo) %>%
        group_by(system, school, test, subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "ot_m"), sum, na.rm = TRUE) %>%
        ungroup() %>% 
        mutate(subgroup = deparse(g_quo))

}

ach <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(Black), quo(Hawaiian), quo(Hispanic), quo(Native), quo(White),
        quo(BHN), quo(ED), quo(SWD), quo(EL_T1234), quo(Super)
    ),
    .f = ~ collapse(!!.)
) %>% 
    rename(valid_tests = valid_test) %>%
    # bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        ),
        subject = case_when(
            subject %in% english_eoc ~ "HS English",
            subject %in% math_eoc ~ "HS Math"
        )
    ) %>%
# Aggregate HS Math/English
    group_by(system, subject, subgroup, grade) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_tests", "ot_m"), ~ if_else(valid_tests < 30, 0L, as.integer(.))) %>%
    group_by(system, school, subgroup) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(success_rate = if_else(valid_tests != 0, round5(100 * ot_m/valid_tests, 1), NA_real_)) %>%
    left_join(amo_ach, by = c("system", "school", "subgroup")) %>%
    transmute(
        system,
        school,
        indicator = "Achievement",
        subgroup,
        n_count = valid_tests,
        metric = success_rate,
        metric_prior,
        ci_bound = ci_upper_bound(n_count, metric),
        AMO_target, AMO_target_double,
        score_abs = case_when(
            metric >= 45 ~ 4,
            metric >= 35 ~ 3,
            metric >= 27.5 ~ 2,
            metric >= 20 ~ 1,
            metric < 20 ~ 0
        ),
        score_target = case_when(
            metric >= AMO_target_double ~ 4,
            metric >= AMO_target ~ 3,
            ci_bound >= AMO_target ~ 2,
            ci_bound > metric_prior ~ 1,
            ci_bound <= metric_prior ~ 0
        ),
    # Schools need both absolute and AMO to get a grade
        score = pmax(score_abs, score_target)
    )

# Absenteeism
amo_abs <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv") %>%
    transmute(
        system,
        school,
        subgroup,
        metric_prior = if_else(n_students >= 30, pct_chronically_absent, NA_real_),
        AMO_target,
        AMO_target_double
    )

abs <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism.csv",
         col_types = "iicicccdid") %>%
    left_join(pools, by = c("system", "school")) %>%
    transmute(
        system, school, pool, indicator = "Chronic Absenteeism", subgroup,
        n_count = if_else(n_students < 30, 0, n_students),
        metric = if_else(n_count < 30, NA_real_, pct_chronically_absent),
        ci_bound = if_else(n_students >= 30, ci_lower_bound(n_count, metric), NA_real_)
    ) %>%
    left_join(amo_absenteeism, by = c("system", "school", "subgroup")) %>%
    mutate(
        score_abs = case_when(
            pool == "K8" & metric <= 6 ~ 4,
            pool == "K8" & metric <= 9 ~ 3,
            pool == "K8" & metric <= 13 ~ 2,
            pool == "K8" & metric <= 20 ~ 1,
            pool == "K8" & metric > 20 ~ 0,
            pool == "HS" & metric <= 10 ~ 4,
            pool == "HS" & metric <= 14 ~ 3,
            pool == "HS" & metric <= 20 ~ 2,
            pool == "HS" & metric <= 30 ~ 1,
            pool == "HS" & metric > 30 ~ 0
        ),
        score_target = case_when(
            metric <= AMO_target_double ~ 4,
            metric <= AMO_target ~ 3,
            ci_bound <= AMO_target ~ 2,
            ci_bound < metric_prior ~ 1,
            ci_bound >= metric_prior ~ 0
        ),
    # Schools need both absolute and AMO to get a grade
        score = pmax(score_abs, score_target)
    ) %>%
    select(-pool)

# Graduation Rate
amo_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/grad_school.csv") %>%
    transmute(
        system,
        school,
        subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup),
        metric_prior = if_else(grad_cohort >= 30, grad_rate, NA_real_),
        AMO_target,
        AMO_target_double
    )

grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>%
    transmute(
        system,
        school,
        indicator = "Graduation Rate",
        subgroup,
        n_count = if_else(grad_cohort < 30, 0, grad_cohort),
        metric = if_else(n_count < 30, NA_real_, grad_rate),
        ci_bound = ci_upper_bound(n_count, metric)
    ) %>%
    filter(system != 0 & school != 0, system != 90, subgroup %in% unique(amo_grad$subgroup)) %>%
    left_join(amo_grad, by = c("system", "school", "subgroup")) %>%
    mutate(
        score_abs = case_when(
            metric >= 95 ~ 4,
            metric >= 90 ~ 3,
            metric >= 80 ~ 2,
            metric >= 67 ~ 1,
            metric < 67 ~ 0
        ),
        score_target = case_when(
            metric >= AMO_target_double ~ 4,
            metric >= AMO_target ~ 3,
            ci_bound >= AMO_target ~ 2,
            ci_bound > metric_prior ~ 1,
            ci_bound <= metric_prior ~ 0
        ),
    # Schools need both absolute and AMO to get a grade
        score = pmax(score_abs, score_target)
    )

# Raedy Grad
amo_ready_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/ready_grad_school.csv") %>%
    transmute(
        system,
        school,
        subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup),
        metric_prior = if_else(grad_cohort >= 30, pct_ready_grad, NA_real_),
        AMO_target, 
        AMO_target_double
    )

ready_grad <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv") %>%
    transmute(
        system,
        school,
        indicator = "Ready Graduates",
        subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup),
        n_count,
        metric = if_else(n_count < 30, NA_real_, pct_ready_grad),
        ci_bound = ci_upper_bound(n_count, metric)
    ) %>%
    left_join(amo_ready_grad, by = c("system", "school", "subgroup")) %>%
    mutate(
        score_abs = case_when(
            metric >= 40 ~ 4,
            metric >= 30 ~ 3,
            metric >= 25 ~ 2,
            metric >= 16 ~ 1,
            metric < 16 ~ 0
        ),
        score_target = case_when(
            metric >= AMO_target_double ~ 4,
            metric >= AMO_target ~ 3,
            metric > metric_prior ~ 2,
            metric == metric_prior ~ 1,
            metric < metric_prior ~ 0
        ),
    # Schools need both absolute and AMO to get a grade
        score = pmax(score_abs, score_target)
    )

bind_rows(ach, grad, ready_grad) %>%
    arrange(system, school, indicator, subgroup) %>%
    write_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv", na = "")
