library(acct)
library(janitor)
library(tidyverse)

# TVAAS for Success Rate VA
tvaas <- readxl::read_excel("N:/ORP_accountability/data/2019_tvaas/2019-District-Level-Accountability-Results-EOC-TCAP.xlsx") %>%
    clean_names() %>%
    mutate(
        grade = case_when(
            grade %in% c("Grades 3-5", "Grades 4-5") ~ "3rd through 5th",
            grade == "Grades 6-8" ~ "6th through 8th",
            grade == "Grades 9-12" ~ "9th through 12th"
        ),
        subgroup = if_else(subgroup == "English Learners (includes EL and T1-T4)", "English Learners with Transitional 1-4", subgroup)
    ) %>%
# Take better of with and without grade 3
    group_by(system_number, subgroup, grade) %>%
    mutate(temp = max(index)) %>%
    ungroup() %>%
    filter(temp == index) %>%
    transmute(
        system = as.numeric(system_number),
        subgroup,
        grade,
        value_add_metric = level
    ) %>%
    distinct()

# Success Rates
amo_ach <- read_csv("N:/ORP_accountability/projects/2019_amo/success_rate_targets_district.csv") %>%
    select(system, subgroup, grade, metric_prior = success_rate_prior, AMO_target, AMO_target_double)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
    filter(
        residential_facility == 0,
        enrolled_50_pct_district == "Y",
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, T1234 = t1234) %>%
    mutate_at(vars(BHN, ED, SWD, EL, T1234), as.logical) %>%
    mutate(
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        All = TRUE,
        EL_T1234 = EL | T1234,
        grade = case_when(
            grade %in% 3:5 ~ "3rd through 5th",
            grade %in% 6:8 ~ "6th through 8th",
            grade %in% 9:12 | is.na(grade) ~ "9th through 12th"
        )
    )

# ACT_substitution <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_district.csv") %>%
#     transmute(
#         system,
#         subject = "HS Math",
#         grade = "9th through 12th",
#         subgroup = "All",
#         valid_tests,
#         ot_m = n_met_benchmark
#     )

collapse <- function(g) {

    g_quo <- enquo(g)

    student_level %>%
        filter(!!g_quo) %>%
        group_by(system, subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "ot_m"), sum, na.rm = TRUE) %>%
        mutate(subgroup = deparse(g_quo))

}

success_rates <- map_dfr(
    .x = list(quo(All), quo(BHN), quo(ED), quo(SWD), quo(EL_T1234)),
    .f = ~ collapse(!!.)
) %>%
    rename(valid_tests = valid_test) %>%
    # bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~SWD" ~ "Students with Disabilities"
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
    group_by(system, subgroup, grade) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(metric = if_else(valid_tests != 0, round5(100 * ot_m/valid_tests, 1), NA_real_)) %>%
    left_join(amo_ach, by = c("system", "subgroup", "grade")) %>%
    full_join(tvaas, by = c("system", "subgroup", "grade")) %>%
    transmute(
        system,
        indicator = "Achievement",
        grade,
        subgroup,
        n_count = valid_tests,
        metric,
        metric_prior,
        ci_bound = ci_upper_bound(n_count, metric),
        AMO_target,
        AMO_target_double,
        absolute_pathway = case_when(
            metric >= 45 ~ 4,
            metric >= 35 ~ 3,
            metric >= 27.5 ~ 2,
            metric >= 20 ~ 1,
            metric < 20 ~ 0
        ),
        AMO_pathway = case_when(
            metric >= AMO_target_double ~ 4,
            metric >= AMO_target ~ 3,
            ci_bound >= AMO_target ~ 2,
            ci_bound > metric_prior ~ 1,
            ci_bound <= metric_prior ~ 0
        ),
        value_add_metric,
        value_add_pathway = case_when(
            value_add_metric == 5 ~ 4,
            value_add_metric == 4 ~ 3,
            value_add_metric == 3 ~ 2,
            value_add_metric == 2 ~ 1,
            value_add_metric == 1 ~ 0
        )
    )

# Grad
amo_grad <- read_csv("N:/ORP_accountability/projects/2019_amo/grad_district.csv") %>%
    transmute(
        system,
        subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup),
        metric_prior = if_else(grad_cohort >= 30, grad_rate, NA_real_),
        AMO_target,
        AMO_target_double
    )

grad_va <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va.csv")

grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/district_grad_rate.csv") %>%
    transmute(
        system,
        indicator = "Graduation Rate",
        grade = "All Grades",
        subgroup,
        n_count = if_else(grad_cohort < 30, 0, grad_cohort),
        metric = if_else(n_count < 30, NA_real_, grad_rate),
        ci_bound = ci_upper_bound(n_count, metric)
    ) %>%
    left_join(amo_grad, by = c("system", "subgroup")) %>%
    mutate(
        absolute_pathway = case_when(
            metric >= 95 ~ 4,
            metric >= 90 ~ 3,
            metric >= 80 ~ 2,
            metric >= 67 ~ 1,
            metric < 67 ~ 0
        ),
        AMO_pathway = case_when(
            metric >= AMO_target_double ~ 4,
            metric >= AMO_target ~ 3,
            ci_bound >= AMO_target ~ 2,
            ci_bound > metric_prior ~ 1,
            ci_bound <= metric_prior ~ 0
        )
    ) %>%
    full_join(grad_va, by = c("system", "subgroup", "indicator")) %>%
    filter(!system %in% c(0, 90), subgroup %in% unique(amo_grad$subgroup))

# Absenteeism
amo_abs <- read_csv("N:/ORP_accountability/projects/2019_amo/absenteeism_targets_district_primary_enrollment.csv") %>%
    transmute(
        system,
        subgroup,
        metric_prior = if_else(n_students >= 30, pct_chronically_absent, NA_real_),
        AMO_target,
        AMO_target_double
    )

abs_va <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_absenteeism_va.csv")

abs <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/system_chronic_absenteeism.csv",
        col_types = "icicccdid") %>%
    transmute(
        system,
        indicator = "Chronic Absenteeism",
        subgroup,
        n_count = if_else(n_students < 30, 0, n_students),
        metric = if_else(n_count < 30, NA_real_, pct_chronically_absent),
        ci_bound = if_else(n_students >= 30, ci_lower_bound(n_count, metric), NA_real_)
    ) %>%
    left_join(amo_abs, by = c("system", "subgroup")) %>%
    mutate(
        score_abs = case_when(
            metric <= 8 ~ 4,
            metric <= 11.5 ~ 3,
            metric <= 16.5 ~ 2,
            metric <= 25 ~ 1,
            metric > 25 ~ 0,
        ),
        score_target = case_when(
            metric <= AMO_target_double ~ 4,
            metric <= AMO_target ~ 3,
            ci_bound <= AMO_target ~ 2,
            ci_bound < metric_prior ~ 1,
            ci_bound >= metric_prior ~ 0
        )
    ) %>%
    full_join(abs_va, by = c("system", "subgroup"))

# ELPA
amo_elpa <- read_csv("N:/ORP_accountability/projects/2019_amo/elpa_district.csv")

elpa_va <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_elpa_va.csv")

elpa <- read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_district.csv") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "English Learners", "Students with Disabilities")) %>%
    transmute(
        system,
        indicator = "ELPA Growth Standard",
        subgroup,
        n_count = if_else(growth_standard_denom < 30, 0L, growth_standard_denom),
        metric = if_else(n_count < 30, NA_real_, pct_met_growth_standard),
        score_abs = case_when(
            metric >= 60 ~ 4,
            metric >= 50 ~ 3,
            metric >= 40 ~ 2,
            metric >= 25 ~ 1,
            metric < 25 ~ 0
        ),
        score_target = case_when(
            metric <= AMO_target_double ~ 4,
            metric <= AMO_target ~ 3,
            ci_bound <= AMO_target ~ 2,
            ci_bound < metric_prior ~ 1,
            ci_bound >= metric_prior ~ 0
        )
    ) %>%
    full_join(elpa_va, by = c("system", "subgroup"))

district_accountability <- bind_rows(success_rates, grad, absenteeism, elpa) %>%
    rowwise() %>%
    mutate(
        overall_score = mean(c(mean(c(absolute_pathway, AMO_pathway), na.rm = TRUE), value_add_pathway), na.rm = TRUE),
        overall_score = if_else(is.nan(overall_score), NA_real_, overall_score)) %>%
    ungroup() %>%
    arrange(system, indicator, grade, subgroup)

write_csv(district_accountability, "N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file.csv", na = "")
