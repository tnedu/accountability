library(acct)
library(tidyverse)

pools <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

amo_success <- read_csv("N:/ORP_accountability/projects/2018_amo/school_success_rate.csv") %>%
    select(system, school, subgroup, success_rate_prior = pct_on_mastered_prior, AMO_target, AMO_target_4) %>%
    mutate(subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup))

student_level <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(grade %in% 3:12, residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y") %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(
        grade = if_else(is.na(grade), 0L, grade),
        test = if_else(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test),
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_),
        All = 1L,
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic/Latino",
        Hawaiian = race == "Native Hawaiian/Pac. Islander",
        Native = race == "American Indian/Alaska Native",
        White = race == "White",
        EL_T1234 = if_else(EL == 1, 1L, EL_T1234),
        Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1234 == 1L)) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD", "EL_T1234", "Super"), as.integer)

int_math_systems <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

ACT_substitution <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_school.csv") %>%
    transmute(system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, subgroup = "All", valid_tests, n_on_track = n_met_benchmark)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD", "EL_T1234", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(system, school, test, original_subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

success_rates <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        ),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    group_by(system, school, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(success_rate = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_)) %>%
    left_join(amo_success, by = c("system", "school", "subgroup")) %>%
    transmute(system, school, indicator = "Success Rate", subgroup,
        n_count = valid_tests, metric = success_rate, metric_prior = success_rate_prior,
        ci_bound = ci_upper_bound(n_count, metric),
        AMO_target, AMO_target_4,
        grade_abs = case_when(
            metric >= 50 ~ "A",
            metric >= 45 ~ "B",
            metric >= 35 ~ "C",
            metric >= 25 ~ "D",
            metric < 25 ~ "F"
        ),
        grade_target = case_when(
            metric >= AMO_target_4 ~ "A",
            metric > AMO_target ~ "B",
            ci_bound >= AMO_target ~ "C",
            ci_bound > metric_prior ~ "D",
            ci_bound <= metric_prior ~ "F"
        ),
    # Not setting na.rm = TRUE because schools need both absolute and AMO to get a grade
        grade = pmin(grade_abs, grade_target)
    )

amo_grad <- read_csv("N:/ORP_accountability/projects/2018_amo/school_ready_grad.csv") %>%
    transmute(system, school, subgroup, 
        metric_prior = if_else(grad_cohort >= 30, grad_rate, NA_real_),
        AMO_target = grad_target, AMO_target_4 = grad_target_double)

grad <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>%
    transmute(system, school, indicator = "Graduation Rate",
        subgroup = case_when(
            subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
            TRUE ~ subgroup
        ),
        n_count = grad_cohort, metric = grad_rate, ci_bound = ci_upper_bound(n_count, metric)) %>%
    filter(system != 0 & school != 0, system != 90, subgroup %in% unique(success_rates$subgroup)) %>%
    left_join(amo_grad, by = c("system", "school", "subgroup")) %>%
    mutate(
        grade_abs = case_when(
            n_count < 30 ~ NA_character_,
            metric >= 95 ~ "A",
            metric >= 90 ~ "B",
            metric >= 80 ~ "C",
            metric >= 67 ~ "D",
            metric < 67 ~ "F"
        ),
        grade_target = case_when(
            n_count < 30 ~ NA_character_,
            metric >= AMO_target_4 ~ "A",
            metric > AMO_target ~ "B",
            ci_bound >= AMO_target ~ "C",
            ci_bound > metric_prior ~ "D",
            ci_bound <= metric_prior ~ "F"
        ),
        grade = pmin(grade_abs, grade_target)
    )

amo_ready_grad <- read_csv("N:/ORP_accountability/projects/2018_amo/school_ready_grad.csv") %>%
    transmute(system = as.numeric(system), school = as.numeric(school), subgroup, 
        metric_prior = ACT_21_or_higher,
        AMO_target = ACT_grad_target, AMO_target_4 = ACT_grad_target_double)

ready_grad <- haven::read_dta("N:/ORP_accountability/data/2018_final_accountability_files/ready_grad_school2018_JW.dta") %>%
    mutate(subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup)) %>%
    filter(subgroup %in% unique(success_rates$subgroup)) %>%
    transmute(system, school, indicator = "Ready Graduates", subgroup, n_count = grad_cohort, metric = pct_ready_grad, ci_bound = ci_upper_bound(n_count, metric)) %>%
    left_join(amo_ready_grad, by = c("system", "school", "subgroup")) %>%
    mutate(
        grade_abs = case_when(
            metric >= 40 ~ "A",
            metric >= 30 ~ "B",
            metric >= 25 ~ "C",
            metric >= 16 ~ "D",
            metric < 16 ~ "F"
        ),
        grade_target = case_when(
            metric >= AMO_target_4 ~ "A",
            metric >= AMO_target ~ "B",
            metric > metric_prior ~ "C",
            metric == metric_prior ~ "D",
            metric < metric_prior ~ "F"
        ),
        grade = pmin(grade_abs, grade_target)
    )

amo_absenteeism <- read_csv("N:/ORP_accountability/projects/2018_amo/school_chronic_absenteeism.csv",
        col_types = "iicicccddddddddd") %>%
    transmute(system, school, subgroup,
        metric_prior = if_else(n_students >= 30, pct_chronically_absent, NA_real_),
        AMO_target = AMO_reduction_target, AMO_target_4 = AMO_reduction_target_double)

absenteeism <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/school_chronic_absenteeism.csv",
    col_types = "iicicccdid") %>%
    left_join(pools, by = c("system", "school")) %>%
    transmute(system, school, pool, indicator = "Chronic Absenteeism", subgroup, n_count = n_students, metric = pct_chronically_absent,
        ci_bound = ci_lower_bound(n_count, metric)) %>%
    left_join(amo_absenteeism, by = c("system", "school", "subgroup")) %>%
    mutate(
        grade_abs = case_when(
            n_count < 30 ~ NA_character_,
            pool == "K8" & metric <= 6 ~ "A",
            pool == "K8" & metric <= 9 ~ "B",
            pool == "K8" & metric <= 13 ~ "C",
            pool == "K8" & metric <= 20 ~ "D",
            pool == "K8" & metric > 20 ~ "F",
            pool == "HS" & metric <= 10 ~ "A",
            pool == "HS" & metric <= 14 ~ "B",
            pool == "HS" & metric <= 20 ~ "C",
            pool == "HS" & metric <= 30 ~ "D",
            pool == "HS" & metric > 30 ~ "F"
        ),
        grade_target = case_when(
            n_count < 30 ~ NA_character_,
            metric <= AMO_target_4 ~ "A",
            metric < AMO_target ~ "B",
            ci_bound <= AMO_target ~ "C",
            ci_bound < metric ~ "D",
            ci_bound >= metric ~ "F"
        ),
    # Not setting na.rm = TRUE because schools need both absolute and AMO to get a grade
        grade = pmin(grade_abs, grade_target)
    ) %>%
    select(-pool)

elpa <- read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_school.csv") %>%
    transmute(system, school, indicator = "ELPA Growth Standard", subgroup,
        n_count = growth_standard_denom, metric = pct_met_growth_standard,
        grade_abs = case_when(
            n_count < 10 ~ NA_character_,
            metric >= 60 ~ "A",
            metric >= 50 ~ "B",
            metric >= 40 ~ "C",
            metric >= 25 ~ "D",
            metric < 25 ~ "F"
        )
    )

school_names <- student_level %>%
    select(system, system_name, school, school_name) %>%
    distinct()

school_accountability <- bind_rows(success_rates, grad, ready_grad, absenteeism, elpa) %>%
    left_join(school_names, by = c("system", "school")) %>%
    left_join(pools, by = c("system", "school")) %>%
    filter(!is.na(pool)) %>%
    select(system, system_name, school, school_name, pool, designation_ineligible, indicator, subgroup,
        n_count, metric, ci_bound, everything()) %>%
    arrange(system, school, indicator, subgroup)

write_csv(school_accountability, path = "N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv",
    na = "")
