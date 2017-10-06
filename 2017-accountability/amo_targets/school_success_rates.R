library(acct)
library(haven)
library(tidyverse)

numeric_subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners", "Students with Disabilities", "Super Subgroup")

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool)

student_level <- read_dta("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10012017.dta") %>%
    filter(greater_than_60_pct == "Y",
        original_subject != "US History",
        !(grade %in% c(3, 4) & original_subject == "Science")) %>%
# Homebound and Residential Facility students are dropped from system level
    filter(homebound == 0 | is.na(homebound),
        residential_facility != 1 | is.na(residential_facility)) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = ell, EL_T1_T2 = ell_t1t2) %>%
    mutate(valid_test = as.integer(valid_test),
        grade = if_else(is.na(grade), 0, grade),
        original_subject = if_else(test == "MSAA", subject, original_subject),
        n_below = if_else(performance_level %in% c("1. Below", "1. Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("2. Approaching", "2. Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_),
        All = 1L,
        EL_T1_T2 = if_else(EL == 1, 1, EL_T1_T2),
        Super = as.numeric(BHN == 1 | ED == 1 | SWD == 1 | EL_T1_T2 == 1))

int_math_systems <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

# ACT for HS Success Rate AMOs
ACT <- read_dta("K:/ORP_accountability/data/2016_ACT/ACT_school2017.dta") %>%
    mutate(subgroup = if_else(subgroup == "English Langauge Learners with T1/T2", "English Learners", subgroup)) %>%
    filter(subgroup %in% numeric_subgroups) %>%
    select(system, school, subject, grade, subgroup, valid_tests, n_on_track = n_21_orhigher) %>%
    mutate_at(c("valid_tests", "n_on_track"), as.integer) %>%
    mutate_at(c("valid_tests", "n_on_track"), funs(if_else(valid_tests < 30, 0L, .)))

ACT_substitution <- read_csv("K:/ORP_accountability/data/2017_ACT/school_act_substitution_2017.csv") %>%
    transmute(system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, subgroup = "All", valid_tests, n_on_track = n_met_benchmark)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "BHN", "ED", "SWD", "EL_T1_T2", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(system, school, original_subject, grade) %>%
        summarise_at(c("valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

subjects_suppressed <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    bind_rows(ACT_substitution) %>%
    mutate(subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        ),
        grade = case_when(
            grade %in% 3:8 ~ "3rd through 8th",
            grade %in% c(0, 9:12) ~ "9th through 12th"
        ),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL_T1_T2" ~ "English Learners",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup"
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(system, school, subject, grade, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    bind_rows(ACT) %>%
# Suppress for subjects with < 30 valid tests
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    left_join(pools, by = c("system", "school"))

# AMOs by English, Math, Science
subject_targets <- subjects_suppressed %>%
    filter(subject != "ACT Composite") %>%
    mutate(subject = case_when(
            subject %in% math_eoc ~ "HS Math",
            subject %in% english_eoc ~ "HS English",
            subject %in% science_eoc ~ "HS Science",
            TRUE ~ subject
        )
    ) %>%
    group_by(system, school, pool, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(year = 2018, system, school, pool, subject, subgroup,
        valid_tests_prior = valid_tests,
        pct_on_mastered_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests_prior, 1), NA_real_),
        AMO_target = amo_target(valid_tests_prior, pct_on_mastered_prior),
        AMO_target_4 = amo_target(valid_tests_prior, pct_on_mastered_prior, double = TRUE))

write_csv(subject_targets, "K:/ORP_accountability/projects/2018_amo/school_subject.csv")

# One year success rates
success_rate_targets <- subjects_suppressed %>%
    group_by(system, school, pool, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(year = 2018, system, school, pool, subgroup,
        valid_tests_prior = valid_tests,
        pct_on_mastered_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests_prior, pct_on_mastered_prior),
        AMO_target_4 = amo_target(valid_tests_prior, pct_on_mastered_prior, double = TRUE))

write_csv(success_rate_targets, path = "K:/ORP_accountability/projects/2018_amo/school_success_rates.csv", na = "")
