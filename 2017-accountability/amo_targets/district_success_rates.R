library(acct)
library(haven)
library(tidyverse)

numeric_subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners", "Students with Disabilities", "Super Subgroup")
math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

ACT <- read_dta("K:/ORP_accountability/data/2016_ACT/ACT_district2017.dta") %>%
    mutate(subgroup = if_else(subgroup == "English Langauge Learners with T1/T2", "English Learners", subgroup)) %>%
    filter(subgroup %in% numeric_subgroups) %>%
    select(system, subject, grade, subgroup, valid_tests, n_on_track = n_21_orhigher) %>%
    mutate_at(c("valid_tests", "n_on_track"), as.integer) %>%
    mutate_at(c("valid_tests", "n_on_track"), funs(if_else(valid_tests < 30, 0L, .)))

student_level <- read_dta("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10012017.dta") %>%
    filter(greater_than_60_pct == "Y",
        original_subject != "US History",
        !(grade %in% c(3, 4) & original_subject == "Science")) %>%
# Residential Facility students are dropped from system level
    filter(residential_facility != 1 | is.na(residential_facility)) %>%
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

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "BHN", "ED", "SWD", "EL_T1_T2", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(system, original_subject, grade) %>%
        summarise_at(c("valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

system_base <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        ),
        grade = case_when(
            grade %in% 3:5 ~ "3rd through 5th",
            grade %in% 6:8 ~ "6th through 8th",
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
    group_by(system, subject, grade, subgroup) %>%
    summarise_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    bind_rows(ACT) %>%
# Suppress for subjects with < 30 valid tests
    mutate_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, .))) %>%
# Aggregate across replaced subjects
    group_by(system, subgroup) %>%
    summarise_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_))

# Names crosswalk
system_names <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/system_name_crosswalk.csv")

success_rate_AMO <- system_base %>%
    left_join(system_names, by = "system") %>%
    transmute(year = 2018, system, system_name, subgroup, valid_tests_prior = valid_tests,
        pct_below_prior = pct_below,
        AMO_target_below = amo_reduction_double(valid_tests, pct_below),
        AMO_target_below_4 = amo_reduction_double(valid_tests, pct_below, double = TRUE),
        pct_on_mastered_prior = pct_on_mastered,
        AMO_target = amo_target(valid_tests, pct_on_mastered),
        AMO_target_4 = amo_target(valid_tests, pct_on_mastered, double = TRUE))

write_csv(success_rate_AMO, path = "K:/ORP_accountability/projects/2018_amo/district_success_rate.csv", na = "")
