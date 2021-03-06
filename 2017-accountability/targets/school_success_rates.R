library(acct)
library(haven)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools <- read_csv("N:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool)

t3_t4 <- readxl::read_excel("N:/ORP_accountability/projects/Jessica/Report Card files/ELB_T3_T4.xls") %>%
    transmute(id = STUDENT_KEY, T3_T4 = 1L) %>%
    distinct()

student_level <- read_dta("N:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10192017.dta") %>%
# Combine two Bartlett schools that merged
    mutate(school = if_else(system == 794 & school == 170, 25, school)) %>%
    filter(greater_than_60_pct == "Y",
        original_subject != "US History",
        grade %in% 3:12,
# Grades 3-4 science are omitted from 2018 success rate because of new standards setting
        !(grade %in% c(3, 4) & original_subject == "Science"),
# Homebound and Residential Facility students are dropped from system level
        homebound == 0 | is.na(homebound),
        residential_facility != 1 | is.na(residential_facility)) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = ell, EL_T1_T2 = ell_t1t2) %>%
    left_join(t3_t4, by = "id") %>%
    mutate(valid_test = as.integer(valid_test),
        original_subject = if_else(test == "MSAA", subject, original_subject),
        n_below = if_else(performance_level %in% c("1. Below", "1. Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("2. Approaching", "2. Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_),
        All = 1L,
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic",
        Hawaiian = race == "Native Hawaiian or Pacific Islander",
        Native = race == "American Indian or Alaskan Native",
        White = race == "White",
        EL_T1234 = pmax(EL, EL_T1_T2, T3_T4, na.rm = TRUE),
        Super = as.numeric(BHN == 1 | ED == 1 | SWD == 1 | EL_T1234 == 1))

int_math_systems <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

# ACT Sub for HS Success Rate AMOs
ACT_substitution <- read_csv("N:/ORP_accountability/data/2017_ACT/Pre-Appeals Data/school_act_substitution_2017.csv") %>%
    transmute(system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, subgroup = "All", valid_tests, n_on_track = n_met_benchmark)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "BHN", "ED", "SWD", "EL_T1234", "Super", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White")) {
    
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
            grade %in% 9:12 ~ "9th through 12th"
        ),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            TRUE ~ subgroup
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(system, school, subject, grade, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress for subjects with < 30 valid tests
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    left_join(pools, by = c("system", "school"))

# AMOs by English, Math, Science
subject_targets <- subjects_suppressed %>%
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

write_csv(subject_targets, "N:/ORP_accountability/projects/2018_amo/school_subject.csv")

# One year success rates
success_rate_targets <- subjects_suppressed %>%
    group_by(system, school, pool, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(year = 2018, system, school, pool, subject = "Success Rate", subgroup,
        valid_tests_prior = valid_tests,
        pct_on_mastered_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests_prior, pct_on_mastered_prior),
        AMO_target_4 = amo_target(valid_tests_prior, pct_on_mastered_prior, double = TRUE))

write_csv(success_rate_targets, path = "N:/ORP_accountability/projects/2018_amo/school_success_rate.csv", na = "")
