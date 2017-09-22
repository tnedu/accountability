library(haven)
library(tidyverse)

student_level <- read_dta("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_09142017.dta") %>%
    filter(!grade %in% c(1, 2)) %>%
    # Homebound and Residential Facility students are dropped from school level
    filter(homebound == 0 | is.na(homebound)) %>%
    filter(residential_facility != 1 | is.na(residential_facility)) %>%
    # Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = ell, EL_T1_T2 = ell_t1t2) %>%
    mutate(year = 2017,
        grade = if_else(is.na(grade), 0, grade),
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
        EL_T1_T2 = if_else(EL == 1, 1, EL_T1_T2),
        Non_BHN = BHN == 0,
        Non_ED = ED == 0,
        Non_SWD = SWD == 0,
        Non_EL = EL == 0,
        Non_EL_T1_T2 = EL_T1_T2 == 0,
        Super = (BHN == 1 | ED == 1 | SWD == 1 | EL_T1_T2 == 1L)) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD", "EL", "EL_T1_T2",
        "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Non_EL_T1_T2", "Super"), as.integer)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
    "EL", "EL_T1_T2", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Non_EL_T1_T2", "Super")) {

    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, original_subject) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(collapse, .)

    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, original_subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)

}

school_base <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round(100 * n_approaching/valid_tests + 1e-10, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round(100 * n_on_track/valid_tests + 1e-10, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round(100 * n_mastered/valid_tests + 1e-10, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round(100 - pct_approaching - pct_on_track - pct_mastered + 1e-10, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round(100 * (n_on_track + n_mastered)/valid_tests + 1e-10, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        flag_below = pct_below != 0 & n_below == 0,
        pct_approaching = if_else(flag_below, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(flag_below, 0, pct_below),
        flag_approaching = pct_approaching != 0 & n_approaching == 0,
        pct_on_track = if_else(flag_approaching, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(flag_approaching, 0, pct_approaching),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL" ~ "English Learners",
            subgroup == "EL_T1_T2" ~ "English Learners with T1/T2",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "Non_EL" ~ "Non-English Learners",
            subgroup == "Non_EL_T1_T2" ~ "Non-English Learners/T1 or T2",
            subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup)
    ) %>%
    select(year, system, school, subject, grade, subgroup, enrolled, tested, valid_tests,
        n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

# Append ACT, grad, 2016 base
ACT <- read_dta("K:/ORP_accountability/data/2016_ACT/ACT_school2017.dta") %>%
    transmute(year = 2017, system, school, subject = "ACT Composite", grade = "All Grades",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with T1/T2",
            subgroup == "Non-English Language Learners" ~ "Non-English Learners",
            TRUE ~ subgroup),
        enrolled, tested, valid_tests, n_below = n_below19, n_on_track = n_21_orhigher,
        ACT_21_and_above = pct_21_orhigher, ACT_18_and_below = pct_below19)

ACT_prior <- read_dta("K:/ORP_accountability/data/2015_ACT/ACT_school2016.dta") %>%
    filter(school != -9999) %>%
    transmute(year = 2016, system, school, subject = "ACT Composite", grade = "All Grades",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with T1/T2",
            subgroup == "Non-English Language Learners" ~ "Non-English Learners",
            TRUE ~ subgroup),
        enrolled, tested, valid_tests, n_below = n_below19, n_on_track = n_21_orhigher,
        ACT_21_and_above = pct_21_orhigher_reporting, ACT_18_and_below = pct_below19)

ACT_substitution <- read_csv("K:/ORP_accountability/data/2017_ACT/school_act_substitution_2017.csv") %>%
    filter(school != -9999) %>%
    mutate(grade = as.character(grade)) %>%
    rename(n_approaching = n_not_met_benchmark, n_on_track = n_met_benchmark,
        pct_approaching = pct_not_met_benchmark, pct_on_track = pct_met_benchmark)

grad_prior <- read_dta("K:/ORP_accountability/data/2015_graduation_rate/school_grad_rate2016.dta") %>%
    filter(system != 90) %>%
    transmute(year = 2016, system, school, subject, grade = "All Grades",
        subgroup = case_when(
            subgroup == "Black" ~ "Black or African American",
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with T1/T2",
            subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/T1 or T2",
            subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native American" ~ "American Indian or Alaska Native",
            TRUE ~ subgroup),
        grad_cohort, grad_count, grad_rate, dropout_count = drop_count, dropout_rate = drop_rate)

grad <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta") %>%
    filter(system != 90) %>%
    transmute(year, system, school, subject, grade = "All Grades",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with T1/T2",
            subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners/T1 or T2",
            subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            TRUE ~ subgroup),
        grad_count, grad_cohort, grad_rate, dropout_count = drop_count, dropout_rate)

base_2016 <- read_csv("K:/ORP_accountability/data/2016_accountability/school_base_with_unaka_correction_2016.csv") %>%
    select(year, system, school, subject, grade, subgroup,
        enrolled, enrolled_part_1 = enrolled_part_1_only, enrolled_part_2 = enrolled_part_2_only, enrolled_both,
        tested, tested_part_1 = tested_part_1_only, tested_part_2 = tested_part_2_only, tested_both,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

# Output file
base_2017 <- bind_rows(base_2016, school_base, ACT, ACT_prior, ACT_substitution, grad, grad_prior) %>%
# Drop schools with only prior year data
    group_by(system, school) %>%
    mutate(temp = max(year)) %>%
    filter(temp == 2017) %>%
    ungroup() %>%
    select(-temp) %>%
    arrange(desc(year), system, school, subject, grade, subgroup) %>%
    select(year, system, school, everything()) %>%
    mutate(grade = if_else(grade == "0", "Missing Grade", grade))

write_csv(base_2017, path = "K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_sep18.csv", na = "")
