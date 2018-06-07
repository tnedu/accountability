library(acct)
library(haven)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

student_level <- read_dta("N:/ORP_accountability/projects/2016_student_level_file/state_student_level_2016.dta") %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1_T2 = el_t1_t2) %>%
    mutate(year = 2016,
        grade = if_else(is.na(grade), 0, grade),
        test = if_else(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test),
        test = if_else(test == "Achievement" & original_subject %in% c(math_eoc, english_eoc, science_eoc, "US History"), "EOC", test),
        original_subject = if_else(test == "MSAA", subject, original_subject),
        n_below = if_else(proficiency_level %in% c("1. Below", "1. Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(proficiency_level %in% c("2. Approaching", "2. Basic"), 1L, NA_integer_),
        n_on_track = if_else(proficiency_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(proficiency_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_),
        All = 1L,
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic/Latino",
        Hawaiian = race == "Native Hawaiian/Pac. Islander",
        Native = race == "American Indian/Alaska Native",
        White = race == "White",
        T1_T2 = EL_T1_T2,
        EL_T1_T2 = if_else(EL == 1, 1, EL_T1_T2),
        Non_BHN = BHN == 0,
        Non_ED = ED == 0,
        Non_SWD = SWD == 0,
        Non_EL = EL_T1_T2 == 0,
        Super = (BHN == 1 | ED == 1 | SWD == 1 | EL_T1_T2 == 1)) %>%
    rowwise() %>%
    mutate(enrolled = sum(enrolled, enrolled_part_1_only, enrolled_part_2_only, enrolled_both, na.rm = TRUE),
        tested = sum(tested, tested_part_1_only, tested_part_2_only, tested_both, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
        "EL", "T1_T2", "EL_T1_T2", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Super"), as.integer)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
    "EL", "T1_T2", "EL_T1_T2", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, test, original_subject) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(collapse, .)
    
    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, test, original_subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

school_assessment <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        pct_approaching = if_else(pct_below != 0 & n_below == 0, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(pct_below != 0 & n_below == 0, 0, pct_below),
        pct_on_track = if_else(pct_approaching != 0 & n_approaching == 0, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(pct_approaching != 0 & n_approaching == 0, 0, pct_approaching),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL" ~ "English Learners",
            subgroup == "T1_T2" ~ "English Learner Transitional 1-2",
            subgroup == "EL_T1_T2" ~ "English Learners with Transitional 1-2",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "Non_EL" ~ "Non-English Learners",
            subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        )
    ) %>%
    select(year, system, school, test, subject, grade, subgroup, enrolled, tested, valid_tests,
        n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

ACT_substitution <- read_csv("N:/ORP_accountability/data/2016_ACT/school_act_substitution_2016.csv") %>%
    mutate(grade = as.character(grade)) %>%
    rename(n_approaching = n_not_met_benchmark, n_on_track = n_met_benchmark,
        pct_approaching = pct_not_met_benchmark, pct_on_track = pct_met_benchmark)

ACT <- read_dta("N:/ORP_accountability/data/2015_ACT/ACT_school2016.dta") %>%
    transmute(year = 2016, system, school, subject = "ACT Composite", grade = "All Grades",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-2",
            subgroup == "Non-English Language Learners" ~ "Non-English Learners",
            TRUE ~ subgroup
        ),
        enrolled, tested, valid_tests, n_below = n_below19, n_on_track = n_21_orhigher,
        ACT_21_and_above = pct_21_orhigher_reporting, ACT_18_and_below = pct_below19)

grad <- read_dta("N:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2016.dta") %>%
    transmute(year = 2016, subject, grade = "All Grades",
        subgroup = case_when(
            subgroup == "Black" ~ "Black or African American",
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-2",
            subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners",
            subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native American" ~ "American Indian or Alaska Native",
            TRUE ~ subgroup
        ),
        grad_cohort, grad_count, grad_rate, dropout_count = drop_count, dropout_rate = drop_rate)

assessment_2016 <- bind_rows(school_assessment, ACT, ACT_substitution, grad) %>%
    arrange(desc(year), system, school, subject, grade, subgroup) %>%
    mutate(grade = if_else(grade == "0", "Missing Grade", grade)) %>%
    select(year, system, school, everything())

write_csv(assessment_2016, "N:/ORP_accountability/data/2018_final_accountability_files/2016_school_assessment_file.csv", na = "")
