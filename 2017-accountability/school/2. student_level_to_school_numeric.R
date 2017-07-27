library(haven)
library(readxl)
library(tidyverse)

numeric_subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners", "Students with Disabilities", "Super Subgroup")
math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")

student_level <- read_dta("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final.dta") %>%
    filter(!grade %in% c(1, 2), greater_than_60_pct == "Y") %>%
    mutate(year = 2017) %>%
    # Homebound and Residential Facility students are dropped from school level
    filter(homebound == 0 | is.na(homebound)) %>%
    # filter(residential_facility != 1) %>%
    # Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = ell, EL_T1_T2 = ell_t1t2) %>%
    mutate(n_below = if_else(performance_level %in% c("1. Below", "1. Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("2. Approaching", "2. Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_),
        All = 1,
        EL_T1_T2 = if_else(EL == 1, 1, EL_T1_T2),
        Super = as.numeric(BHN == 1 | ED == 1 | SWD == 1 | EL_T1_T2 == 1)) %>%
    # Numeric subject/grade combinations
    filter(subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%
    mutate(grade = as.character(grade),
        grade = if_else(grade %in% c("3", "4", "5"), "3rd through 5th", grade),
        grade = if_else(grade %in% c("6", "7", "8"), "6th through 8th", grade),
        grade = if_else(grade %in% c("Missing Grade", "9", "10", "11", "12"), "9th through 12th", grade),
        subject = if_else(subject %in% math_eoc, "HS Math", subject),
        subject = if_else(subject %in% english_eoc, "HS English", subject))

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "BHN", "ED", "SWD", "EL_T1_T2", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1")) %>%
        group_by(year, system, school, subject, grade) %>%
        summarise_at(c("valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

school_numeric <- collapse %>%
    rename(valid_tests = valid_test) %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round(100 * n_approaching/valid_tests + 1e-10, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round(100 * n_on_track/valid_tests + 1e-10, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round(100 * n_mastered/valid_tests + 1e-10, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round(100 - pct_approaching - pct_on_track - pct_mastered + 1e-10, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round(100 * (n_on_track + n_mastered)/valid_tests + 1e-10, 1), NA_real_),
        # Fix % B/A/O if there are no n_B/A/O
        flag_below = as.integer(pct_below != 0 & n_below == 0),
        pct_approaching = if_else(flag_below == 1L, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(flag_below == 1L, 0, pct_below),
        flag_approaching = as.integer(pct_approaching != 0 & n_approaching == 0),
        pct_on_track = if_else(flag_approaching == 1L, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(flag_approaching == 1L, 0, pct_approaching)) %>%
    mutate_at("subgroup", funs(recode(.,
        "All" = "All Students",
        "BHN" = "Black/Hispanic/Native American",
        "ED" = "Economically Disadvantaged",
        "EL_T1_T2" = "English Learners",
        "Super" = "Super Subgroup",
        "SWD" = "Students with Disabilities"))) %>%
    select(year, system, school, subject, grade, subgroup, valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

# Append ACT, grad
ACT <- read_dta("K:/ORP_accountability/data/2016_ACT/ACT_school2017.dta") %>%
    transmute(year = 2017, system, school, subject = "ACT Composite", grade = "All Grades", subgroup,
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        enrolled, participation_rate_1yr = participation_rate, valid_tests, 
        n_on_track = n_21_orhigher,
        n_below = n_below19,
        pct_on_mastered = pct_21_orhigher,
        pct_below = pct_below19) %>%
    filter(subgroup %in% numeric_subgroups)

grad <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta") %>%
    transmute(year, system, school, subject, grade = "All Grades", 
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        grad_count, grad_cohort, grad_rate) %>%
    filter(system != 90, subgroup %in% numeric_subgroups)

# Participation Rate from Base
base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(grade != "All Grades",
        subgroup %in% c(numeric_subgroups, "English Learners with T1/T2"),
        subgroup != "English Learners",
        subject %in% c("Math", "ELA", math_eoc, english_eoc)) %>%
    mutate(subject = if_else(subject %in% math_eoc & grade %in% c("6", "7", "8"), "Math", subject),
        subject = if_else(subject %in% english_eoc & grade %in% c("6", "7", "8"), "ELA", subject),
        grade = if_else(grade %in% c("3", "4", "5"), "3rd through 5th", grade),
        grade = if_else(grade %in% c("6", "7", "8"), "6th through 8th", grade),
        grade = if_else(grade %in% c("Missing Grade", "9", "10", "11", "12"), "9th through 12th", grade)) %>%
    select(year, system, school, subject, grade, subgroup, matches("enrolled|tested")) %>%
    mutate_at(vars(matches("enrolled|tested")), as.numeric) %>%
    mutate(subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup),
        subject = if_else(grade %in% c("3rd through 5th", "6th through 8th") & subject %in% math_eoc, "Math", subject),
        subject = if_else(grade %in% c("3rd through 5th", "6th through 8th") & subject %in% english_eoc, "ELA", subject),
        subject = if_else(grade == "9th through 12th" & subject %in% c("ACT Math", math_eoc), "HS Math", subject),
        subject = if_else(grade == "9th through 12th" & subject %in% c("ACT English", english_eoc), "HS English", subject))

participation_1yr <- base %>%
    rowwise() %>%
    mutate(enrolled = sum(enrolled, enrolled_part_1, enrolled_part_2, enrolled_both, na.rm = TRUE),
        tested = sum(tested, tested_part_1, tested_part_2, tested_both, na.rm = TRUE)) %>%
    ungroup() %>% 
    group_by(year, system, school, subject, grade, subgroup) %>%
    summarise_at(c("enrolled", "tested"), sum) %>%
    ungroup() %>%
    transmute(year, system, school, subject, grade, subgroup, enrolled, tested,
        participation_rate_1yr = if_else(enrolled != 0, round(100 * tested/enrolled + 1e-10), NA_real_))

participation <- participation_1yr %>%
    group_by(system, school, subject, grade, subgroup) %>%
    summarise_at(c("enrolled", "tested"), sum) %>%
    ungroup() %>%
    mutate(year = 2017,
        participation_rate_2yr = if_else(enrolled != 0, round(100 * tested/enrolled + 1e-10), NA_real_)) %>%
    select(-enrolled, -tested) %>%
    full_join(participation_1yr, by = c("year", "system", "school", "subject", "grade", "subgroup")) %>%
    select(year, system, school, subject, grade, subgroup, enrolled, participation_rate_1yr, participation_rate_2yr)

# 2016 ACT and Grad
ACT_prior <- read_dta("K:/ORP_accountability/data/2015_ACT/ACT_school2016.dta") %>%
    transmute(year = 2016, system, school, subject = "ACT Composite", grade = "All Grades",
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        valid_tests,
        pct_below = pct_below19,
        pct_on_mastered = pct_21_orhigher_reporting) %>%
    filter(subgroup %in% numeric_subgroups)

grad_prior <- read_dta("K:/ORP_accountability/data/2015_graduation_rate/school_grad_rate2016.dta") %>%
    transmute(year = 2016, system, school, subject, grade = "All Grades",
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        grad_cohort, grad_count, grad_rate) %>%
    filter(subgroup %in% numeric_subgroups)

# 2016 numeric
numeric_2016 <- read_excel("K:/ORP_accountability/data/2016_accountability/school_numeric_with_unaka_correction_2016.xlsx") %>%
    transmute(year, system, school, subject, grade,
        subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup),
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>%
    bind_rows(ACT_prior, grad_prior)

# 2016 AMO targets
AMOs <- read_excel("K:/ORP_accountability/data/2016_AMOs/2016_school_eoc_amos.xlsx") %>%
    filter(subgroup %in% c(numeric_subgroups, "English Learners with T1/T2"),
        subgroup != "English Learners") %>%
    mutate(subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup)) %>%
    transmute(year = 2016, system, school, subject, grade, subgroup,
        AMO_target_below, AMO_target_below_4, AMO_target, AMO_target_4)

# Put everything together
numeric_2017 <- school_numeric %>%
    bind_rows(numeric_2016) %>%
    full_join(participation, by = c("year", "system", "school", "subgroup", "subject", "grade")) %>%
    bind_rows(ACT, grad) %>%
    left_join(AMOs, by = c("year", "system", "school", "subject", "grade", "subgroup"))
    
# Clean and output numeric file
output <- numeric_2017 %>%
    arrange(desc(year), system, school, subject, grade, subgroup) %>%
    select(year, system, school, subject, grade, subgroup, enrolled, participation_rate_1yr, participation_rate_2yr,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered,
        pct_on_mastered, AMO_target_below, AMO_target_below_4, AMO_target, AMO_target_4,
        grad_count, grad_cohort, grad_rate)

# Output file
write_csv(output, path = "K:/ORP_accountability/data/2017_final_accountability_files/school_numeric_2017.csv", na = "")
