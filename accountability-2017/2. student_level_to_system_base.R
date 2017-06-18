library(tidyverse)

student_level <- read_csv("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017.csv") %>%
    mutate(year = 2017,
    # MSAA tests above grade 9 are reassigned to EOCs
    original_subject = if_else(original_subject == "Math" & test == "MSAA" & grade >= 9 &
        system %in% c(30, 60, 80, 100, 110, 140, 150, 190, 440, 580, 590, 710, 800, 821, 850, 890, 930),
        "Integrated Math I", subject),
    original_subject = if_else(original_subject == "Math" & test == "MSAA" & grade >= 9 &
        !system %in% c(30, 60, 80, 100, 110, 140, 150, 190, 440, 580, 590, 710, 800, 821, 850, 890, 930),
        "Algebra I", subject),
    original_subject = if_else(original_subject == "ELA" & test == "MSAA" & grade >= 9, "English II", subject)) %>%
    # Residential Facility students are dropped from system level
    filter(!residential_facility == 1) %>%
    # Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1_T2 = el_t1_t2) %>%
    mutate(n_below = if_else(proficiency_level %in% c("1. Below", "1. Below Basic"), 1L, NA_integer_),
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
        EL_T1_T2 = if_else(EL == 1L, 1L, EL_T1_T2),
        Non_ED = ED == 0L,
        Non_SWD = SWD == 0L,
        Non_EL = EL == 0L,
        Non_EL_T1_T2 = (EL == 0L & EL_T1_T2 == 0L),
        Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1_T2 == 1L)) %>%
    mutate_each(funs(as.integer), Asian, Black, Hispanic, Hawaiian, Native, White, BHN, ED, SWD, EL, EL_T1_T2,
        Non_ED, Non_SWD, Non_EL, Non_EL_T1_T2, Super)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
    "EL", "EL_T1_T2", "Non_ED", "Non_SWD", "Non_EL", "Non_EL_T1_T2", "Super")) {

    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, system_name, original_subject) %>%
        summarise_each(funs(sum(., na.rm = TRUE)), enrolled, tested, valid_tests = valid_test, starts_with("n_")) %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(collapse, .)

    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, system_name, original_subject, grade) %>%
        summarise_each(funs(sum(., na.rm = TRUE)), enrolled, tested, valid_tests = valid_test, starts_with("n_")) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)

}

system_base <- collapse %>%
    rename(subject = original_subject) %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round(100 * n_approaching/valid_tests + 1e-10, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round(100 * n_on_track/valid_tests + 1e-10, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round(100 * n_mastered/valid_tests + 1e-10, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round(100 - pct_approaching - pct_on_track - pct_mastered + 1e-10, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round(100 * (n_on_track + n_mastered)/valid_tests + 1e-10, 1), NA_real_)) %>%
    # Fix % B/A/O if there are no n_B/A/O
    mutate(flag_below = as.integer(pct_below != 0 & n_below == 0),
        pct_approaching = if_else(flag_below == 1L, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(flag_below == 1L, 0, pct_below),
        flag_approaching = as.integer(pct_approaching != 0 & n_approaching == 0),
        pct_on_track = if_else(flag_approaching == 1L, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(flag_approaching == 1L, 0, pct_approaching)) %>%
    # Check everything sums to 100
    rowwise() %>%
    mutate(pct_total = sum(pct_below, pct_approaching, pct_on_track, pct_mastered, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-flag_below, -flag_approaching, -pct_total) %>%
    mutate(subgroup = if_else(subgroup == "All", "All Students", subgroup),
        subgroup = if_else(subgroup == "BHN", "Black/Hispanic/Native American", subgroup),
        subgroup = if_else(subgroup == "Black", "Black or African American", subgroup),
        subgroup = if_else(subgroup == "ED", "Economically Disadvantaged", subgroup),
        subgroup = if_else(subgroup == "EL", "English Learners", subgroup),
        subgroup = if_else(subgroup == "EL_T1_T2", "English Learners with T1/T2", subgroup),
        subgroup = if_else(subgroup == "Hawaiian", "Native Hawaiian or Other Pacific Islander", subgroup),
        subgroup = if_else(subgroup == "Native", "American Indian or Alaska Native", subgroup),
        subgroup = if_else(subgroup == "Non_ED", "Non-Economically Disadvantaged", subgroup),
        subgroup = if_else(subgroup == "Non_EL", "Non-English Learners", subgroup),
        subgroup = if_else(subgroup == "Non_EL_T1_T2", "Non-English Learners/T1 or T2", subgroup),
        subgroup = if_else(subgroup == "Non_SWD", "Non-Students with Disabilities", subgroup),
        subgroup = if_else(subgroup == "Super", "Super Subgroup", subgroup),
        subgroup = if_else(subgroup == "SWD", "Students with Disabilities", subgroup)) %>%
    select(year, system, system_name, subject, grade, subgroup, enrolled, tested, valid_tests,
        n_below, n_approaching, n_on_track, n_mastered, pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

# Append ACT, grad, 2016 base
ACT <- haven::read_dta("K:/ORP_accountability/data/2016_ACT/ACT_district2017.dta") %>%
    transmute(year = 2017, system, subject, grade, subgroup,
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup),
        subgroup = if_else(subgroup == "Non-English Language Learners", "Non-English Learners/T1 or T2", subgroup),
        enrolled, tested, valid_tests,
        n_on_track = n_21_orhigher,
        n_below = n_below19,
        ACT_21_and_above = pct_21_orhigher,
        ACT_18_and_below = pct_below19)

grad <- haven::read_dta("K:/ORP_accountability/data/2016_graduation_rate/District_grad_rate2017_JP.dta") %>%
    select(year, system, system_name, subject, grade, subgroup, grad_count, grad_cohort, grad_rate) %>%
    mutate(subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup))

base_2016 <- readxl::read_excel("K:/ORP_accountability/data/2016_accountability/system_base_with_unaka_correction_2016.xlsx") %>%
    select(year, system, system_name, subject, grade, subgroup,
        enrolled, enrolled_part_1 = enrolled_part_1_only, enrolled_part_2 = enrolled_part_2_only, enrolled_both,
        tested, tested_part_1 = tested_part_1_only, tested_part_2 = tested_part_2_only, tested_both,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

# Output file
base_2017 <- bind_rows(base_2016, system_base, ACT, grad) %>%
    arrange(desc(year), system, subject, grade, subgroup)

write_csv(base_2017, path = "K:/ORP_accountability/data/2017_final_accountability_files/system_base_2017.csv", na = "")
