library(acct)
library(haven)
library(readxl)
library(tidyverse)

numeric_subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners", "Students with Disabilities",
    "Black or African American", "Hispanic", "American Indian or Alaska Native")

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

student_level <- read_dta("K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10192017.dta") %>%
# Residential Facility students are dropped from system level
    filter(residential_facility == 0 | is.na(residential_facility)) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = ell, EL_T1_T2 = ell_t1t2) %>%
    mutate(year = 2017,
        grade = if_else(is.na(grade), 0, grade),
        n_below = if_else(performance_level %in% c("1. Below", "1. Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("2. Approaching", "2. Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_),
        All = 1L,
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic",
        Native = race == "American Indian or Alaskan Native",
        EL_T1_T2 = if_else(EL == 1, 1, EL_T1_T2)
    ) %>%
    filter(subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc)) %>%
    mutate(subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        ),
        grade = case_when(
            grade %in% 3:8 ~ "3rd through 8th",
            grade %in% c(0, 9:12) ~ "9th through 12th"
        )
    )

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "BHN", "ED", "SWD", "EL_T1_T2", "Black", "Hispanic", "Native")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1")) %>%
        group_by(year, system, subject, grade) %>%
        summarise_at(c("valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1")) %>%
        group_by(year, system, school, subject, grade) %>%
        summarise_at(c("valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1")) %>%
        group_by(year, subject, grade) %>%
        summarise_at(c("valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

hs_subjects <- collapse %>%
    filter(subject %in% c(math_eoc, english_eoc, science_eoc)) %>%
    mutate(subject = case_when(
            subject %in% math_eoc ~ "HS Math",
            subject %in% english_eoc ~ "HS English",
            subject %in% science_eoc ~ "HS Science"
        )
    ) %>%
    group_by(year, system, school, subject, grade, subgroup) %>%
    summarise_at(c("valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup()

numeric <- collapse %>%
    bind_rows(hs_subjects) %>%
    rename(valid_tests = valid_test) %>%
    mutate(system = if_else(is.na(system), 0, system),
        school = if_else(is.na(school), 0, school),
        pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        flag_below = pct_below != 0 & n_below == 0,
        pct_approaching = if_else(flag_below, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(flag_below, 0, pct_below),
        flag_approaching = pct_approaching != 0 & n_approaching == 0,
        pct_on_track = if_else(flag_approaching, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(flag_approaching, 0, pct_approaching),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL_T1_T2" ~ "English Learners",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            TRUE ~ subgroup
        )
    ) %>%
    select(year, system, school, subject, grade, subgroup, valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

# Merge on names
system_names <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/system_name_crosswalk.csv")

# Output files
numeric %>%
    left_join(system_names, by = "system") %>%
    transmute(Level = case_when(
            system == 0 & school == 0 ~ "State",
            system != 0 & school == 0 ~ "System",
            system != 0 & school != 0 ~ "School"
        ),
        year, system, system_name, school, school_name = "", subject, grade,
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup),
        participation_rate_1yr = NA, participation_rate_2yr = NA, participation_rate_3yr = NA,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered,
        pct_below_approaching = 100 - pct_on_mastered, pct_on_mastered,
        amo_target = NA, gap_size = NA, gap_target = NA, grad_cohort = NA, grad_rate = NA,
        tvaas = NA, upper_bound_ci = NA, red_perc_below_or_bsc_1yr = NA, red_perc_below_or_bsc_2yr = NA,
        red_perc_below_or_bsc_3yr = NA, year_to_year_diff = NA, maas_adjusted_amo_target = NA,
        maas_adjusted_gap_target = NA, maas_adjusted_prior_pct_prof_adv = NA, maas_adj_year_to_year_diff = NA) %>%
    arrange(Level, system, school, subject, grade, subgroup) %>%
    write_csv("K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ReportCard_Numeric_Part_Prof.csv", na = "")

numeric %>%
    filter(Level = "System") %>%
    select(year, system, subject, grade, subgroup, valid_tests,
        n_below, n_approaching, n_on_track, n_mastered,
        pct_on_track, pct_mastered, pct_on_mastered, pct_approaching, pct_below) %>%
    write_csv("K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ReportCard_district_complete.csv", na = "")
