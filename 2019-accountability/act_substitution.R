library(acct)
library(readxl)
library(tidyverse)

hs_crosswalk <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Helpful Documents/ACT_xwalkAug2018rev.csv")

# Current Year ACT Junior Day File
act_highest_math <- read_excel("N:/Assessment_Data Returns/ACT/2018-19/2019 Spring/Spring 2019 Final File/Spring 2019 Final School Day ACT.xlsx") %>%
    janitor::clean_names() %>%
    mutate_at(
        .vars = c("act_h_s_code_number", "act_scale_score_composite", "act_scale_score_mathematics", "state_assigned_student_id_number"), 
        .f = as.numeric
    ) %>%
    filter(not_na(state_assigned_student_id_number), test_location != "M", grade_level == 11) %>%
    inner_join(hs_crosswalk, by = c("act_h_s_code_number" = "acthscode")) %>%
    filter(system != 99855) %>%
    group_by(state_assigned_student_id_number) %>%
    mutate(highest = max(act_scale_score_mathematics)) %>%
    ungroup() %>%
    filter(act_scale_score_mathematics == highest) %>%
    transmute(
        system,
        school,
        first_name = student_first_name,
        last_name = student_last_name,
        grade = grade_level,
        subject = "ACT Math",
        state_student_id = state_assigned_student_id_number,
        act_subscore = act_scale_score_mathematics
    )

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv")

# Student ACT Substitution File
substitution_student <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")) %>%
    anti_join(act_highest_math, ., by = "state_student_id") %>%
    arrange(system, school, state_student_id)

write_csv(substitution_student, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_student.csv", na = "")

# Split Student Level File
district_numbers <- sort(unique(substitution_student$system))

substitution_student %>%
    split(., .$system) %>%
    walk2(
        .x = ., 
        .y = district_numbers, 
        .f = ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_final_accountability_files/split/", .y, "_ACTSubstitutionStudentLevelFile_20Jun2019.csv"), na = "")
    )

school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

# School ACT Substitution File
substitution_school <- substitution_student %>%
    filter(not_na(school)) %>%
    mutate(
        valid_tests = 1,
        n_met_benchmark = act_subscore >= 22,
        n_not_met_benchmark = act_subscore < 22
    ) %>%
    group_by(system, school, subject) %>%
    summarise_at(c("valid_tests", "n_met_benchmark", "n_not_met_benchmark"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
        pct_met_benchmark = round5(100 * n_met_benchmark/valid_tests, 1),
        pct_not_met_benchmark = 100 - pct_met_benchmark
    ) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, everything())

write_csv(substitution_school, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv", na = "")

district_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv") %>%
    select(system, system_name) %>%
    distinct()

# District ACT Substitution File
substitution_district <- substitution_student %>%
    mutate(
        valid_tests = 1,
        n_met_benchmark = as.integer(act_subscore >= 22),
        n_not_met_benchmark = as.integer(act_subscore < 22)
    ) %>%
    group_by(system, subject) %>%
    summarise_at(c("valid_tests", "n_met_benchmark", "n_not_met_benchmark"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
        pct_met_benchmark = round5(100 * n_met_benchmark/valid_tests, 1),
        pct_not_met_benchmark = 100 - pct_met_benchmark
    ) %>%
    left_join(district_names, by = "system") %>%
    select(system, system_name, everything())

write_csv(substitution_district, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_district.csv", na = "")

# State ACT Substitution File
substitution_state <- substitution_student %>%
    mutate(
        valid_tests = 1,
        n_met_benchmark = as.integer(act_subscore >= 22),
        n_not_met_benchmark = as.integer(act_subscore < 22)
    ) %>%
    group_by(subject) %>%
    summarise_at(c("valid_tests", "n_met_benchmark", "n_not_met_benchmark"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
        system = 0,
        system_name = "State of Tennessee",
        pct_met_benchmark = round5(100 * n_met_benchmark/valid_tests, 1),
        pct_not_met_benchmark = 100 - pct_met_benchmark
    ) %>%
    select(system, system_name, everything())

write_csv(substitution_state, "N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_state.csv", na = "")
