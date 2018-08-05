library(acct)
library(readxl)
library(tidyverse)

crosswalk <- read_excel("N:/Assessment_Data Returns/ACT/2017-18/2018 Spring/Tennessee Crosswalk - spring 2018 - 07-18-18.xlsx") %>%
    janitor::clean_names() %>%
    filter(str_length(local_site_code) == 10)

hs_crosswalk <- bind_cols(
    crosswalk["act_organization_code"],
    as_data_frame(str_split_fixed(crosswalk$local_site_code, " ", n = 2))
) %>%
    transmute(act_organization_code = as.numeric(act_organization_code), system = as.integer(V1), school = as.integer(V2)) %>%
    filter()

act_highest_read <- read_excel("N:/Assessment_Data Returns/ACT/2017-18/2018 Spring/20180521_ACT_JuniorDayPreLimFile_SY2017-18_Whalen_v1.xlsx") %>%
    filter(!is.na(state_stud_id), test_location != "M", grade == 11) %>%
    inner_join(hs_crosswalk, by = c("acthscode" = "act_organization_code")) %>%
    filter(system != 99855) %>%
    group_by(state_stud_id) %>%
    mutate(highest = max(act_read)) %>%
    filter(act_read == highest) %>%
    transmute(system, school, first_name, last_name, grade, subject = "ACT Reading", state_student_id = state_stud_id, act_subscore = act_read)

act_highest_math <- read_excel("N:/Assessment_Data Returns/ACT/2017-18/2018 Spring/20180521_ACT_JuniorDayPreLimFile_SY2017-18_Whalen_v1.xlsx") %>%
    filter(!is.na(state_stud_id), test_location != "M", grade == 11) %>%
    inner_join(hs_crosswalk, by = c("acthscode" = "act_organization_code")) %>%
    filter(system != 99855) %>%
    group_by(state_stud_id) %>%
    mutate(highest = max(act_math)) %>%
    filter(act_math == highest) %>%
    transmute(system, school, first_name, last_name, grade, subject = "ACT Math", state_student_id = state_stud_id, act_subscore = act_math)

student_level <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii")

substitution_math <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")) %>%
    anti_join(act_highest_math, ., by = "state_student_id")

substitution_read <- student_level %>%
    filter(original_subject %in% c("English I", "English II", "English III")) %>%
    anti_join(act_highest_read, ., by = "state_student_id")

substitution_student <- bind_rows(substitution_math, substitution_read) %>%
    arrange(system, school, state_student_id, subject)
    
write_csv(substitution_student, "N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_student.csv", na = "")

school_names <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/school_names.csv")

substitution_school <- bind_rows(substitution_math, substitution_read) %>%
    mutate(valid_tests = 1, n_met_benchmark = as.integer(act_subscore >= 22), n_not_met_benchmark = as.integer(act_subscore < 22)) %>%
    group_by(system, school, subject) %>%
    summarise_at(c("valid_tests", "n_met_benchmark", "n_not_met_benchmark"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(pct_met_benchmark = round5(100 * n_met_benchmark/valid_tests, 1), pct_not_met_benchmark = 100 - pct_met_benchmark) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, everything())

write_csv(substitution_school, "N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_school.csv", na = "")

district_names <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/system_names.csv")

substitution_district <- bind_rows(substitution_math, substitution_read) %>%
    mutate(valid_tests = 1, n_met_benchmark = as.integer(act_subscore >= 22), n_not_met_benchmark = as.integer(act_subscore < 22)) %>%
    group_by(system, subject) %>%
    summarise_at(c("valid_tests", "n_met_benchmark", "n_not_met_benchmark"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(pct_met_benchmark = round5(100 * n_met_benchmark/valid_tests, 1), pct_not_met_benchmark = 100 - pct_met_benchmark) %>%
    left_join(district_names, by = "system") %>%
    select(system, system_name, everything())

write_csv(substitution_district, "N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_district.csv", na = "")

substitution_state <- bind_rows(substitution_math, substitution_read) %>%
    mutate(valid_tests = 1, n_met_benchmark = as.integer(act_subscore >= 22), n_not_met_benchmark = as.integer(act_subscore < 22)) %>%
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

write_csv(substitution_state, "N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_state.csv", na = "")
