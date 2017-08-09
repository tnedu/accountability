library(tidyverse)

instructional_days <- readxl::read_excel("K:/ORP_accountability/data/2017_chronic_absenteeism/School Level - Instructional days.xlsx") %>%
    rename(year = SCHOOL_YEAR, system_name = DISTRICT_NAME, system = DISTRICT_NO,
        school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)

attendance <- read_csv("K:/ORP_accountability/data/2017_chronic_absenteeism/ATTENDANCE REPORT 2016-17.txt",
    col_names = c(
        "instructional_program_num",
        "system",
        "school",
        "year",
        "service_descr",
        "instructional_program_id",
        "student_key",
        "student_ssn",
        "student_pin",
        "first_name",
        "middle_name",
        "last_name",
        "begin_date",
        "end_date",
        "enrollment_reason",
        "withdrawal_reason",
        "type_of_service",
        "date_of_birth",
        "ethnic_origin",
        "ethnic_description",
        "gender",
        "gender_description",
        "school_building_id",
        "district_building_id",
        "grade",
        "ethnicity",
        "race_i",
        "race_a",
        "race_p",
        "race_b",
        "race_w",
        "race",
        "count_unexcused",
        "count_unexcused_trans",
        "count_excused",
        "count_excused_trans",
        "count_total",
        "isp_days")) %>%
    # For students with same system, school, student ID, enrollment dates, take maximum instructional program days
    group_by(system, school, student_key, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days,
    # take maximum number of absences (Doesn't drop any records)
    group_by(system, school, student_key, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days, absences,
    # take maximum instuctional program number
    group_by(system, school, student_key, begin_date, end_date, isp_days, count_total) %>%
    mutate(count = n(), temp = max(instructional_program_num)) %>%
    filter(count == 1 | instructional_program_num == temp) %>%
    # Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
    group_by(system, school, student_key, begin_date, end_date, isp_days, count_total, instructional_program_num) %>%
    mutate(count = 1, temp = cumsum(count)) %>%
    filter(temp == 1) %>%
    # Collapse across multiple enrollments at the same school
    group_by(system, school, student_key) %>%
    summarise(n_absences = sum(count_total, na.rm = TRUE), isp_days = sum(isp_days, na.rm = TRUE)) %>%
    # Merge on instructional calendar file
    left_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        All = 1L)

school_CA <- tibble()
system_CA <- tibble()
state_CA <- tibble()

for (s in c("All")) {
    
    school_CA <- attendance %>%
        # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
        # Drop students enrolled less than 50 % of school year
        filter(isp_days/instructional_days >= 0.5) %>%
        group_by(year, system, system_name, school, school_name) %>%
        summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round(100 * mean(chronic_absence) + 1e-10, 1)) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(school_CA, .)
    
    system_CA <- attendance %>%
        # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
        # Check enrollment at the district level
        group_by(year, system, system_name, student_key) %>%
        summarise(n_absences = sum(n_absences, na.rm = TRUE), isp_days = sum(isp_days, na.rm = TRUE)) %>%
        mutate(n_students = 1,
            chronic_absence = as.numeric(n_absences/isp_days >= 0.1)) %>%
        group_by(year, system, system_name) %>%
        summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round(100 * mean(chronic_absence) + 1e-10, 1)) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(system_CA, .)
    
    state_CA <- attendance %>%
        # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
        group_by(year) %>%
        summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round(100 * mean(chronic_absence) + 1e-10, 1)) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(state_CA, .)
    
}

school_output <- school_CA %>%
    mutate(subgroup = case_when(subgroup == "All" ~ "All Students",
        subgroup == "BHN" ~ "Black/Hispanic/Native American",
        subgroup == "ED" ~ "Economically Disadvantaged",
        subgroup == "SWD" ~ "Students with Disabilities",
        subgroup == "EL" ~ "English Learners",
        subgroup == "Super" ~ "Super Subgroup")) %>%
    select(year, system, system_name, school, school_name, subgroup,
        n_students, n_chronically_absent, pct_chronically_absent)

system_output <- system_CA %>%
    mutate(subgroup = case_when(subgroup == "All" ~ "All Students",
        subgroup == "BHN" ~ "Black/Hispanic/Native American",
        subgroup == "ED" ~ "Economically Disadvantaged",
        subgroup == "SWD" ~ "Students with Disabilities",
        subgroup == "EL" ~ "English Learners",
        subgroup == "Super" ~ "Super Subgroup")) %>%
    select(year, system, system_name, subgroup, n_students, n_chronically_absent, pct_chronically_absent)

state_output <- state_CA %>%
    mutate(subgroup = case_when(subgroup == "All" ~ "All Students",
        subgroup == "BHN" ~ "Black/Hispanic/Native American",
        subgroup == "ED" ~ "Economically Disadvantaged",
        subgroup == "SWD" ~ "Students with Disabilities",
        subgroup == "EL" ~ "English Learners",
        subgroup == "Super" ~ "Super Subgroup")) %>%
    transmute(year, system = 0, system_name = "State of Tennessee", subgroup,
        n_students, n_chronically_absent, pct_chronically_absent)
