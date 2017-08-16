library(tidyverse)

instructional_days <- readxl::read_excel("K:/ORP_accountability/data/2017_chronic_absenteeism/School Level - Instructional days.xlsx") %>%
    rename(year = SCHOOL_YEAR, system_name = DISTRICT_NAME, system = DISTRICT_NO,
        school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)

econ_dis <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_classification_JUIH only.txt",
    delim = "\t") %>%
    transmute(student_key = STUDENT_KEY, ED = 1) %>%
    filter(!duplicated(student_key))

special_ed <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo_Special Education.txt",
    delim = "\t") %>%
    filter(SPECIAL_ED_LEVEL == "P" & !OPTION_NUMBER %in% c(3, 16)) %>%
    transmute(student_key = STUDENT_KEY, SWD = 1) %>%
    filter(!duplicated(student_key))

el <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo.txt",
    delim = "\t") %>%
    filter(ENGLISH_LANGUAGE_BACKGROUND %in% c("L", "W", "1", "2", "3", "4")) %>%
    select(student_key = STUDENT_KEY, EL = 1) %>%
    filter(!duplicated(student_key))

bhn <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo.txt",
    delim = "\t") %>%
    filter(ETHNICITY == "H" | RACE_B == "Y" | RACE_I == "Y") %>%
    transmute(student_key = STUDENT_KEY, BHN = 1)

attendance <- read_delim("K:/Research_Transfers/Data_Management/06_attendance data/data/Attendance data 2016-17 as of 081617.csv",
    delim = "\t") %>%
    transmute(instructional_program_num = INSTRUCTIONAL_PROGRAM_NUM, system = DISTRICT_NO, school = SCHOOL_NO,
        student_key = STUDENT_KEY, begin_date = BEGIN_DATE, end_date = END_DATE,
        count_total = if_else(is.na(CNT_TOTAL), 0L, CNT_TOTAL), isp_days = ISP_DAYS) %>%
    # For students with same system, school, student ID, enrollment dates, take maximum instructional program days
    # (Doesn't drop any records)
    group_by(system, school, student_key, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days,
    # take maximum number of absences (Doesn't drop any records)
    group_by(system, school, student_key, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days, absences,
    # take maximum instuctional program number (Doesn't drop any records)
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
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        All = 1L) %>%
    left_join(econ_dis, by = "student_key") %>%
    left_join(special_ed, by = "student_key") %>%
    left_join(el, by = "student_key")

school_CA <- tibble()
system_CA <- tibble()
state_CA <- tibble()

for (s in c("All", "BHN", "ED", "SWD", "EL")) {

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
    transmute(year, system, system_name, school, school_name,
        subgroup = case_when(subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners",
            subgroup == "Super" ~ "Super Subgroup"),
        n_students, n_chronically_absent, pct_chronically_absent)

write_csv(school_output, "K:/ORP_accountability/data/2017_chronic_absenteeism/school_chronic_absenteeism.csv", na = "")

system_output <- system_CA %>%
    transmute(year, system, system_name,
        subgroup = case_when(subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners",
            subgroup == "Super" ~ "Super Subgroup"),
        n_students, n_chronically_absent, pct_chronically_absent)

write_csv(system_output, "K:/ORP_accountability/data/2017_chronic_absenteeism/system_chronic_absenteeism.csv", na = "")

state_output <- state_CA %>%
    transmute(year, system = 0, system_name = "State of Tennessee",
        subgroup = case_when(subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners",
            subgroup == "Super" ~ "Super Subgroup"),
        n_students, n_chronically_absent, pct_chronically_absent)

write_csv(state_output, "K:/ORP_accountability/data/2017_chronic_absenteeism/state_chronic_absenteeism.csv", na = "")
