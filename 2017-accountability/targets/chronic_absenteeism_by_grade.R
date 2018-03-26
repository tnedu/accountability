library(acct)
library(tidyverse)

instructional_days <- readxl::read_excel("N:/ORP_accountability/data/2017_chronic_absenteeism/School Level - Instructional days.xlsx") %>%
    transmute(year = 2017, system_name = DISTRICT_NAME, system = DISTRICT_NO,
        school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)

econ_dis <- read_delim("N:/ORP_accountability/data/2017_chronic_absenteeism/Student_classification_JUIH only.txt",
        delim = "\t") %>%
    transmute(student_key = STUDENT_KEY, ED = 1) %>%
    filter(!duplicated(student_key))

special_ed <- read_delim("N:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo_Special Education.txt",
        delim = "\t") %>%
    filter(SPECIAL_ED_LEVEL == "P" & !OPTION_NUMBER %in% c(3, 16)) %>%
    transmute(student_key = STUDENT_KEY, SWD = 1) %>%
    filter(!duplicated(student_key))

el <- read_delim("N:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo.txt", delim = "\t") %>%
    filter(ENGLISH_LANGUAGE_BACKGROUND %in% c("L", "W", "1", "2", "3", "4")) %>%
    transmute(student_key = STUDENT_KEY, EL = 1) %>%
    filter(!duplicated(student_key))

race <- read_delim("N:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo.txt", delim = "\t") %>%
    mutate(race = case_when(
        ETHNICITY == "H" ~ "Hispanic",
        RACE_B == "Y" ~ "Black",
        RACE_I == "Y" ~ "Native",
        RACE_P == "Y" ~ "HPI",
        RACE_A == "Y" ~ "Asian",
        RACE_W == "Y" ~ "White"
    )
    ) %>%
    transmute(student_key = STUDENT_KEY, race, BHN = race %in% c("Black", "Hispanic", "Native"),
        Black = race == "Black", Hispanic = race == "Hispanic", Native = race == "Native",
        HPI = race == "HPI", Asian = race == "Asian", White = race == "White") %>%
    mutate_at(c("BHN", "Black", "Hispanic", "Native", "HPI", "Asian", "White"), as.numeric) %>%
    # Apply race determination (5.1.1)
    mutate(priority = case_when(
            race == "Hispanic" ~ 6,
            race == "Black" ~ 5,
            race == "Native" ~ 4,
            race == "HPI" ~ 3,
            race == "Asian" ~ 2,
            race == "White" ~ 1
        )
    ) %>%
    group_by(student_key) %>%
    mutate(temp = max(priority, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(priority == temp) %>%
    filter(!duplicated(student_key)) %>%
    select(-c(race, priority, temp))

attendance_henry <- readxl::read_excel("N:/ORP_accountability/data/2017_chronic_absenteeism/Absenteeism - Henry Co  SY 2016-17.xlsx") %>%
    mutate_at(c("BEGIN_DATE", "END_DATE", "WITHDRAWAL_REASON", "DATE_OF_BIRTH", "SCHOOL_BU_ID", "DISTRICT_BU_ID",
        "CNT_UNEXCUSED", "CNT_UNEXCUSED_TRANS"), as.character) %>%
    mutate(GRADE = if_else(GRADE %in% as.character(1:9), paste0("0", GRADE), GRADE))

names(attendance_henry) <- tolower(names(attendance_henry))

attendance <- haven::read_dta("N:/ORP_accountability/data/2017_chronic_absenteeism/Attendance data as of 08152017.dta") %>%
    filter(district_no != 400) %>%
    bind_rows(attendance_henry) %>%
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
    transmute(instructional_program_num, system = district_no, school = school_no, grade,
        student_key = as.integer(student_key), begin_date, end_date, isp_days,
        count_excused = if_else(is.na(cnt_excused), 0, as.numeric(cnt_excused)),
        count_unexcused = if_else(cnt_unexcused == "", 0, as.numeric(cnt_unexcused)),
        count_total = if_else(is.na(cnt_total), 0, cnt_total)) %>%
    # Combine two Bartlett schools that merged
    mutate(school = if_else(system == 794 & school == 170, 25, school)) %>%
    # For students with same system, school, student ID, enrollment dates, take maximum instructional program days
    # (Doesn't drop any records)
    group_by(system, school, student_key, grade, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days,
    # take maximum number of absences (Drops two records)
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
    # For students with same system, school, student ID, enrollment dates, instructional program days, absences,
    # take maximum instuctional program number (Doesn't drop any records)
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total) %>%
    mutate(count = n(), temp = max(instructional_program_num)) %>%
    filter(count == 1 | instructional_program_num == temp) %>%
    # Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total, instructional_program_num) %>%
    mutate(count = 1, temp = cumsum(count)) %>%
    filter(temp == 1) %>%
    # Collapse multiple enrollments at the same school
    rename(n_absences = count_total) %>%
    group_by(system, school, grade, student_key) %>%
    summarise_at(c("count_excused", "count_unexcused", "n_absences", "isp_days"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    # Merge on instructional calendar file
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        absent_10_to_20_pct = as.numeric(n_absences/isp_days >= 0.1 & n_absences/isp_days <= 0.2),
        absent_greater_than_20_pct = as.numeric(n_absences/isp_days > 0.2),
        All = 1L) %>%
    left_join(race, by = "student_key") %>%
    left_join(econ_dis, by = "student_key") %>%
    left_join(special_ed, by = "student_key") %>%
    left_join(el, by = "student_key")

# School by grade
school_CA <- attendance %>%
    # Drop students enrolled less than 50% of school year
    filter(isp_days/instructional_days >= 0.5) %>%
    group_by(year, system, system_name, school, school_name, grade) %>%
    summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
        n_10_to_20_pct = sum(absent_10_to_20_pct), n_greater_than_20_pct = sum(absent_greater_than_20_pct),
        days_excused = sum(count_excused), days_unexcused = sum(count_unexcused),
        pct_chronically_absent = round5(100 * mean(chronic_absence), 1),
        pct_10_to_20_pct = round5(100 * mean(absent_10_to_20_pct), 1),
        pct_greater_than_20_pct = round5(100 * mean(absent_greater_than_20_pct), 1)) %>%
    ungroup()

# System by grade
system_CA <- attendance %>%
    # Collapse multiple enrollments in the same district
    group_by(year, system, system_name, student_key, grade) %>%
    summarise(n_absences = sum(n_absences, na.rm = TRUE), isp_days = sum(isp_days, na.rm = TRUE),
        count_excused = sum(count_excused, na.rm = TRUE), count_unexcused = sum(count_unexcused, na.rm = TRUE),
        instructional_days = max(instructional_days)) %>%
    ungroup() %>%
    # Drop students enrolled less than 50% of school year
    filter(isp_days/instructional_days >= 0.5) %>%
    mutate(n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        absent_10_to_20_pct = as.numeric(n_absences/isp_days >= 0.1 & n_absences/isp_days <= 0.2),
        absent_greater_than_20_pct = as.numeric(n_absences/isp_days > 0.2)) %>%
    group_by(year, system, system_name, grade) %>%
    summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
        n_10_to_20_pct = sum(absent_10_to_20_pct), n_greater_than_20_pct = sum(absent_greater_than_20_pct),
        days_excused = sum(count_excused), days_unexcused = sum(count_unexcused),
        pct_chronically_absent = round5(100 * mean(chronic_absence), 1),
        pct_10_to_20_pct = round5(100 * mean(absent_10_to_20_pct), 1),
        pct_greater_than_20_pct = round5(100 * mean(absent_greater_than_20_pct), 1)) %>%
    ungroup()
    
# State by grade
state_CA <- attendance %>%
    # Add up absences and ISP days across every enrollment
    group_by(year, student_key, grade) %>%
    summarise(n_absences = sum(n_absences, na.rm = TRUE), isp_days = sum(isp_days, na.rm = TRUE),
        count_excused = sum(count_excused), count_unexcused = sum(count_unexcused),
        instructional_days = max(instructional_days)) %>%
    ungroup() %>%
    filter(isp_days >= 45) %>%
    mutate(n_students = 1,
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        absent_10_to_20_pct = as.numeric(n_absences/isp_days >= 0.1 & n_absences/isp_days <= 0.2),
        absent_greater_than_20_pct = as.numeric(n_absences/isp_days > 0.2)) %>%
    group_by(year, grade) %>%
    summarise(n_students = sum(n_students), n_chronically_absent = sum(chronic_absence),
        n_10_to_20_pct = sum(absent_10_to_20_pct), n_greater_than_20_pct = sum(absent_greater_than_20_pct),
        days_excused = sum(count_excused), days_unexcused = sum(count_unexcused),
        pct_chronically_absent = round5(100 * mean(chronic_absence), 1),
        pct_10_to_20_pct = round5(100 * mean(absent_10_to_20_pct), 1),
        pct_greater_than_20_pct = round5(100 * mean(absent_greater_than_20_pct), 1)) %>%
    ungroup()

school_output <- school_CA %>%
    transmute(year, system, system_name, school, school_name, grade,
        n_students, days_excused, days_unexcused,
        n_chronically_absent, n_10_to_20_pct, n_greater_than_20_pct,
        pct_chronically_absent, pct_10_to_20_pct, pct_greater_than_20_pct) %>%
    arrange(system, school, grade)

write_csv(school_output, "N:/ORP_accountability/data/2017_chronic_absenteeism/school_chronic_absenteeism_by_grade.csv", na = "")

system_output <- system_CA %>%
    transmute(year, system, system_name, grade,
        n_students, days_excused, days_unexcused,
        n_chronically_absent, n_10_to_20_pct, n_greater_than_20_pct,
        pct_chronically_absent, pct_10_to_20_pct, pct_greater_than_20_pct) %>%
    arrange(system, grade)

write_csv(system_output, "N:/ORP_accountability/data/2017_chronic_absenteeism/system_chronic_absenteeism_by_grade.csv", na = "")

state_output <- state_CA %>%
    transmute(year, system = 0, system_name = "State of Tennessee", grade,
        n_students, days_excused, days_unexcused,
        n_chronically_absent, n_10_to_20_pct, n_greater_than_20_pct,
        pct_chronically_absent, pct_10_to_20_pct, pct_greater_than_20_pct) %>%
    arrange(grade)

write_csv(state_output, "N:/ORP_accountability/data/2017_chronic_absenteeism/state_chronic_absenteeism_by_grade.csv", na = "")
