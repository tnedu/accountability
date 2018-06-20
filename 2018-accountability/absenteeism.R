library(acct)
library(tidyverse)

instructional_days <- readxl::read_excel("N:/ORP_accountability/data/2018_chronic_absenteeism/Instructional_Days_SchoolFile.xls") %>%
    transmute(year = 2018, system_name = DISTRICT_NAME, system = DISTRICT_NO,
        school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)

demographic <- read_delim("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/Demographics_SY2017_18.txt", delim = "\t") %>%
    janitor::clean_names() %>%
    mutate(
        Hispanic = race == 4,
        Black = race == 3,
        Native = race == 1,
        HPI = race == 5,
        Asian = race == 2,
        White = race == 6
    ) %>%
    group_by(student_key) %>%
    summarise_at(c("ed", "swd", "ell", "t1t4", "Hispanic", "Black", "Native", "HPI", "Asian", "White"), max, na.rm = TRUE) %>%
    transmute(student_key, BHN = pmax(Black, Hispanic, Native), ED = ed, SWD = swd, EL = pmax(ell, t1t4),
        Hispanic, Black, Native, HPI, Asian, White)

attendance <- read_delim("N:/ORP_accountability/data/2018_chronic_absenteeism/Instructional_Days_Student file.txt", delim = "\t",
        col_types = "ciiiiiccccccicccciiccccccciiiiii") %>%
    janitor::clean_names() %>%
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
    transmute(instructional_program_num, system = district_no, school = school_no, grade,
        student_key = as.integer(student_key), begin_date, end_date, isp_days,
        count_total = if_else(is.na(cnt_total), 0L, cnt_total)) %>%
# For students with same system, school, student ID, enrollment dates, take maximum instructional program days
# (Drops 0 records)
    group_by(system, school, student_key, grade, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
# For students with same system, school, student ID, enrollment dates, instructional program days,
# take maximum number of absences (Drops 9 records)
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
    summarise_at(c("n_absences", "isp_days"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Merge on instructional calendar file
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
        grade = case_when(
            grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ "K-8",
            grade %in% c("09", "10", "11", "12") ~ "9-12"
        ),
        chronic_absence = as.integer(n_absences/isp_days >= 0.1),
        All = 1L) %>%
    left_join(demographic, by = "student_key")

school_CA <- tibble()
system_CA <- tibble()
state_CA <- tibble()

for (s in c("All", "BHN", "ED", "SWD", "EL", "Black", "Hispanic", "Native", "HPI", "Asian", "White")) {
    
# School all grades
    school_CA <- attendance %>%
    # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
    # Drop students enrolled less than 50% of school year
        filter(isp_days/instructional_days >= 0.5) %>%
        group_by(year, system, system_name, school, school_name) %>%
        summarise(
            n_students = sum(n_students),
            n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round5(100 * mean(chronic_absence), 1)
        ) %>%
        ungroup() %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(school_CA, .)
    
# System by grade band
    system_CA <- attendance %>%
    # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
    # Collapse multiple enrollments in the same district
        group_by(year, system, system_name, student_key, grade) %>%
        summarise(
            n_absences = sum(n_absences, na.rm = TRUE),
            isp_days = sum(isp_days, na.rm = TRUE),
            instructional_days = max(instructional_days)
        ) %>%
        ungroup() %>%
    # Drop students enrolled less than 50% of school year
        filter(isp_days/instructional_days >= 0.5) %>%
        mutate(
            n_students = 1,
            chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
        ) %>%
        group_by(year, system, system_name, grade) %>%
        summarise(
            n_students = sum(n_students),
            n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round5(100 * mean(chronic_absence), 1)
        ) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(system_CA, .)
    
# System all grades
    system_CA <- attendance %>%
    # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
    # Collapse multiple enrollments in the same district
        group_by(year, system, system_name, student_key, grade) %>%
        summarise(
            n_absences = sum(n_absences, na.rm = TRUE),
            isp_days = sum(isp_days, na.rm = TRUE),
            instructional_days = max(instructional_days)
        ) %>%
        ungroup() %>%
    # Drop students enrolled less than 50% of school year
        filter(isp_days/instructional_days >= 0.5) %>%
        mutate(
            n_students = 1,
            chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
        ) %>%
        group_by(year, system, system_name) %>%
        summarise(
            n_students = sum(n_students),
            n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round5(100 * mean(chronic_absence), 1)
        ) %>%
        ungroup() %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(system_CA, .)
    
# State by grade band
    state_CA <- attendance %>%
        # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
        # Add up absences and ISP days across every enrollment
        group_by(year, student_key, grade) %>%
        summarise(
            n_absences = sum(n_absences, na.rm = TRUE),
            isp_days = sum(isp_days, na.rm = TRUE),
            instructional_days = max(instructional_days)
        ) %>%
        ungroup() %>%
        filter(isp_days >= 45) %>%
        mutate(
            n_students = 1,
            chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
        ) %>%
        group_by(year, grade) %>%
        summarise(
            n_students = sum(n_students),
            n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round5(100 * mean(chronic_absence), 1)
        ) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(state_CA, .)
    
# State all grades
    state_CA <- attendance %>%
    # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
    # Add up absences and ISP days across every enrollment
        group_by(year, student_key, grade) %>%
        summarise(
            n_absences = sum(n_absences, na.rm = TRUE),
            isp_days = sum(isp_days, na.rm = TRUE),
            instructional_days = max(instructional_days)
        ) %>%
        ungroup() %>%
        filter(isp_days >= 45) %>%
        mutate(
            n_students = 1,
            chronic_absence = as.numeric(n_absences/isp_days >= 0.1)
        ) %>%
        group_by(year) %>%
        summarise(
            n_students = sum(n_students),
            n_chronically_absent = sum(chronic_absence),
            pct_chronically_absent = round5(100 * mean(chronic_absence), 1)
        ) %>%
        ungroup() %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(state_CA, .)

}

school_output <- school_CA %>%
    transmute(year, system, system_name, school, school_name,
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners with Transitional 1-4",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
            TRUE ~ subgroup
        ),
        grade_band = grade,
        n_students, n_chronically_absent, pct_chronically_absent) %>%
    arrange(system, school, subgroup, grade_band)

write_csv(school_output, "N:/ORP_accountability/data/2018_chronic_absenteeism/school_chronic_absenteeism.csv", na = "")

system_output <- system_CA %>%
    transmute(year, system, system_name,
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners with Transitional 1-4",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
            TRUE ~ subgroup
        ),
        grade_band = grade,
        n_students, n_chronically_absent, pct_chronically_absent) %>%
    arrange(system, subgroup, grade_band)

write_csv(system_output, "N:/ORP_accountability/data/2018_chronic_absenteeism/system_chronic_absenteeism.csv", na = "")

state_output <- state_CA %>%
    transmute(year, system = 0, system_name = "State of Tennessee",
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "EL" ~ "English Learners with Transitional 1-4",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
            TRUE ~ subgroup
        ),
        grade_band = grade,
        n_students, n_chronically_absent, pct_chronically_absent) %>%
    arrange(subgroup, grade_band)

write_csv(state_output, "N:/ORP_accountability/data/2018_chronic_absenteeism/state_chronic_absenteeism.csv", na = "")

student_output <- attendance %>%
    transmute(system, system_name, school, school_name, student_id = student_key,
        n_absences, isp_days, instructional_calendar_days = instructional_days,
        absentee_rate = round5(100 * n_absences/isp_days, 1),
        Black, Hispanic, Native, HPI, Asian, White, ED, SWD, EL) %>%
    mutate_at(c("Black", "Hispanic", "Native", "HPI", "Asian", "White", "ED", "SWD", "EL"),
        funs(if_else(is.na(.), 0L, as.integer(.))))

write_csv(student_output, "N:/ORP_accountability/data/2018_chronic_absenteeism/student_chronic_absenteeism.csv", na = "")
