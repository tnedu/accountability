library(acct)
library(tidyverse)

instructional_days <- readxl::read_excel("K:/ORP_accountability/data/2017_chronic_absenteeism/School Level - Instructional days.xlsx") %>%
    transmute(year = 2017, system_name = DISTRICT_NAME, system = DISTRICT_NO,
        school_name = SCHOOL_NAME, school = SCHOOL_NO, instructional_days = INSTRUCTIONAL_DAYS)

econ_dis <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_classification_JUIH only.txt",
        delim = "\t") %>%
    transmute(student_key = STUDENT_KEY, ED = 1, 
        Homeless = as.numeric(STUDENT_CLASSIFICATION_TYPE == "H")) %>%
    filter(!duplicated(student_key))

special_ed <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo_Special Education.txt",
        delim = "\t") %>%
    filter(SPECIAL_ED_LEVEL == "P" & !OPTION_NUMBER %in% c(3, 16)) %>%
    transmute(student_key = STUDENT_KEY, SWD = 1) %>%
    filter(!duplicated(student_key))

el <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo.txt", delim = "\t") %>%
    filter(ENGLISH_LANGUAGE_BACKGROUND %in% c("L", "W", "1", "2", "3", "4")) %>%
    transmute(student_key = STUDENT_KEY, EL = 1) %>%
    filter(!duplicated(student_key))

race <- read_delim("K:/ORP_accountability/data/2017_chronic_absenteeism/Student_Demo.txt", delim = "\t") %>%
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

attendance <- haven::read_dta("K:/ORP_accountability/data/2017_chronic_absenteeism/Attendance data as of 08152017.dta") %>%
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
    transmute(instructional_program_num, system = district_no, school = school_no, grade, gender,
        student_key = as.integer(student_key), begin_date, end_date, isp_days,
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
    group_by(system, school, grade, student_key, gender) %>%
    summarise(n_absences = sum(count_total, na.rm = TRUE), isp_days = sum(isp_days, na.rm = TRUE)) %>%
    ungroup() %>%
    # Merge on instructional calendar file
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(n_students = 1,
        grade = case_when(
            grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ "K-8",
            grade %in% c("09", "10", "11", "12") ~ "9-12"
        ),
        chronic_absence = as.numeric(n_absences/isp_days >= 0.1),
        All = 1L) %>%
    left_join(race, by = "student_key") %>%
    left_join(econ_dis, by = "student_key") %>%
    left_join(special_ed, by = "student_key") %>%
    left_join(el, by = "student_key")

school_CA <- tibble()

for (s in c("All", "BHN", "ED", "SWD", "EL", "Homeless", "Native", "Black", "Asian", "Hispanic", "HPI", "White")) {
    
    # School all grades
    school_CA <- attendance %>%
        # Filter for relevant subgroup
        filter_(paste(s, "== 1L")) %>%
        # Drop students enrolled less than 50% of school year
        filter(isp_days/instructional_days >= 0.5) %>%
        group_by(year, system, system_name, school, school_name, gender) %>%
        summarise(n_chronically_absent = sum(chronic_absence)) %>%
        ungroup() %>%
        mutate(subgroup = s) %>%
        bind_rows(school_CA, .)

}

school_output <- school_CA %>%
    arrange(system, school, subgroup, gender) %>%
    rowid_to_column(var = "first") %>%
    transmute(first, state_code = "TN",
        state_agency = "01",
        system = sprintf("%03d", system),
        school = sprintf("%04d", school),
        table_name = "CHRONABSENT",
        gender,
        racial_ethnic = case_when(
            subgroup == "Native" ~ "AM7",
            subgroup == "Asian" ~ "AS7",
            subgroup == "Black" ~ "BL7",
            subgroup == "Hispanic" ~ "HI7",
            subgroup == "HPI" ~ "PI7",
            subgroup == "White" ~ "WH7"
        ),
        disability = if_else(subgroup == "SWD", "WDIS", ""),
        lep = if_else(subgroup == "EL", "LEP", ""),
        homeless = if_else(subgroup == "Homeless", "HOMELESENRL", ""),
        filler = "",
        total_indicator = "N",
        explanation = "",
        student_count = n_chronically_absent)

write_csv(school_output, "H:/EDEN Data/EDEN 16-17/Done/C195/TNSCHCHRONABSE2017-01.csv", na = "", col_names = FALSE)
