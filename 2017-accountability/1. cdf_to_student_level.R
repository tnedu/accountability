library(tidyverse)
library(haven)
library(readxl)

fall_cdf <- read_dta("K:/ORP_accountability/data/2017_cdf/fall_eoc_cdf_JW_07242017.dta") %>%
    # Student level file variables
    mutate(test = "EOC",
        semester = "Fall",
        el_excluded = as.numeric(el_excluded == 1 | EL_accommodationsU_part1 == "U" | EL_accommodationsU_part2 == "U"),
        ri_status_final = if_else(content_area_code %in% c("E1", "E2", "E3", "U1") & (is.na(ri_status_part_1) | ri_status_part_1 %in% c(0, 5)),
            ifelse(!is.na(ri_status_part_2), ri_status_part_2, ri_status_part_1), ri_status_part_1),
        ri_status_final = if_else(el_excluded == 1, 0, ri_status_final),
        ri_status_final = if_else(is.na(ri_status_final), 0, ri_status_final),
        absent = as.numeric(ri_status_final == 5),
        did_not_attempt = as.numeric(ri_status_final == 4),
        residential_facility = as.numeric(ri_status_final == 1),
        nullify_flag = as.numeric(ri_status_final == 2),
        original_subject = case_when(content_area_code == "A1" ~ "Algebra I",
            content_area_code == "A1" ~ "Algebra I",
            content_area_code == "A2" ~ "Algebra II",
            content_area_code == "B1" ~ "Biology I",
            content_area_code == "C1" ~ "Chemistry",
            content_area_code == "E1" ~ "English I",
            content_area_code == "E2" ~ "English II",
            content_area_code == "E3" ~ "English III",
            content_area_code == "G1" ~ "Geometry",
            content_area_code == "M1" ~ "Integrated Math I",
            content_area_code == "M2" ~ "Integrated Math II",
            content_area_code == "M3" ~ "Integrated Math III",
            content_area_code == "U1" ~ "US History"))

spring_cdf <- read_dta("K:/ORP_accountability/data/2017_cdf/Spring_EOC_CDF_JW_07242017.dta") %>%
    # Student level file variables
    mutate(test = "EOC",
        semester = "Spring",
        el_excluded = ifelse(el_excluded == 1 | EL_accommodationsU_part1 == "U" | EL_accommodationsU_part2 == "U", 1, 0),
        ri_status_final = ifelse(el_excluded == 1, 0, ri_status_final),
        ri_status_final = ifelse(is.na(ri_status_final), 0, ri_status_final),
        absent = as.numeric(ri_status_final == 5),
        did_not_attempt = as.numeric(ri_status_final == 4),
        residential_facility = as.numeric(ri_status_final == 1),
        nullify_flag = as.numeric(ri_status_final == 2),
        original_subject = case_when(content_area_code == "A1" ~ "Algebra I",
            content_area_code == "A1" ~ "Algebra I",
            content_area_code == "A2" ~ "Algebra II",
            content_area_code == "B1" ~ "Biology I",
            content_area_code == "C1" ~ "Chemistry",
            content_area_code == "E1" ~ "English I",
            content_area_code == "E2" ~ "English II",
            content_area_code == "E3" ~ "English III",
            content_area_code == "G1" ~ "Geometry",
            content_area_code == "M1" ~ "Integrated Math I",
            content_area_code == "M2" ~ "Integrated Math II",
            content_area_code == "M3" ~ "Integrated Math III",
            content_area_code == "U1" ~ "US History")) 

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

int_math_systems <- bind_rows(fall_cdf, spring_cdf) %>%
    filter(content_area_code %in% c("A1", "M1")) %>%
    count(system, content_area_code) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, content_area_code == "M1") %>%
    magrittr::extract2("system")

# MSAA
msaa_math <- read_csv("K:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2016-17/20170630_MSAA_StateStudentResults_SY2016-17_Whalen_v1.csv") %>%
    transmute(system = as.numeric(DistrictID), school = as.numeric(SchoolID),
        unique_student_id = State_Student_ID, grade = as.numeric(Grade), original_subject = "Math",
        performance_level = MatPerfLevel, reporting_status = MatReportingStatus)

msaa_ela <- read_csv("K:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2016-17/20170630_MSAA_StateStudentResults_SY2016-17_Whalen_v1.csv") %>%
    transmute(system = as.numeric(DistrictID), school = as.numeric(SchoolID),
        unique_student_id = State_Student_ID, grade = as.numeric(Grade), original_subject = "ELA",
        performance_level = ELAPerfLevel, reporting_status = ELAReportingStatus)

msaa <- bind_rows(msaa_math, msaa_ela) %>%
    filter(grade > 8, !reporting_status %in% c("INV", "EXE", "WDR", "NLE")) %>%
    mutate(test = "MSAA",
        absent = as.numeric(is.na(performance_level)),
        enrolled = 1,
        tested = if_else(reporting_status == "PRF", 0, 1))

alt_cte_adult <- read_excel("K:/ORP_accountability/data/2017_tdoe_provided_files/List of Schools Acct 2016-17.xlsx") %>%
    transmute(system = DISTRICT_NUMBER, school = SCHOOL_NUMBER, cte_alt_adult = as.numeric(INSTRUCTIONAL_TYPE_ID %in% c(6, 8, 9)))

# Student level file
student_level <- bind_rows(fall_cdf, spring_cdf, msaa) %>%
    anti_join(alt_cte_adult, by = c("system", "school")) %>%
    mutate_at(c("nullify_flag", "did_not_attempt"), funs(ifelse(is.na(.), 0, .))) %>% 
    mutate(system_name = stringr::str_to_title(system_name),
        enrolled = 1,
        tested = if_else(test != "MSAA", 1, tested),
        valid_test = NA_integer_,
        grade = as.numeric(grade),
        race = case_when(reported_race == 4 ~ "Hispanic/Latino",
            reported_race == 3 ~ "Black or African American",
            reported_race == 1 ~ "American Indian/Alaska Native",
            reported_race == 5 ~ "Native Hawaiian/Pac. Islander",
            reported_race == 2 ~ "Asian",
            reported_race == 6 ~ "White",
            reported_race == 0 ~ "Unknown"),
        bhn_group = as.numeric(race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native")),
        economically_disadvantaged = if_else(economically_disadvantaged == 2, 0, economically_disadvantaged),
        el_excluded = if_else(is.na(el_excluded), 0, el_excluded),
        el_t1_t2 = if_else(el_t1_t2 == 2, 1, el_t1_t2),
        special_ed = as.numeric(special_ed %in% c(1, 2, 3)),
        original_proficiency_level = case_when(
            original_subject %in% c(english_eoc, math_eoc, "US History") & performance_level == 1 ~ "1. Below",
            original_subject %in% c(english_eoc, math_eoc, "US History") & performance_level == 2 ~ "2. Approaching",
            original_subject %in% c(english_eoc, math_eoc, "US History") & performance_level == 3 ~ "3. On Track",
            original_subject %in% c(english_eoc, math_eoc, "US History") & performance_level == 4 ~ "4. Mastered",
            original_subject %in% science_eoc & performance_level == 1 ~ "1. Below Basic",
            original_subject %in% science_eoc & performance_level == 2 ~ "2. Basic",
            original_subject %in% science_eoc & performance_level == 3 ~ "3. Proficient",
            original_subject %in% science_eoc & performance_level == 4 ~ "4. Advanced"),
        subject = original_subject) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_proficiency_level, proficiency_level,
        scale_score, enrolled, tested, valid_test, state_student_id = unique_student_id, last_name, first_name,
        grade, race, bhn_group, functionally_delayed, special_ed, economically_disadvantaged, el, el_t1_t2, el_excluded,
        greater_than_60_pct, homebound, absent, did_not_attempt, nullify_flag, residential_facility, semester, ri_status_final) %>%
    mutate_at(c("system", "school", "state_student_id"), as.numeric) %>% 
    # Drop excluded records
    filter(!is.na(system)) %>%
    filter(grade != 13) %>%
    filter(!(school == 981 | system > 1000)) %>%
    # Drop void and medically exempt
    filter(!ri_status_final %in% c(9, 3)) %>% 
    # Apply testing flag hierarchy (5.2.1)
    mutate(
        # Absent students have a missing proficiency and tested value
        tested = if_else(absent == 1, 0, tested),
        proficiency_level = if_else(absent == 1, NA_character_, proficiency_level),
        # EL Excluded students with missing proficiency are not considered tested
        proficiency_level = if_else(el_excluded == 1, NA_character_, proficiency_level),
        tested = if_else(el_excluded == 1 & is.na(original_proficiency_level) & subject %in% c(math_eoc, science_eoc), 0, tested),
        tested = if_else(el_excluded == 1 & !is.na(original_proficiency_level) & subject %in% c(math_eoc, science_eoc), 1, tested),
        # Proficiency modified to missing if nullify or did not attempt
        proficiency_level = if_else(nullify_flag == 1 | did_not_attempt == 1, NA_character_, proficiency_level),
        # Students taking MSAA are considered special education (5.5)
        special_ed = if_else(test == "MSAA", 1, special_ed),
        # Modify subject for MSAA tests in grades >= 9 (6.8)
        subject = if_else(original_subject == "Math" & test == "MSAA" & grade >= 9 & system %in% int_math_systems,
            "Integrated Math I", subject),
        subject = if_else(original_subject == "Math" & test == "MSAA" & grade >= 9 & !system %in% int_math_systems,
            "Algebra I", subject),
        subject = if_else(original_subject == "ELA" & test == "MSAA" & grade >= 9, "English II", subject),
        # Convert subjects per accountability rules
        subject = case_when(grade %in% 2:8 & original_subject %in% math_eoc ~ "Math",
            grade %in% 2:8 & original_subject %in% english_eoc ~ "ELA",
            grade %in% 2:8 & original_subject %in% science_eoc ~ "Science",
            grade %in% 2:8 & original_subject == "US History" ~ "Social Studies",
            TRUE ~ subject),
        test = if_else(grade %in% 2:8 & original_subject %in% c(math_eoc, english_eoc, science_eoc, "US History"), "Achievement", test))

dedup <- student_level %>%
    # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    mutate(test_priority = ifelse(test == "MSAA", 2, 1)) %>%
    group_by(state_student_id, subject) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
    filter(test_priority == temp | temp == -Inf) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
    # For students with multiple records within the same test, take highest proficiency level
    mutate(prof_priority = as.integer(substr(proficiency_level, 1, 1))) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(prof_priority == temp | temp == -Inf) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
    # For students with multiple test records with the same proficiency across administrations, take the most recent
    mutate(semester_priority = case_when(test == "MSAA" | test == "Achievement" | (test == "EOC" & semester == "Spring") ~ 3),
        test == "EOC" & semester == "Fall" ~ 2,
        test == "EOC" & semester == "Summer" ~ 1) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
    filter(semester_priority == temp | temp == -Inf) %>%
    select(-semester_priority, -temp) %>%
    ungroup() %>%
    # Valid test if there is a proficiency level
    mutate(valid_test = as.numeric(!is.na(proficiency_level)))

output <- dedup %>%
    filter(!is.na(state_student_id)) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_proficiency_level, proficiency_level,
        scale_score, enrolled, tested, valid_test, state_student_id, last_name, first_name, grade, race,
        bhn_group, special_ed, economically_disadvantaged, el, el_t1_t2, el_excluded, greater_than_60_pct,
        homebound, absent, did_not_attempt, nullify_flag, residential_facility) %>%
    arrange(system, school, state_student_id)

# Output file
write_csv(output, "K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017.csv", na = "")
