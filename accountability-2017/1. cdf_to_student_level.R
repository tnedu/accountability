library(tidyverse)

fall_cdf <- read_csv("K:/ORP_accountability/projects/2017_dictionary_coding/fall_eoc_cdf.csv") %>%
# Student level file variables
    mutate(test = "EOC",
        semester = "Fall",
        ri_status = if_else(ri_status_part_1 %in% c(0, 5), ri_status_part_2, ri_status_part_1),
        ri_status = if_else(is.na(ri_status), ri_status_part_1, ri_status),
        absent = as.numeric(ri_status  == 5),
        did_not_attempt = as.numeric(ri_status == 4),
        residential_facility = as.numeric(ri_status == 1),
        nullify_flag = as.numeric(ri_status == 2),
        original_subject = if_else(content_area_code == "A1", "Algebra I", NA_character_),
        original_subject = if_else(content_area_code == "A2", "Algebra II", original_subject),
        original_subject = if_else(content_area_code == "B1", "Biology I", original_subject),
        original_subject = if_else(content_area_code == "C1", "Chemistry", original_subject),
        original_subject = if_else(content_area_code == "E1", "English I", original_subject),
        original_subject = if_else(content_area_code == "E2", "English II", original_subject),
        original_subject = if_else(content_area_code == "E3", "English III", original_subject),
        original_subject = if_else(content_area_code == "G1", "Geometry", original_subject),
        original_subject = if_else(content_area_code == "M1", "Integrated Math I", original_subject),
        original_subject = if_else(content_area_code == "M2", "Integrated Math II", original_subject),
        original_subject = if_else(content_area_code == "M3", "Integrated Math III", original_subject),
        original_subject = if_else(content_area_code == "U1", "US History", original_subject)) %>%
    # Drop void and medically exempt
    filter(!ri_status %in% c(9, 3))

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

# Student level file
student_level <- bind_rows(fall_cdf) %>%
    mutate(system_name = stringr::str_to_title(system_name),
        enrolled = 1,
        tested = 1,
        valid_test = NA_integer_,
        grade = as.numeric(grade),
        race = if_else(reported_race == 4, "Hispanic/Latino", NA_character_),
        race = if_else(reported_race == 3, "Black or African American", race),
        race = if_else(reported_race == 1, "American Indian/Alaska Native", race),
        race = if_else(reported_race == 5, "Native Hawaiian/Pac. Islander", race),
        race = if_else(reported_race == 2, "Asian", race),
        race = if_else(reported_race == 6, "White", race),
        race = if_else(reported_race == 0, "Unknown", race),
        bhn_group = as.numeric(race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native")),
        economically_disadvantaged = if_else(economically_disadvantaged == 2, 0L, economically_disadvantaged),
        el_t1_t2 = if_else(el_t1_t2 == 2, 1L, el_t1_t2),
        special_ed = as.numeric(special_ed %in% c(1, 2, 3)),
        original_proficiency_level = if_else(performance_level == 1, "1. Below", NA_character_),
        original_proficiency_level = if_else(performance_level == 2, "2. Approaching", original_proficiency_level),
        original_proficiency_level = if_else(performance_level == 3, "3. On Track", original_proficiency_level),
        original_proficiency_level = if_else(performance_level == 4, "4. Mastered", original_proficiency_level),
        proficiency_level = original_proficiency_level,
        subject = original_subject) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_proficiency_level, proficiency_level,
        scale_score, enrolled, tested, valid_test, state_student_id = unique_student_id, last_name, first_name, middle_initial,
        grade, race, bhn_group, functionally_delayed, special_ed, economically_disadvantaged, el, el_t1_t2, el_excluded,
        greater_than_60_pct, homebound, absent, did_not_attempt, nullify_flag, residential_facility, semester) %>%
    mutate_each(funs(as.numeric), system, school, state_student_id) %>% 
# Drop excluded records
    filter(!is.na(system)) %>%
    filter(!grade == 13) %>%
    filter(!(system == 981 | system > 1000)) %>%
# Apply testing flag hierarchy (5.2.1)
    mutate(
    # Absent students have a missing proficiency and tested value
        tested = if_else(absent == 1, 0, tested),
        proficiency_level = if_else(absent == 1, NA_character_, proficiency_level),
    # EL Excluded students with missing proficiency are not considered tested
        proficiency_level = if_else(el_excluded == 1, NA_character_, proficiency_level),
        tested = if_else(el_excluded == 1 & subject %in% c(math_eoc, science_eoc), 0, tested),
    # Proficiency modified to missing if nullify or did not attempt
        proficiency_level = if_else(nullify_flag == 1 | did_not_attempt == 1, NA_character_, proficiency_level),
    # Students taking MSAA are considered special education (5.5)
        special_ed = if_else(test == "MSAA", 1, special_ed),
    # Modify subject for MSAA tests in grades >= 9 (6.8)
        subject = if_else(original_subject == "Math" & test == "MSAA" & grade >= 9 &
            system %in% c(30, 60, 80, 100, 110, 140, 150, 190, 440, 580, 590, 710, 800, 821, 850, 890, 930),
            "Integrated Math I", subject),
        subject = if_else(original_subject == "Math" & test == "MSAA" & grade >= 9 &
            !system %in% c(30, 60, 80, 100, 110, 140, 150, 190, 440, 580, 590, 710, 800, 821, 850, 890, 930),
            "Algebra I", subject),
        subject = if_else(original_subject == "ELA" & test == "MSAA" & grade >= 9, "English II", subject),
    # Convert subjects per accountability rules
        subject = if_else(grade < 9 & original_subject %in% math_eoc, "Math", subject),
        subject = if_else(grade < 9 & original_subject %in% english_eoc, "ELA", subject),
        subject = if_else(grade < 9 & original_subject %in% science_eoc, "Science", subject),
        subject = if_else(grade < 9 & original_subject == "US History", "Social Studies", subject),
        test = if_else(grade < 9 & original_subject %in% c(math_eoc, english_eoc, science_eoc, "US History"), "Achievement", test))

dedup <- student_level %>%
    # For students with multiple records across test types, MSAA has priority, then TCAP/EOC
    mutate(test_priority = ifelse(test == "MSAA", 2, 1)) %>%
    group_by(state_student_id, subject, grade) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
    filter(!test_priority != temp & !is.na(temp)) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
    # For students with multiple records within the same test, take highest proficiency level
    mutate(prof_priority = as.integer(substr(proficiency_level, 1, 1))) %>%
    group_by(state_student_id, subject, grade, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(!(prof_priority != temp & !is.na(temp))) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
    # For students with multiple test records with the same proficiency across administrations, take the most recent
    mutate(semester_priority = if_else(test == "MSAA" | test == "Achievement" | (test == "EOC" & semester == "Spring"), 3, NA_real_),
        semester_priority = if_else(test == "EOC" & semester == "Fall", 2, semester_priority),
        semester_priority = if_else(test == "EOC" & semester == "Summer", 1, semester_priority)) %>%
    group_by(state_student_id, subject, grade, test) %>%
    mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
    filter(!semester_priority != temp & !is.na(temp)) %>%
    select(-semester_priority, -temp) %>%
    ungroup() %>%
    # Valid test if there is a proficiency level
    mutate(valid_test = as.numeric(!is.na(proficiency_level)))

output <- dedup %>%
    filter(!is.na(state_student_id)) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_proficiency_level, proficiency_level,
        scale_score, enrolled, tested, valid_test, unique_student_id = state_student_id, last_name, first_name, grade, race,
        bhn_group, functionally_delayed, special_ed, economically_disadvantaged, el, el_t1_t2, el_excluded, greater_than_60_pct,
        homebound, absent, did_not_attempt, nullify_flag, residential_facility) %>%
    arrange(system, school)
    # Drop duplicates for match
    # group_by(system, school, unique_student_id, test, subject, grade) %>%
    # mutate(dup = n()) %>%
    # ungroup() %>%
    # filter(!dup > 1) %>%
    # select(-dup)

write_csv(output, "K:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017.csv", na = "")
