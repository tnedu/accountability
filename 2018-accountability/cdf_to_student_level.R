library(tidyverse)

fall_cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_fall_cdf.csv") %>%
    mutate(semester = "Fall",
        raw_score = as.integer(raw_score))

spring_cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_spring_cdf.csv") %>%
    mutate(semester = "Spring")

cdf <- bind_rows(fall_cdf, spring_cdf) %>%
    mutate(test = "EOC",
        absent = reason_not_tested == 1,
        medically_exempt = reason_not_tested == 4,
        residential_facility = reason_not_tested == 5,
        breach_adult = ri_status == 1,
        breach_student = ri_status == 2,
        irregular_admin = ri_status == 3,
        incorrect_grade_subject = ri_status == 4,
        did_not_attempt = ri_status %in% c(5, 6),
        original_subject = case_when(
            content_area_code == "ENG" ~ "ELA",
            content_area_code == "MAT" ~ "Math",
            content_area_code == "SCI" ~ "Science",
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
            content_area_code == "U1" ~ "US History"
        )
    )

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

student_level <- cdf %>%
    mutate(enrolled = 1,
        tested = 1,
        valid_test = NA_integer_,
        race = case_when(
            hispanic == "Y" ~ "Hispanic/Latino",
            black == "Y" ~ "Black or African American",
            native_american == "Y" ~ "American Indian/Alaska Native",
            hawaiian_pi == "Y" ~ "Native Hawaiian/Pac. Islander",
            asian == "Y" ~ "Asian",
            white == "Y" ~ "White",
            TRUE ~ "Unknown"
        ),
        bhn_group = race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"),
        economically_disadvantaged = economically_disadvantaged == "Y",
        el = el == "Y",
        el_excluded = (el_arrived_year_1 == "Y" | el_arrived_year_2 == "Y"),
        el_t1234 = el_t1234 %in% 1:4,
        special_ed = special_ed == "Y",
        functionally_delayed = functionally_delayed == "Y",
        homebound = homebound == "Y",
        original_performance_level = performance_level,
        subject = original_subject
    ) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_performance_level, performance_level,
        scale_score, enrolled, tested, valid_test, state_student_id = unique_student_id, last_name, first_name, grade, 
        race, bhn_group, functionally_delayed, special_ed, economically_disadvantaged, el, el_t1234, el_excluded,
        enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent, breach_adult, breach_student, irregular_admin,
        incorrect_grade_subject, did_not_attempt, residential_facility, semester, ri_status, medically_exempt) %>%
    mutate_at(c("system", "school", "state_student_id", "grade", "bhn_group", "functionally_delayed", "special_ed",
            "economically_disadvantaged", "el", "el_t1234", "el_excluded", "homebound", "absent",
            "breach_adult", "breach_student", "irregular_admin", "incorrect_grade_subject", "did_not_attempt", "residential_facility"),
        as.numeric) %>%
# Drop excluded records
    filter(!is.na(system),
        grade != 13 | is.na(grade),
        !(school == 981 | system > 1000),
    # Drop medically exempt
        medically_exempt == FALSE) %>%
# Apply testing flag hierarchy (5.2.1)
    mutate(
    # Absent students have a missing proficiency and tested value
        tested = if_else(absent == 1, 0, tested),
        performance_level = if_else(absent == 1, NA_character_, performance_level),
    # EL Excluded students with missing proficiency are not considered tested
        performance_level = if_else(el_excluded == 1, NA_character_, performance_level),
        tested = if_else(el_excluded == 1 & is.na(original_performance_level) & subject %in% c("Math", "Science", math_eoc, science_eoc), 0, tested),
        tested = if_else(el_excluded == 1 & !is.na(original_performance_level) & subject %in% c("Math", "Science", math_eoc, science_eoc), 1, tested),
    # Proficiency modified to missing if nullify or did not attempt
        performance_level = if_else(did_not_attempt == 1, NA_character_, performance_level),
    # Convert subjects per accountability rules
        subject = case_when(
            grade %in% 3:8 & original_subject %in% math_eoc ~ "Math",
            grade %in% 3:8 & original_subject %in% english_eoc ~ "ELA",
            grade %in% 3:8 & original_subject %in% science_eoc ~ "Science",
            grade %in% 3:8 & original_subject == "US History" ~ "Social Studies",
            TRUE ~ subject
        )
    )

dedup <- student_level %>%
# For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    mutate(
        test_priority = case_when(
            test %in% c("MSAA", "Alt-Science") ~ 3,
            test == "EOC" ~ 2,
            test == "Achievement" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
    filter(test_priority == temp | temp == -Inf) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same test, take highest proficiency level
    mutate(prof_priority = case_when(
            performance_level %in% c("Below", "Below Basic") ~ 4,
            performance_level %in% c("Approaching", "Basic") ~ 3,
            performance_level %in% c("On track", "Proficient") ~ 2,
            performance_level %in% c("Mastered", "Advaced") ~ 1
        )
    ) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(prof_priority == temp | temp == -Inf) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
# For students with multiple test records with the same proficiency across administrations, take the most recent
    mutate(
        semester_priority = case_when(
            test %in% c("MSAA", "Alt-Science") | test == "Achievement" | (test == "EOC" & semester == "Spring") ~ 3,
            test == "EOC" & semester == "Fall" ~ 2,
            test == "EOC" & semester == "Summer" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
    filter(semester_priority == temp | temp == -Inf) %>%
    select(-semester_priority, -temp) %>%
    ungroup() %>%
# Valid test if there is a proficiency level
    mutate(valid_test = as.numeric(!is.na(performance_level)))

output <- dedup %>%
    filter(!is.na(state_student_id)) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, semester,
        original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
        state_student_id, last_name, first_name, grade, race, bhn_group,
        functionally_delayed, special_ed, economically_disadvantaged, el, el_t1234, el_excluded,
        enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent, breach_adult, breach_student,
        irregular_admin, incorrect_grade_subject, did_not_attempt, residential_facility) %>%
    arrange(system, school, state_student_id)

write_csv(output, "N:/ORP_accountability/projects/2018_student_level_file/2018_eoc_student_level_file.csv", na = "")
