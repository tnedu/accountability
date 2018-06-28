library(tidyverse)

fall_cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_fall_cdf.csv") %>%
    mutate(
        test = "EOC",
        semester = "Fall",
        raw_score = as.integer(raw_score)
    )

el_hs <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/el_recently_arrived.csv") %>%
    transmute(unique_student_id = student_key,
        el = if_else(isel == 1, "Y", "N"),
        el_arrived_year_1 = if_else(ELRECENTLYARRIVEDYEARONE == 1, "Y", "N"),
        el_arrived_year_2 = if_else(ELRECENTLYARRIVEDYEARTWO == 1, "Y", "N")
    ) %>%
    distinct()

spring_cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_spring_cdf.csv") %>%
    mutate(
        test = "EOC",
        semester = "Spring"
    ) %>%
    select(-el, -el_arrived_year_1, -el_arrived_year_2) %>%
    left_join(el_hs, by = "unique_student_id") %>%
    mutate_at(c("el", "el_arrived_year_1", "el_arrived_year_2"), ~ if_else(is.na(.), "N", .))

el_38 <- readxl::read_excel("N:/ORP_accountability/projects/2018_student_level_file/el_recently_arrived_3_8.xlsx") %>%
    transmute(unique_student_id = student_key,
        el = if_else(isel == 1, "Y", "N"),
        el_arrived_year_1 = if_else(ELRECENTLYARRIVEDYEARONE == 1, "Y", "N"),
        el_arrived_year_2 = if_else(ELRECENTLYARRIVEDYEARTWO == 1, "Y", "N")
    ) %>%
    distinct()

cdf_38 <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_3_8_cdf.csv") %>%
    select(-el, -el_arrived_year_1, -el_arrived_year_2) %>%
    left_join(el_38, by = "unique_student_id") %>%
    mutate_at(c("el", "el_arrived_year_1", "el_arrived_year_2"), ~ if_else(is.na(.), "N", .)) %>%
    mutate(
        test = "TNReady",
        semester = "Spring",
        teacher_of_record_tln = as.integer(teacher_of_record_tln)
    )

cdf <- bind_rows(fall_cdf, spring_cdf, cdf_38) %>%
    mutate(
        absent = reason_not_tested == 1,
        medically_exempt = reason_not_tested == 4,
        residential_facility = reason_not_tested == 5,
        did_not_submit = reason_not_tested == 7,
        breach_adult = ri_status == 1,
        breach_student = ri_status == 2,
        irregular_admin = ri_status == 3,
        incorrect_grade_subject = ri_status == 4,
        refused_to_test = ri_status == 5,
        failed_attemptedness = ri_status == 6,
        original_subject = case_when(
            content_area_code == "ENG" ~ "ELA",
            content_area_code == "MAT" ~ "Math",
            content_area_code == "SCI" ~ "Science",
            content_area_code == "SOC" ~ "Social Studies",
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
        el_recently_arrived = (el_arrived_year_1 == "Y" | el_arrived_year_2 == "Y"),
        el_t1234 = el_t1234 %in% 1:4,
        special_ed = special_ed == "Y",
        functionally_delayed = functionally_delayed == "Y",
        homebound = homebound == "Y",
        original_performance_level = performance_level,
        subject = original_subject
    ) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, 
        original_performance_level, performance_level, scale_score,
        enrolled, tested, valid_test, state_student_id = unique_student_id,
        last_name, first_name, grade, race, bhn_group, functionally_delayed, special_ed,
        economically_disadvantaged, el, el_t1234, el_recently_arrived,
        enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent,
        breach_adult, breach_student, irregular_admin, incorrect_grade_subject,
        refused_to_test, failed_attemptedness, residential_facility, did_not_submit,
        semester, ri_status, medically_exempt, teacher_of_record_tln) %>%
    mutate_at(c("system", "school", "state_student_id", "grade", "bhn_group", "functionally_delayed", "special_ed",
            "economically_disadvantaged", "el", "el_t1234", "el_recently_arrived", "homebound", "absent",
            "breach_adult", "breach_student", "irregular_admin", "incorrect_grade_subject", 
            "refused_to_test", "failed_attemptedness", "residential_facility"),
        as.numeric) %>%
# Drop excluded records
    filter(!is.na(system),
        grade != 13 | is.na(grade),
        !(school == 981 | system > 1000),
    # Drop medically exempt
        medically_exempt == FALSE) %>%
# Apply testing flag hierarchy
    # Absent (reason_not_tested 1) students have a missing proficiency and are not tested
    # EL Recently Arrived students with missing proficiency are not considered tested
    # EL Recently Arrived students performance level are converted to missing
    # Proficiency modified to missing if refused to test or failed attemptedness
    # Any record with an RI status of 0 or 3 (Irregular Administration) is enrolled and tested, but do not have performance levels
    # Any record with an RI status other than 0 or 3 is neither enrolled nor tested
    mutate(
        enrolled = case_when(
            absent == 1 ~ 1,
            breach_adult == 1 | breach_student == 1 | incorrect_grade_subject == 1 | refused_to_test == 1 | failed_attemptedness == 1 ~ 0,
            TRUE ~ enrolled
        ),
        tested = case_when(
            absent == 1 ~ 0,
            irregular_admin == 1 ~ 1,
            el_recently_arrived == 1 & is.na(original_performance_level) ~ 0,
            breach_adult == 1 | breach_student == 1 | incorrect_grade_subject == 1 | refused_to_test == 1 | failed_attemptedness == 1 ~ 0,
            TRUE ~ tested
        ),
        performance_level = case_when(
            absent == 1 | el_recently_arrived == 1 | refused_to_test == 1 | failed_attemptedness == 1 | irregular_admin == 1 ~ NA_character_,
            TRUE ~ performance_level
        ),
    # Convert subjects per accountability rules
        subject = case_when(
            grade %in% 2:8 & original_subject %in% math_eoc ~ "Math",
            grade %in% 2:8 & original_subject %in% english_eoc ~ "ELA",
            grade %in% 2:8 & original_subject %in% science_eoc ~ "Science",
            grade %in% 2:8 & original_subject == "US History" ~ "Social Studies",
            TRUE ~ subject
        )
    )

# Records from Alternative, CTE, Adult HS are dropped from student level
alt_cte_adult <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
    transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), cte_alt_adult = 1)

dedup <- student_level %>%
    anti_join(alt_cte_adult, by = c("system", "school")) %>%
# For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    mutate(
        test_priority = case_when(
            test %in% c("MSAA", "Alt-Science") ~ 3,
            test == "EOC" ~ 2,
            test == "TNReady" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
    filter(test_priority == temp | temp == -Inf) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same test, take highest proficiency level
    mutate(prof_priority = case_when(
            performance_level %in% c("Below", "Below Basic") ~ 1,
            performance_level %in% c("Approaching", "Basic") ~ 2,
            performance_level %in% c("On track", "Proficient") ~ 3,
            performance_level %in% c("Mastered", "Advanced") ~ 4
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
    filter(!(original_subject == "Social Studies" | (original_subject == "Science" & grade %in% c("3", "4")))) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, semester,
        original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
        state_student_id, last_name, first_name, grade, race, bhn_group, teacher_of_record_tln,
        functionally_delayed, special_ed, economically_disadvantaged, el, el_t1234, el_recently_arrived,
        enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent, refused_to_test, residential_facility) %>%
    mutate(performance_level = if_else(performance_level == "On track", "On Track", performance_level)) %>%
    arrange(system, school, state_student_id)

write_csv(output, "N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv", na = "")
