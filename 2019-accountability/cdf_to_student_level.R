library(acct)
library(tidyverse)

fall_eoc <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_fall_eoc_cdf.csv",
        col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc") %>%
    mutate(
        test = "EOC",
        semester = "Fall"
    )

cdf <- bind_rows(fall_eoc) %>%
    mutate(
        performance_level = if_else(performance_level == "On track", "On Track", performance_level),
        absent = reason_not_tested == 1,
        not_enrolled = reason_not_tested == 2,
        not_scheduled = reason_not_tested == 3,
        medically_exempt = reason_not_tested == 4,
        residential_facility = reason_not_tested == 5,
        tested_alt = reason_not_tested == 6,
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
            content_area_code == "E1" ~ "English I",
            content_area_code == "E2" ~ "English II",
            content_area_code == "G1" ~ "Geometry",
            content_area_code == "M1" ~ "Integrated Math I",
            content_area_code == "M2" ~ "Integrated Math II",
            content_area_code == "M3" ~ "Integrated Math III",
            content_area_code == "U1" ~ "US History"
        )
    )

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

# Integrated Math districts for reassigning MSAA subjects
int_math_systems <- cdf %>%
    filter(content_area_code %in% c("A1", "M1")) %>%
    count(system, content_area_code) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, content_area_code == "M1") %>%
    magrittr::extract2("system") %>%
    as.integer()

student_level <- bind_rows(cdf) %>%
    transmute(
        system,
        system_name,
        school,
        school_name,
        test,
        semester,
        original_subject,
        subject = original_subject,
        original_performance_level = performance_level, 
        performance_level,
        scale_score,
        state_student_id = unique_student_id,
        last_name,
        first_name,
        grade,
        gender,
        reported_race, 
        bhn_group = reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"),
        economically_disadvantaged,
        el,
        el_recently_arrived = (el_arrived_year_1 == 1 | el_arrived_year_2 == 1),
        t1234 = t1234 %in% 1:4,
        special_ed,
        functionally_delayed,
        gifted,
        migrant,
        enrolled_50_pct_district, 
        enrolled_50_pct_school,
        breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness,
        absent, not_enrolled, not_scheduled, medically_exempt, residential_facility, tested_alt, did_not_submit
    ) %>%
    mutate_at(
        .vars = vars(bhn_group, t1234, el_recently_arrived),
        .f = as.integer
    ) %>%
    rowwise() %>%
    # Apply testing flag hierarchy
    mutate(
        enrolled = case_when(
            any(breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness) ~ 0,
            any(not_enrolled, not_scheduled) ~ 0,
            TRUE ~ 1
        ),
        # EL Recently Arrived students with missing proficiency are not considered tested
        tested = case_when(
            any(breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness) ~ 0,
            any(absent, not_enrolled, not_scheduled) ~ 0,
            el_recently_arrived == 1L & is.na(original_performance_level) ~ 0,
            TRUE ~ 1
        ),
        # EL Recently Arrived students performance level are converted to missing
        performance_level = case_when(
            any(breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness) ~ NA_character_,
            any(absent, not_enrolled, not_scheduled, medically_exempt, residential_facility, did_not_submit) ~ NA_character_,
            el_recently_arrived == 1 ~ NA_character_,
            TRUE ~ performance_level
        ),
        # Modify subject for MSAA tests in grades >= 9 (6.8)
        subject = case_when(
            original_subject == "Math" & test == "MSAA" & grade >= 9 & system %in% int_math_systems ~ "Integrated Math I",
            original_subject == "Math" & test == "MSAA" & grade >= 9 & !system %in% int_math_systems ~ "Algebra I",
            original_subject == "ELA" & test == "MSAA" & grade >= 9 ~ "English II",
            TRUE ~ subject
        ),
        # Convert subjects per accountability rules
        subject = case_when(
            grade %in% 3:8 & original_subject %in% math_eoc ~ "Math",
            grade %in% 3:8 & original_subject %in% english_eoc ~ "ELA",
            grade %in% 3:8 & original_subject == "US History" ~ "Social Studies",
            TRUE ~ subject
        )
    ) %>%
    ungroup()

# Records from Alternative, CTE, Adult HS are dropped from student level
# cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
#     transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

dedup <- student_level %>%
    # anti_join(cte_alt_adult, by = c("system", "school")) %>%
    mutate(
        # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
        test_priority = case_when(
            test %in% c("MSAA", "Alt-Social Studies") ~ 3,
            test == "EOC" ~ 2,
            test == "TNReady" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
    filter(test_priority == temp | temp == -Inf) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same test, take highest performance level
    mutate(
        prof_priority = case_when(
            performance_level %in% c("Below", "Below Basic") ~ 1,
            performance_level %in% c("Approaching", "Basic") ~ 2,
            performance_level %in% c("On Track", "Proficient") ~ 3,
            performance_level %in% c("Mastered", "Advanced") ~ 4
        )
    ) %>%
    group_by(state_student_id, original_subject, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(prof_priority == temp | temp == -Inf) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same performance level, take highest scale score
    group_by(state_student_id, original_subject, test, performance_level) %>%
    mutate(temp = max(scale_score, na.rm = TRUE)) %>%
    filter(scale_score == temp | temp == -Inf) %>%
    select(-temp) %>%
    ungroup() %>%
# For students with multiple test records with the same proficiency across administrations, take the most recent
    mutate(
        semester_priority = case_when(
            test %in% c("MSAA", "Alt-Science/Social Studies", "Achievement") | (test == "EOC" & semester == "Spring") ~ 2,
            test == "EOC" & semester == "Fall" ~ 1
        )
    ) %>%
    group_by(state_student_id, original_subject, test) %>%
    mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
    filter(semester_priority == temp | temp == -Inf) %>%
    select(-semester_priority, -temp) %>%
    ungroup() %>%
# For students with multiple test records with the same original subject, performance level, scale score
# Deduplicate by missing race/ethnicity (!)
    group_by(state_student_id, original_subject, test, performance_level, scale_score, semester) %>%
    mutate(
        n = n(),                           # Tag duplicates by id, subject, test, performance level, scale_score, semester
        temp = mean(is.na(reported_race))  # Check whether one among duplicates has non-missing race/ethnicity
    ) %>%
    filter(!(n > 1 & temp != 0 & is.na(reported_race))) %>%
    ungroup() %>%
    select(-n, -temp) %>%
# For students multiple test records with the same original subject, performance level, scale score, demographics
# Deduplicate for non-missing grade (!)
    group_by(state_student_id, original_subject, test, performance_level, scale_score, semester, reported_race) %>%
    mutate(
        n = n(),                   # Tag duplicates by id, subject, test, performance, leve, scale_score, semester
        temp = mean(is.na(grade))  # Check whether one among duplicates has non-missing race/ethnicity
    ) %>%
    filter(!(n > 1 & temp != 0 & is.na(grade))) %>%
    ungroup() %>%
    select(-n, -temp) %>%
# Valid test if there is a proficiency level
    mutate(valid_test = as.integer(!is.na(performance_level)))

# Reassigned schools for accountability
enrollment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/enrollment.csv") %>%
    mutate_at(vars(acct_system, acct_school), as.integer)

output <- dedup %>%
    select(
        system, system_name, school, school_name, test, original_subject, subject, semester,
        original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
        state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group,
        functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
        enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test, residential_facility
    ) %>%
    mutate_at(vars(absent, refused_to_test, residential_facility), as.integer) %>%
# Percentiles by grade and original subject for 3-8
    group_by(test, original_subject, grade) %>%
    mutate(
        rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
        denom = sum(!is.na(scale_score)),
        percentile = if_else(test == "Achievement", round5(100 * rank/denom, 1), NA_real_)
    ) %>%
# Percentiles by original subject for EOCs
    group_by(test, original_subject) %>%
    mutate(
        rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
        denom = sum(!is.na(scale_score)),
        percentile = if_else(test == "EOC", round5(100 * rank/denom, 1), percentile)
    ) %>%
    ungroup() %>%
    select(-rank, -denom) %>%
    arrange(system, school, state_student_id) %>%
# Add system and school for accountability purposes
    left_join(enrollment, by = c("state_student_id" = "student_id")) %>%
    mutate(
        acct_system = if_else(is.na(acct_system), system, acct_system),
        acct_school = if_else(is.na(acct_school), school, acct_school)
    )

write_csv(output, "N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv", na = "")
