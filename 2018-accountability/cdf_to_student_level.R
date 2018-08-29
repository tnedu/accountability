library(haven)
library(tidyverse)

alt_science_ss <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_alt_science_ss_cdf.csv") %>%
    mutate(
        test = "Alt-Science/Social Studies",
        grade = if_else(grade == "HS", "11", grade),
        semester = "Spring",
        performance_level = case_when(
            performance_level == "Level 3" ~ "Mastered",
            performance_level == "Level 2" ~ "On Track",
            performance_level == "Level 1" ~ "Approaching"
        ),
        original_subject = case_when(
            content_area_code == "SCI" ~ "Science",
            content_area_code == "SOC" ~ "Social Studies",
            content_area_code == "B1" ~ "Biology I"
        ),
        absent = reason_not_tested == 1,
        medically_exempt = reason_not_tested == 4,
        residential_facility = reason_not_tested == 5,
        did_not_submit = reason_not_tested == 7,
        breach_adult = ri_status == 1,
        breach_student = ri_status == 2,
        irregular_admin = ri_status == 3,
        incorrect_grade_subject = ri_status == 4,
        refused_to_test = ri_status == 5,
        failed_attemptedness = ri_status == 6
    ) %>%
    mutate_at(c("unique_student_id", "teacher_of_record_tln", "raw_score", "scale_score"), as.integer)

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

el_38 <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/EL status and variables 2018.csv") %>%
    transmute(unique_student_id = `Student Key`,
        el = if_else(`IS EL` == 1, "Y", "N"),
        el_arrived_year_1 = if_else(`Recently Arrived Year 1` == 1, "Y", "N"),
        el_arrived_year_2 = if_else(`Recently Arrived Year 2` == 1, "Y", "N"),
        el_t1234 = `T1T2  (T1T4)`
    ) %>%
    distinct()

# MSAA
msaa_math <- read_dta("N:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2017-18/20180828_MSAA_StateStudentResults_SY2017-18_Whalen_v1.dta") %>%
    transmute(
        system = as.character(districtid),
        school = as.character(schoolid),
        unique_student_id = state_student_id,
        last_name = lastorsurname,
        first_name = firstname,
        economically_disadvantaged = if_else(economicdisadvantagestatus == "Yes", "Y", "N"),
        el = if_else(lepstatus == "Yes", "Y", "N"),
        reported_race = case_when(
            hispanicorlatinaethnicity == "Yes" ~ "H",
            blackorafricanamerican == "Yes" ~ "B",
            americanindianoralaskanative == "Yes" ~ "I",
            nativehawaiianothpacificislander == "Yes" ~ "P",
            asian == "Yes" ~ "A",
            white == "Yes" ~ "W",
            TRUE ~ NA_character_
        ),
        grade = as.character(grade), original_subject = "Math",
        scale_score = matscaledscore,
        performance_level = case_when(
            matperflevel == 1 ~ "Below",
            matperflevel == 2 ~ "Approaching",
            matperflevel == 3 ~ "On Track",
            matperflevel == 4 ~ "Mastered"
        ),
        reporting_status = matreportingstatus
    )

msaa_ela <- read_dta("N:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2017-18/20180828_MSAA_StateStudentResults_SY2017-18_Whalen_v1.dta") %>%
    transmute(
        system = as.character(districtid),
        school = as.character(schoolid),
        unique_student_id = state_student_id,
        last_name = lastorsurname,
        first_name = firstname,
        economically_disadvantaged = if_else(economicdisadvantagestatus == "Yes", "Y", "N"),
        el = if_else(lepstatus == "Yes", "Y", "N"),
        reported_race = case_when(
            hispanicorlatinaethnicity == "Yes" ~ "H",
            blackorafricanamerican == "Yes" ~ "B",
            americanindianoralaskanative == "Yes" ~ "I",
            nativehawaiianothpacificislander == "Yes" ~ "P",
            asian == "Yes" ~ "A",
            white == "Yes" ~ "W",
            TRUE ~ NA_character_
        ),
        grade = as.character(grade), original_subject = "ELA",
        performance_level = case_when(
            elaperflevel == 1 ~ "Below",
            elaperflevel == 2 ~ "Approaching",
            elaperflevel == 3 ~ "On Track",
            elaperflevel == 4 ~ "Mastered"
        ),
        scale_score = elascaledscore,
        reporting_status = elareportingstatus
    )

enrollment <- read_csv("C:/Users/ca18761/Desktop/Student Demographics 2017 Final and Fall Snapshot.csv") %>%
    filter(Snapshottitle == "2017Final") %>%
    transmute(unique_student_id = STUDENT_KEY, system = as.character(DISTRICT_ID), school = as.character(SCHOOL_ID),
        enrolled_50_pct_district = DISTRICT50PERCENT, enrolled_50_pct_school = SCHOOL50PERCENT) %>%
    distinct()

msaa <- bind_rows(msaa_math, msaa_ela) %>%
    filter(!reporting_status %in% c("INV", "EXE", "WDR", "NLE")) %>%
    mutate(
        test = "MSAA",
        semester = "Spring",
        special_ed = "Y",
        performance_level = if_else(reporting_status == "DNT", "Approaching", performance_level),
        absent = is.na(performance_level),
        enrolled = 1,
        tested = if_else(reporting_status == "PRF", 0, 1)
    ) %>%
    left_join(enrollment, by = c("system", "school", "unique_student_id")) %>%
    mutate_at(c("enrolled_50_pct_school", "enrolled_50_pct_district"), ~ if_else(is.na(.), "Y", .))

cdf_38 <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_3_8_cdf.csv") %>%
    select(-el, -el_arrived_year_1, -el_arrived_year_2, -el_t1234) %>%
    left_join(el_38, by = "unique_student_id") %>%
    mutate_at(c("el", "el_arrived_year_1", "el_arrived_year_2"), ~ if_else(is.na(.), "N", .)) %>%
    mutate(
        el_t1234 = if_else(is.na(el_t1234), 0L, el_t1234),
        test = "TNReady",
        semester = "Spring",
        teacher_of_record_tln = as.integer(teacher_of_record_tln)
    )

cdf <- bind_rows(fall_cdf, spring_cdf, cdf_38) %>%
    mutate(
        performance_level = if_else(performance_level == "On track", "On Track", performance_level),
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

# Integrated Math districts for reassigning MSAA subjects
int_math_systems <- cdf %>%
    filter(content_area_code %in% c("A1", "M1")) %>%
    count(system, content_area_code) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, content_area_code == "M1") %>%
    magrittr::extract2("system") %>%
    as.integer()

student_level <- bind_rows(cdf, alt_science_ss, msaa) %>%
    mutate(enrolled = 1,
        tested = if_else(test != "MSAA", 1, tested),
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
    select(system, school, test, original_subject, subject, 
        original_performance_level, performance_level, scale_score,
        enrolled, tested, valid_test, state_student_id = unique_student_id,
        last_name, first_name, grade, race, bhn_group, functionally_delayed, special_ed,
        economically_disadvantaged, el, el_t1234, el_recently_arrived,
        enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent,
        breach_adult, breach_student, irregular_admin, incorrect_grade_subject,
        refused_to_test, failed_attemptedness, residential_facility, did_not_submit,
        semester, ri_status, medically_exempt, teacher_of_record_tln) %>%
    mutate_at(
        c("system", "school", "state_student_id", "grade", "bhn_group", "functionally_delayed", "special_ed",
            "economically_disadvantaged", "el", "el_t1234", "el_recently_arrived", "homebound", "absent",
            "breach_adult", "breach_student", "irregular_admin", "incorrect_grade_subject", 
            "refused_to_test", "failed_attemptedness", "residential_facility"),
        as.numeric) %>%
# Drop excluded records
    filter(!is.na(system),
        grade != 13 | is.na(grade),
        !(school == 981 | system > 1000),
    # Drop medically exempt
        medically_exempt == FALSE | is.na(medically_exempt)) %>%
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
    # Modify subject for MSAA tests in grades >= 9 (6.8)
        subject = case_when(
            original_subject == "Math" & test == "MSAA" & grade >= 9 & system %in% int_math_systems ~ "Integrated Math I",
            original_subject == "Math" & test == "MSAA" & grade >= 9 & !system %in% int_math_systems ~ "Algebra I",
            original_subject == "ELA" & test == "MSAA" & grade >= 9 ~ "English II",
            TRUE ~ subject
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
    mutate(
    # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
        test_priority = case_when(
            test %in% c("MSAA", "Alt-Science/Social Studies") ~ 3,
            test == "EOC" ~ 2,
            test == "TNReady" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
# There are about 5% missing student IDs in Alt-Science/SS; Avoid dropping these
    filter(test_priority == temp | temp == -Inf | (is.na(state_student_id) & test == "Alt-Science/Social Studies")) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same test, take highest proficiency level
    mutate(
        prof_priority = case_when(
            performance_level %in% c("Below", "Below Basic") ~ 1,
            performance_level %in% c("Approaching", "Basic") ~ 2,
            performance_level %in% c("On Track", "Proficient") ~ 3,
            performance_level %in% c("Mastered", "Advanced") ~ 4
        )
    ) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(prof_priority == temp | temp == -Inf | (is.na(state_student_id) & test == "Alt-Science/Social Studies")) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
# For students with multiple test records with the same proficiency across administrations, take the most recent
    mutate(
        semester_priority = case_when(
            test %in% c("MSAA", "Alt-Science/Social Studies", "Achievement") | (test == "EOC" & semester == "Spring") ~ 2,
            test == "EOC" & semester == "Fall" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
    filter(semester_priority == temp | temp == -Inf | (is.na(state_student_id) & test == "Alt-Science/Social Studies")) %>%
    select(-semester_priority, -temp) %>%
    ungroup() %>%
# Valid test if there is a proficiency level
    mutate(valid_test = as.numeric(!is.na(performance_level)))

school_names <- readxl::read_excel("N:/ORP_accountability/data/2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>% 
    transmute(
        system = as.integer(`DG 4 LEA ID (State)`), school = as.integer(`DG 5 School ID (State)`),
        system_name = `EXTRA ITEM - LEA Name`, school_name = `DG 7 School Name`
    ) %>%
    bind_rows(
        tribble(
            ~system, ~system_name, ~school, ~school_name,
            970, "Department of Children's Services", 25, "Gateway to Independence",
            970, "Department of Children's Services", 45, "Wilder Youth Development Center",
            970, "Department of Children's Services", 65, "Mountain View Youth Development Center",
            970, "Department of Children's Services", 140, "DCS Affiliated Schools"
        )
    )

output <- dedup %>%
    filter(!(original_subject == "Science" & grade %in% c("3", "4"))) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, semester,
        original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
        state_student_id, last_name, first_name, grade, race, bhn_group, teacher_of_record_tln,
        functionally_delayed, special_ed, economically_disadvantaged, el, el_t1234, el_recently_arrived,
        enrolled_50_pct_district, enrolled_50_pct_school, homebound, absent, refused_to_test, residential_facility) %>%
    arrange(system, school, state_student_id)

write_csv(output, "N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv", na = "")
