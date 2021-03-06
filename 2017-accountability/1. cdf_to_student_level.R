library(haven)
library(readxl)
library(tidyverse)

alt_science <- read_dta("N:/ORP_accountability/data/2017_cdf/311_alt_091517.dta") %>%
    filter(content_area_code != "SOC") %>%
    mutate(greater_than_60_pct = "Y",
        enrolled = 1,
        test = "Alt-Science",
        original_subject = case_when(
            content_area_code == "B1" ~ "Biology I",
            content_area_code == "SCI" ~ "Science"
        ),
        reported_race = case_when(
            ethnic_origin == "H" ~ 4,
            black == 1 ~ 3,
            native_american == 1 ~ 1,
            hawaiian_pi == 1 ~ 5,
            asian == 1 ~ 2,
            white == 1 ~ 6,
            TRUE ~ 0
        ),
        absent = as.numeric(ri_status_final == 5),
        performance_level = case_when(
            performance_level == 3 ~ 4,
            performance_level == 2 ~ 3,
            performance_level == 1 ~ 2
        )
    )

ach_cdf <- read_dta("N:/ORP_accountability/data/2017_cdf/38_cdf_091417.dta") %>%
    mutate(test = "Achievement",
        semester = "Spring",
        el_excluded = as.numeric(el_excluded == 1 | EL_accommodationsU_part1 == "U" | EL_accommodationsU_part2 == "U"),
        scale_score_lb_ci = as.numeric(scale_score_lb_ci),
        ri_status_final = if_else(content_area_code == "ENG" & el_excluded == 1, 0, ri_status_final)) %>%
    filter(content_area_code != "SOC") %>%
# Drop 8th grade math for Athens City on appeals
    filter(!(system == 541 & content_area_code == "MAT" & grade == 8)) %>%
# Van Buren Modification on Appeal
    mutate(greater_than_60_pct = if_else(system == 880 & grade >= 6, "Y", greater_than_60_pct))

fall_cdf <- read_dta("N:/ORP_accountability/data/2017_cdf/fall_eoc_cdf_081517.dta") %>%
# Student level file variables
    mutate(test = "EOC",
        semester = "Fall",
        el_excluded = as.numeric(el_excluded == 1 | EL_accommodationsU_part1 == "U" | EL_accommodationsU_part2 == "U"),
        ri_status_final = if_else(content_area_code %in% c("E1", "E2", "E3", "U1") & (is.na(ri_status_part_1) | ri_status_part_1 %in% c(0, 5)),
            if_else(!is.na(ri_status_part_2), ri_status_part_2, ri_status_part_1), ri_status_part_1),
        ri_status_final = if_else(el_excluded == 1, 0, ri_status_final),
        ri_status_final = if_else(is.na(ri_status_final), 0, ri_status_final),
    # McKenzie SSD Modification on Appeal
        greater_than_60_pct = if_else(system == 94, "Y", greater_than_60_pct))

cdf <- read_dta("N:/ORP_accountability/data/2017_cdf/Spring_EOC_CDF_101117.dta") %>%
# Van Buren Modification on Appeal
    mutate(greater_than_60_pct = if_else(system == 880, "Y", greater_than_60_pct)) %>%
# Student level file variables
    mutate(test = "EOC",
        semester = "Spring",
        el_excluded = if_else(el_excluded == 1 | EL_accommodationsU_part1 == "U" | EL_accommodationsU_part2 == "U", 1, 0),
        ri_status_final = if_else(el_excluded == 1, 0, ri_status_final),
        ri_status_final = if_else(is.na(ri_status_final), 0, ri_status_final)) %>%
    bind_rows(fall_cdf, ach_cdf) %>%
    mutate(absent = ri_status_final == 5,
        did_not_attempt = ri_status_final == 4,
        residential_facility = ri_status_final == 1,
        nullify_flag = ri_status_final == 2,
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
    ) %>%
    mutate_at(c("absent", "did_not_attempt", "residential_facility", "nullify_flag"), as.numeric)

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
    magrittr::extract2("system")

# MSAA
msaa_math <- read_csv("N:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2016-17/20170630_MSAA_StateStudentResults_SY2016-17_Whalen_v1.csv") %>%
    transmute(system = as.numeric(DistrictID), school = as.numeric(SchoolID),
        unique_student_id = State_Student_ID,
        economically_disadvantaged = if_else(EconomicDisadvantageStatus == "Yes", 1, 0),
        el = if_else(LEPStatus == "Yes", 1, 0),
        reported_race = case_when(
            HispanicOrLatinaEthnicity == "Yes" ~ 4,
            BlackorAfricanAmerican == "Yes" ~ 3,
            AmericanIndianOrAlaskaNative == "Yes" ~ 1,
            NativeHawaiianOthPacificIslander == "Yes" ~ 5,
            Asian == "Yes" ~ 2,
            White == "Yes" ~ 6,
            TRUE ~ 0
        ),
        grade = as.numeric(Grade), original_subject = "Math",
        performance_level = MatPerfLevel, reporting_status = MatReportingStatus)

msaa_ela <- read_csv("N:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2016-17/20170630_MSAA_StateStudentResults_SY2016-17_Whalen_v1.csv") %>%
    transmute(system = as.numeric(DistrictID), school = as.numeric(SchoolID),
        unique_student_id = State_Student_ID,
        economically_disadvantaged = if_else(EconomicDisadvantageStatus == "Yes", 1, 0),
        el = if_else(LEPStatus == "Yes", 1, 0),
        reported_race = case_when(
            HispanicOrLatinaEthnicity == "Yes" ~ 4,
            BlackorAfricanAmerican == "Yes" ~ 3,
            AmericanIndianOrAlaskaNative == "Yes" ~ 1,
            NativeHawaiianOthPacificIslander == "Yes" ~ 5,
            Asian == "Yes" ~ 2,
            White == "Yes" ~ 6,
            TRUE ~ 0
        ),
        grade = as.numeric(Grade), original_subject = "ELA",
        performance_level = ELAPerfLevel, reporting_status = ELAReportingStatus)

msaa <- bind_rows(msaa_math, msaa_ela) %>%
    filter(!reporting_status %in% c("INV", "EXE", "WDR", "NLE")) %>%
    mutate(test = "MSAA",
        performance_level = if_else(reporting_status == "DNT", 2L, performance_level),
        absent = as.numeric(is.na(performance_level)),
        enrolled = 1,
        tested = if_else(reporting_status == "PRF", 0, 1))

# Records from Alternative, CTE, Adult HS are dropped from student level
alt_cte_adult <- read_excel("N:/ORP_accountability/data/2017_tdoe_provided_files/List of Schools Acct 2016-17.xlsx") %>%
    transmute(system = DISTRICT_NUMBER, school = SCHOOL_NUMBER, cte_alt_adult = 1)

# Student level file
student_level <- bind_rows(cdf, msaa, alt_science) %>%
    anti_join(alt_cte_adult, by = c("system", "school")) %>%
    mutate_at(c("nullify_flag", "did_not_attempt"), funs(if_else(is.na(.), 0, .))) %>%
    mutate(system_name = stringr::str_to_title(system_name),
        enrolled = 1,
        tested = if_else(test != "MSAA", 1, tested),
        valid_test = NA_integer_,
        race = case_when(
            reported_race == 4 ~ "Hispanic/Latino",
            reported_race == 3 ~ "Black or African American",
            reported_race == 1 ~ "American Indian/Alaska Native",
            reported_race == 5 ~ "Native Hawaiian/Pac. Islander",
            reported_race == 2 ~ "Asian",
            reported_race == 6 ~ "White",
            reported_race == 0 ~ "Unknown"
        ),
        bhn_group = as.numeric(race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native")),
        economically_disadvantaged = if_else(economically_disadvantaged == 2, 0, economically_disadvantaged),
        el_excluded = if_else(is.na(el_excluded), 0, el_excluded),
        el_t1_t2 = if_else(el_t1_t2 == 2, 1, el_t1_t2),
        special_ed = as.numeric(special_ed %in% c(1, 2, 3)),
        original_proficiency_level = case_when(
            original_subject %in% c(english_eoc, math_eoc, "Math", "ELA", "US History") & performance_level == 1 ~ "1. Below",
            original_subject %in% c(english_eoc, math_eoc, "Math", "ELA", "US History") & performance_level == 2 ~ "2. Approaching",
            original_subject %in% c(english_eoc, math_eoc, "Math", "ELA", "US History") & performance_level == 3 ~ "3. On Track",
            original_subject %in% c(english_eoc, math_eoc, "Math", "ELA", "US History") & performance_level == 4 ~ "4. Mastered",
            original_subject %in% c("Science", science_eoc) & performance_level == 1 ~ "1. Below Basic",
            original_subject %in% c("Science", science_eoc) & performance_level == 2 ~ "2. Basic",
            original_subject %in% c("Science", science_eoc) & performance_level == 3 ~ "3. Proficient",
            original_subject %in% c("Science", science_eoc) & performance_level == 4 ~ "4. Advanced"
        ),
        proficiency_level = original_proficiency_level,
        subject = original_subject) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_proficiency_level, proficiency_level,
        scale_score, enrolled, tested, valid_test, state_student_id = unique_student_id, last_name, first_name,
        grade, race, bhn_group, functionally_delayed, special_ed, economically_disadvantaged, el, el_t1_t2, el_excluded,
        greater_than_60_pct, homebound, absent, did_not_attempt, nullify_flag, residential_facility, semester, ri_status_final) %>%
    mutate_at(c("system", "school", "state_student_id", "grade"), as.numeric) %>% 
# Drop excluded records
    filter(!is.na(system)) %>%
    filter(grade != 13 | is.na(grade)) %>%
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
        tested = if_else(el_excluded == 1 & is.na(original_proficiency_level) & subject %in% c("Math", "Science", math_eoc, science_eoc), 0, tested),
        tested = if_else(el_excluded == 1 & !is.na(original_proficiency_level) & subject %in% c("Math", "Science", math_eoc, science_eoc), 1, tested),
    # Proficiency modified to missing if nullify or did not attempt
        proficiency_level = if_else(nullify_flag == 1 | did_not_attempt == 1, NA_character_, proficiency_level),
    # Students taking MSAA are considered special education (5.5)
        special_ed = if_else(test == "MSAA", 1, special_ed),
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
            grade %in% 3:8 & original_subject %in% science_eoc ~ "Science",
            grade %in% 3:8 & original_subject == "US History" ~ "Social Studies",
            TRUE ~ subject
        ),
        test = if_else(grade %in% 3:8 & original_subject %in% c(math_eoc, english_eoc, science_eoc, "US History"), "Achievement", test)
    )

dedup <- student_level %>%
# For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    mutate(test_priority = case_when(
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
    mutate(prof_priority = as.integer(substr(proficiency_level, 1, 1))) %>%
    group_by(state_student_id, subject, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(prof_priority == temp | temp == -Inf) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
# For students with multiple test records with the same proficiency across administrations, take the most recent
    mutate(semester_priority = case_when(
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
    mutate(valid_test = as.numeric(!is.na(proficiency_level)))

output <- dedup %>%
    filter(!is.na(state_student_id)) %>%
    select(system, system_name, school, school_name, test, original_subject, subject, original_proficiency_level, proficiency_level,
        scale_score, enrolled, tested, valid_test, state_student_id, last_name, first_name, grade, race,
        bhn_group, special_ed, economically_disadvantaged, el, el_t1_t2, el_excluded, greater_than_60_pct,
        homebound, absent, did_not_attempt, nullify_flag, residential_facility) %>%
    arrange(system, school, state_student_id)

# Output file
write_csv(output, "N:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_oct19.csv", na = "")
