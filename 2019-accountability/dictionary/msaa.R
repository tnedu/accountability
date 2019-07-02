library(tidyverse)

math <- read_csv("N:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2018-19/2019_TN_StateStudentResults.csv") %>%
    transmute(
        system = as.integer(DistrictID),
        school = as.integer(SchoolID),
        unique_student_id = State_Student_ID,
        last_name = LastOrSurname,
        first_name = FirstName,
        grade = as.character(Grade),
        original_subject = "Math",
        scale_score = MatScaledScore,
        performance_level = case_when(
            MatPerfLevel == 1 ~ "Below",
            MatPerfLevel == 2 ~ "Approaching",
            MatPerfLevel == 3 ~ "On Track",
            MatPerfLevel == 4 ~ "Mastered"
        ),
        reporting_status = MatReportingStatus
    )

ela <- read_csv("N:/Assessment_Data Returns/TCAP ALT_ Grades 3-11_MSAA/2018-19/2019_TN_StateStudentResults.csv") %>%
    transmute(
        system = as.integer(DistrictID),
        school = as.integer(SchoolID),
        unique_student_id = State_Student_ID,
        last_name = LastOrSurname,
        first_name = FirstName,
        grade = as.character(Grade),
        original_subject = "ELA",
        scale_score = ELAScaledScore,
        performance_level = case_when(
            ELAPerfLevel == 1 ~ "Below",
            ELAPerfLevel == 2 ~ "Approaching",
            ELAPerfLevel == 3 ~ "On Track",
            ELAPerfLevel == 4 ~ "Mastered"
        ),
        reporting_status = ELAReportingStatus
    )

# Demographic file
demographics <- read_csv("N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv") %>%
    # Student IDs should be 7 digits
    filter(str_length(student_key) == 7) %>%
    transmute(
        unique_student_id = student_key,
        system = district_id,
        school = school_id,
        el = isel,
        t1234 = t1t2,
        reported_race = case_when(
            ethnicity == "H" ~ "Hispanic/Latino",
            isblack == 1 ~ "Black or African American",
            isamericanindian == 1 ~ "American Indian/Alaska Native",
            ispacificislander == 1 ~ "Native Hawaiian/Pac. Islander",
            isasian == 1 ~ "Asian",
            iswhite == 1 ~ "White",
            TRUE ~ "Unknown"
        ),
        el_arrived_year_1 = elrecentlyarrivedyearone,
        el_arrived_year_2 = elrecentlyarrivedyeartwo,
        gender,
        gifted = isgifted,
        migrant = ismigrant,
        title_1 = title1,
        special_ed = specialeducation,
        functionally_delayed = isfunctionallydelayed,
        economically_disadvantaged = case_when(
            codeab == 1 ~ 1,
            codeab == 2 ~ 0
        ),
        enrolled_50_pct_district = district50percent,
        enrolled_50_pct_school = school50percent
    )

names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

msaa <- bind_rows(math, ela) %>%
    mutate(
        school = if_else(system == 961 & school == 961, 5L, school),
        school = if_else(system == 963 & school == 963, 5L, school)
    ) %>%
    left_join(names, by = c("system", "school")) %>%
    left_join(demographics, by = c("system", "school", "unique_student_id")) %>%
    select(system, system_name, school, school_name, everything())

write_csv(msaa, path = "N:/ORP_accountability/data/2019_cdf/2019_msaa_cdf.csv", na = "")
