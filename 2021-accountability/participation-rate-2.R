library(acct)
library(DBI)
library(haven)
library(janitor)
library(lubridate)
library(magrittr)
library(openxlsx)
library(rlang)
library(tidyverse)

setwd(str_c(Sys.getenv('tnshare_data_use'), 'team-members/josh-carson/accountability/2021-accountability'))

# Functions ----

convert_date <- function(v) {
  as_date(
    str_c(str_split(v, '/', simplify = T)[, 3],
          str_split(v, '/', simplify = T)[, 1],
          str_split(v, '/', simplify = T)[, 2],
          sep = '-')
  )
}

# Read input data ----

# Set all column types to character because read_csv() incorrectly identifies
# some column types (e.g., modified format, RI sub-part 1).

# WIDA ACCESS and enrollment

access_alt_raw <- clean_names(
  read_csv(
    'N:/Assessment_Data Returns/ACCESS for ELs and ALT/2020-21/TN_Alternate_StudRR_File_2021-04-29.csv',
    col_types = glue::glue_collapse(rep('c', 179))
  )
)

access_summative_raw <- clean_names(
  read_csv(
    'N:/Assessment_Data Returns/ACCESS for ELs and ALT/2020-21/TN_Summative_StudRR_File_2021-04-29.csv',
    col_types = glue::glue_collapse(rep('c', 164))
  )
)

access_css_raw <- clean_names(
  read_csv(
    'N:/Assessment_Data Returns/WIDA/2021/Cumulative_Status/Cumulative_Student_Status_21.csv',
    col_types = glue::glue_collapse(rep('c', 22)),
    skip = 5
  )
)

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2021_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

enr_raw <- read_csv('N:/Data Mgmt and Reporting/DU_Data/Student_Enrollment_Demographics/Student_Enrollment_Demographics/Cleaned_Data/student_enrollment_Oct1_2021/2021-05-25/school_enrollment_Oct1_2021.csv') %>%
  mutate(across(ends_with('_date'), convert_date))

## TODO: School Numbers for 964/964 and 970/970
msaa <- read.xlsx("2021_TN_StateStudentResults.xlsx") %>%
  # filter(!(reporting_status %in% c("WDR", "NLE"))) %>%
  mutate(
    test = "MSAA",
    semester = "Spring",
    special_ed = 1L
    # performance_level = if_else(reporting_status != "TES", NA_character_, performance_level)
  )

# alt_ss <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_alt_ss_cdf.csv") %>%
#   filter(school != 0) %>%
#   mutate(
#     system_name = if_else(system_name == str_to_upper(system_name), str_to_title(system_name), system_name),
#     test = "Alt-Social Studies",
#     semester = "Spring",
#     special_ed = 1L,
#     performance_level = case_when(
#       performance_level == "Level 3" ~ "Mastered",
#       performance_level == "Level 2" ~ "On Track",
#       performance_level == "Level 1" ~ "Approaching"
#     )
#   )

regis_fall_eoc_raw <- clean_names(
  read_csv(
    "N:/Assessment_Data Returns/Student Registration file/SY2020-21/EOC fall Student Registration Export 2021-06-15.csv",
    col_types = 'nnncccccncccncnnccccccccccccccnnnnnnnn'
  )
)

regis_spring_eoc_raw <- clean_names(
  read_csv(
    "N:/Assessment_Data Returns/Student Registration file/SY2020-21/EOC spring Student Registration Export 2021-06-15.csv",
    col_types = 'nnncccccncccncnnccccccccccccccnnnnnnnn'
  )
)

regis_spring_3_8_raw <- clean_names(
  read_csv(
    "N:/Assessment_Data Returns/Student Registration file/SY2020-21/ACH Student Registration Export 2021-06-15.csv",
    col_types = 'nnncccccncccncnnccccccccccccccnnnnnnnn'
  )
)

regis_spring_alt_raw <- clean_names(
  read_csv(
    "N:/Assessment_Data Returns/Student Registration file/SY2020-21/Alt Student Registration Export 2021-06-15.csv",
    col_types = 'nnncccccncccncnnccccccccccccccnnnnnnnn'
  )
)

fall_eoc <- read_csv(
  "N:/ORP_accountability/data/2021_cdf/2021_fall_eoc_cdf.csv", 
  col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc"
) %>%
  mutate(
    test = "EOC",
    semester = "Fall"
  )

# spring_eoc <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_spring_eoc_cdf.csv",
#                        col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc") %>%
#   mutate(
#     test = "EOC",
#     semester = "Spring"
#   )

# tn_ready <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_3_8_cdf.csv",
#                      col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc") %>%
#   mutate(
#     test = "TNReady",
#     semester = "Spring"
#   )

# Apply participation rate business rules ----

partic <- regis_raw %>%
  filter(
    as.numeric(district_id) <= 986,
    as.numeric(school_id) < 9000
  ) %>%
  # Drop records from CTE, Alternative, or Adult HS.
  anti_join(
    cte_alt_adult,
    by = c('district_id' = 'system', 'school_id' = 'school')
  ) %>%
  # The lowest SNT among the sub-parts of the test in the registration file is
  # used as the overall SNT for the registration. The two parts of the English
  # tests are combined and the lowest SNT is kept between the two parts as the
  # overall SNT for English I and English II.
  mutate(
    test_code_2 = if_else(
      str_detect(test_name, 'Subpart'),
      str_remove(test_code, '[:digit:]U[:digit:]$'),
      test_code
    )
  ) %>%
  mutate(across(starts_with('snt'), as.numeric)) %>%
  group_by(usid, test_code_2) %>%
  mutate(
    overall_snt = min(pmin(snt_subpart1, snt_subpart2, snt_subpart3, snt_subpart4, na.rm = T), na.rm = T),
    overall_snt = if_else(overall_snt == Inf, NA_real_, overall_snt)
  ) %>%
  ungroup() %>%
  # Join registration (denominator?) and CDF (numerator?) data sets.
  full_join(
    cdf_fall_eoc_raw %>%
      mutate(
        across(
          content_area_code,
          ~ case_when(
            .x == 'A1' ~ 'TNMATAL1',
            .x == 'A2' ~ 'TNMATAL2',
            .x == 'B1' ~ 'TNSCIEBI',
            .x == 'E1' & modified_format == 'BR' ~ 'TNBRELAEN1',
            .x == 'E1' ~ 'TNELAEN1',
            .x == 'E2' & modified_format == 'BR' ~ 'TNBRELAEN2',
            .x == 'E2' ~ 'TNELAEN2',
            .x == 'G1' ~ 'TNMATGEO',
            .x == 'M1' ~ 'TNMATIM1',
            .x == 'M2' ~ 'TNMATIM2',
            .x == 'M3' ~ 'TNMATIM3',
            .x == 'U1' & modified_format == 'BR' ~ 'TNBRSOCSUH',
            .x == 'U1' ~ 'TNSOCSUH'
          )
        )
      ) %>%
      mutate(semester = 'Fall', in_cdf = T),
    by = c(
      'usid' = 'unique_student_id',
      'test_code_2' = 'content_area_code',
      'semester'
    )
  ) %>%
  # If the CDF indicates a reason not tested, use that. If not, but the
  # registration file does, use the reason from the registration file. Assign
  # an SNT code of 1 if there is no record in the CDF and no SNT in the
  # registration file.
  mutate(
    reason_not_tested_2 = case_when(
      !is.na(reason_not_tested) ~ reason_not_tested,
      is.na(in_cdf) & is.na(overall_snt) ~ '1',
      T ~ as.character(overall_snt)
    )
  )

count(partic, test_code, test_code_2, test_name) %>% View()

# Confirm that the overall SNT code equals 1 anywhere at least one sub-part has
# an SNT code of 1.

partic %>%
  filter(pmin(snt_subpart1, snt_subpart2, snt_subpart3, snt_subpart4, na.rm = T) == 1) %>%
  summarize(m = mean(overall_snt == 1)) %>%
  pull(m) %>%
  testthat::expect_equal(1)

count(
  partic,
  overall_snt, reason_not_tested, reason_not_tested_2, in_cdf,
  sort = T
)

# Apply remaining accountability business rules for excluding and counting
# records in participation rates.

cdf <- bind_rows(fall_eoc, spring_eoc, tn_ready, alt_ss) %>%
  mutate(
    ri_status = if_else(reason_not_tested == 1 & ri_status == 6, 0, ri_status),
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
  magrittr::extract2("system")

student_level <- bind_rows(cdf, msaa) %>%
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
    teacher_of_record_tln,
    reporting_status,
    breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness,
    absent, not_enrolled, not_scheduled, medically_exempt, residential_facility, tested_alt, did_not_submit
  ) %>%
  mutate_at(vars(bhn_group, t1234, el_recently_arrived), as.integer) %>%
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
      test == "MSAA" & reporting_status == "DNT" ~ 0,
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
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Modify subject for MSAA tests in grades >= 9 (6.8)
    subject = case_when(
      original_subject == "Math" & test == "MSAA" & grade >= 9 & system %in% int_math_systems ~ "Integrated Math I",
      original_subject == "Math" & test == "MSAA" & grade >= 9 & !(system %in% int_math_systems) ~ "Algebra I",
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
  )

# Records from Alternative, CTE, Adult HS are dropped from student level
cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

dedup <- student_level %>%
  anti_join(cte_alt_adult, by = c("system", "school")) %>%
  # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
  mutate(
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
      test %in% c("MSAA", "Alt-Social Studies", "Achievement") | (test == "EOC" & semester == "Spring") ~ 2,
      test == "EOC" & semester == "Fall" ~ 1
    )
  ) %>%
  group_by(state_student_id, original_subject, test) %>%
  mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
  filter(semester_priority == temp | temp == -Inf) %>%
  select(-semester_priority, -temp) %>%
  ungroup() %>%
  # For students with multiple test records with the same original subject, performance level, scale score
  # Deduplicate by missing race/ethnicity
  group_by(state_student_id, original_subject, test, performance_level, scale_score, semester) %>%
  mutate(
    n = n(),                           # Tag duplicates by id, subject, test, performance level, scale score, semester
    temp = mean(is.na(reported_race))  # Check whether one among duplicates has non-missing race/ethnicity
  ) %>%
  filter(!(n > 1 & temp != 0 & is.na(reported_race))) %>%
  ungroup() %>%
  select(-n, -temp) %>%
  # For students multiple test records with the same original subject, performance level, scale score, demographics
  # Deduplicate for non-missing grade
  group_by(state_student_id, original_subject, test, performance_level, scale_score, semester, reported_race) %>%
  mutate(
    n = n(),                   # Tag duplicates by id, subject, test, performance level, scale score, semester
    temp = mean(is.na(grade))  # Check whether one among duplicates has non-missing race/ethnicity
  ) %>%
  filter(!(n > 1 & temp != 0 & is.na(grade))) %>%
  ungroup() %>%
  select(-n, -temp) %>%
  # Valid test if there is a proficiency level
  mutate(valid_test = as.integer(not_na(performance_level)))

# Reassigned schools for accountability
enrollment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/enrollment.csv")

# ELPA Students should be EL = 1
elpa <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv") %>%
  select(student_id)

student_level <- dedup %>%
  select(
    system, system_name, school, school_name, test, original_subject, subject, semester,
    original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
    state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group, teacher_of_record_tln,
    functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
    enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test, residential_facility
  ) %>%
  mutate_at(vars(absent, refused_to_test, residential_facility), as.integer) %>%
  # Percentiles by grade and original subject for 3-8
  group_by(test, original_subject, grade) %>%
  mutate(
    rank = if_else(not_na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(not_na(scale_score)),
    percentile = if_else(test == "TNReady", round5(100 * rank/denom, 1), NA_real_)
  ) %>%
  # Percentiles by original subject for EOCs
  group_by(test, original_subject) %>%
  mutate(
    rank = if_else(not_na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(not_na(scale_score)),
    percentile = if_else(test == "EOC", round5(100 * rank/denom, 1), percentile)
  ) %>%
  ungroup() %>%
  select(-rank, -denom) %>%
  arrange(system, school, state_student_id) %>%
  # Add system and school for accountability purposes
  left_join(enrollment, by = "state_student_id") %>%
  mutate(
    acct_system = if_else(is.na(acct_system), system, acct_system),
    acct_school = if_else(is.na(acct_school), school, acct_school)
    # Assign EL = 1 if student tested ELPA
    mutate(
      el = if_else(state_student_id %in% elpa$student_id, 1, el)
    )
    
    write_csv(student_level, "N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv", na = "")
    
    # Split student level file
    district_numbers <- sort(unique(student_level$system))
    
    # Split files should contain either students with assessment or accountability school number 
    split_by_district <- function(s) {
      filter(student_level, system == s | acct_system == s)
    }
    
    map(district_numbers, split_by_district) %>%
      walk2(
        .x = .,
        .y = district_numbers,
        .f = ~ write_csv(.x,
                         path = paste0("N:/ORP_accountability/data/2019_assessment_files/Split/", .y, "_StudentLevelFiles_30Jul2019.csv"), 
                         na = ""
        )
      )
    