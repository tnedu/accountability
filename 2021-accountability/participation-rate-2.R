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

connection_eis <- dbConnect(
  RJDBC::JDBC(
    "oracle.jdbc.OracleDriver",
    classPath = Sys.getenv("jar_path")
  ),
  Sys.getenv("eis_connection_string"),
  "EIS_MGR", Sys.getenv("eis_password")
)

# Start with small districts: 542, 171, 274, 690, 92, 151, 880, 371, 11, 480.
# Then move on to larger districts: 10, 190.
test_district <- 10 # c(542, 171)

# Functions ----

convert_date <- function(v) {
  as_date(
    str_c(str_split(v, '/', simplify = T)[, 3],
          str_split(v, '/', simplify = T)[, 1],
          str_split(v, '/', simplify = T)[, 2],
          sep = '-')
  )
}

count_categories <- function(df, ...) {
  q <- enquos(...)
  map(q, ~ count(df, !!.x, sort = T))
}

summarize_missingness <- function(df) {
  map(as.list(df), ~ mean(is.na(.x)))
}

summarize_numeric_vars <- function(df) {
  summary(select(df, where(~ !is.character(.x))))
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

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2021_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

demographics <- read_csv("N:/TNReady/2020-21/spring/demographics/student_demographics_07202021.csv")

# enr_el_raw <- dbGetQuery(
#   connection_eis,
#   "
#   SELECT isp.school_year,
#   isp.student_key as student_id,
#   s.district_no as system,
#   s.school_no as school,
#   isp.first_name,
#   isp.last_name,
#   --sn.gender,
#   isp.begin_date,
#   isp.end_date,
#   isp.withdrawal_reason,
#   isp.english_language_background,
#   EIS_MGR.FN_GET_IG(isp.ISP_ID) as grade
#   FROM isp
#   LEFT JOIN school s on isp.school_bu_id = s.school_bu_id
#   LEFT JOIN (
#     SELECT DISTINCT student_key,
#             gender
#     FROM  student_new
#   ) sn ON sn.student_key = isp.student_key
#   WHERE school_year = 2020
#     AND english_language_background IN ('L', 'W')
#     AND begin_date <= DATE '2021-02-01'
#     AND (end_date IS NULL OR end_date > begin_date)
#     AND (end_date IS NULL OR end_date >= DATE '2021-04-15')
#     AND type_of_service = 'P'
#     AND EIS_MGR.FN_GET_IG(isp.ISP_ID) NOT IN ('P3', 'P4')
#   "
# ) %>% 
#   as_tibble() %>% 
#   janitor::clean_names() %>% 
#   mutate(
#     last_name = str_to_upper(last_name),
#     first_name = str_to_upper(first_name)
#   ) %>% 
#   arrange(system, school) %>%
#   write_csv(str_c("enrollment-el-", today(), ".csv"))

enr_el_raw <- read_csv(last(list.files(pattern = "enrollment-el")))

msaa <- read_csv("N:/ORP_accountability/data/2021_cdf/2021_msaa_cdf.csv") %>%
  filter(!(reporting_status %in% c("WDR", "NLE"))) %>%
  mutate(
    test = "MSAA",
    semester = "Spring",
    # All students who take the TCAP-Alternative Assessment are considered students with disabilities (SWD)
    special_ed = 1L,
    performance_level = if_else(reporting_status != "TES", NA_character_, performance_level),
    grade = as.numeric(grade),
    tested = if_else(reporting_status == "DNT", 0, 1)
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
    "N:/Assessment_Data Returns/Student Registration file/SY2020-21/ACH Student Registration Export 2021-07-20.csv",
    col_types = 'nnncccccncccncnnccccccccccccccnnnnnnnn'
  )
)

regis_spring_alt_raw <- clean_names(
  read_csv(
    "N:/Assessment_Data Returns/Student Registration file/SY2020-21/Alt Student Registration Export 2021-06-15.csv",
    col_types = 'nnncccccncccncnnccccccccccccccnnnnnnnn'
  )
)

cdf_raw <- bind_rows(
  pmap(
    .l = list(
      ..1 = c(
        "N:/ORP_accountability/data/2021_cdf/2021_fall_eoc_cdf.csv",
        "N:/ORP_accountability/data/2021_cdf/2021_spring_eoc_cdf.csv",
        "N:/ORP_accountability/data/2021_cdf/2021_3_8_cdf.csv"
      ),
      ..2 = c("EOC", "EOC", "TNReady"),
      ..3 = c("Fall", "Spring", "Spring")
    ),
    .f = ~ read_csv(..1, col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc") %>%
      mutate(test = ..2, semester = ..3)
  )
)

# Clean demographic data ----

demos_filtered <- demographics %>% 
  filter(str_length(student_key) == 7) %>% 
  transmute(
    unique_student_id = student_key,
    system = district_no,
    school = school_no,
    gender, 
    hispanic = if_else(ethnicity == 'H', 'Y', 'N'),
    economically_disadvantaged = case_when(
      codeab == 1 ~ 'Y',
      codeab == 2 ~ 'N',
      TRUE ~ NA_character_
    ),
    reported_race = reportedrace,
    title_1 = title1,
    gifted = isgifted,
    functionally_delayed = isfunctionallydelayed,
    # foster = isfostercare,
    migrant = ismigrant,
    # foster = isfostercare,
    el_arrived_year_1 = elrecentlyarrivedyearone,
    el_arrived_year_2 = elrecentlyarrivedyeartwo,
    el = isel,
    t1234 = t1t2,
    special_ed = specialeducation,
    enrolled_50_pct_district = district50percent,
    enrolled_50_pct_school = school50percent
  ) %>% 
  mutate(
    native_american = if_else(reported_race == 1, 'Y', 'N'),
    asian = if_else(reported_race == 2, 'Y', 'N'),
    black = if_else(reported_race == 3, 'Y', 'N'),
    hawaiian_pi = if_else(reported_race == 5, 'Y', 'N'),
    white = if_else(reported_race == 6, 'Y', 'N'),
    reported_race = case_when(
      reported_race == 1 ~ 'American Indian/Alaska Native',
      reported_race == 2 ~ 'Asian',
      reported_race == 3 ~ 'Black or African American',
      reported_race == 4 ~ 'Hispanic/Latino',
      reported_race == 5 ~ 'Native Hawaiian/Pac. Islander',
      reported_race == 6 ~ 'White',
      TRUE ~ 'Unknown'
    ),
    bhn_group = if_else(!reported_race %in% c('American Indian/Alaska Native','Black or African American','Hispanic/Latino') | is.na(reported_race), 0, 1)
  )

# Clean and explore registration data ----

regis_raw <- regis_fall_eoc_raw %>%
  mutate(test = 'EOC', semester = 'Fall') %>%
  bind_rows(regis_spring_eoc_raw %>% mutate(test = 'EOC', semester = 'Spring')) %>%
  bind_rows(regis_spring_3_8_raw %>% mutate(test = 'TNReady', semester = 'Spring')) %>%
  bind_rows(
    regis_spring_alt_raw %>%
      mutate(
        test = case_when(
          str_detect(test_name, 'Biology') ~ 'Alt-Science',
          str_detect(test_name, 'English') ~ 'Alt-ELA',
          str_detect(test_name, 'Math') ~ 'Alt-Math',
          str_detect(test_name, 'Science') ~ 'Alt-Science',
          str_detect(test_name, 'Social Studies') ~ 'Alt-Social Studies'
        ),
        semester = 'Spring'
      ) %>%
      rename(
        snt_subpart4 = filler,
        ri_subpart4 = filler_1
      )
  )

summarize(
  regis_raw,
  n0 = n(),
  n1 = nrow(distinct(regis_raw)),
  n2 = n_distinct(district_id),
  n3 = n_distinct(district_id, school_id),
  n4 = n_distinct(usid),
  n5 = n_distinct(district_id, school_id, usid),
  n6 = n_distinct(usid, enrolled_grade),
  n7 = n_distinct(usid, local_class_number),
  # Almost distinct by student and test
  n8 = n_distinct(usid, test_code),
  # Distinct by student-test-semester: A few variables are embedded within test
  # code - subject, grade, sub-part, and format (i.e., Braille).
  n9 = n_distinct(usid, test_code, semester)
)

# Includes private districts and schools
# Includes grades 0-12
summarize_numeric_vars(regis_raw)

# No demographic data except gender
# SNT and RI fields mostly NA (no zeroes)
summarize_missingness(regis_raw)

# Test format = P
count_categories(regis_raw, gender, enrolled_grade, test_format, test_name)
count(regis_raw, test, test_name) %>% View()

# SNT and RI fields for sub-parts 2+ are empty for sub-part 1 (and vice versa).
regis_raw %>%
  filter(str_detect(test_name, "Subpart 1")) %>%
  count(
    test_code, test_name, snt_subpart2, snt_subpart3, snt_subpart4,
    ri_subpart2, ri_subpart3, ri_subpart4,
    sort = T
  ) %>%
  View()

regis <- regis_raw %>%
  filter(
    district_id < 990,
    school_id < 9000,
    !school_id %in% c(981, 982, 999)
    # Exclude all Grade 2 assessments.
    # !str_detect(test_name, "Gr 2")
    # , district_id %in% test_district
  ) %>%
  # Drop records from CTE, Alternative, or Adult HS.
  anti_join(
    cte_alt_adult,
    by = c('district_id' = 'system', 'school_id' = 'school')
  ) %>%
  # ELA and HS English tests have sub-parts. The lowest SNT among the sub-parts
  # of the test in the registration file is used as the overall SNT for the
  # registration. The two parts of the English tests are combined and the
  # lowest SNT is kept between the two parts as the overall SNT for English I
  # and English II.
  mutate(
    grade = if_else(
      str_detect(test_name, 'Gr \\d'),
      as.numeric(str_extract(test_name, '\\d')),
      enrolled_grade
    ),
    test_name = case_when(
      str_detect(test_name, 'Social Studies') ~ 'Social Studies',
      str_detect(test_name, 'Science') ~ 'Science',
      str_detect(test_name, 'Gr \\d Math') ~ 'Math',
      str_detect(test_name, 'English Lang Arts') ~ 'ELA',
      str_detect(test_name, 'English I ') ~ 'English I',
      str_detect(test_name, 'English II') ~ 'English II',
      str_detect(test_name, 'U.S. History') ~ 'US History',
      str_detect(test_name, 'Braille Algebra I') ~ 'Algebra I',
      str_detect(test_name, 'Braille Algebra II') ~ 'Algebra II',
      str_detect(test_name, 'Braille Biology') ~ 'Biology',
      str_detect(test_name, 'Braille Geometry') ~ 'Geometry',
      str_detect(test_name, 'Braille Integrated Math II') ~ 'Integrated Math II',
      TRUE ~ test_name
    ),
    content_area_code = case_when(
      test_name == "ELA" ~ "EN",
      test_name == "Math" ~ "MA",
      test_name == "Science" ~ "SCI",
      test_name == "Social Studies" ~ "SS",
      test_name == "Algebra I" ~ "A1",
      test_name == "Algebra II" ~ "A2",
      test_name == "Biology" ~ "B1",
      test_name == "Chemistry" ~ "C1", # Not used
      test_name == "English I" ~ "E1",
      test_name == "English II" ~ "E2",
      test_name == "English III" ~ "E3", # Not used
      test_name == "Geometry" ~ "G1",
      test_name == "Integrated Math I" ~ "M1",
      test_name == "Integrated Math II" ~ "M2",
      test_name == "Integrated Math III" ~ "M3",
      test_name == "US History" ~ "U1",
      TRUE ~ NA_character_
    ),
    overall_snt_regis = pmin(
      snt_subpart1, snt_subpart2, snt_subpart3, snt_subpart4,
      na.rm = T
    ),
    overall_ri_regis = pmin(
      ri_subpart1, ri_subpart2, ri_subpart3, ri_subpart4,
      na.rm = T
    )
  ) %>%
  arrange(
    usid, semester, test, test_name,
    overall_snt_regis, overall_ri_regis,
    -grade
  ) %>%
  distinct(usid, semester, test, test_name, .keep_all = T) %>%
  # group_by(usid, semester, test, test_name) %>%
  # filter(
  #   overall_snt_regis == first(overall_snt_regis)
  #   | mean(is.na(overall_snt_regis)) == 1
  # ) %>%
  # ungroup() %>%
  select(
    district_id, school_id, usid,
    semester, test, test_name, grade, content_area_code,
    overall_snt_regis, overall_ri_regis
  ) %>%
  distinct() %>%
  arrange_all()

summarize(
  regis,
  n0 = n(),
  n1 = nrow(distinct(regis)),
  n2 = n_distinct(district_id),
  n3 = n_distinct(district_id, school_id),
  n4 = n_distinct(usid),
  n5 = n_distinct(district_id, school_id, usid),
  n6 = n_distinct(usid, grade),
  # Almost distinct by student and content area code (or test name)
  n7 = n_distinct(usid, content_area_code),
  # Distinct by student, semester, test, and content area code (or test name)
  n8 = n_distinct(usid, semester, test, content_area_code)
)

summarize_missingness(regis)

count_categories(regis, overall_snt_regis, overall_ri_regis)
count(regis, test, grade) %>% View()

rm(regis_fall_eoc_raw, regis_spring_3_8_raw, regis_spring_alt_raw, regis_spring_eoc_raw)

# Clean and explore CDF data ----

summarize(
  cdf_raw,
  n0 = n(),
  n1 = nrow(distinct(cdf_raw)),
  n2 = n_distinct(system),
  n3 = n_distinct(system, school),
  n4 = n_distinct(unique_student_id),
  n5 = n_distinct(system, school, unique_student_id),
  n6 = n_distinct(unique_student_id, content_area_code),
  # Distinct by student, content area, test, semester, and raw score
  n7 = n_distinct(unique_student_id, content_area_code, test, semester, raw_score)
)

summarize_numeric_vars(cdf_raw)

# 1% of records missing raw score
# 1% of records missing scale score and performance level
summarize_missingness(cdf_raw)

# In most instances where the raw score is missing, reason not tested and/or RI
# status are non-zero. Where the raw score is missing, reason not tested is 0,
# and RI status is 0, attempted equals "N".
cdf_raw %>%
  filter(is.na(raw_score), reason_not_tested == 0, ri_status == 0) %>%
  count(attempted, modified_format, sort = T)

# Scale scores are missing where raw scores are missing.
cdf_raw %>%
  mutate(
    missing_raw_score = is.na(raw_score),
    missing_scale_score = is.na(scale_score)
  ) %>%
  count(missing_raw_score, missing_scale_score)

# Grades 2-12
# Test mode = "P"
# Reason not tested and RI status include zeroes, not NAs.
count_categories(
  cdf_raw,
  grade, content_area_code, test_mode, attempted, modified_format,
  reason_not_tested, ri_status,
  enrolled_50_pct_district, enrolled_50_pct_school
)

count(cdf_raw %>% filter(test == "EOC"), grade)

cdf <- cdf_raw %>%
  # filter(system %in% test_district) %>%
  filter(
    system < 990,
    school < 9000,
    !school %in% c(981, 982, 999)
    # grade %in% 3:12
  ) %>%
  # Drop records from CTE, Alternative, or Adult HS.
  anti_join(cte_alt_adult, by = c('system', 'school')) %>%
  mutate(
    content_area_code = case_when(
      content_area_code == "ENG" ~ "EN",
      content_area_code == "MAT" ~ "MA",
      content_area_code == "SOC" ~ "SS",
      T ~ content_area_code
    ),
    performance_level = if_else(
      performance_level == "On-Track",
      "On Track",
      performance_level
    )
  )

summarize(
  cdf,
  n0 = n(),
  n1 = nrow(distinct(cdf)),
  n2 = n_distinct(system),
  n3 = n_distinct(system, school),
  n4 = n_distinct(unique_student_id),
  n5 = n_distinct(system, school, unique_student_id),
  # Previously distinct by student and subject
  n6 = n_distinct(unique_student_id, content_area_code),
  # Now distinct by student, subject, semester, and raw score
  n7 = n_distinct(unique_student_id, content_area_code, semester, raw_score)
)

summarize_missingness(cdf)

count(cdf, test, content_area_code)
count_categories(cdf, reason_not_tested, ri_status)

# About 14,000 records (across all tests/semesters) lack raw scores but have
# reason not tested equal to 0.
cdf %>%
  mutate(missing_raw_score = is.na(raw_score)) %>%
  count(reason_not_tested, missing_raw_score)

# About 21,000 records lack raw scores but have RI status equal to 0.
cdf %>%
  mutate(missing_raw_score = is.na(raw_score)) %>%
  count(ri_status, missing_raw_score)

# Join CDF and registration data ----

count_categories(cdf, reason_not_tested, ri_status)
count(regis, overall_snt_regis)
# count_categories(regis_raw, ri_subpart1, ri_subpart2, ri_subpart3, ri_subpart4)

cdf_2 <- cdf %>%
  mutate(in_cdf = T) %>%
  full_join(
    regis %>%
      mutate(in_regis = T) %>%
      # rename(grade = test_grade) %>%
      mutate(across(grade, as.integer)),
    by = c(
      'system' = 'district_id',
      'school' = 'school_id',
      'unique_student_id' = 'usid',
      'grade',
      'semester',
      'test',
      'content_area_code'
    ),
    suffix = c("_cdf", "_regis")
  ) %>%
  mutate(
    original_subject = case_when(
      content_area_code == "EN" ~ "ELA",
      content_area_code == "MA" ~ "Math",
      content_area_code == "SCI" ~ "Science",
      content_area_code == "SS" ~ "Social Studies",
      content_area_code == "A1" ~ "Algebra I",
      content_area_code == "A2" ~ "Algebra II",
      content_area_code == "B1" ~ "Biology",
      content_area_code == "E1" ~ "English I",
      content_area_code == "E2" ~ "English II",
      content_area_code == "G1" ~ "Geometry",
      content_area_code == "M1" ~ "Integrated Math I",
      content_area_code == "M2" ~ "Integrated Math II",
      content_area_code == "M3" ~ "Integrated Math III",
      content_area_code == "U1" ~ "US History"
    )
  ) %>%
  # If the CDF indicates a reason not tested, use that. If not, but the
  # registration file does, use the reason from the registration file. Assign
  # an SNT code of 1 if there is no record in the CDF and no SNT in the
  # registration file.
  mutate(
    # reason_not_tested = case_when(
    #   !is.na(reason_not_tested) ~ as.numeric(reason_not_tested),
    #   !is.na(overall_snt_regis) ~ as.numeric(overall_snt_regis),
    #   is.na(in_cdf) ~ 1,
    #   # If the raw score is missing in the raw score file and there is no SNT
    #   # or RI in either the raw score file or the registration file, then
    #   # assign an SNT of 1 (absent) to that record. # NOTE: This code does not
    #   # check if overall RI status is missing in the registration data because
    #   # it's unclear how to calculate an overall RI status in the registration
    #   # files.
    #   !is.na(in_cdf) & in_cdf &
    #     semester == "Spring" &
    #     is.na(reason_not_tested) & is.na(ri_status) &
    #     is.na(overall_snt_regis) &
    #     is.na(raw_score) ~ 1,
    #   T ~ as.numeric(overall_snt_regis)
    # ),
    ri_status = if_else(
      # Use the registration RI if it is non-zero, the CDF RI is missing (or
      # zero), and the raw score is missing. Otherwise, use the CDF RI.
      (ri_status == 0 | is.na(ri_status)) & !is.na(overall_ri_regis) & is.na(raw_score),
      as.integer(overall_ri_regis),
      as.integer(ri_status)
    ),
    reason_not_tested = if_else(
      # Use the registration SNT if it is non-zero, the CDF SNT is missing (or
      # zero), and the raw score is missing. Otherwise, use the CDF SNT.
      (reason_not_tested == 0 | is.na(reason_not_tested)) & !is.na(overall_snt_regis) & is.na(raw_score),
      as.integer(overall_snt_regis),
      as.integer(reason_not_tested)
    ),
    reason_not_tested = if_else(
      # Change the SNT to 1 (i.e., absent) if it is missing (or zero) BUT all
      # three of the following conditions are true:
      # 1) RI status is missing or zero.
      # 2) Raw score is missing.
      # 3) Scale score is missing.
      (is.na(reason_not_tested) | reason_not_tested == 0) &
        (is.na(ri_status) | ri_status == 0) &
        is.na(raw_score) &
        is.na(scale_score),
      1L,
      reason_not_tested
    ),
    ri_status = if_else(reason_not_tested == 1 & ri_status == 6, 0, as.numeric(ri_status)),
    performance_level = if_else(performance_level == "On track", "On Track", performance_level),
    # absent = reason_not_tested == 1,
    # not_enrolled = reason_not_tested == 2,
    # not_scheduled = reason_not_tested == 3,
    # medically_exempt = reason_not_tested == 4,
    # residential_facility = reason_not_tested == 5,
    # tested_alt = reason_not_tested == 6,
    # did_not_submit = reason_not_tested == 7,
    # breach_adult = ri_status == 1,
    # breach_student = ri_status == 2,
    # irregular_admin = ri_status == 3,
    # incorrect_grade_subject = ri_status == 4,
    # refused_to_test = ri_status == 5,
    # failed_attemptedness = ri_status == 6
    absent = if_else(reason_not_tested == 1, 1,0),
    not_enrolled = if_else(reason_not_tested == 2, 1,0),
    not_scheduled = if_else(reason_not_tested == 3, 1 ,0),
    medically_exempt = if_else(reason_not_tested == 4, 1,0),
    residential_facility = if_else(reason_not_tested == 5, 1,0),
    tested_alt = if_else(reason_not_tested == 6, 1,0), # Not in AM's
    did_not_submit = if_else(reason_not_tested == 7, 1,0),
    breach_adult = if_else(ri_status == 1, 1,0),
    breach_student = if_else(ri_status == 2, 1,0),
    irregular_admin = if_else(ri_status == 3, 1,0),
    incorrect_grade_subject = if_else(ri_status == 4, 1,0),
    refused_to_test = if_else(ri_status == 5, 1,0),
    failed_attemptedness = if_else(ri_status == 6, 1,0)
  ) %>%
  mutate(
    across(
      absent:failed_attemptedness,
      ~ if_else(is.na(.x), F, as.logical(.x))
    )
  )

summarize(
  cdf_2,
  n0 = n(),
  n1 = nrow(distinct(cdf_2)),
  n2 = n_distinct(unique_student_id),
  # Distinct by system, school, student, semester, test, and content area
  n3 = n_distinct(
    system, school, unique_student_id, grade,
    semester, test, content_area_code
  )
)

# No missing SNT codes, 7% missing RI codes
summarize_missingness(cdf_2)

count(cdf_2, original_subject)

cdf_2 %>%
  filter(is.na(original_subject)) %>%
  View()

# Create student-level data set ----

# Apply remaining accountability business rules for excluding and counting
# records in participation rates.

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

# Integrated Math districts for reassigning MSAA subjects
int_math_systems <- cdf_2 %>%
  filter(content_area_code %in% c("A1", "M1")) %>%
  count(system, content_area_code) %>%
  group_by(system) %>%
  mutate(temp = max(n)) %>%
  filter(n == temp, content_area_code == "M1") %>%
  magrittr::extract2("system")

count_categories(msaa, grade, reporting_status)

# DNT = no scale score
msaa %>%
  mutate(missing_scale_score = is.na(scale_score)) %>%
  count(missing_scale_score, tested, reporting_status)

student_level <- cdf_2 %>%
  bind_rows(
    msaa %>%
      # filter(system %in% test_district) %>%
      # filter(grade %in% 3:12) %>%
      mutate(across(grade, as.integer)) %>%
      mutate(
        enrolled = 1,
        reason_not_tested = if_else(reporting_status == "DNT", 1, 0),
        ri_status = 0,
        absent = if_else(reason_not_tested == 1, 1,0),
        not_enrolled = if_else(reason_not_tested == 2, 1,0),
        not_scheduled = if_else(reason_not_tested == 3, 1 ,0),
        medically_exempt = if_else(reason_not_tested == 4, 1,0),
        residential_facility = if_else(reason_not_tested == 5, 1,0),
        tested_alt = if_else(reason_not_tested == 6, 1,0), # Not in AM's
        did_not_submit = if_else(reason_not_tested == 7, 1,0),
        breach_adult = if_else(ri_status == 1, 1,0),
        breach_student = if_else(ri_status == 2, 1,0),
        irregular_admin = if_else(ri_status == 3, 1,0),
        incorrect_grade_subject = if_else(ri_status == 4, 1,0),
        refused_to_test = if_else(ri_status == 5, 1,0),
        failed_attemptedness = if_else(ri_status == 6, 1,0),
        in_msaa = T
      )
  ) %>%
  filter(
    !is.na(system),
    system < 990,
    school < 9000,
    !school %in% c(981, 982, 999) # 981 is homeschool  residential_facility != 1 | is.na(residential_facility),
    # Drop CTE/Alt/Adult
    # !(paste0(system, '/', school) %in% paste0(alt_cte_adult$system, '/', alt_cte_adult$school))#, # 981 is homeschool  residential_facility != 1 | is.na(residential_facility),
  ) %>%
  anti_join(cte_alt_adult, by = c("system", "school")) %>%
  # This transmute creates perfect duplicates by removing two fields: content
  # area code 2 (which entails content area and modified format) and test code
  # (which entails content area, grade, and Braille format).
  transmute(
    in_cdf, in_regis, in_msaa,
    # content_area_code, test_code, # content_area_code_2
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
    raw_score, # Add raw scores to eliminate perfect duplicates?
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
    reason_not_tested,
    ri_status,
    breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness,
    absent, not_enrolled, not_scheduled, medically_exempt, residential_facility, tested_alt, did_not_submit,
    enrolled, tested
  ) %>%
  # left_join(demos_filtered, by = c("system", "school", "state_student_id" = "unique_student_id")) %>%
  # mutate(el_recently_arrived = (el_arrived_year_1 == 1 | el_arrived_year_2 == 1)) %>%
  mutate_at(vars(bhn_group, t1234, el_recently_arrived), as.integer) %>%
  # Apply testing flag hierarchy
  mutate(
    enrolled = case_when(
      test == "MSAA" ~ enrolled,
      (is.na(reason_not_tested) | reason_not_tested == 0) &
        (breach_adult | breach_student | irregular_admin | incorrect_grade_subject | refused_to_test | failed_attemptedness) ~ 0,
      not_enrolled | not_scheduled ~ 0,
      TRUE ~ 1
    ),
    tested = case_when(
      # test == "MSAA" & reporting_status == "DNT" ~ 0,
      test == "MSAA" ~ tested,
      (is.na(reason_not_tested) | reason_not_tested == 0) &
        (breach_adult | breach_student | irregular_admin | incorrect_grade_subject | refused_to_test | failed_attemptedness) ~ 0,
      absent | not_enrolled | not_scheduled ~ 0,
      # Recently Arrived English Learners who tested but lack proficiency
      # levels do not count as tested.
      el_recently_arrived == 1L & is.na(original_performance_level) & (is.na(reason_not_tested) | reason_not_tested == 0) ~ 0,
      TRUE ~ 1
    ),
    # EL Recently Arrived students performance level are converted to missing
    performance_level = case_when(
      (is.na(reason_not_tested) | reason_not_tested == 0) &
        (breach_adult | breach_student | irregular_admin | incorrect_grade_subject | refused_to_test | failed_attemptedness) ~ NA_character_,
      absent | not_enrolled | not_scheduled | medically_exempt | residential_facility | did_not_submit ~ NA_character_,
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
      grade %in% 3:8 & original_subject == "Biology" ~ "Science",
      TRUE ~ subject
    )
  )

summarize(
  student_level,
  n0 = n(),
  n1 = nrow(distinct(student_level)),
  n2 = n_distinct(state_student_id),
  n3 = n_distinct(state_student_id, original_subject),
  n4 = n_distinct(
    system, school, state_student_id, grade,
    semester, test, original_subject
  )
)

summarize_missingness(student_level)

count_categories(student_level, test, grade, reported_race)

# De-duplicate student-level data ----

dedup <- student_level %>%
  anti_join(cte_alt_adult, by = c("system", "school")) %>%
  mutate(prof_not_missing = if_else(is.na(performance_level), 1, 2)) %>%
  # group_by(state_student_id, subject) %>%
  # mutate(temp = max(prof_not_missing, na.rm = TRUE)) %>%
  # filter(prof_not_missing == temp | temp == -Inf) %>%
  # select(-prof_not_missing, -temp) %>%
  # ungroup() %>%
  # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
  mutate(
    test_priority = case_when(
      test %in% c("MSAA", "Alt-Science", "Alt-Social Studies") ~ 3,
      test == "EOC" ~ 2,
      test == "TNReady" ~ 1
    )
  ) %>%
  # group_by(state_student_id, subject) %>%
  # mutate(temp = max(test_priority, na.rm = TRUE)) %>%
  # filter(test_priority == temp | temp == -Inf) %>%
  # select(-test_priority, -temp) %>%
  # ungroup() %>%
  # For students with multiple records within the same test, take highest performance level
  mutate(
    prof_priority = case_when(
      performance_level %in% c("Below", "Below Basic") ~ 1,
      performance_level %in% c("Approaching", "Basic") ~ 2,
      performance_level %in% c("On Track", "Proficient") ~ 3,
      performance_level %in% c("Mastered", "Advanced") ~ 4
    )
  ) %>%
  # group_by(state_student_id, original_subject, test) %>%
  # mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
  # filter(prof_priority == temp | temp == -Inf) %>%
  # select(-prof_priority, -temp) %>%
  # ungroup() %>%
  # For students with multiple records within the same performance level, take highest scale score
  # group_by(state_student_id, original_subject, test, performance_level) %>%
  # mutate(temp = max(scale_score, na.rm = TRUE)) %>%
  # filter(scale_score == temp | temp == -Inf) %>%
  # select(-temp) %>%
  # ungroup() %>%
  # For students with multiple test records with the same proficiency across administrations, take the most recent
  mutate(
    semester_priority = case_when(
      test %in% c("MSAA", "Alt-Social Studies", "Achievement") | (test == "EOC" & semester == "Spring") ~ 2,
      test == "EOC" & semester == "Fall" ~ 1
    )
  ) %>%
  # group_by(state_student_id, original_subject, test) %>%
  # mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
  # filter(semester_priority == temp | temp == -Inf) %>%
  # select(-semester_priority, -temp) %>%
  # ungroup() %>%
  # For students with multiple test records with the same original subject, performance level, scale score
  # Deduplicate by missing race/ethnicity
  mutate(
    demo_priority = case_when(
      reported_race %in% c(
        "American Indian/Alaska Native", "Asian", "Black or African American",
        "Native Hawaiian/Pac. Islander", "Hispanic/Latino", "White"
      ) ~ 2,
      reported_race == 'Unknown' | is.na(reported_race) ~ 1
    )
  ) %>%
  # group_by(state_student_id, original_subject, test, performance_level, scale_score, semester) %>%
  # # mutate(
  # #   n = n(),                           # Tag duplicates by id, subject, test, performance level, scale score, semester
  # #   temp = mean(is.na(reported_race))  # Check whether one among duplicates has non-missing race/ethnicity
  # # ) %>%
  # # filter(!(n > 1 & temp != 0 & is.na(reported_race))) %>%
  # mutate(temp = max(demo_priority, na.rm = TRUE)) %>%
  # filter(demo_priority == temp | temp == -Inf) %>%
  # select(-demo_priority, -temp) %>%
  # ungroup() %>%
  # select(-n, -temp) %>%
  # For students multiple test records with the same original subject, performance level, scale score, demographics
  # Deduplicate for non-missing grade
  mutate(grade_priority = if_else(is.na(grade), 1, 2)) %>%
  # group_by(state_student_id, original_subject, test, performance_level, scale_score, semester, reported_race) %>%
  # # mutate(
  # #   n = n(),                   # Tag duplicates by id, subject, test, performance level, scale score, semester
  # #   temp = mean(is.na(grade))  # Check whether one among duplicates has non-missing race/ethnicity
  # # ) %>%
  # # filter(!(n > 1 & temp != 0 & is.na(grade))) %>%
  # mutate(temp = max(grade_priority, na.rm = TRUE)) %>%
  # filter(grade_priority == temp | temp == -Inf) %>%
  # select(-grade_priority, -temp) %>%
  # ungroup() %>%
  # select(-n, -temp) %>%
  arrange(
    state_student_id, subject, -prof_not_missing, -test_priority, -prof_priority, -scale_score,
    -semester_priority, -demo_priority, -grade_priority
  ) %>%
  distinct(state_student_id, subject, .keep_all = T) %>% # original_subject
  # Valid test if there is a proficiency level
  mutate(valid_test = as.integer(not_na(performance_level)))

summarize(
  dedup,
  n0 = n(),
  n1 = nrow(distinct(dedup)),
  n2 = n_distinct(state_student_id),
  n3 = n_distinct(state_student_id, original_subject),
  n4 = n_distinct(
    state_student_id, semester, test, original_subject,
    performance_level, scale_score
  )
)

dedup %>%
  mutate(has_raw_score = !is.na(raw_score)) %>%
  count(
    enrolled, tested,
    has_raw_score, reason_not_tested,
    breach_adult, breach_student, irregular_admin, incorrect_grade_subject,
    refused_to_test, failed_attemptedness,
    sort = T
  ) %>%
  View()

# Each student should have a raw score (scale score for MSAA), an SNT code, or
# an RI code.
dedup %>%
  mutate(
    has_score = !is.na(raw_score) | (test == "MSAA" & !is.na(scale_score)),
    has_snt_code = !is.na(reason_not_tested) & reason_not_tested > 0,
    has_ri_code = breach_adult | breach_student | irregular_admin | incorrect_grade_subject | refused_to_test | failed_attemptedness
  ) %>%
  count(has_score, has_snt_code, has_ri_code, sort = T)
  # count(enrolled, tested, has_score, has_snt_code, has_ri_code, sort = T)
  # filter(has_score, tested == 0) %>%
  # count(test, semester) # Stems from spring TNReady raw scores - wait and see if switch to CDF fixes it
  # count(el_recently_arrived, performance_level) # All RAEL
  # filter(has_snt_code, tested == 1) %>% # 4, 5, 6
  # count(reason_not_tested)
  # View()
  # filter(!has_raw_score, !has_snt_code, !has_ri_code) %>%
  # count(test)

# MSAA records
dedup %>%
  filter(is.na(raw_score), is.na(reason_not_tested), is.na(breach_adult | breach_student | irregular_admin | incorrect_grade_subject | refused_to_test | failed_attemptedness)) %>%
  count(test)

# Compare de-duplicated data with Andrew's.

dedup_comp <- dedup %>%
  filter(system == 531) %>%
  mutate(in_mine = T) %>%
  full_join(
    dedup_am %>% filter(system == 531) %>% mutate(in_am = T),
    by = c("state_student_id", "original_subject"),
    suffix = c("", "_am")
  )

names(dedup_comp)

dedup_comp %>%
  filter(is.na(in_mine) | is.na(in_am)) %>%
  # View()
  # count(original_subject)
  # count(original_subject, in_mine, in_am)
  count(school)

# Reassigned schools for accountability
enrollment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/enrollment.csv")

# ELPA Students should be EL = 1
elpa <- read_csv("N:/ORP_accountability/data/2021_ELPA/wida_growth_standard_student.csv") %>%
  select(student_id)

student_level_2 <- dedup %>%
  select(
    system, system_name, school, school_name, test, original_subject, subject, semester,
    original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
    state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group, teacher_of_record_tln,
    functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
    enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test, residential_facility, reason_not_tested,
    ri_status
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
  ) %>%
  # Assign EL = 1 if student tested ELPA
  mutate(
    el = if_else(state_student_id %in% elpa$student_id, 1, el)
  ) %>%
  filter(grade %in% c(0, 3:12))

count_categories(student_level_2, test, original_subject, semester, enrolled, tested)
count(student_level_2, grade)
count(student_level_2, test)

# write_csv(student_level_2, "student-level-file.csv", na = "")
# write_csv(student_level_2, "N:/ORP_accountability/projects/2021_student_level_file/student-level-file-jc.csv")

# Compare student-level data with Andrew's ----

student_level_am <- read_csv("N:/ORP_accountability/projects/2021_student_level_file/2021_student_level_file.csv")

names(student_level_2)
names(student_level_am)

summarize(
  student_level_2,
  n0 = n(),
  n1 = nrow(distinct(student_level_2)),
  n2 = n_distinct(state_student_id),
  n3 = n_distinct(state_student_id, original_subject),
  n4 = n_distinct(
    state_student_id, semester, test, original_subject,
    performance_level, scale_score, grade
  )
)

summarize(
  student_level_am,
  n0 = n(),
  n1 = nrow(distinct(student_level_am)),
  n2 = n_distinct(state_student_id),
  n3 = n_distinct(state_student_id, original_subject),
  n4 = n_distinct(
    state_student_id, semester, test, original_subject,
    performance_level, scale_score
    # enrolled, tested,
    # teacher_of_record_tln,
    # reason_not_tested
  )
)

student_level_comp <- list(student_level_2, student_level_am) %>%
  map(
    ~ .x %>%
      # filter(semester == "Spring", test %in% c("TNReady", "EOC")) %>%
      transmute(
        present = T,
        system, school,
        test = if_else(test %in% c("Alt-Science", "Alt-Social Studies"), "Alt-Science/Social Studies", test),
        original_subject = if_else(original_subject == "Biology I", "Biology", original_subject),
        subject, semester, scale_score,
        enrolled, tested,
        state_student_id, grade, reason_not_tested, absent,
        ri_status, refused_to_test, residential_facility
      ) %>%
      filter(
        # (is.na(grade) | grade %in% 3:12),
        !str_detect(test, "WIDA")
      )
  ) %>%
  reduce(
    full_join,
    by = c("system", "school", "state_student_id", "semester", "test", "original_subject"),
    suffix = c("", "_am")
  )

count(student_level_comp, present, present_am)

missing_in_am <- student_level_comp %>% filter(is.na(present_am)) %>% arrange(state_student_id)
missing_in_jc <- student_level_comp %>% filter(is.na(present)) %>% arrange(state_student_id)

# write.xlsx(
#   list("missing-in-am" = missing_in_am, "missing-in-jc" = missing_in_jc),
#   str_c("mismatches-", today(), ".xlsx"),
#   colWidths = "auto",
#   overwrite = T
# )

count_categories(
  missing_in_am, 
  system, school, test, grade,
  original_subject,
  semester, enrolled, tested
)

count_categories(
  missing_in_jc,
  system_am, school_am, test, grade_am,
  original_subject,
  semester, enrolled_am, tested_am
)

count_categories(student_level_am, test, original_subject)

# missing_in_am %>%
missing_in_jc %>%
  distinct(state_student_id, original_subject) %>%
  inner_join(
    student_level_2,
    by = c("state_student_id", "original_subject")
  ) %>%
  arrange(state_student_id) %>%
  View()

count(
  student_level_comp %>% filter(!str_detect(test, "WIDA")),
  enrolled, enrolled_am, sort = T
)

count(
  student_level_comp %>% filter(!str_detect(test, "WIDA")),
  tested, tested_am, sort = T
)

student_level_comp %>%
  filter(!str_detect(test, "WIDA")) %>%
  # filter(enrolled == 1, enrolled_am == 0) %>%
  filter(enrolled == 1 & enrolled_am == 0 | enrolled == 0 & enrolled_am == 1) %>%
  # filter(reason_not_tested == 1, reason_not_tested_am == 0) %>%
  # filter(is.na(reason_not_tested), is.na(reason_not_tested_am)) %>%
  # count(reason_not_tested, reason_not_tested_am, ri_status, ri_status_am, sort = T)
  # filter(reason_not_tested == 0, ri_status == 3) %>%
  # count(test)
  # count(ri_status, ri_status_am)
  arrange(state_student_id) %>%
  View()

x <- student_level_comp %>%
  # filter(tested == 0, tested_am == 1) %>%
  filter(tested == 0 & tested_am == 1 | tested == 1 & tested_am == 0) %>%
  # count(reason_not_tested, reason_not_tested_am, ri_status, ri_status_am, sort = T)
  arrange(state_student_id)

View(x)

# x2 <- x$state_student_id
x2 <- missing_in_am$state_student_id

student_level_2 %>% filter(enrolled == 0, reason_not_tested == 0) %>% View()

sum(student_level_2$school == 999)

# # Split student level file
# district_numbers <- sort(unique(student_level$system))
# 
# # Split files should contain either students with assessment or accountability school number 
# split_by_district <- function(s) {
#   filter(student_level, system == s | acct_system == s)
# }
# 
# map(district_numbers, split_by_district) %>%
#   walk2(
#     .x = .,
#     .y = district_numbers,
#     .f = ~ write_csv(.x,
#                      path = paste0("N:/ORP_accountability/data/2019_assessment_files/Split/", .y, "_StudentLevelFiles_30Jul2019.csv"), 
#                      na = ""
#     )
#   )

summarize(
  student_level_2,
  n0 = n(),
  n1 = n_distinct(state_student_id),
  n2 = n_distinct(state_student_id, original_subject),
  n3 = n_distinct(state_student_id, subject, semester)
)

View(student_level_2 %>% group_by(state_student_id, original_subject) %>% filter(n() > 1))

count(student_level_2, test, original_subject)

partic_dist <- student_level_2 %>%
  filter(semester == "Spring", test %in% c("TNReady", "EOC")) %>% # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # arrange(state_student_id, subject, semester, desc(enrolled), desc(tested)) %>%
  # distinct(state_student_id, subject, semester, .keep_all = T) %>%
  group_by(system) %>%
  summarize(across(c(enrolled, tested), sum)) %>%
  ungroup() %>%
  mutate(participation_rate = round(100 * tested / enrolled, 1))

partic_dist

summary(partic_dist)

partic_dist %>% arrange(participation_rate) %>% View()

# write_csv(partic_dist, str_c("participation-rate-district-", today(), ".csv"))
# write_csv(partic_dist, str_c("participation-rate-district-spring-tnready-eoc-", today(), ".csv"))

# Combine and explore WIDA ACCESS summative files ----

summarize(
  access_alt_raw,
  n0 = n(),
  n1 = nrow(distinct(access_alt_raw)),
  n2 = n_distinct(district_number),
  n3 = n_distinct(district_number, school_number),
  # Almost distinct by student
  n4 = n_distinct(state_student_id),
  # Distinct by DRC student ID
  n5 = n_distinct(unique_drc_student_id),
  # Distinct by district, school, and student
  n6 = n_distinct(district_number, school_number, state_student_id)
)

summarize(
  access_summative_raw,
  n0 = n(),
  n1 = nrow(distinct(access_summative_raw)),
  n2 = n_distinct(district_number),
  n3 = n_distinct(district_number, school_number),
  # Almost distinct by student
  n4 = n_distinct(state_student_id),
  # Distinct by DRC student ID
  n5 = n_distinct(unique_drc_student_id),
  n6 = n_distinct(district_number, school_number, state_student_id),
  # Almost distinct by district, school, student, and grade: Some students have
  # NA for state student ID, and some students have results for multiple
  # domains split across multiple observations.
  n7 = n_distinct(district_number, school_number, state_student_id, grade)
)

# 67 district-school-student combinations appear multiple times. Most of them
# have test results split across multiple rows: Usually the writing cluster is
# separate from the other clusters. Some of the student information (e.g.,
# middle initial, spelling of name, date of birth) also differs across
# duplicates.

access_summative_raw %>%
  group_by(district_number, school_number, state_student_id) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(district_number, school_number, state_student_id) %>%
  View()

map(list(access_alt_raw, access_summative_raw), ~ count(.x, listening_status))
map(list(access_alt_raw, access_summative_raw), ~ count(.x, reading_status))
map(list(access_alt_raw, access_summative_raw), ~ count(.x, speaking_status))
map(list(access_alt_raw, access_summative_raw), ~ count(.x, writing_status))

map(list(access_alt_raw, access_summative_raw), ~ count(.x, cluster_listening))
map(list(access_alt_raw, access_summative_raw), ~ count(.x, cluster_reading))
map(list(access_alt_raw, access_summative_raw), ~ count(.x, cluster_speaking))
map(list(access_alt_raw, access_summative_raw), ~ count(.x, cluster_writing))

map(
  list(access_alt_raw, access_summative_raw),
  ~ .x %>%
    select(
      composite_overall_scale_score,
      literacy_scale_score,
      reading_scale_score
    ) %>%
    mutate(across(everything(), as.numeric)) %>%
    summary()
)

access <- list(access_alt_raw, access_summative_raw) %>%
  map2(
    .y = c("ACCESS Alt", "ACCESS"),
    ~ .x %>%
      mutate(test = .y) %>%
      select(
        test,
        unique_drc_student_id, reported_record,
        district_number, school_number,
        state_student_id, grade,
        starts_with("cluster"),
        ends_with("tier"),
        listening_status:writing_status,
        composite_overall_scale_score,
        literacy_scale_score,
        reading_scale_score
      )
  ) %>%
  bind_rows() %>%
  # Eliminate leading zeroes in student ID strings.
  mutate(across(state_student_id, as.numeric)) %>%
  filter(!is.na(state_student_id))

summarize(
  access,
  n0 = n(),
  n1 = nrow(distinct(access)),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  # Almost distinct by state student ID
  n3 = n_distinct(state_student_id),
  # Distinct by DRC student ID and test (no longer distinct by DRC student ID
  # alone)
  n4 = n_distinct(unique_drc_student_id, test),
  n5 = n_distinct(district_number, school_number, state_student_id),
  # Almost distinct by district, school, student, and grade: Some students had
  # NA for state student ID, and some students have results for multiple
  # domains split across multiple observations.
  n6 = n_distinct(district_number, school_number, state_student_id, grade)
)

summarize_missingness(access)

# Reported record equals 1 for all rows
# Grades 0-12
count_categories(access, reported_record, grade)
count(access, test, tier) # Equals "T" for all Alt records
count(access, test, reported_tier)

# Explore EL enrollment data ----

summarize(
  enr_el_raw,
  n0 = n(),
  n1 = nrow(distinct(enr_el_raw)),
  n2 = n_distinct(system),
  n3 = n_distinct(system, school),
  # Distinct by state student ID
  n4 = n_distinct(student_id)
)

summarize_numeric_vars(enr_el_raw)
summarize_missingness(enr_el_raw)
count_categories(enr_el_raw, english_language_background, grade) # Grades K-12

# Join WIDA ACCESS data and EL enrollment ----

partic_access <- access %>%
  mutate(in_file = T) %>%
  full_join(
    enr_el_raw %>%
      # mutate(across(student_id, as.character)) %>%
      mutate(in_eis = T),
    by = c("state_student_id" = "student_id"),
    suffix = c("_file", "_eis")
  ) %>%
  # Only grades 3 and up are being used for 80% participation rate check
  filter(as.numeric(grade_file) %in% 3:12 | as.numeric(grade_eis) %in% 3:12) %>%
  mutate(
    district_number = if_else(
      is.na(in_file),
      system,
      as.numeric(str_remove(district_number, "TN"))
    )
  )

summarize(
  partic_access,
  n0 = n(),
  n1 = nrow(distinct(partic_access)),
  n2 = n_distinct(district_number),
  n3 = n_distinct(district_number, school_number),
  # Almost distinct by state student ID
  n4 = n_distinct(state_student_id),
  # Distinct by DRC student ID, test, state student ID
  n5 = n_distinct(unique_drc_student_id, test, state_student_id),
  # Fewer than 0.1% of students appear in multiple schools or districts.
  n6 = n_distinct(district_number, school_number, state_student_id),
  # Almost distinct by district, school, student, and grade: Some students have
  # NA for state student ID, and some students have results for multiple
  # domains split across multiple observations.
  n7 = n_distinct(district_number, school_number, state_student_id, grade_file)
)

summarize_missingness(partic_access)

partic_access %>%
  count(in_file, in_eis, sort = T) %>%
  mutate(pct = n / nrow(partic_access))

partic_access %>%
  mutate(
    same_district_no = as.numeric(str_remove(district_number, "TN")) == system,
    same_school_no = as.numeric(school_number) == school
  ) %>%
  count(same_district_no, same_school_no, sort = T) %>%
  mutate(pct = n / nrow(partic_access))

count(partic_access, grade_file, grade_eis) %>% View()

partic_access %>%
  mutate(same_grade = grade_file == grade_eis) %>%
  count(same_grade, sort = T) %>%
  mutate(pct = n / nrow(partic_access))

students_who_took_alt <- partic_access %>%
  filter(test == "ACCESS Alt") %>%
  extract2("state_student_id")

partic_access_2 <- partic_access %>%
  select(test:in_file, english_language_background, grade_eis, in_eis) %>%
  # Alt test is kept if there are both Alt and regular records
  filter(
    is.na(test) # Observations from EIS only
    | test == "ACCESS Alt"
    | !state_student_id %in% students_who_took_alt
  ) %>%
  # Within each district-student combination, de-duplicate with the following
  # hierarchy:
  # 1) Keep the record with maximum composite score.
  # 2) Keep the record with maximum literacy score.
  # 3) Keep the record with maximum reading score.
  arrange(
    district_number, state_student_id,
    desc(composite_overall_scale_score),
    desc(literacy_scale_score),
    desc(reading_scale_score)
  ) %>%
  distinct(district_number, state_student_id, .keep_all = T)

# All students in this data set should count in the denominator. Every student
# either is in the ACCESS file or has English language background L or W.
count(partic_access_2, in_file, english_language_background)

partic_access_3 <- partic_access_2 %>%
  mutate(
    tested = listening_status %in% c("C", "P") |
      reading_status %in% c("C", "P") |
      speaking_status %in% c("C", "P") |
      writing_status %in% c("C", "P")
  ) %>%
  group_by(district_number) %>%
  summarize(
    enrolled = n(),
    tested = sum(tested),
    participation_rate = round(100 * tested / enrolled, 1)
  ) %>%
  ungroup() %>%
  rename(system = district_number)

partic_dist_w_wida <- partic_dist %>%
  bind_rows(partic_access_3) %>%
  group_by(system) %>%
  summarize(across(c(enrolled, tested), sum)) %>%
  ungroup() %>%
  mutate(participation_rate = round(100 * tested / enrolled, 1))

partic_dist_w_wida

partic_dist_w_wida %>% arrange(participation_rate) %>% View()

write_csv(partic_dist_w_wida, str_c("participation-rate-district-with-wida-", today(), ".csv"))

# Compare output with Andrew's ----

partic_dist <- read_csv(last(list.files(pattern = "participation-rate-district")))

partic_dist_am <- read_csv("N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/Participation Rate/district_participation_rate_MSAA_TNReady_EOC_06182021.csv")

partic_dist_w_wida_am <- read_csv("N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/Participation Rate/district_participation_rate_MSAA_TNReady_EOC_WIDA_07212021.csv")

summary(partic_dist)
summary(partic_dist_am)

comp <- partic_dist %>%
  full_join(partic_dist_am, by = "system", suffix = c("", "_am")) %>%
  mutate(
    ratio_enrolled = enrolled / n_enrolled,
    ratio_tested = tested / n_tested,
    ratio_partic = participation_rate / participation_rate_am
  )

comp %>% filter(!complete.cases(.))
comp %>% arrange(ratio_partic) %>% View()
comp %>% arrange(participation_rate) %>% View()
comp %>% arrange(enrolled) %>% View()

write_csv(comp, str_c("participation-rate-comparison-", today(), ".csv"))

comp_w_wida <- partic_dist_w_wida %>%
  full_join(partic_dist_w_wida_am, by = "system", suffix = c("", "_am")) %>%
  mutate(
    ratio_enrolled = enrolled / n_enrolled,
    ratio_tested = tested / n_tested,
    ratio_partic = participation_rate / participation_rate_am
  )

comp_w_wida %>% filter(!complete.cases(.))
comp_w_wida %>% arrange(ratio_partic) %>% View()
comp_w_wida %>% arrange(participation_rate) %>% View()
comp_w_wida %>% arrange(enrolled) %>% View()

write_csv(comp_w_wida, str_c("participation-rate-comparison-with-wida-", today(), ".csv"))
