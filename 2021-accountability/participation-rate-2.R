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
test_district <- 542

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

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2021_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

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

## TODO: School Numbers for 964/964 and 970/970
msaa <- read_csv("N:/ORP_accountability/data/2021_cdf/2021_msaa_cdf.csv") %>% # read.xlsx("2021_TN_StateStudentResults.xlsx") %>%
  filter(!(reporting_status %in% c("WDR", "NLE"))) %>%
  mutate(
    test = "MSAA",
    semester = "Spring",
    special_ed = 1L,
    performance_level = if_else(reporting_status != "TES", NA_character_, performance_level)
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

cdf_fall_eoc_raw <- read_csv(
  "N:/ORP_accountability/data/2021_cdf/2021_fall_eoc_cdf.csv", 
  col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc"
) %>%
  mutate(
    test = "EOC",
    semester = "Fall"
  )

# cdf_spring_eoc_raw <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_spring_eoc_cdf.csv",
#                        col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc") %>%
#   mutate(
#     test = "EOC",
#     semester = "Spring"
#   )

# cdf_tn_ready_raw <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_3_8_cdf.csv",
#                      col_types = "iciccccdiccccdiiiiciiciiciiciiiiiicc") %>%
#   mutate(
#     test = "TNReady",
#     semester = "Spring"
#   )

scores_eoc_raw <- read_csv("N:/Assessment_Data Returns/Student Registration file/SY2020-21/Raw Score/2021_TN_Spring_2021_EOC_RSF_20210607.csv") %>%
  clean_names()

scores_2_8_raw <- read_csv("N:/Assessment_Data Returns/Student Registration file/SY2020-21/Raw Score/2021_TN_Spring_2021_Grades_2_8_RSF_20210607.csv") %>%
  clean_names()

# Combine and explore registration data sets ----

Sys.time()

regis_raw <- regis_fall_eoc_raw %>%
  mutate(test = 'EOC', semester = 'Fall') %>%
  bind_rows(regis_spring_eoc_raw %>% mutate(test = 'EOC', semester = 'Spring')) %>%
  bind_rows(regis_spring_3_8_raw %>% mutate(test = 'TNReady', semester = 'Spring')) %>%
  bind_rows(
    regis_spring_alt_raw %>%
      mutate(test = 'Alt', semester = 'Spring') %>%
      rename(
        snt_subpart2 = filler,
        snt_subpart3 = filler_1,
        snt_subpart4 = filler_2,
        ri_subpart2 = filler_3,
        ri_subpart3 = filler_4,
        ri_subpart4 = filler_5
      )
  )

nrow(distinct(regis_raw))
nrow(regis_raw)

summarize(
  regis_raw,
  n0 = n(),
  n1 = n_distinct(district_id),
  n2 = n_distinct(district_id, school_id),
  n3 = n_distinct(usid),
  n4 = n_distinct(district_id, school_id, usid),
  n5 = n_distinct(usid, enrolled_grade),
  n6 = n_distinct(usid, local_class_number),
  # Almost distinct by student and test
  n7 = n_distinct(usid, test_code),
  # Distinct by student-test-semester: A few variables are embedded within test
  # code - subject, grade, sub-part, and format (i.e., Braille).
  n8 = n_distinct(usid, test_code, semester)
)

# Includes private districts and schools
# Includes grades 0-12
summary(select(regis_raw, where(~ !is.character(.x))))

# No demographic data except gender
# SNT and RI fields mostly NA (no zeroes)
map(as.list(regis_raw), ~mean(is.na(.x)))

# Grades 0-12, test format = P
map(
  quos(gender, enrolled_grade, test_format),
  ~ count(regis_raw, !!.x, sort = T)
)

# ELA and HS English tests have sub-parts.
count(regis_raw, test_name, test_code) %>% View()

count(regis_raw, semester)
count(regis_raw, snt_subpart1)
count(regis_raw, snt_subpart2)
count(regis_raw, snt_subpart3)
count(regis_raw, snt_subpart4)
count(regis_raw, ri_subpart1)
count(regis_raw, ri_subpart2)
count(regis_raw, ri_subpart3)
count(regis_raw, ri_subpart4)

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
    district_id <= 986,
    school_id < 9000,
    enrolled_grade %in% 3:12,
    # Exclude all Grade 2 assessments.
    !str_detect(test_name, "Gr 2")
    # Where the test is TN Ready, keep observations where the test grade and
    # enrolled grade match. Check student counts with and without this filter.
    # NOTE: This filter dropped eight students when it only needs to drop one
    # to de-duplicate. Perhaps instead only use this filter for duplicates at
    # the end. Maybe wait until AFTER joining with the CDF to explore duplicates
    # and remove them as needed.
    # (test == "EOC" | str_detect(test_code, str_c("G", enrolled_grade))),
    # For now, just test code with one small district.
    # , district_id == test_district
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
      # str_remove(test_code, '[:digit:]U[:digit:]$'),
      str_remove(test_code, 'U[:digit:]$'),
      test_code
    )
    # test_code_3 = str_remove(test_code_2, "G[:digit:]$")
  ) %>%
  mutate(across(starts_with('snt'), as.numeric)) %>%
  group_by(usid, test_code_2) %>%
  mutate(
    overall_snt = min(pmin(snt_subpart1, snt_subpart2, snt_subpart3, snt_subpart4, na.rm = T), na.rm = T),
    overall_snt = if_else(overall_snt == Inf, NA_real_, overall_snt)
  ) %>%
  ungroup() %>%
  mutate(
    content_area_code = case_when(
      test_code == 'TNMATAL1' ~ 'A1',
      test_code == 'TNMATAL2' ~ 'A2',
      test_code %in% c('TNSCIEBI', 'TNALTSCBI') ~ 'B1',
      str_detect(test_code, 'ELAEN1') ~ 'E1',
      str_detect(test_code, 'ELAEN2') ~ 'E2',
      test_code == 'TNMATGEO' ~ 'G1',
      test_code == 'TNMATIM1' ~ 'M1',
      test_code == 'TNMATIM2' ~ 'M2',
      test_code == 'TNMATIM3' ~ 'M3',
      str_detect(test_code, 'SOCSUH') ~ 'U1',
      str_detect(test_code, 'ELA') ~ 'ENG',
      str_detect(test_code, 'MAT') ~ 'MAT',
      str_detect(test_code, 'SCIE|ALTSC') ~ 'SCI',
      str_detect(test_code, 'SOCS|ALTSS') ~ 'SOC'
    ),
    modified_format = if_else(
      str_detect(test_code, "BR"),
      "BR",
      NA_character_
    )
  ) %>%
  select(
    district_id, school_id, usid, enrolled_grade,
    semester, test, test_code, test_code_2, content_area_code, modified_format, # test_code_3,
    test_name,
    overall_snt,
    snt_subpart1:ri_subpart4
  ) %>%
  arrange_all()

nrow(regis)
nrow(distinct(regis))

summarize(
  regis,
  n0 = n(),
  n1 = n_distinct(district_id),
  n2 = n_distinct(district_id, school_id),
  n3 = n_distinct(usid),
  n4 = n_distinct(district_id, school_id, usid),
  n5 = n_distinct(usid, enrolled_grade),
  # Almost distinct by student and test
  n7 = n_distinct(usid, test_code),
  # Distinct by student-test-semester: A few variables are embedded within test
  # code - subject, grade, sub-part, and format (i.e., Braille).
  n8 = n_distinct(usid, test_code, semester),
  # Not distinct using test code 2 because the sub-part variable was removed
  # from test code
  n9 = n_distinct(usid, test_code_2, semester),
  # n10 = n_distinct(usid, test_code_3, semester)
  n10 = n_distinct(usid, content_area_code, semester),
  n11 = n_distinct(usid, content_area_code, modified_format, semester)
)

regis %>%
  filter(test_code != test_code_2) %>%
  count(test_code, test_code_2, test_name)

count(regis, enrolled_grade)
count(regis, semester)
count(regis, test)
# count(regis, test_code, test_code_2, test_code_3, test_name) %>% View()
count(regis, test, test_code, content_area_code, modified_format) %>% View()
count(regis, test_code_2, content_area_code) %>% View()
count(regis, overall_snt)

regis %>%
  group_by(usid, test_code_3, semester) %>%
  filter(n_distinct(test_code_2) > 1) %>%
  ungroup() %>%
  View()

regis %>%
  filter(test == "TNReady") %>%
  count(enrolled_grade, test_code_2) %>%
  View()

# Make the data distinct by student, test code 2 (i.e., subject, grade, and
# format - excluding sub-part), and semester.

regis_2 <- regis %>%
  # distinct(usid, test_code_2, semester, .keep_all = T) %>%
  distinct(usid, content_area_code, test_code_2, semester, .keep_all = T) %>%
  select(-test_code, -(snt_subpart1:ri_subpart4)) %>%
  rename(
    test_code = test_code_2
    # content_area_code = test_code_3
  ) %>%
  mutate(across(test_name, ~ str_remove_all(.x, " Subpart 1| Subparts 2-3| Subparts 2-4")))

summarize(
  regis_2,
  n0 = n(),
  n1 = n_distinct(district_id),
  n2 = n_distinct(district_id, school_id),
  n3 = n_distinct(usid),
  n4 = n_distinct(district_id, school_id, usid),
  # Almost distinct by student and test
  n6 = n_distinct(usid, test_code),
  # Distinct by student-test-semester: A few variables are embedded within test
  # code - subject, grade, and format (i.e., Braille).
  n7 = n_distinct(usid, test_code, semester),
  n8 = n_distinct(usid, content_area_code, modified_format, semester)
)

count(regis_2, enrolled_grade)
count(regis_2, semester)
count(regis_2, test)
count(regis_2, test, test_code, content_area_code, modified_format) %>% View()
count(regis_2, overall_snt)

# Explore fall EOC CDF data ----

nrow(cdf_fall_eoc_raw)
nrow(distinct(cdf_fall_eoc_raw))

summarize(
  cdf_fall_eoc_raw,
  n0 = n(),
  n1 = n_distinct(system),
  n2 = n_distinct(system, school),
  n3 = n_distinct(unique_student_id),
  # One school per student
  n4 = n_distinct(system, school, unique_student_id),
  # Distinct by student and content area
  n5 = n_distinct(unique_student_id, content_area_code)
)

summary(select(cdf_fall_eoc_raw, where(~ !is.character(.x))))

# 5% of records missing raw score
# 33% of records missing scale score and performance level
map(as.list(cdf_fall_eoc_raw), ~mean(is.na(.x)))

# In most instances where the raw score is missing, reason not tested and/or RI
# status are non-zero. Where the raw score is missing, reason not tested is 0,
# and RI status is 0, attempted equals "N".
cdf_fall_eoc_raw %>%
  filter(is.na(raw_score), reason_not_tested == 0, ri_status == 0) %>%
  count(attempted, modified_format, sort = T)

# Scale scores are missing where either raw scores are missing or the subject
# is Biology or U.S. History (with a single English II exception). Standard-
# setting is still underway for these subjects.
cdf_fall_eoc_raw %>%
  mutate(missing_raw_score = is.na(raw_score)) %>%
  filter(is.na(scale_score)) %>%
  count(content_area_code, missing_raw_score, sort = T)

count(cdf_fall_eoc_raw, grade) # Grades 7-12 (mostly 9-11)
count(cdf_fall_eoc_raw, content_area_code)
count(cdf_fall_eoc_raw, test_mode) # Test mode = "P"
count(cdf_fall_eoc_raw, attempted)
count(cdf_fall_eoc_raw, modified_format)
count(cdf_fall_eoc_raw, reason_not_tested) # Includes zeroes
count(cdf_fall_eoc_raw, ri_status) # Includes zeroes
count(cdf_fall_eoc_raw, enrolled_50_pct_district, enrolled_50_pct_school)

# Combine and explore raw score data sets ----

count(scores_2_8_raw, administration)
count(scores_eoc_raw, administration)

count(scores_2_8_raw, s1op_max_pts_possible, sort = T)
summary(scores_eoc_raw$s1op_max_pts_possible)

scores_raw <- bind_rows(
  scores_2_8_raw %>%
    mutate(across(s1op_raw_score:total_point_possible, as.numeric)) %>%
    mutate(test = "TNReady", semester = "Spring"),
  scores_eoc_raw %>%
    mutate(across(s1op_raw_score:total_point_possible, as.numeric)) %>%
    mutate(test = "EOC", semester = "Spring")
) %>%
  mutate(across(c(district_number, school_number, usid), as.numeric))

nrow(scores_raw)
nrow(distinct(scores_raw))

summarize(
  scores_raw,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  n3 = n_distinct(usid),
  n4 = n_distinct(district_number, school_number, usid),
  # Almost distinct by student and subject
  n5 = n_distinct(usid, content_area_code),
  # Distinct by student-subject-class (could substitute test grade or lithocode
  # part 1 for local class number)
  n6 = n_distinct(usid, content_area_code, local_class_number)
)

# Explore student-subject duplicates.
scores_raw %>%
  group_by(usid, content_area_code) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(usid) %>%
  View()

# Includes private districts and schools
summary(select(scores_raw, where(~ !is.character(.x))))

# 19% of records missing test grade - unclear why, just use enrolled grade
# 1% missing total raw score and total points possible
map(as.list(scores_raw), ~ mean(is.na(.x)))

# In most instances where the raw score is missing, SNT and/or RI status are
# non-zero. Where the raw score is missing, reason not tested is 0, and RI
# status is 0, attempt equals "N".
scores_raw %>%
  filter(is.na(total_raw_score), overall_snt == 0, overall_ri_status == 0) %>%
  count(attempt, total_point_possible)

count(scores_raw, enrolled_grade) # Grades 0-12
count(scores_raw, content_area_code)
count(scores_raw, attempt)
count(scores_raw, test, semester)

# What does attempt mean? Why do 9,000 records have "N" attempt but zeroes for
# SNT and RI?
count(scores_raw, attempt, overall_snt, overall_ri_status, sort = T) %>% View()

map(as.list(scores_raw), ~ mean(is.na(.x)))

scores <- scores_raw %>%
  filter(
    district_number <= 986,
    school_number < 9000,
    as.numeric(enrolled_grade) %in% 3:12
    # For now, just test code with one small district.
    # , district_number == test_district
  ) %>%
  # Drop records from CTE, Alternative, or Adult HS.
  anti_join(
    cte_alt_adult,
    by = c('district_number' = 'system', 'school_number' = 'school')
  ) %>%
  group_by(usid, content_area_code) %>%
  # Drop records where total raw score is missing (unless total raw score is
  # missing for every record within each student-subject).
  filter(!is.na(total_raw_score) | mean(is.na(total_raw_score)) == 1) %>%
  ungroup() %>%
  select(
    district_number, school_number, usid, enrolled_grade,
    semester, test, content_area_code,
    attempt, overall_snt, overall_ri_status,
    total_raw_score
  ) %>%
  arrange_all()

summarize(
  scores,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  n3 = n_distinct(usid),
  n4 = n_distinct(district_number, school_number, usid),
  # Distinct by student and subject
  n5 = n_distinct(usid, content_area_code)
)

# 0.5% of records missing total raw score (Anderson County)
summary(select(scores, where(~ !is.character(.x))))

count(scores, test, content_area_code)

# Apply new participation rate business rules ----

names(cdf_fall_eoc_raw)
names(scores)

cdf <- cdf_fall_eoc_raw %>% # bind_rows(fall_eoc, spring_eoc, tn_ready, alt_ss) %>%
  # filter(system == test_district) %>%
  # Drop records from CTE, Alternative, or Adult HS.
  anti_join(
    cte_alt_adult,
    by = c('system', 'school')
  ) %>%
  bind_rows(
    scores %>%
      mutate(across(c(district_number, school_number, usid, enrolled_grade), as.integer)) %>%
      rename(
        system = district_number,
        school = school_number,
        unique_student_id = usid,
        grade = enrolled_grade,
        attempted = attempt,
        reason_not_tested = overall_snt,
        ri_status = overall_ri_status,
        raw_score = total_raw_score
      )
  )

nrow(cdf)
nrow(distinct(cdf))

summarize(
  cdf,
  n0 = n(),
  n1 = n_distinct(system),
  n2 = n_distinct(system, school),
  n3 = n_distinct(unique_student_id),
  # One school per student
  n4 = n_distinct(system, school, unique_student_id),
  # Almost distinct by student and subject
  n5 = n_distinct(unique_student_id, content_area_code),
  # Distinct by student-subject-semester
  n6 = n_distinct(unique_student_id, content_area_code, semester)
)

# Explore student-subject duplicates.
cdf %>%
  group_by(unique_student_id, content_area_code) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(unique_student_id) %>%
  View()

count(cdf, test, content_area_code)

# Join CDF and registration.

cdf_2 <- cdf %>%
  mutate(in_cdf = T) %>%
  full_join(
    regis_2 %>% mutate(in_regis = T),
    by = c(
      'system' = 'district_id',
      'school' = 'school_id',
      'unique_student_id' = 'usid',
      # 'content_area_code_2' = 'content_area_code',
      'content_area_code',
      'modified_format',
      'test',
      'semester'
    ),
    suffix = c("_cdf", "_regis")
  ) %>%
  mutate(
    original_subject = case_when(
      content_area_code == "ENG" ~ "ELA",
      content_area_code == "MAT" ~ "Math",
      content_area_code == "SCI" ~ "Science",
      content_area_code == "SOC" ~ "Social Studies",
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
    reason_not_tested_2 = case_when(
      !is.na(reason_not_tested) ~ as.numeric(reason_not_tested),
      is.na(in_cdf) & is.na(overall_snt) ~ 1,
      # If the raw score is missing in the raw score file and there is no SNT
      # or RI in either the raw score file or the registration file, then
      # assign an SNT of 1 (absent) to that record. # NOTE: This code does not
      # check if overall RI status is missing in the registration data because
      # it's unclear how to calculate an overall RI status in the registration
      # files.
      !is.na(in_cdf) & in_cdf &
        semester == "Spring" &
        is.na(reason_not_tested) & is.na(ri_status) &
        is.na(overall_snt) &
        is.na(raw_score) ~ 1,
      T ~ as.numeric(overall_snt)
    ),
    ri_status = if_else(reason_not_tested_2 == 1 & ri_status == 6, 0, as.numeric(ri_status)),
    performance_level = if_else(performance_level == "On track", "On Track", performance_level),
    absent = reason_not_tested_2 == 1,
    not_enrolled = reason_not_tested_2 == 2,
    not_scheduled = reason_not_tested_2 == 3,
    medically_exempt = reason_not_tested_2 == 4,
    residential_facility = reason_not_tested_2 == 5,
    tested_alt = reason_not_tested_2 == 6,
    did_not_submit = reason_not_tested_2 == 7,
    breach_adult = ri_status == 1,
    breach_student = ri_status == 2,
    irregular_admin = ri_status == 3,
    incorrect_grade_subject = ri_status == 4,
    refused_to_test = ri_status == 5,
    failed_attemptedness = ri_status == 6,
  )

nrow(cdf_2)
nrow(distinct(cdf_2))

summarize(
  cdf_2,
  n0 = n(),
  n1 = n_distinct(unique_student_id),
  # Two variables are embedded within content area code 2: content area code
  # and modified format (Braille).
  # n2 = n_distinct(unique_student_id, content_area_code_2, semester),
  # n3 = n_distinct(unique_student_id, content_area_code_2, test_code, semester)
  n2 = n_distinct(unique_student_id, content_area_code, semester),
  # Test code entails two variables: test and grade.
  n3 = n_distinct(unique_student_id, content_area_code, test_code, semester)
)

count(cdf_2, original_subject, test)
count(cdf_2, test_code) %>% View()

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

student_level <- bind_rows(
  cdf_2,
  msaa %>%
    # filter(system == test_district) %>%
    mutate(across(grade, as.integer)) %>%
    mutate(in_msaa = T)
) %>% # bind_rows(cdf, msaa) %>%
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
    # raw_score, # Add raw scores to eliminate perfect duplicates?
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

nrow(student_level)
nrow(distinct(student_level))

dups <- student_level %>%
  group_by(across(system:did_not_submit)) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  arrange(state_student_id)

summarize(
  dups,
  n0 = n(),
  n1 = n_distinct(state_student_id),
  # Two variables are embedded within content area code 2: content area code
  # and modified format (Braille).
  # n2 = n_distinct(unique_student_id, content_area_code_2, semester),
  # n3 = n_distinct(unique_student_id, content_area_code_2, test_code, semester)
  n4 = n_distinct(state_student_id, original_subject)
)

summary(select(dups, where(~ !is.character(.x))))
keep(map(as.list(dups), ~ mean(is.na(.x))), ~ .x != 0)
count(dups, test_code, content_area_code, original_subject) %>% View()

summarize(
  student_level,
  n0 = n(),
  n1 = n_distinct(state_student_id),
  n2 = n_distinct(state_student_id, original_subject),
  n3 = n_distinct(state_student_id, original_subject, semester, test)
)

count(student_level, grade)

student_level %>%
  filter(is.na(grade)) %>%
  count(test, semester, original_subject)

# Records from Alternative, CTE, Adult HS are dropped from student level
# cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
#   transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

# summarize(
#   dedup,
#   n0 = n(),
#   n1 = n_distinct(state_student_id),
#   n2 = n_distinct(state_student_id, original_subject)
# )

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
  # group_by(state_student_id, original_subject, test, performance_level, scale_score, semester) %>%
  # mutate(
  #   n = n(),                           # Tag duplicates by id, subject, test, performance level, scale score, semester
  #   temp = mean(is.na(reported_race))  # Check whether one among duplicates has non-missing race/ethnicity
  # ) %>%
  # filter(!(n > 1 & temp != 0 & is.na(reported_race))) %>%
  # ungroup() %>%
  # select(-n, -temp) %>%
  # For students multiple test records with the same original subject, performance level, scale score, demographics
  # Deduplicate for non-missing grade
  # group_by(state_student_id, original_subject, test, performance_level, scale_score, semester, reported_race) %>%
  # mutate(
  #   n = n(),                   # Tag duplicates by id, subject, test, performance level, scale score, semester
  #   temp = mean(is.na(grade))  # Check whether one among duplicates has non-missing race/ethnicity
  # ) %>%
  # filter(!(n > 1 & temp != 0 & is.na(grade))) %>%
  # ungroup() %>%
  # select(-n, -temp) %>%
  # Valid test if there is a proficiency level
  mutate(valid_test = as.integer(not_na(performance_level)))

nrow(dedup)
nrow(distinct(dedup))

summarize(
  dedup,
  n0 = n(),
  n1 = n_distinct(state_student_id),
  n2 = n_distinct(state_student_id, original_subject),
  n3 = n_distinct(
    state_student_id, original_subject, test, performance_level, scale_score,
    semester
  )
)

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
  ) %>%
  # Assign EL = 1 if student tested ELPA
  mutate(
    el = if_else(state_student_id %in% elpa$student_id, 1, el)
  )

# write_csv(student_level, "N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv", na = "")
#     
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
  n2 = n_distinct(state_student_id, subject),
  n3 = n_distinct(state_student_id, subject, semester)
)

count(student_level_2, test, original_subject)

partic_dist <- student_level_2 %>%
  # arrange(state_student_id, subject, semester, desc(enrolled), desc(tested)) %>%
  # distinct(state_student_id, subject, semester, .keep_all = T) %>%
  group_by(system) %>%
  summarize(across(c(enrolled, tested), sum)) %>%
  ungroup()

partic_dist <- partic_dist %>%
  mutate(participation_rate = round(100 * tested / enrolled, 1))

summary(partic_dist)

partic_dist %>% arrange(participation_rate) %>% View()

Sys.time()

write_csv(partic_dist, str_c("participation-rate-district-", today(), ".csv"))

# Combine and explore WIDA ACCESS summative files ----

names(access_alt_raw)
names(access_summative_raw)

nrow(access_alt_raw)
nrow(distinct(access_alt_raw))

nrow(access_summative_raw)
nrow(distinct(access_summative_raw))

summarize(
  access_alt_raw,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  # Almost distinct by student
  n3 = n_distinct(state_student_id),
  # Distinct by DRC student ID
  n4 = n_distinct(unique_drc_student_id),
  # Distinct by district, school, and student
  n5 = n_distinct(district_number, school_number, state_student_id)
)

summarize(
  access_summative_raw,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  # Almost distinct by student
  n3 = n_distinct(state_student_id),
  # Distinct by DRC student ID
  n4 = n_distinct(unique_drc_student_id),
  n5 = n_distinct(district_number, school_number, state_student_id),
  # Almost distinct by district, school, student, and grade: Some students have
  # NA for state student ID, and some students have results for multiple
  # domains split across multiple observations.
  n6 = n_distinct(district_number, school_number, state_student_id, grade)
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

names(access)
nrow(access)
nrow(distinct(access))

summarize(
  access,
  n0 = n(),
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

map(as.list(access), ~ mean(is.na(.x)))
count(access, reported_record) # Equals 1 for all rows
count(access, grade) # Grades 0-12
count(access, test, tier) # Equals "T" for all Alt records
count(access, test, reported_tier)

# Explore demographic data ----

# names(demos_raw)
# nrow(demos_raw)
# nrow(distinct(demos_raw))
# 
# summarize(
#   demos_raw,
#   n0 = n(),
#   n1 = n_distinct(district_no),
#   n2 = n_distinct(district_no, school_no),
#   n3 = n_distinct(student_key),
#   # Distinct by district, school, student
#   n4 = n_distinct(district_no, school_no, student_key)
# )
# 
# keep(map(as.list(demos_raw), ~ mean(is.na(.x))), ~ .x != 0)
# count(demos_raw, grade) %>% View()
# count(demos_raw, limitedenglishproficiency)
# 
# demos <- demos_raw %>%
#   filter(
#     !is.na(grade),
#     (as.numeric(grade) %in% 3:12 | grade %in% str_c("T", 3:12))
#   ) %>%
#   select()

# Explore EL enrollment data ----

nrow(enr_el_raw)
nrow(distinct(enr_el_raw))

summarize(
  enr_el_raw,
  n0 = n(),
  n1 = n_distinct(system),
  n2 = n_distinct(system, school),
  # Distinct by state student ID
  n3 = n_distinct(student_id)
)

summary(select(enr_el_raw, where(~ !is.character(.x))))
map(as.list(enr_el_raw), ~ mean(is.na(.x)))
count(enr_el_raw, english_language_background)
count(enr_el_raw, grade) # Grades K-12

# Apply WIDA participation rate business rules ----

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

nrow(partic_access)
nrow(distinct(partic_access))

summarize(
  partic_access,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  # Almost distinct by state student ID
  n3 = n_distinct(state_student_id),
  # Distinct by DRC student ID, test, state student ID
  n4 = n_distinct(unique_drc_student_id, test, state_student_id),
  # Fewer than 0.1% of students appear in multiple schools or districts.
  n5 = n_distinct(district_number, school_number, state_student_id),
  # Almost distinct by district, school, student, and grade: Some students have
  # NA for state student ID, and some students have results for multiple
  # domains split across multiple observations.
  n6 = n_distinct(district_number, school_number, state_student_id, grade_file)
)

map(as.list(partic_access), ~ mean(is.na(.x)))

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
    is.na(test)
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
    # district_number = as.numeric(str_remove(district_number, "TN")),
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
  ungroup()

# Compare output with Andrew's ----

partic_dist <- read_csv(last(list.files(pattern = "participation-rate-district")))

partic_dist_am <- read_csv("N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/district_participation_rate_MSAA_TNReady_EOC_06182021.csv")

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
