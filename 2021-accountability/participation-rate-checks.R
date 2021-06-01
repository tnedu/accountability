library(DBI)
library(haven)
library(janitor)
library(lubridate)
library(magrittr)
library(openxlsx)
library(rlang)
library(tidyverse)

setwd(str_c(Sys.getenv('tnshare_data_use'), 'team-members/josh-carson/accountability/2021-accountability'))

connection_eis <- DBI::dbConnect(
  RJDBC::JDBC(
    "oracle.jdbc.OracleDriver",
    classPath = Sys.getenv("jar_path")
  ),
  Sys.getenv("eis_connection_string"),
  "EIS_MGR", Sys.getenv("eis_password")
)

# Read raw data ----

course_catalog_2021_raw <- read_csv("course-catalog-2020-2021-v20210506.csv") %>%
  clean_names()

course_enr_raw <- dbGetQuery(connection_eis, read_file('course-enrollment.sql')) %>%
  rename_all(tolower) %>%
  mutate(across(matches('_date'), as_date))

cte_alt_adult_schs <- read_csv('N:/ORP_accountability/data/2021_tdoe_provided_files/cte_alt_adult_schools.csv') %>%
  clean_names()

# Set all column types to character because read_csv() incorrectly identifies
# some column types (e.g., RI sub-part 1).

regis_fall_eoc_raw <- clean_names(
  read_csv(
    'N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/Student Registration Export_Fall 2020 EOC_03292021.csv',
    col_types = glue::glue_collapse(rep('c', 38))
  )
)

wida_status_19_raw <- clean_names(
  read_csv(
    'N:/Assessment_Data Returns/WIDA/2019/Cumulative_Student_Status_19.csv',
    col_types = glue::glue_collapse(rep('c', 22)),
    skip = 5
  )
)

wida_status_21_raw <- clean_names(
  read_csv(
    'N:/Assessment_Data Returns/WIDA/2021/Cumulative_Student_Status_21.csv',
    col_types = glue::glue_collapse(rep('c', 22)),
    skip = 5
  )
)

# Explore fall EOC registration data ----

nrow(distinct(regis_fall_eoc_raw)) == nrow(regis_fall_eoc_raw)

# Distinct by student and test, one school per student

summarize(
  regis_fall_eoc_raw,
  n0 = n(),
  n1 = n_distinct(district_id),
  n2 = n_distinct(district_id, school_id),
  n3 = n_distinct(usid),
  n4 = n_distinct(district_id, school_id, usid),
  n5 = n_distinct(usid, local_class_number),
  n6 = n_distinct(usid, test_code)
)

map(as.list(regis_fall_eoc_raw), ~mean(is.na(.x)))

map(
  quos(gender, enrolled_grade, test_format),
  ~ count(regis_fall_eoc_raw, !!.x, sort = T)
)

count(regis_fall_eoc_raw, test_name, test_code, sort = T)
count(regis_fall_eoc_raw, snt_subpart1)
count(regis_fall_eoc_raw, snt_subpart2)
count(regis_fall_eoc_raw, snt_subpart3)
count(regis_fall_eoc_raw, ri_subpart1)

# Explore WIDA data ----

nrow(distinct(wida_status_19_raw)) == nrow(wida_status_19_raw)
nrow(distinct(wida_status_21_raw)) == nrow(wida_status_21_raw)

# Almost distinct by district, school, student, and domain

map(
  list(wida_status_19_raw, wida_status_21_raw),
  ~ summarize(
    .x,
    n0 = n(),
    n1 = n_distinct(district),
    n2 = n_distinct(district, school),
    n3 = n_distinct(state_student_id),
    n4 = n_distinct(district, school, state_student_id),
    n5 = n_distinct(district, school, state_student_id, domain),
    n6 = n_distinct(district, school, grade, state_student_id, domain, tests_ended)
  )
)

map(as.list(wida_status_19_raw), ~ mean(is.na(.x)))
map(as.list(wida_status_21_raw), ~ mean(is.na(.x)))

count(wida_status_19_raw, grade)
count(wida_status_21_raw, grade)

count(wida_status_19_raw, domain)
count(wida_status_19_raw, domain, assessment)
count(wida_status_21_raw, domain)
count(wida_status_21_raw, domain, assessment)

# Large increase in tests 'not started'
count(wida_status_19_raw, test_status, sort = T)
count(wida_status_21_raw, test_status, sort = T)

count(wida_status_19_raw, invalidation, sort = T)
count(wida_status_21_raw, invalidation, sort = T)

count(wida_status_19_raw, test_part, sort = T)
count(wida_status_21_raw, test_part, sort = T)

# Explore course enrollment data ----

# Almost distinct by district, school, student, and local class number

summarize(
  course_enr_raw,
  n0 = n(),
  n1 = n_distinct(district_no),
  n2 = n_distinct(district_no, school_no),
  n3 = n_distinct(student_key),
  n4 = n_distinct(isp_id),
  n5 = n_distinct(district_no, school_no, student_key),
  n6 = n_distinct(district_no, school_no, student_key, local_class_number, begin_date)
)

map(as.list(course_enr_raw), ~mean(is.na(.x)))

# Compare registrations and course enrollments ----

# First, identify fall EOC courses in the course catalog.

course_catalog_2021_raw %>%
  filter(str_detect(attributes, 'Fall')) %>%
  extract2('attributes') %>%
  unique()

course_catalog_2021_raw %>%
  filter(str_detect(course_name, 'Algebra')) %>%
  extract2('attributes') %>%
  unique()

course_catalog_eoc <- course_catalog_2021_raw %>%
  filter(str_detect(course_name, 'English|Algebra|Geometry|Integrated|Biology|U.S. History')) %>%
  mutate(
    fall_eoc = str_detect(attributes, 'Fall'),
    spring_eoc = str_detect(attributes, 'Spring')
  )

# Most courses with names containing the key words above actually are not
# assessed (according to the catalog):

count(course_catalog_eoc, fall_eoc, spring_eoc)

# Biology I seems incorrectly identified as a non-assessed course in the
# course catalog. Also, the catalog includes IGCSE (Cambridge) courses with
# notes saying we expect students in these courses to take EOCs:

course_catalog_eoc_fall <- course_catalog_eoc %>%
  filter(fall_eoc | course_code == 'G03H03')

course_catalog_eoc_fall %>%
  distinct(course_name, fall_eoc, spring_eoc, notes) %>%
  arrange(course_name) %>%
  View()

# Next, filter fall EOC course enrollments.

course_enr <- course_enr_raw %>%
  filter(
    course_code %in% course_catalog_eoc_fall$course_code,
    test_window == 'F'
  )

# Some of these counts differ significantly from the counts in the fall EOC
# registration file (e.g., 91 vs. 115 districts, 266 vs. 312 schools). The
# student counts are similar (both about 76k-77k):

summarize(
  course_enr,
  n0 = n(),
  n1 = n_distinct(district_no),
  n2 = n_distinct(district_no, school_no),
  n3 = n_distinct(student_key),
  n4 = n_distinct(isp_id),
  n5 = n_distinct(district_no, school_no, student_key),
  n6 = n_distinct(district_no, school_no, student_key, local_class_number, begin_date)
)

map(as.list(course_enr_raw), ~ mean(is.na(.x)))

course_enr %>% count(course_name) %>% View()

# Very few observations are one-day enrollments:

course_enr %>%
  mutate(
    d1 = end_date - begin_date,
    d2 = sca_end_date - sca_begin_date,
    d3 = cs_end_date - cs_begin_date
  ) %>%
  summarize(
    m1 = mean(!is.na(d1) & d1 < 2),
    m2 = mean(!is.na(d2) & d2 < 2),
    m3 = mean(!is.na(d3) & d3 < 2)
  )

# Join and compare course enrollment and fall EOC registration data.

course_enr_regis <- course_enr %>%
  mutate(
    # local_class_number = case_when(
    #   district_no == 830 ~ str_remove(local_class_number, '^00|^0'),
    #   district_no == 830 & str_length(local_class_number) == 12 ~ str_trunc(local_class_number, 6, 'right', ''),
    #   T ~ local_class_number
    # ),
    local_class_number = if_else(
      district_no %in% c(500, 830),
      str_remove(local_class_number, '^00|^0'),
      local_class_number
    ),
    local_class_number = if_else(
      district_no == 830 & str_length(local_class_number) == 12,
      str_trunc(local_class_number, 6, 'right', ''),
      local_class_number
    ),
    in_eis = T
  ) %>%
  full_join(
    regis_fall_eoc_raw %>%
      mutate(
        district_id = as.numeric(district_id),
        school_id = as.numeric(school_id),
        usid = as.numeric(usid),
        local_class_number = if_else(
          district_id == 500,
          str_remove(local_class_number, '^00|^0'),
          local_class_number
        ),
        local_class_number = if_else(
          district_id == 830,
          str_remove_all(local_class_number, '\\.|E\\+11'),
          local_class_number
        ),
        in_regis = T
      ),
    by = c(
      'district_no' = 'district_id',
      'school_no' = 'school_id',
      'student_key' = 'usid',
      'local_class_number'
    ),
    suffix = c('_eis', '_regis')
  )

# About 14% of registration records do not match EIS course enrollments on
# district, school, student, and local class number:

count(course_enr_regis, in_eis, in_regis)

summarize(
  course_enr_regis,
  s = sum(in_eis & in_regis, na.rm = T),
  m = s / sum(in_regis, na.rm = T)
)

# Each data set appears not to include about 3-5% of all students in the union
# of the two data sets.

course_enr %>%
  distinct(student_key) %>%
  mutate(in_eis = T) %>%
  full_join(
    distinct(regis_fall_eoc_raw, usid) %>% mutate(usid = as.numeric(usid), in_regis = T),
    by = c('student_key' = 'usid')
  ) %>%
  summarize(
    n0 = n(),
    n1 = n_distinct(student_key),
    n2 = sum(in_eis, na.rm = T),
    n3 = sum(in_regis, na.rm = T),
    n2_n1 = n2 / n1,
    n3_n1 = n3 / n1
  )

mismatches <- course_enr_regis %>% filter(is.na(in_eis) | is.na(in_regis))

slice_head(count(mismatches, district_no, in_eis, in_regis, sort = T), n = 20)

summary(mismatches$school_no)
sum(mismatches$school_no >= 9000)
sum(mismatches$school_no %in% c(981, 982, 999))

mismatches %>%
  inner_join(
    transmute(
      cte_alt_adult_schs,
      district_no = as.numeric(district_number),
      school_no = as.numeric(school_number)
    )
  ) %>%
  nrow()

# In Sumner County (district 830), some local class numbers seem correctly
# recorded in EIS but incorrectly recorded (as scientific notation converted to
# strings) in the registration file. Other discrepancies exist due to leading
# zeroes. The vendor or district might have converted some local class numbers
# to numeric form then back to string.

mismatches %>%
  filter(district_no == 830) %>%
  count(district_no, in_eis, in_regis, local_class_number, sort = T) %>%
  slice_head(n = 20)

regis_fall_eoc_raw %>%
  filter(district_id == '830', str_detect(local_class_number, 'E+')) %>%
  count(local_class_number, sort = T)

course_enr %>%
  filter(district_no == 830, str_detect(local_class_number, '102103')) %>%
  count(local_class_number, sort = T)

course_enr %>%
  filter(district_no == 830, str_detect(local_class_number, '2510300201')) %>%
  count(local_class_number, sort = T)

course_enr_regis %>%
  filter(district_no == 830) %>%
  count(local_class_number_eis, local_class_number_regis, sort = T) %>%
  # filter(
  #   is.na(local_class_number_eis)
  #   | is.na(local_class_number_regis)
  #   | local_class_number_eis != local_class_number_regis
  # ) %>%
  View()

# Wilson County (district 950) does not appear in the fall EOC course
# enrollments from EIS because almost no course enrollments in this district
# have code 'F' for test window.

summarize(course_enr_regis, s = sum(district_no == 950 & in_eis, na.rm = T))

mismatches %>%
  filter(district_no == 950) %>%
  count(district_no, in_eis, in_regis, local_class_number, sort = T) %>%
  slice_head(n = 20)

course_enr_raw %>%
  filter(district_no == 950, str_detect(local_class_number, '012G01H0902')) %>%
  count(course_code, test_window, sort = T)

course_catalog_2021_raw %>% filter(course_code == 'G01H09')
course_catalog_eoc %>% filter(course_code == 'G01H09')
course_catalog_eoc_fall %>% filter(course_code == 'G01H09')

course_enr_raw %>%
  filter(district_no == 950) %>%
  count(test_window)

# District 500

mismatches %>%
  filter(district_no == 500) %>%
  count(district_no, in_eis, in_regis, local_class_number, sort = T) %>%
  slice_head(n = 20)

course_enr %>%
  filter(district_no == 500, str_detect(local_class_number, '403001003')) %>%
  count(local_class_number, sort = T)

# Knox County (district 470)

summarize(course_enr_regis, s = sum(district_no == 470 & in_regis, na.rm = T))
