library(DBI)
library(haven)
library(janitor)
library(lubridate)
library(magrittr)
library(openxlsx)
library(rlang)
library(tidyverse)

setwd(str_c(Sys.getenv('tnshare_data_use'), 'team-members/josh-carson/accountability/2021-accountability'))

# Read input data ----

# Set all column types to character because read_csv() incorrectly identifies
# some column types (e.g., modified format, RI sub-part 1).

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

cdf_fall_eoc_raw <- read_csv(
  'N:/ORP_accountability/data/2021_cdf/2021_fall_eoc_cdf.csv',
  col_types = glue::glue_collapse(rep('c', 36))
)

regis_fall_eoc_raw <- clean_names(
  read_csv(
    'N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/Student Registration Export_Fall 2020 EOC_03292021.csv',
    col_types = glue::glue_collapse(rep('c', 38))
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

count(regis_fall_eoc_raw, test_name, test_code)
count(regis_fall_eoc_raw, snt_subpart1)
count(regis_fall_eoc_raw, snt_subpart2)
count(regis_fall_eoc_raw, snt_subpart3)
count(regis_fall_eoc_raw, ri_subpart1)

# Explore fall EOC CDF data ----

nrow(distinct(cdf_fall_eoc_raw)) == nrow(cdf_fall_eoc_raw)

# Distinct by student and content area, one school per student

summarize(
  cdf_fall_eoc_raw,
  n0 = n(),
  n1 = n_distinct(system),
  n2 = n_distinct(system, school),
  n3 = n_distinct(unique_student_id),
  n4 = n_distinct(system, school, unique_student_id),
  n5 = n_distinct(unique_student_id, content_area_code)
)

map(as.list(cdf_fall_eoc_raw), ~mean(is.na(.x)))

count(cdf_fall_eoc_raw, grade)
count(cdf_fall_eoc_raw, content_area_code)
count(cdf_fall_eoc_raw, test_mode)
count(cdf_fall_eoc_raw, attempted)
count(cdf_fall_eoc_raw, modified_format)
count(cdf_fall_eoc_raw, reason_not_tested)
count(cdf_fall_eoc_raw, ri_status)

# Why do 805 rows have zeroes for reason not tested and RI status but no raw
# or scale scores?

cdf_fall_eoc_raw %>%
  mutate(raw_score_available = !is.na(raw_score)) %>%
  count(reason_not_tested, ri_status, raw_score_available)

# Why do 20,015 rows have raw scores but no scale scores?

cdf_fall_eoc_raw %>%
  mutate(
    raw_score_available = !is.na(raw_score),
    scale_score_available = !is.na(scale_score),
    performance_level_available = !is.na(performance_level)
  ) %>%
  count(
    reason_not_tested, ri_status, raw_score_available, scale_score_available,
    performance_level_available
  )

# Apply fall EOC business rules ----

partic_fall_eoc <- regis_fall_eoc_raw %>%
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
  # Join registration (denominator) and CDF (numerator) data sets.
  left_join(
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
      mutate(in_cdf = T),
    by = c('usid' = 'unique_student_id', 'test_code_2' = 'content_area_code')
  )

temp <- partic_fall_eoc %>%
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

# Confirm that the overall SNT code equals 1 anywhere at least one sub-part has
# an SNT code of 1.

partic_fall_eoc %>%
  filter(pmin(snt_subpart1, snt_subpart2, snt_subpart3, snt_subpart4, na.rm = T) == 1) %>%
  summarize(m = mean(overall_snt == 1)) %>%
  pull(m) %>%
  testthat::expect_equal(1)

count(temp, overall_snt, reason_not_tested, reason_not_tested_2, in_cdf, sort = T)

# Explore WIDA Cumulative Student Status file (denominator) ----

nrow(distinct(access_css_raw)) == nrow(access_css_raw)

# Distinct by district, school, student, grade, domain, assessment, and test
# start datetime

summarize(
  access_css_raw,
  n0 = n(),
  n1 = n_distinct(district),
  n2 = n_distinct(district, school),
  n3 = n_distinct(state_student_id),
  n4 = n_distinct(district, school, state_student_id),
  n5 = n_distinct(state_student_id, domain),
  n6 = n_distinct(
    district, school, state_student_id, grade,
    domain, assessment, tests_started
  )
)

# The "not tested" and alternate assessment fields (as well as the invalidation
# field, below) have no values.

map(as.list(access_css_raw), ~mean(is.na(.x)))

access_css_raw %>%
  mutate(test_start_missing = is.na(tests_started)) %>%
  count(test_status, test_start_missing)

count(access_css_raw, grade)
count(access_css_raw, domain)
count(access_css_raw, domain, assessment) %>% View() # What does HW mean?
count(access_css_raw, invalidation)
count(access_css_raw, accomodations, sort = T)
count(access_css_raw, test_part) # All values are '#1'.
count(access_css_raw, tests_started, sort = T)

str_split(access_css_raw$tests_started, ' ') %>%
  map_if(~ length(.x) < 3, ~ NA_Date_) %>%
  map_if(
    ~ length(.x) >= 3,
    ~ str_c(.x[[length(.x) - 1]], .x[[1]], .x[[length(.x) - 2]], sep = '-')
  ) %>%
  unlist() %>%
  as_date() %>% # Most dates failed to parse.
  summary()

# Explore WIDA ACCESS test results (numerator) ----

nrow(distinct(access_alt_raw)) == nrow(access_alt_raw)
nrow(distinct(access_summative_raw)) == nrow(access_summative_raw)

# Distinct by district, school, and student

summarize(
  access_alt_raw,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  n3 = n_distinct(state_student_id),
  n4 = n_distinct(unique_drc_student_id),
  n5 = n_distinct(district_number, school_number, state_student_id)
  # n5 = n_distinct(state_student_id, content_area_code)
)

# Almost distinct by district, school, student, and grade

summarize(
  access_summative_raw,
  n0 = n(),
  n1 = n_distinct(district_number),
  n2 = n_distinct(district_number, school_number),
  n3 = n_distinct(state_student_id),
  n4 = n_distinct(unique_drc_student_id),
  n5 = n_distinct(district_number, school_number, state_student_id),
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

map(as.list(access_alt_raw), ~mean(is.na(.x)))
map(as.list(access_summative_raw), ~mean(is.na(.x)))

count(
  access_alt_raw,
  ethnicity_hispanic_latino, race_american_indian_alaskan_native, race_asian,
  race_black_african_american, race_pacific_islander_hawaiian, race_white,
  sort = T
)

count(
  access_summative_raw,
  ethnicity_hispanic_latino, race_american_indian_alaskan_native, race_asian,
  race_black_african_american, race_pacific_islander_hawaiian, race_white,
  sort = T
)

count(access_alt_raw, native_language, sort = T)
count(access_summative_raw, native_language, sort = T)

count(access_alt_raw, migrant)
count(access_summative_raw, migrant)

# Apply WIDA ACCESS business rules ----

access_css <- access_css_raw %>%
  # For any student with multiple records within the same school and same
  # domain, keep the record(s) where test status is "Completed" (if there is
  # one).
  group_by(district, school, domain, state_student_id) %>%
  filter(sum(test_status == 'Completed') == 0 | test_status == 'Completed') %>%
  # Apply the same rule for any student with multiple records ACROSS multiple
  # schools but within the same domain.
  group_by(domain, state_student_id) %>%
  filter(sum(test_status == 'Completed') == 0 | test_status == 'Completed') %>%
  # Determine the highest test status within district-school-student but
  # across domains.
  group_by(district, school, state_student_id) %>%
  mutate(
    test_status_level = case_when(
      test_status == 'Completed' ~ 3,
      test_status == 'In Progress' ~ 2,
      test_status == 'Not Started' ~ 1
    ),
    max_test_status_level_1 = max(test_status_level)
  ) %>%
  # Determine the highest test status within student but across district-school-
  # domain.
  group_by(state_student_id) %>%
  mutate(max_test_status_level_2 = max(max_test_status_level_1)) %>%
  # Keep observations from the school where the highest test status occurred.
  filter(max_test_status_level_2 == max_test_status_level_1) %>%
  ungroup() %>%
  select(-test_status_level, -max_test_status_level_1, -max_test_status_level_2)

# count(access_css, keep)

# access_css %>%
#   group_by(state_student_id) %>%
#   filter(sum(!keep) > 0) %>%
#   ungroup() %>%
#   arrange(state_student_id, domain) %>%
#   View()

# Distinct by district, school, student, grade, domain, assessment, and test
# start datetime

summarize(
  access_css,
  n0 = n(),
  n1 = n_distinct(district),
  n2 = n_distinct(district, school),
  n3 = n_distinct(state_student_id),
  n4 = n_distinct(district, school, state_student_id),
  n5 = n_distinct(state_student_id, domain),
  n6 = n_distinct(
    district, school, state_student_id, grade,
    domain, assessment, tests_started
  )
)

map(as.list(access_css), ~mean(is.na(.x)))

count(access_css, grade)
count(access_css, domain)
