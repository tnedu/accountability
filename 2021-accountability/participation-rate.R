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
# some column types (e.g., RI sub-part 1).

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

# Apply business rules to the registration data ----

regis_fall_eoc <- regis_fall_eoc_raw %>%
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
  ungroup()

regis_fall_eoc %>%
  filter(pmin(snt_subpart1, snt_subpart2, snt_subpart3, snt_subpart4, na.rm = T) == 1) %>%
  summarize(m = mean(overall_snt == 1)) %>%
  pull(m) %>%
  testthat::expect_equal(1)
