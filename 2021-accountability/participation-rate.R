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
