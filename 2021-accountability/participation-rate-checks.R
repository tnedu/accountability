library(DBI)
library(haven)
library(janitor)
library(lubridate)
library(magrittr)
library(openxlsx)
library(rlang)
library(tidyverse)

# Read raw data ----

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
  n5 = n_distinct(usid, test_code)
)

map(as.list(regis_fall_eoc_raw), ~mean(is.na(.x)))

map(
  quos(gender, enrolled_grade, test_format),
  ~ count(regis_fall_eoc_raw, !!.x, sort = T)
)

count(regis_fall_eoc_raw, test_name, test_code, sort = T)

# Explore WIDA data ----

nrow(distinct(wida_status_19_raw)) == nrow(wida_status_19_raw)
nrow(distinct(wida_status_21_raw)) == nrow(wida_status_21_raw)

# Almost distinct by district, school, grade, student, and domain

map(
  list(wida_status_19_raw, wida_status_21_raw),
  ~ summarize(
    .x,
    n0 = n(),
    n1 = n_distinct(district),
    n2 = n_distinct(district, school),
    n3 = n_distinct(state_student_id),
    n4 = n_distinct(district, school, state_student_id),
    n5 = n_distinct(district, school, grade, state_student_id, domain),
    n6 = n_distinct(district, school, grade, state_student_id, domain, tests_ended)
  )
)

map(as.list(wida_status_19_raw), ~mean(is.na(.x)))
map(as.list(wida_status_21_raw), ~mean(is.na(.x)))

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
