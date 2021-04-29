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
