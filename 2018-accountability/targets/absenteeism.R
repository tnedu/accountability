library(acct)
library(tidyverse)

district <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/district_chronic_absenteeism.csv", col_types = "iicccddd") %>%
    mutate(
        AMO_target = amo_target(n_students, pct_chronically_absent),
        AMO_target_double = amo_target(n_students, pct_chronically_absent, double = TRUE)
    )

write_csv(district, "N:/ORP_accountability/projects/2019_amo/absenteeism_targets_district.csv", na = "")

school <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/school_chronic_absenteeism.csv", col_types = "iicicccddd") %>%
    mutate(
        AMO_target = amo_target(n_students, pct_chronically_absent),
        AMO_target_double = amo_target(n_students, pct_chronically_absent, double = TRUE)
    )

write_csv(school, "N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school.csv", na = "")
