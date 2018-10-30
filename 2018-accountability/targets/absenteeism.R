library(acct)
library(tidyverse)

district <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/district_chronic_absenteeism_primary_enrollment_only.csv", col_types = "iicccddd") %>%
    mutate(
        AMO_target = amo_reduction(n_students, pct_chronically_absent),
        AMO_target_double = amo_reduction(n_students, pct_chronically_absent, double = TRUE)
    )

write_csv(district, "N:/ORP_accountability/projects/2019_amo/absenteeism_targets_district_primary_enrollment.csv", na = "")

school <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/school_chronic_absenteeism_primary_enrollment_only.csv", col_types = "iicicccddd") %>%
    mutate(
        AMO_target = amo_reduction(n_students, pct_chronically_absent),
        AMO_target_double = amo_reduction(n_students, pct_chronically_absent, double = TRUE)
    )

write_csv(school, "N:/ORP_accountability/projects/2019_amo/absenteeism_targets_school_primary_enrollment.csv", na = "")
