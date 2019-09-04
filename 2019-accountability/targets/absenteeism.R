library(acct)
library(tidyverse)

district <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/district_chronic_absenteeism_Jul11.csv") %>%
    filter(
        grade_band == "All Grades",
        subgroup %in% c(
            "All Students",
            "Black/Hispanic/Native American",
            "Economically Disadvantaged",
            "English Learners with Transitional 1-4",
            "Students with Disabilities"
        )
    ) %>%
    mutate(
        AMO_target = amo_reduction(n_students, pct_chronically_absent, n_minimum = 10),
        AMO_target_double = amo_reduction(n_students, pct_chronically_absent, double = TRUE, n_minimum = 10)
    )

write_csv(district, "N:/ORP_accountability/projects/2020_amo/absenteeism_targets_district.csv", na = "")

school <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/school_chronic_absenteeism_Jul11.csv") %>%
    mutate(
        AMO_target = amo_reduction(n_students, pct_chronically_absent, n_minimum = 10),
        AMO_target_double = amo_reduction(n_students, pct_chronically_absent, double = TRUE, n_minimum = 10)
    )

write_csv(school, "N:/ORP_accountability/projects/2020_amo/absenteeism_targets_school.csv", na = "")
