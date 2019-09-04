library(acct)
library(tidyverse)

# district <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district.csv") %>%
#     filter(subgroup %in% c(
#         "All Students",
#         "Black/Hispanic/Native American",
#         "Economically Disadvantaged",
#         "English Learners",
#         "Students with Disabilities"
#     )) %>%
#     transmute(
#         system,
#         subgroup,
#         growth_standard_denom,
#         pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
#         AMO_target = amo_target(growth_standard_denom, pct_met_growth_standard, n_minimum = 10),
#         AMO_target_double = amo_target(growth_standard_denom, pct_met_growth_standard, double = TRUE, n_minimum = 10)
#     )
# 
# write_csv(district, "N:/ORP_accountability/projects/2020_amo/elpa_targets_district.csv", na = "")

school <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv") %>%
    filter(subgroup %in% c(
        "All Students",
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Black/Hispanic/Native American",
        "Economically Disadvantaged",
        "English Learners",
        "Hispanic",
        "Native Hawaiian or Other Pacific Islander",
        "Students with Disabilities",
        "White"
    )) %>%
    transmute(
        system,
        school,
        subgroup,
        growth_standard_denom,
        pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
        AMO_target = amo_target(growth_standard_denom, pct_met_growth_standard, n_minimum = 10),
        AMO_target_double = amo_target(growth_standard_denom, pct_met_growth_standard, double = TRUE, n_minimum = 10)
    )

write_csv(school, "N:/ORP_accountability/projects/2020_amo/elpa_targets_school.csv", na = "")
