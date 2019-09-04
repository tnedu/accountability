library(acct)
library(tidyverse)

school_grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>%
    filter(subgroup %in% c(
        "All Students",
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Black/Hispanic/Native American",
        "Economically Disadvantaged",
        "English Learners with Transitional 1-4",
        "Hispanic",
        "Native Hawaiian or Other Pacific Islander",
        "Students with Disabilities",
        "White"
    )) %>%
    transmute(
        system,
        system_name,
        school,
        school_name,
        subgroup,
        grad_cohort_prior = grad_cohort,
        grad_rate = if_else(grad_cohort_prior >= 10, grad_rate, NA_real_),
        AMO_target = amo_target(grad_cohort, grad_rate, n_minimum = 10),
        AMO_target_double = amo_target(grad_cohort, grad_rate, double = TRUE, n_minimum = 10)
    )

write_csv(school_grad, "N:/ORP_accountability/projects/2020_amo/grad_targets_school.csv", na = "")

school_ready_grad <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv") %>%
    filter(subgroup %in% c(
        "All Students",
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Black/Hispanic/Native American",
        "Economically Disadvantaged",
        "English Learners with Transitional 1-4",
        "Hispanic",
        "Native Hawaiian or Other Pacific Islander",
        "Students with Disabilities",
        "White",
        "Super Subgroup"
    )) %>%
    transmute(
        system,
        system_name,
        school,
        school_name,
        subgroup,
        grad_count_prior = n_count,
        pct_ready_grad_prior = if_else(grad_count_prior >= 10, pct_ready_grad, NA_real_),
        AMO_target = amo_target(grad_count_prior, pct_ready_grad, n_minimum = 10),
        AMO_target_double = amo_target(grad_count_prior, pct_ready_grad, double = TRUE, n_minimum = 10)
    ) %>%
    arrange(system, school, subgroup)

write_csv(school_ready_grad, "N:/ORP_accountability/projects/2020_amo/ready_grad_targets_school.csv", na = "")

district_grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/district_grad_rate.csv") %>%
    filter(subgroup %in% c(
        "All Students",
        "Black/Hispanic/Native American",
        "Economically Disadvantaged",
        "English Learners with Transitional 1-4",
        "Students with Disabilities"
    )) %>%
    transmute(
        system,
        system_name, 
        subgroup,
        grad_cohort_prior = grad_cohort,
        grad_rate = if_else(grad_cohort_prior < 10, NA_real_, grad_rate),
        AMO_target = amo_target(grad_cohort, grad_rate, n_minimum = 10),
        AMO_target_double = amo_target(grad_cohort, grad_rate, double = TRUE, n_minimum = 10)
    )

write_csv(district_grad, "N:/ORP_accountability/projects/2020_amo/grad_targets_district.csv", na = "")
