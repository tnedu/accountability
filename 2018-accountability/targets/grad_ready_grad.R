library(acct)
library(tidyverse)

grad_base <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv")

district_grad <- grad_base %>%
    filter(
        system != 0, school == 0,
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Learners", "Super Subgroup")
    ) %>%
    transmute(
        system, system_name, subgroup,
        grad_cohort, grad_rate,
        AMO_target = amo_target(grad_cohort, grad_rate),
        AMO_target_double = amo_target(grad_cohort, grad_rate, double = TRUE)
    )

write_csv(district_grad, path = "N:/ORP_accountability/projects/2019_amo/grad_district.csv", na = "")

school_grad <- grad_base %>%
    filter(system != 0, school != 0, !grepl("Non-", subgroup)) %>%
    transmute(
        system, system_name, school, school_name, subgroup,
        grad_cohort, grad_rate,
        AMO_target = amo_target(grad_cohort, grad_rate),
        AMO_target_double = amo_target(grad_cohort, grad_rate, double = TRUE)
    )

write_csv(school_grad, path = "N:/ORP_accountability/projects/2019_amo/grad_school.csv", na = "")

school_ready_grad <- haven::read_dta("N:/ORP_accountability/data/2018_final_accountability_files/ready_grad_school2018_JW.dta") %>%
    filter(system != 0, school != 0, !grepl("Non-", subgroup)) %>%
    transmute(
        system, system_name, school, school_name, subgroup,
        grad_cohort, pct_ready_grad,
        AMO_target = amo_target(grad_cohort, pct_ready_grad),
        AMO_target_double = amo_target(grad_cohort, pct_ready_grad, double = TRUE)
    )

write_csv(school_ready_grad, path = "N:/ORP_accountability/projects/2019_amo/ready_grad_school.csv", na = "")
