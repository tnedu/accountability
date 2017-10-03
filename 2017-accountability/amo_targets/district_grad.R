library(acct)
library(haven)
library(tidyverse)

district_grad <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/District_grad_rate2017_JP.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    transmute(year = 2018, system, system_name,
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        grad_cohort, grad_rate,
        grad_target = amo_target(grad_cohort, grad_rate),
        grad_target_double = amo_target(grad_cohort, grad_rate, double = TRUE)
    )

write_csv(district_grad, path = "K:/ORP_accountability/projects/2018_amo/district_grad.csv", na = "")