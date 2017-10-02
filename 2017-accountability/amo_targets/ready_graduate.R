library(acct)
library(haven)
library(tidyverse)

school_grad <- read_dta("K:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta") %>%
    transmute(year = 2018, system, system_name, school, school_name,
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Non-English Language Learners with T1/T2" ~ "Non-English Learners",
            TRUE ~ subgroup
        ),
        grad_cohort, grad_rate,
        grad_target = amo_target(grad_cohort, grad_rate),
        grad_target_double = amo_target(grad_cohort, grad_rate, double = TRUE)
    )

school_ACT <- read_dta("K:/ORP_accountability/data/2016_ACT/ACT_school2017.dta") %>%
    transmute(system, school,
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "Hawaiian or Pacific Islander" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Non-English Language Learners" ~ "Non-English Learners",
            TRUE ~ subgroup
        ),
        valid_tests_ACT = valid_tests, ACT_21_or_higher = pct_21_orhigher
    )

school_ACT_grad <- left_join(school_grad, school_ACT, by = c("system", "school", "subgroup")) %>%
    mutate(ACT_grad = if_else(grad_cohort >= 30 & valid_tests_ACT >= 30, round5(grad_rate * ACT_21_or_higher/100, 1), NA_real_),
        temp = pmin(grad_cohort, valid_tests_ACT),
        ACT_grad_target = amo_target(temp, ACT_grad),
        ACT_grad_target_double = amo_target(temp, ACT_grad, double = TRUE)) %>%
    select(-temp)

write_csv(school_ACT_grad, path = "K:/ORP_accountability/projects/2018_amo/ready_grad.csv", na = "")
