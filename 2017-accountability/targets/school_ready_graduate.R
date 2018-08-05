library(acct)
library(haven)
library(tidyverse)

school_grad <- read_dta("N:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta") %>%
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

school_ACT_grad <- read_dta("N:/ORP_accountability/data/2016_ACT/ACT_school2017_individualsubgroups.dta") %>%
    mutate(subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native American" ~ "American Indian or Alaska Native",
            subgroup == "Non-English Language Learners" ~ "Non-English Learners",
            TRUE ~ subgroup
        )
    ) %>% 
    full_join(school_grad, by = c("system", "school", "subgroup")) %>%
    transmute(system, school, subgroup,
        grad_cohort, grad_rate, grad_target, grad_target_double,
        valid_tests_ACT = valid_tests, 
        ACT_21_or_higher = if_else(grad_cohort >= 30 & valid_tests_ACT >= 30,
            round5(100 * n_21_orhigher/grad_cohort, 1), NA_real_),
        ACT_grad_target = amo_target(grad_cohort, ACT_21_or_higher),
        ACT_grad_target_double = amo_target(grad_cohort, ACT_21_or_higher, double = TRUE)
    )

write_csv(school_ACT_grad, path = "N:/ORP_accountability/projects/2018_amo/school_ready_grad.csv", na = "")
