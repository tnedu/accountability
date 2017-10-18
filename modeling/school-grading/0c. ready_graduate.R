## Ready Graduate Indicator for A-F School Grading

library(acct)
library(tidyverse)

school_accountability <- read_csv("data/school_accountability_file_Sep29.csv", col_types = c("cicicccciddddddddcc"))

# ACT and Grad
grad <- school_accountability %>%
    filter(subject == "Graduation Rate") %>%
    select(system, school, subgroup, grad_cohort = valid_tests, grad_rate = pct_prof_adv,
        grad_cohort_prior = valid_tests_prior, grad_rate_prior = pct_prof_adv_prior,
        upper_bound_ci = upper_bound_ci_PA, AMO_target = AMO_target_PA, AMO_target_4 = AMO_target_PA_4)

grad_grades <- grad %>%
    transmute(system, school, subgroup,
        grade_grad = case_when(
            grad_cohort < 30 ~ NA_character_,
            grad_rate >= 95 ~ "A",
            grad_rate >= 90 ~ "B",
            grad_rate >= 80 ~ "C",
            grad_rate >= 67 ~ "D",
            grad_rate < 67 ~ "F"
        ),
        grade_grad_target = case_when(
            grad_cohort < 30 ~ NA_character_,
            grad_rate >= AMO_target_4 ~ "A",
            grad_rate > AMO_target ~ "B",
            upper_bound_ci >= AMO_target ~ "C",
            upper_bound_ci > grad_rate_prior ~ "D",
            upper_bound_ci <= grad_rate_prior ~ "F"
        )
    )

ACT_grad <- school_accountability %>%
    filter(subject == "ACT Composite") %>%
    select(system, school, subgroup, valid_tests, pct_prof_adv, valid_tests_prior, pct_prof_adv_prior) %>%
    full_join(grad, by = c("system", "school", "subgroup")) %>%
    transmute(system, school, subgroup,
        pct_prof_adv_prior = if_else(valid_tests_prior < 30, NA_real_, pct_prof_adv_prior),
        grad_rate_prior = if_else(grad_cohort_prior < 30, NA_real_, grad_rate_prior),
        pct_prof_adv = if_else(valid_tests < 30, NA_real_, pct_prof_adv),
        grad_rate = if_else(grad_cohort < 30, NA_real_, grad_rate),
        ACT_grad_prior = round5(grad_rate_prior * pct_prof_adv_prior/100, 1),
        ACT_grad = round5(grad_rate * pct_prof_adv/100, 1),
        AMO_target = amo_target(grad_cohort, ACT_grad_prior),
        AMO_target_4 = amo_target(grad_cohort, ACT_grad_prior, double = TRUE),
        grade_ready_grad = case_when(
            ACT_grad >= 50 ~ "A",
            ACT_grad >= 40 ~ "B",
            ACT_grad >= 30 ~ "C",
            ACT_grad >= 25 ~ "D",
            ACT_grad < 25 ~ "F"
        ),
        grade_ready_grad_target = case_when(
            ACT_grad >= AMO_target_4 ~ "A",
            ACT_grad >= AMO_target ~ "B",
            ACT_grad > ACT_grad_prior ~ "C",
            ACT_grad == ACT_grad_prior ~ "D",
            ACT_grad < ACT_grad_prior ~ "F"
        )
    )

full_join(grad_grades, ACT_grad, by = c("system", "school", "subgroup")) %>%
    write_csv("data/ready_graduate.csv", na = "")
