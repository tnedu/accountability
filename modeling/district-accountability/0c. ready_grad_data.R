library(tidyverse)

# Prior year
grad_prior <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2015.dta") %>%
    select(system, subgroup, grad_cohort_prior = grad_cohort, grad_rate_prior = grad_rate)

ACT_grad_prior <- haven::read_dta("K:/ORP_accountability/data/2015_ACT/ACT_district2015.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    select(system, subgroup, valid_tests, pct_21_orhigher) %>%
    left_join(grad_prior, by = c("system", "subgroup")) %>%
    mutate(act_grad_prior = ifelse(valid_tests >= 30 & grad_cohort_prior >= 30, round(grad_rate_prior * pct_21_orhigher/100, 1), NA),
        grad_AMO_target = ifelse(grad_cohort_prior >= 30, round(grad_rate_prior + (100 - grad_rate_prior)/16, 1), NA),
        grad_AMO_target_4 = ifelse(grad_cohort_prior >= 30, round(grad_rate_prior + (100 - grad_rate_prior)/8, 1), NA)) %>%
    select(system, subgroup, grad_cohort_prior, grad_rate_prior, grad_AMO_target, grad_AMO_target_4, valid_tests_ACT_prior = valid_tests, act_grad_prior)

# Current year
grad <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2016.dta") %>%
    select(system, subgroup, grad_cohort, grad_rate)

ACT_grad <- haven::read_dta("K:/ORP_accountability/data/2015_ACT/ACT_district2016.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    select(system, system_name, subgroup, valid_tests, pct_21_orhigher) %>%
    left_join(grad, by = c("system", "subgroup")) %>%
    mutate(act_grad = round(grad_rate * pct_21_orhigher/100, 1),
        grad_rate = grad_rate/100,
        upper_bound_ci_grad = round(100 * (grad_cohort/(grad_cohort + qnorm(0.975)^2)) * (grad_rate + ((qnorm(0.975)^2)/(2 * grad_cohort)) +
            qnorm(0.975) * sqrt((grad_rate * (1 - grad_rate))/grad_cohort + (qnorm(0.975)^2)/(4 * grad_cohort^2))), 1),
        grad_rate = 100 * grad_rate) %>%
    left_join(ACT_grad_prior, by = c("system", "subgroup")) %>%
    group_by(subgroup) %>%
    mutate(
    # Grad quintiles
        grad_rate = ifelse(grad_cohort < 30, NA, grad_rate),
        rank_grad = ifelse(grad_cohort >= 30, rank(grad_rate, ties.method = "max"), NA),
        denom = sum(grad_cohort >= 30, na.rm = TRUE),
        grad_quintile = ifelse(rank_grad/denom < 0.2, 0, NA),
        grad_quintile = ifelse(rank_grad/denom >= 0.2, 1, grad_quintile),
        grad_quintile = ifelse(rank_grad/denom >= 0.4, 2, grad_quintile),
        grad_quintile = ifelse(rank_grad/denom >= 0.6, 3, grad_quintile),
        grad_quintile = ifelse(rank_grad/denom >= 0.8, 4, grad_quintile),
    # Grad Target
        grad_amo = ifelse(upper_bound_ci_grad < grad_rate_prior, 0, NA),
        grad_amo = ifelse(upper_bound_ci_grad >= grad_rate_prior, 1, grad_amo),
        grad_amo = ifelse(upper_bound_ci_grad >= grad_AMO_target, 2, grad_amo),
        grad_amo = ifelse(grad_rate >= grad_AMO_target, 3, grad_amo),
        grad_amo = ifelse(grad_rate >= grad_AMO_target_4, 4, grad_amo),
    # Change in ACT * grad quintiles
        act_grad_change = act_grad - act_grad_prior,
        rank_act_grad_change = ifelse(!is.na(act_grad_change), rank(act_grad_change, ties.method = "max"), NA),
        denom_change = sum(!is.na(act_grad_change)),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change < 0.2, 0, NA),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.2, 1, act_grad_change_quintile),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.4, 2, act_grad_change_quintile),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.6, 3, act_grad_change_quintile),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.8, 4, act_grad_change_quintile)) %>%
    ungroup()

write_csv(ACT_grad, path = "data/ready_grad_data.csv", na = "")