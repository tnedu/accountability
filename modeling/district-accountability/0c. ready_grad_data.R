library(tidyverse)

# Prior year
grad_prior <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2015.dta") %>%
    select(system, subgroup, grad_cohort, grad_rate)

ACT_grad_prior <- haven::read_dta("K:/ORP_accountability/data/2015_ACT/ACT_district2015.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", 
        "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    select(system, subgroup, valid_tests, pct_21_orhigher) %>%
    left_join(grad_prior, by = c("system", "subgroup")) %>%
    mutate(act_grad_prior = ifelse(valid_tests >= 30 & grad_cohort >= 30, round(grad_rate * pct_21_orhigher/100, 1), NA),
        AMO_target = ifelse(valid_tests >= 30 & grad_cohort >= 30, round(act_grad_prior + (100 - act_grad_prior)/16, 1), NA),
        AMO_target_4 = ifelse(valid_tests >= 30 & grad_cohort >= 30, round(act_grad_prior + (100 - act_grad_prior)/8, 1), NA)) %>%
    select(system, subgroup, act_grad_prior, AMO_target, AMO_target_4)

rm(grad_prior)

# Current year
grad <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2016.dta") %>%
    select(system, subgroup, grad_cohort, grad_rate)

ACT_grad <- haven::read_dta("K:/ORP_accountability/data/2015_ACT/ACT_district2016.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    select(system, system_name, subgroup, valid_tests, pct_21_orhigher) %>%
    left_join(grad, by = c("system", "subgroup")) %>%
    mutate(act_grad = round(grad_rate * pct_21_orhigher/10000, 3),
        upper_bound_ci = round(100 * (grad_cohort/(grad_cohort + qnorm(0.975)^2)) * (act_grad + ((qnorm(0.975)^2)/(2 * grad_cohort)) +
            qnorm(0.975) * sqrt((act_grad * (1 - act_grad))/grad_cohort + (qnorm(0.975)^2)/(4 * grad_cohort^2))), 1),
        act_grad = 100 * act_grad) %>%
    select(-pct_21_orhigher, -grad_rate) %>%
    left_join(ACT_grad_prior, by = c("system", "subgroup")) %>%
    group_by(subgroup) %>%
    mutate(
    # ACT * grad quintiles
        act_grad = ifelse(grad_cohort < 30 | valid_tests < 30, NA, act_grad),
        rank_act_grad = ifelse(grad_cohort >= 30 & valid_tests >= 30, rank(act_grad, ties.method = "max"), NA),
        denom = sum(valid_tests >= 30 & grad_cohort >= 30, na.rm = TRUE),
        act_grad_quintile = ifelse(rank_act_grad/denom < 0.2, 0, NA),
        act_grad_quintile = ifelse(rank_act_grad/denom >= 0.2, 1, act_grad_quintile),
        act_grad_quintile = ifelse(rank_act_grad/denom >= 0.4, 2, act_grad_quintile),
        act_grad_quintile = ifelse(rank_act_grad/denom >= 0.6, 3, act_grad_quintile),
        act_grad_quintile = ifelse(rank_act_grad/denom >= 0.8, 4, act_grad_quintile),
    # ACT Grad Target
        act_grad_amo = ifelse(upper_bound_ci < act_grad_prior, 0, NA),
        act_grad_amo = ifelse(upper_bound_ci >= act_grad_prior, 1, act_grad_amo),
        act_grad_amo = ifelse(upper_bound_ci >= AMO_target, 2, act_grad_amo),
        act_grad_amo = ifelse(act_grad >= AMO_target, 3, act_grad_amo),
        act_grad_amo = ifelse(act_grad >= AMO_target_4, 4, act_grad_amo),
        act_grad_amo = ifelse(valid_tests < 30 | grad_cohort < 30, NA, act_grad_amo),
    # Change in ACT * grad quintiles
        act_grad_change = act_grad - act_grad_prior,
        rank_act_grad_change = ifelse(!is.na(act_grad_change), rank(act_grad_change, ties.method = "max"), NA),
        denom_change = sum(!is.na(act_grad_change)),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change < 0.2, 0, NA),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.2, 1, act_grad_change_quintile),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.4, 2, act_grad_change_quintile),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.6, 3, act_grad_change_quintile),
        act_grad_change_quintile = ifelse(rank_act_grad_change/denom_change >= 0.8, 4, act_grad_change_quintile))

write_csv(ACT_grad, path = "data/ready_grad_data.csv", na = "")