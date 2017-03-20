## District Accountability - Minimum Performance Goal

library(tidyverse)

success_rates <- read_csv("data/success_rates_TVAAS.csv")

# Minimum Performance Keys
minimum_performance_super <- success_rates %>%
    filter(subgroup == "Super Subgroup") %>%
    transmute(system, system_name, subject, BB_reduction_key = lower_bound_ci_BB < pct_below_prior)

minimum_performance <- success_rates %>%
    filter(subgroup == "All Students") %>%
    mutate(achievement_key = upper_bound_ci_PA > pct_prof_adv_prior,
        value_added_key = TVAAS_level %in% c("Level 3", "Level 4", "Level 5")) %>%
    full_join(minimum_performance_super, by = c("system", "system_name", "subject")) %>%
    group_by(system, system_name) %>%
# Count Met for Achievement, VA, and BB Reduction
    summarise(achievement_met = sum(achievement_key, na.rm = TRUE),
        achievement_eligible = sum(!is.na(achievement_key), na.rm = TRUE),
        value_added_met = sum(value_added_key, na.rm = TRUE),
        value_added_eligible = sum(!is.na(value_added_key), na.rm = TRUE),
        BB_reduction_met = sum(BB_reduction_key, na.rm = TRUE),
        BB_reduction_eligible = sum(!is.na(BB_reduction_key), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(met_achievement_goal = achievement_met/achievement_eligible >= 1/3,
        met_value_added_goal = value_added_met/value_added_eligible >= 1/3,
        met_BB_reduction_goal = BB_reduction_met/BB_reduction_eligible >= 1/3) %>%
    rowwise() %>%
    mutate(met_minimum_performance_goal = mean(c(met_achievement_goal, met_value_added_goal, met_BB_reduction_goal), na.rm = TRUE) == 1) %>%
    ungroup()

write_csv(minimum_performance, "data/minimum_performance.csv", na = "")
