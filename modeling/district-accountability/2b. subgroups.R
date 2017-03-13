## District Accountability - Subgroups

library(tidyverse)

success_rates <- read_csv("data/success_rates_TVAAS.csv")

achievement <- success_rates %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged", 
        "Students with Disabilities", "English Learners")) %>%
    group_by(subject, subgroup) %>%
    mutate(rank_PA = ifelse(valid_tests >= 30, rank(pct_prof_adv, ties.method = "max"), NA),
        PA_denom = sum(valid_tests >= 30, na.rm = TRUE),
        achievement_quintile = ifelse(rank_PA/PA_denom < 0.2, 0, NA),
        achievement_quintile = ifelse(rank_PA/PA_denom >= 0.2, 1, achievement_quintile),
        achievement_quintile = ifelse(rank_PA/PA_denom >= 0.4, 2, achievement_quintile),
        achievement_quintile = ifelse(rank_PA/PA_denom >= 0.6, 3, achievement_quintile),
        achievement_quintile = ifelse(rank_PA/PA_denom >= 0.8, 4, achievement_quintile),
        achievement_AMO = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, 0, NA),
        achievement_AMO = ifelse(upper_bound_ci_PA > pct_prof_adv_prior, 1, achievement_AMO),
        achievement_AMO = ifelse(upper_bound_ci_PA >= AMO_target_PA, 2, achievement_AMO),
        achievement_AMO = ifelse(pct_prof_adv >= AMO_target_PA, 3, achievement_AMO),
        achievement_AMO = ifelse(pct_prof_adv >= AMO_target_PA_4, 4, achievement_AMO),
        achievement_AMO = ifelse(valid_tests < 30, NA, achievement_AMO),
        TVAAS = ifelse(TVAAS_level == "Level 1", 0, NA),
        TVAAS = ifelse(TVAAS_level == "Level 2", 1, TVAAS),
        TVAAS = ifelse(TVAAS_level == "Level 3", 2, TVAAS),
        TVAAS = ifelse(TVAAS_level == "Level 4", 3, TVAAS),
        TVAAS = ifelse(TVAAS_level == "Level 5", 4, TVAAS)) %>%
    ungroup() %>%
    select(system, system_name, subject, subgroup, achievement_quintile, achievement_AMO, TVAAS)

# Absenteeism
absenteeism <- read_csv("data/cohort_absenteeism.csv") %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners")) %>%
    transmute(system, subject = "Absenteeism", subgroup, CA_quintile, CA_AMO)

# Grad
grad <- read_csv("data/ready_grad_data.csv") %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners")) %>%
    transmute(system, system_name, subject = "Graduation Rate", subgroup, grad_quintile, grad_AMO, ACT_grad_change_quintile)

# ELPA
ELPA_growth_standard <- read_csv("data/elpa_growth_standard.csv") %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners")) %>%
    select(system, subgroup, growth_standard_AMO)

ELPA <- read_csv("data/elpa_exit.csv") %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners")) %>%
    select(system, subgroup, exit_quintile) %>%
    full_join(ELPA_growth_standard, by = c("system", "subgroup")) %>%
    mutate(subject = "ELPA")

# Combine all content areas
all_subjects <- bind_rows(achievement, absenteeism, grad, ELPA) %>%
    mutate(
    # Success Rates
        achievement = ifelse(subject %in% c("3-5 Success Rate", "6-8 Success Rate", "HS Success Rate"),
            pmax(achievement_quintile, achievement_AMO, na.rm = TRUE), NA),
        value_added = ifelse(subject %in% c("3-5 Success Rate", "6-8 Success Rate", "HS Success Rate"), TVAAS, NA),
    # Absenteeism
        achievement = ifelse(subject == "Absenteeism", pmax(CA_quintile, CA_AMO), achievement),
    # Grad
        achievement = ifelse(subject == "Graduation Rate", pmax(grad_quintile, grad_AMO), achievement),
        value_added = ifelse(subject == "Graduation Rate", ACT_grad_change_quintile, value_added),
    # ELPA
        achievement = ifelse(subject == "ELPA", pmax(exit_quintile, growth_standard_AMO), achievement)) %>%
    rowwise() %>%
    # Overall
    mutate(content_area_average = mean(c(achievement, value_added), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(system, subgroup) %>%
    summarise(subgroup_average = mean(content_area_average, na.rm = TRUE)) %>%
    ungroup()

subgroup_average <- all_subjects %>%
    group_by(system) %>%
    summarise(subgroup_average = mean(subgroup_average, na.rm = TRUE)) %>%
    mutate(subgroup_designation = ifelse(subgroup_average == 0, "In Need of Improvement", NA),
        subgroup_designation = ifelse(subgroup_average > 0, "Marginal", subgroup_designation),
        subgroup_designation = ifelse(subgroup_average > 1, "Satisfactory", subgroup_designation),
        subgroup_designation = ifelse(subgroup_average > 2, "Advancing", subgroup_designation),
        subgroup_designation = ifelse(subgroup_average > 3, "Exemplary", subgroup_designation))

write_csv(subgroup_average, path = "data/subgroup_scores.csv", na = "")
