## District Accountability - All Students

library(tidyverse)

# Success Rates, AMOs and TVAAS
success_rates <- read_csv("data/success_rates_TVAAS.csv")

achievement <- success_rates %>%
    filter(subgroup == "All Students") %>%
    group_by(subject) %>%
    mutate(rank_PA = ifelse(valid_tests >= 30, rank(pct_prof_adv, ties.method = "max"), NA),
        PA_denom = sum(valid_tests >= 30, na.rm = TRUE),
        achievement_quintile = ifelse(rank_PA/PA_denom <= 0.2, 0, NA),
        achievement_quintile = ifelse(rank_PA/PA_denom > 0.2, 1, achievement_quintile),
        achievement_quintile = ifelse(rank_PA/PA_denom > 0.4, 2, achievement_quintile),
        achievement_quintile = ifelse(rank_PA/PA_denom > 0.6, 3, achievement_quintile),
        achievement_quintile = ifelse(rank_PA/PA_denom > 0.8, 4, achievement_quintile),
        achievement_AMO = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, 0, NA),
        achievement_AMO = ifelse(upper_bound_ci_PA > pct_prof_adv_prior, 1, achievement_AMO),
        achievement_AMO = ifelse(upper_bound_ci_PA >= AMO_target_PA, 2, achievement_AMO),
        achievement_AMO = ifelse(pct_prof_adv > AMO_target_PA, 3, achievement_AMO),
        achievement_AMO = ifelse(pct_prof_adv >= AMO_target_PA_4, 4, achievement_AMO),
        achievement_AMO = ifelse(valid_tests < 30, NA, achievement_AMO),
        TVAAS = ifelse(TVAAS_level == "Level 1", 0, NA),
        TVAAS = ifelse(TVAAS_level == "Level 2", 1, TVAAS),
        TVAAS = ifelse(TVAAS_level == "Level 3", 2, TVAAS),
        TVAAS = ifelse(TVAAS_level == "Level 4", 3, TVAAS),
        TVAAS = ifelse(TVAAS_level == "Level 5", 4, TVAAS)) %>%
    ungroup() %>%
    select(system, system_name, subject, achievement_quintile, achievement_AMO, TVAAS)

# Absenteeism
absenteeism_VA <- read_csv("data/student_match_absenteeism.csv") %>%
    select(system, CA_reduction_quintile)

absenteeism <- read_csv("data/cohort_absenteeism.csv") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, subject = "Absenteeism", CA_quintile, CA_AMO) %>%
    full_join(absenteeism_VA, by = "system")

# Grad
grad <- read_csv("data/ready_grad_data.csv") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, system_name, subject = "Graduation Rate", grad_quintile, grad_AMO, ACT_grad_change_quintile)

# ELPA
ELPA_growth_standard <- read_csv("data/elpa_growth_standard.csv") %>%
    filter(subgroup == "All Students") %>%
    select(system, growth_standard_AMO)

ELPA <- read_csv("data/elpa_exit.csv") %>%
    filter(subgroup == "All Students") %>%
    select(system, exit_quintile) %>%
    full_join(ELPA_growth_standard, by = "system") %>%
    mutate(subject = "ELPA")

# Combine content areas
all_subjects <- bind_rows(achievement, absenteeism, grad, ELPA) %>%
    # Success Rates
    # Not setting na.rm = TRUE for pmax so that districts are only evaluated if they have absolute, AMO, and TVAAS
    mutate(achievement = ifelse(subject %in% c("3-5 Success Rate", "6-8 Success Rate", "HS Success Rate"),
            pmax(achievement_quintile, achievement_AMO), NA),
        value_added = ifelse(subject %in% c("3-5 Success Rate", "6-8 Success Rate", "HS Success Rate"), TVAAS, NA),
    # Absenteeism
        achievement = ifelse(subject == "Absenteeism", pmax(CA_quintile, CA_AMO), achievement),
        value_added = ifelse(subject == "Absenteeism", CA_reduction_quintile, value_added),
    # Grad
        achievement = ifelse(subject == "Graduation Rate", pmax(grad_quintile, grad_AMO), achievement),
        value_added = ifelse(subject == "Graduation Rate", ACT_grad_change_quintile, value_added),
    # ELPA
        achievement = ifelse(subject == "ELPA", pmax(exit_quintile, growth_standard_AMO), achievement),
    # Overall
        average = (achievement + value_added)/2,
    # For now, ELPA doesn't have a VA metric
        average = ifelse(subject == "ELPA", achievement, average))

achievement_average <- all_subjects %>%
    group_by(system) %>%
    summarise(achievement_average = mean(average, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(achievement_designation = ifelse(achievement_average == 0, "In Need of Improvement", NA),
        achievement_designation = ifelse(achievement_average > 0, "Marginal", achievement_designation),
        achievement_designation = ifelse(achievement_average > 1, "Satisfactory", achievement_designation),
        achievement_designation = ifelse(achievement_average > 2, "Advancing", achievement_designation),
        achievement_designation = ifelse(achievement_average > 3, "Exemplary", achievement_designation))

write_csv(achievement_average, path = "data/achievement_scores.csv", na = "")
