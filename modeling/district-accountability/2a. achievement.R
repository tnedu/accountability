## District Accountability - All Students

library(tidyverse)

success_rates <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/system_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015), subgroup == "All Students") %>%
    filter(!(grade %in% c("All Grades", "Missing Grade"))) %>%
    filter(!(subject %in% c("ACT Composite", "Graduation Rate"))) %>%
    mutate(grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject),
        subject = ifelse(grade %in% c(3, 4, 5), paste("3-5", subject), subject),
        subject = ifelse(grade %in% c(6, 7, 8), paste("6-8", subject), subject),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II"), "HS Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III"), "HS English", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry"), "HS Science", subject)) %>%
    group_by(year, system, system_name, subject) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_prof, n_adv) %>%
    ungroup() %>%
    mutate_each(funs(ifelse(valid_tests < 30, 0, .)), valid_tests, n_prof, n_adv) %>%
    mutate(subject = ifelse(subject %in% c("3-5 Math", "3-5 ELA", "3-5 Science"), "3-5 Success Rate", subject),
        subject = ifelse(subject %in% c("6-8 Math", "6-8 ELA", "6-8 Science"), "6-8 Success Rate", subject),
        subject = ifelse(subject %in% c("HS Math", "HS English", "HS Science"), "HS Success Rate", subject)) %>%
    group_by(year, system, system_name, subject) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_prof, n_adv) %>%
    ungroup() %>%
    mutate(n_PA = n_prof + n_adv,
        pct_prof_adv = ifelse(valid_tests != 0, round(n_PA/valid_tests, 3), NA),
        upper_bound_ci_PA = round(100 * (valid_tests/(valid_tests + qnorm(0.975)^2)) * (pct_prof_adv + ((qnorm(0.975)^2)/(2 * valid_tests)) +
            qnorm(0.975) * sqrt((pct_prof_adv * (1 - pct_prof_adv))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_prof_adv = 100 * pct_prof_adv)

AMOs <- success_rates %>%
    filter(year == 2014) %>%
    transmute(year = 2015, system, system_name, subject, valid_tests_prior = valid_tests, pct_prof_adv_prior = pct_prof_adv,
        AMO_target_PA = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/16, 1), NA),
        AMO_target_PA_4 = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/8, 1), NA))

TVAAS <- read_csv("data/grade_band_tvaas.csv") %>%
    filter(subgroup == "All Students", year == 2015) %>%
    rename(subject = content_area)

achievement <- success_rates %>%
    filter(year == 2015) %>%
    left_join(AMOs, by = c("year", "system", "system_name", "subject")) %>%
    left_join(TVAAS, by = c("year", "system", "system_name", "subject")) %>%
    group_by(subject) %>%
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
        TVAAS = ifelse(tvaas_level == "Level 1", 0, NA),
        TVAAS = ifelse(tvaas_level == "Level 2", 1, TVAAS),
        TVAAS = ifelse(tvaas_level == "Level 3", 2, TVAAS),
        TVAAS = ifelse(tvaas_level == "Level 4", 3, TVAAS),
        TVAAS = ifelse(tvaas_level == "Level 5", 4, TVAAS)) %>%
    ungroup() %>%
    select(system, system_name, subject, achievement_quintile, achievement_AMO, TVAAS)

rm(success_rates, TVAAS, AMOs)

# Absenteeism
absenteeism_VA <- read_csv("data/student_match_absenteeism.csv") %>%
    select(system, CA_reduction_quintile)

absenteeism <- read_csv("data/cohort_absenteeism.csv") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, subject = "Absenteeism", CA_quintile, CA_AMO) %>%
    full_join(absenteeism_VA, by = "system")

rm(absenteeism_VA)

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

rm(ELPA_growth_standard)

# Combine all content areas
all_subjects <- bind_rows(achievement, absenteeism, grad, ELPA) %>%
    mutate(
    # Success Rates
        achievement = ifelse(subject %in% c("3-5 Success Rate", "6-8 Success Rate", "HS Success Rate"),
            pmax(achievement_quintile, achievement_AMO, na.rm = TRUE), NA),
        value_added = ifelse(subject %in% c("3-5 Success Rate", "6-8 Success Rate", "HS Success Rate"), TVAAS, NA),
    # Absenteeism
        achievement = ifelse(subject == "Absenteeism", pmax(CA_quintile, CA_AMO, na.rm = TRUE), achievement),
        value_added = ifelse(subject == "Absenteeism", CA_reduction_quintile, value_added),
    # Grad
        achievement = ifelse(subject == "Graduation Rate", pmax(grad_quintile, grad_AMO, na.rm = TRUE), achievement),
        value_added = ifelse(subject == "Graduation Rate", ACT_grad_change_quintile, value_added),
    # ELPA
        achievement = ifelse(subject == "ELPA", pmax(exit_quintile, growth_standard_AMO, na.rm = TRUE), achievement)) %>%
    rowwise() %>%
    # Overall
    mutate(average = mean(c(achievement, value_added), na.rm = TRUE)) %>%
    ungroup()

achievement_average <- all_subjects %>%
    group_by(system) %>%
    summarise(achievement_average = mean(average, na.rm = TRUE)) %>%
    mutate(achievement_designation = ifelse(achievement_average == 0, "In Need of Improvement", NA),
        achievement_designation = ifelse(achievement_average > 0, "Marginal", achievement_designation),
        achievement_designation = ifelse(achievement_average > 1, "Satisfactory", achievement_designation),
        achievement_designation = ifelse(achievement_average > 2, "Advancing", achievement_designation),
        achievement_designation = ifelse(achievement_average > 3, "Exemplary", achievement_designation))

write_csv(achievement_average, path = "data/achievement_scores.csv", na = "")
