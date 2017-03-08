library(tidyverse)

# TVAAS by Grade Band
tvaas <- read_csv("data/grade_band_tvaas.csv") %>%
    select(year, system, subgroup, subject = content_area, tvaas_level)

# One-Year Success and Below Rates
success_rates <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/system_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    filter(!(grade == "All Grades" | grade == "Missing Grade")) %>%
    filter(!(subject %in% c("ACT Composite", "Graduation Rate"))) %>%
    mutate(grade = as.numeric(grade),
        subgroup = ifelse(subgroup == "English Language Learners with T1/T2", "English Language Learners", subgroup),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject),
        subject = ifelse(grade %in% c(3, 4, 5), paste("3-5", subject), subject),
        subject = ifelse(grade %in% c(6, 7, 8), paste("6-8", subject), subject),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II"), "HS Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III"), "HS English", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry"), "HS Science", subject)) %>%
    group_by(year, system, system_name, subject, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    ungroup() %>%
    mutate_each(funs(ifelse(valid_tests < 30, 0, .)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    mutate(subject = ifelse(subject %in% c("3-5 Math", "3-5 ELA", "3-5 Science"), "3-5 Success Rate", subject),
        subject = ifelse(subject %in% c("6-8 Math", "6-8 ELA", "6-8 Science"), "6-8 Success Rate", subject),
        subject = ifelse(subject %in% c("HS Math", "HS English", "HS Science"), "HS Success Rate", subject)) %>%
    group_by(year, system, system_name, subject, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    ungroup() %>%
    mutate(n_PA = n_prof + n_adv,
        pct_bsc = ifelse(valid_tests != 0, round(100 * n_bsc/valid_tests, 1), NA),
        pct_prof = ifelse(valid_tests != 0, round(100 * n_prof/valid_tests, 1), NA),
        pct_adv = ifelse(valid_tests != 0, round(100 * n_adv/valid_tests, 1), NA),
        pct_below_bsc = ifelse(valid_tests != 0, round(100 - pct_bsc - pct_prof - pct_adv, 1), NA),
        pct_prof_adv = ifelse(valid_tests != 0, round(100 * n_PA/valid_tests, 1), NA),
        pct_below_bsc = ifelse(n_below_bsc == 0 & pct_below_bsc != 0, 0, pct_below_bsc),
        pct_prof_adv = pct_prof_adv/100,
        upper_bound_ci_PA = round(100 * (valid_tests/(valid_tests + qnorm(0.975)^2)) * (pct_prof_adv + ((qnorm(0.975)^2)/(2 * valid_tests)) +
            qnorm(0.975) * sqrt((pct_prof_adv * (1 - pct_prof_adv))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_prof_adv = 100 * pct_prof_adv,
        pct_below_bsc = pct_below_bsc/100,
        lower_bound_ci_BB = round(100 * (valid_tests/(valid_tests + qnorm(0.975)^2)) * (pct_below_bsc + ((qnorm(0.975)^2)/(2 * valid_tests)) -
            qnorm(0.975) * sqrt((pct_below_bsc * (1 - pct_below_bsc))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_below_bsc = 100 * pct_below_bsc) %>%
    select(-pct_bsc, -pct_prof, -pct_adv)

prior <- success_rates %>%
    filter(year == 2014) %>%
    select(system, system_name, subject, subgroup, valid_tests_prior = valid_tests,
        pct_below_prior = pct_below_bsc, pct_prof_adv_prior = pct_prof_adv)

# Minimum Performance Keys
minimum_performance_super <- success_rates %>%
    filter(subgroup == "Super Subgroup", year == 2015) %>%
    left_join(prior, by = c("system", "system_name", "subject", "subgroup")) %>%
    transmute(system, system_name, subject, bb_reduction_key = lower_bound_ci_BB <= pct_below_prior)

minimum_performance <- success_rates %>%
    filter(subgroup == "All Students", year == 2015) %>%
    left_join(prior, by = c("system", "system_name", "subject", "subgroup")) %>%
    left_join(tvaas, by = c("year", "system", "subject", "subgroup")) %>%
    mutate(achievement_key = upper_bound_ci_PA >= pct_prof_adv_prior,
        value_added_key = tvaas_level %in% c("Level 3", "Level 4", "Level 5")) %>%
    full_join(minimum_performance_super, by = c("system", "system_name", "subject")) %>%
    group_by(system, system_name) %>%
    summarise(achievement_met = sum(achievement_key, na.rm = TRUE),
        achievement_eligible = sum(!is.na(achievement_key), na.rm = TRUE),
        value_added_met = sum(value_added_key, na.rm = TRUE),
        value_added_eligible = sum(!is.na(value_added_key), na.rm = TRUE),
        bb_reduction_met = sum(bb_reduction_key, na.rm = TRUE),
        bb_reduction_eligible = sum(!is.na(bb_reduction_key), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(met_achievement_goal = achievement_met/achievement_eligible >= 1/3,
        met_value_added_goal = value_added_met/value_added_eligible >= 1/3,
        met_bb_reduction_goal = bb_reduction_met/bb_reduction_eligible >= 1/3) %>%
    rowwise() %>%
    mutate(met_minimum_performance_goal = mean(c(met_achievement_goal, met_value_added_goal, met_bb_reduction_goal), na.rm = TRUE) == 1) %>%
    ungroup()

write_csv(minimum_performance, "data/minimum_performance.csv", na = "")
