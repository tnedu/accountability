library(tidyverse)

# TVAAS by Grade Band
tvaas <- read_csv("data/grade_band_tvaas.csv") %>%
    rename(subject = content_area)

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
        subject = ifelse(subject %in% c("Math", "ELA", "Science") & grade %in% c(3, 4, 5), paste("3-5", subject), subject),
        subject = ifelse(subject %in% c("Math", "ELA", "Science") & grade %in% c(6, 7, 8), paste("6-8", subject), subject),
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
        pct_below_bsc = ifelse(n_below_bsc == 0 & pct_below_bsc != 0, 0, pct_below_bsc)) %>%
    select(-pct_bsc, -pct_prof, -pct_adv)

prior <- success_rates %>%
    filter(year == 2014) %>% 
    transmute(system, system_name, subject, subgroup, valid_tests_prior = valid_tests,
        pct_below_prior = pct_below_bsc, pct_prof_adv_prior = pct_prof_adv)

# Success Rates Minimum Performance Keys
minimum_performance_all <- success_rates %>%
    filter(subgroup == "All Students") %>%
    filter(year == 2015) %>%
    left_join(prior, by = c("system", "system_name", "subject", "subgroup")) %>%
    left_join(tvaas, by = c("year", "system", "system_name", "subject", "subgroup")) %>% 
    transmute(system, system_name, subject,
        achievement_key = pct_prof_adv >= pct_prof_adv_prior - 2,
        value_added_key = tvaas_level %in% c("Level 3", "Level 4", "Level 5"))

minimum_performance_super <- success_rates %>%
    filter(subgroup == "Super Subgroup") %>%
    filter(year == 2015) %>%
    left_join(prior, by = c("system", "system_name", "subject", "subgroup")) %>%
    transmute(system, system_name, subject, bb_reduction_key = pct_below_bsc <= pct_below_prior + 2)

# Absenteeism Minimum Performance Keys
absenteeism_growth <- read_csv("data/student_match_absenteeism.csv") %>%
    transmute(system, value_added_key = pct_no_longer_chronic_scaled >= -0.5)

absenteeism <- read_csv("data/cohort_absenteeism.csv") %>%
    filter(system != 0, subgroup %in% c("All Students", "Super")) %>%
    mutate(key = ifelse(n_students >= 30 & n_students_prior >= 30, pct_chronic <= pct_chronic_prior + 2, NA)) %>%
    select(system, system_name, subgroup, key) %>%
    spread(subgroup, key) %>%
    transmute(system, system_name, subject = "Absenteeism", achievement_key = `All Students`, bb_reduction_key = Super) %>%
    full_join(absenteeism_growth, by = "system")

# Grad Minimum Performance Keys
grad_prior <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2014.dta") %>%
    filter(subgroup %in% c("All Students", "Super Subgroup")) %>%
    transmute(system, subgroup, grad_cohort_prior = grad_cohort, grad_rate_prior = grad_rate, dropout_rate_prior = drop_rate)

grad_super <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2015.dta") %>%
    filter(subgroup == "Super Subgroup") %>%
    left_join(grad_prior, by = c("system", "subgroup")) %>%
    transmute(system, bb_reduction_key = ifelse(grad_cohort >= 30 & grad_cohort_prior >= 30, drop_rate <= dropout_rate_prior, NA))

grad <- haven::read_dta("K:/ORP_accountability/data/2015_graduation_rate/district_grad_rate2015.dta") %>%
    filter(subgroup == "All Students") %>%
    left_join(grad_prior, by = c("system", "subgroup")) %>%
    mutate(achievement_key = ifelse(grad_cohort >= 30 & grad_cohort_prior >= 30, grad_rate >= grad_rate_prior - 2, NA),
        grad_rate_change = ifelse(grad_cohort >= 30 & grad_cohort_prior >= 30, grad_rate - grad_rate_prior, NA),
        grad_rate_change_scaled = scale(grad_rate_change),
        value_added_key = grad_rate_change_scaled >= -0.5) %>%
    select(system, system_name, subject, achievement_key, value_added_key) %>%
    full_join(grad_super, by = "system")

# Combine Minimum Performance Keys
minimum_performance <- minimum_performance_all %>%
    full_join(minimum_performance_super, by = c("system", "system_name", "subject")) %>% 
    bind_rows(absenteeism) %>% 
    bind_rows(grad)
