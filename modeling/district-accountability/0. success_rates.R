## Success and Below Basic Rates for District Accountability

library(tidyverse)

percent_PA_BB <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/system_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    filter(!(grade %in% c("All Grades", "Missing Grade"))) %>%
    filter(!(subject %in% c("ACT Composite", "Graduation Rate"))) %>%
    mutate(grade = as.numeric(grade),
        subgroup = ifelse(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
    # Reassign EOCs for students in grades < 8
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject),
    # Create content areas for collapse
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
    mutate(pct_bsc = ifelse(valid_tests != 0, round(100 * n_bsc/valid_tests, 1), NA),
        pct_prof = ifelse(valid_tests != 0, round(100 * n_prof/valid_tests, 1), NA),
        pct_adv = ifelse(valid_tests != 0, round(100 * n_adv/valid_tests, 1), NA),
    # Percent PA and Upper Bound CI
        pct_prof_adv = ifelse(valid_tests != 0, round((n_prof + n_adv)/valid_tests, 3), NA),
        upper_bound_ci_PA = round(100 * valid_tests/(valid_tests + qnorm(0.975)^2) * (pct_prof_adv + (qnorm(0.975)^2/(2 * valid_tests)) +
            qnorm(0.975) * sqrt((pct_prof_adv * (1 - pct_prof_adv))/valid_tests + qnorm(0.975)^2/(4 * valid_tests^2))), 1),
        pct_prof_adv = 100 * pct_prof_adv,
    # Percent BB and Lower Bound CI
        pct_below_bsc = ifelse(valid_tests != 0, round(100 - pct_bsc - pct_prof - pct_adv, 1), NA),
        pct_below_bsc = pct_below_bsc/100,
        lower_bound_ci_BB = round(100 * valid_tests/(valid_tests + qnorm(0.975)^2) * (pct_below_bsc + (qnorm(0.975)^2/(2 * valid_tests)) -
            qnorm(0.975) * sqrt((pct_below_bsc * (1 - pct_below_bsc))/valid_tests + qnorm(0.975)^2/(4 * valid_tests^2))), 1),
        pct_below_bsc = 100 * pct_below_bsc) %>%
    select(year, system, system_name, subject, subgroup, valid_tests, pct_below_bsc, pct_prof_adv, upper_bound_ci_PA, lower_bound_ci_BB)

AMOs <- percent_PA_BB %>%
    filter(year == 2014) %>%
    transmute(system, subject, subgroup,
        valid_tests_prior = valid_tests,
        pct_below_prior = pct_below_bsc,
        pct_prof_adv_prior = pct_prof_adv,
        AMO_target_PA = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/16, 1), NA),
        AMO_target_PA_4 = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/8, 1), NA))

TVAAS <- read_csv("data/grade_band_TVAAS.csv") %>%
    filter(year == 2015) %>%
    select(system, subject, subgroup, TVAAS_level)

success_rates <- percent_PA_BB %>%
    filter(year == 2015) %>%
    left_join(AMOs, by = c("system", "subject", "subgroup")) %>%
    left_join(TVAAS, by = c("system", "subject", "subgroup"))

write_csv(success_rates, path = "data/success_rates_TVAAS.csv")
