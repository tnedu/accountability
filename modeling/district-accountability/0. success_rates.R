## Success and Below Basic Rates + TVAAS for District Accountability

library(haven)
library(readxl)
library(tidyverse)

district_names <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/system_base_with_super_subgroup_2016.csv") %>%
    select(system, system_name) %>%
    distinct()

# ACT for Success Rates (with all test takers as denominator)
ACT_prior <- read_dta("K:/ORP_accountability/data/2015_ACT/ACT_district2014.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    transmute(year = 2014, system, subject = "ACT Composite",
        subgroup = ifelse(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        valid_tests = valid_tests_nogradcohort, n_prof = num_21_orhigher_nogradcohort)

ACT <- read_dta("K:/ORP_accountability/data/2015_ACT/ACT_district2015.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    transmute(year = 2015, system, subject = "ACT Composite",
        subgroup = ifelse(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        valid_tests = valid_tests_nogradcohort, n_prof = num_21_orhigher_nogradcohort)

# Apply suppression to base
system_base <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/system_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup"),
        !(grade %in% c("All Grades", "Missing Grade")),
        !(subject %in% c("ACT Composite", "Graduation Rate"))) %>%
    mutate(grade = as.numeric(grade),
        subgroup = ifelse(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject),
        subject = ifelse(grade %in% 3:5, paste("3-5", subject), subject),
        subject = ifelse(grade %in% 6:8, paste("6-8", subject), subject)) %>%
# Aggregate across grades
    group_by(year, system, subject, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    bind_rows(ACT_prior, ACT) %>%
    mutate_each(funs(ifelse(valid_tests < 30, 0, .)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv)

# Calculate percent PA/Below with CI
percent_PA_BB <- system_base %>%
    mutate(subject = ifelse(subject %in% c("3-5 Math", "3-5 ELA", "3-5 Science"), "3-5 Success Rate", subject),
        subject = ifelse(subject %in% c("6-8 Math", "6-8 ELA", "6-8 Science"), "6-8 Success Rate", subject),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II", "English I", "English II", "English III", "Biology I", "Chemistry", "ACT Composite"), "HS Success Rate", subject)) %>%
    group_by(year, system, subject, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    ungroup() %>%
    # Percent PA and Upper Bound CI
    mutate(pct_prof_adv = round((n_prof + n_adv)/valid_tests, 3),
        upper_bound_ci_PA = round(100 * valid_tests/(valid_tests + qnorm(0.975)^2) * (pct_prof_adv + (qnorm(0.975)^2/(2 * valid_tests)) +
            qnorm(0.975) * sqrt((pct_prof_adv * (1 - pct_prof_adv))/valid_tests + qnorm(0.975)^2/(4 * valid_tests^2))), 1),
        pct_prof_adv = 100 * pct_prof_adv,
    # Percent BB and Lower Bound CI
        pct_bsc = round(100 * n_bsc/valid_tests, 1),
        pct_prof = round(100 * n_prof/valid_tests, 1),
        pct_adv = round(100 * n_adv/valid_tests, 1),
        pct_below_bsc = round(100 - pct_bsc - pct_prof - pct_adv, 1)/100,
        lower_bound_ci_BB = round(100 * valid_tests/(valid_tests + qnorm(0.975)^2) * (pct_below_bsc + (qnorm(0.975)^2/(2 * valid_tests)) -
            qnorm(0.975) * sqrt((pct_below_bsc * (1 - pct_below_bsc))/valid_tests + qnorm(0.975)^2/(4 * valid_tests^2))), 1),
        pct_below_bsc = 100 * pct_below_bsc) %>%
    select(year, system, subject, subgroup, valid_tests, pct_below_bsc, lower_bound_ci_BB, pct_prof_adv, upper_bound_ci_PA)

AMOs <- percent_PA_BB %>%
    filter(year == 2014) %>%
    transmute(system, subject, subgroup,
        valid_tests_prior = valid_tests,
        pct_below_prior = round(pct_below_bsc, 1),
        pct_prof_adv_prior = round(pct_prof_adv, 1),
        AMO_target_PA = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/16, 1), NA),
        AMO_target_PA_4 = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/8, 1), NA))

# Grade band TVAAS
TVAAS <- read_excel("K:/ORP_accountability/data/2015_tvaas/TNTDE-2015-All-Students-Across-Subjects-20170314.xlsx") %>%
    bind_rows(read_excel("K:/ORP_accountability/data/2015_tvaas/TNTDE-2015-Subgroup-Measures-Across-Subjects-20170313.xlsx")) %>%
    mutate(Subgroup = ifelse(Subgroup == "English Language Learners", "English Learners", Subgroup),
        Subgroup = ifelse(Subgroup == "Students With Disabilities", "Students with Disabilities", Subgroup),
        Subject = ifelse(Subject == "4-5 Composite", "3-5 Success Rate", Subject),
        Subject = ifelse(Subject == "6-8 Composite", "6-8 Success Rate", Subject),
        Subject = ifelse(Subject == "HS Composite", "HS Success Rate", Subject)) %>%
    transmute(system = as.numeric(`System Number`), subject = Subject, subgroup = Subgroup, TVAAS_level = Level)

success_rates <- percent_PA_BB %>%
    filter(year == 2015) %>%
    left_join(district_names, by = "system") %>%
    left_join(AMOs, by = c("system", "subject", "subgroup")) %>%
    left_join(TVAAS, by = c("system", "subject", "subgroup")) %>%
    select(system, system_name, subject, subgroup, valid_tests_prior, pct_below_prior, pct_prof_adv_prior,
        AMO_target_PA, AMO_target_PA_4, valid_tests, pct_below_bsc, lower_bound_ci_BB, pct_prof_adv, upper_bound_ci_PA,
        TVAAS_level)

write_csv(success_rates, path = "data/success_rates_TVAAS.csv")
