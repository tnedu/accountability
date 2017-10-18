## School Accountability File with Super Subgroup and Science

library(acct)
library(haven)
library(readxl)
library(tidyverse)

# Designation Ineligible
designation_ineligible <- read_dta("K:/ORP_accountability/projects/2016_pre_coding/Output/grade_pools_designation_immune_2016.dta") %>%
    transmute(system = as.numeric(system), system_name, school = as.numeric(school), school_name, designation_ineligible)

# Grade pools based on grad
grade_pools <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year == 2015, subject == "Graduation Rate", subgroup == "All Students") %>%
    transmute(system, school, pool = if_else(grad_cohort >= 30, "HS", "K8")) %>%
    right_join(designation_ineligible, by = c("system", "school")) %>%
    mutate(pool = if_else(is.na(pool), "K8", pool))

# School base + merge on grade pools
school_base <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup",
            "Black or African American", "Hispanic", "American Indian or Alaska Native",
            "Native Hawaiian or Other Pacific Islander", "Asian", "White"
        )
    ) %>%
    mutate(subgroup = case_when(
            subgroup == "Black or African American" ~ "Black",
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "American Indian or Alaska Native" ~ "Native American",
            subgroup == "Native Hawaiian or Other Pacific Islander" ~ "Hawaiian/Pacific Islander",
            TRUE ~ subgroup
        ),
    # Reassign counts for grad and ACT
        grade = if_else(subject %in% c("ACT Composite", "Graduation Rate"), "12", grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_prof = if_else(subject == "Graduation Rate", grad_count, n_prof),
        n_prof = if_else(subject == "ACT Composite", n_21_and_above, n_prof)) %>%
    filter(!grade %in% c("All Grades", "Missing Grade")) %>%
    mutate(grade = as.numeric(grade),
        subject = case_when(
            subject %in% c("Algebra I", "Algebra II") & grade <= 8 ~ "Math",
            subject %in% c("English I", "English II", "English III") & grade <= 8 ~ "ELA",
            subject %in% c("Biology I", "Chemistry") & grade <= 8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    inner_join(grade_pools, by = c("system", "system_name", "school", "school_name")) %>%
# Aggregate across grades
    group_by(year, system, system_name, school, school_name, pool, subject, subgroup, designation_ineligible) %>%
    summarise_at(c("valid_tests", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_prof", "n_adv"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    mutate(subject = case_when(
            subject %in% c("Algebra I", "Algebra II") ~ "HS Math",
            subject %in% c("English I", "English II", "English III") ~ "HS English",
            subject %in% c("Biology I", "Chemistry") ~ "HS Science",
            TRUE ~ subject
        )
    )

# ACT for success rates (with all test takers as denominator)
ACT_prior <- read_dta("K:/ORP_accountability/data/2015_ACT/ACT_school2014_modeling.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup",
        "Black or African American", "Hispanic", "Native American", "Asian", "White", "HPI")) %>%
    transmute(year = 2014, system, school, subject = "ACT Composite",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "Black or African American" ~ "Black",
            subgroup == "HPI" ~ "Hawaiian/Pacific Islander",
            TRUE ~ subgroup
        ),
        n_prof = if_else(valid_tests_nogradcohort < 30, 0, num_21_orhigher_nogradcohort),
        valid_tests = if_else(valid_tests_nogradcohort < 30, 0, valid_tests_nogradcohort)) %>%
    inner_join(grade_pools, by = c("system", "school"))

ACT <- read_dta("K:/ORP_accountability/data/2015_ACT/ACT_school2015_modeling.dta") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup",
        "Black or African American", "Hispanic", "Native American", "Asian", "White", "HPI")) %>%
    transmute(year = 2015, system, school, subject = "ACT Composite",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners",
            subgroup == "Black or African American" ~ "Black",
            subgroup == "HPI" ~ "Hawaiian/Pacific Islander",
            TRUE ~ subgroup
        ),
        n_prof = if_else(valid_tests_nogradcohort < 30, 0, num_21_orhigher_nogradcohort),
        valid_tests = if_else(valid_tests_nogradcohort < 30, 0, valid_tests_nogradcohort)) %>%
    inner_join(grade_pools, by = c("system", "school"))

# One year success rates with ACT
success_rates_1yr <- school_base %>%
    filter(!subject %in% c("ACT Composite", "Graduation Rate")) %>%
    bind_rows(ACT, ACT_prior) %>%
    group_by(year, system, system_name, school, school_name, pool, subgroup, designation_ineligible) %>%
    summarise_at(c("valid_tests", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(year = as.character(year), subject = "Success Rate")

# Three year success rates with ACT
success_rates_3yr <- school_base %>%
    filter(!subject %in% c("ACT Composite", "Graduation Rate")) %>%
    bind_rows(ACT, ACT_prior) %>%
    group_by(system, system_name, school, school_name, pool, subgroup, designation_ineligible) %>%
    summarise_at(c("valid_tests", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(year = "3 Year", subject = "Success Rate")

# ACT/Grad
ACT_grad <- school_base %>%
    filter(subject %in% c("ACT Composite", "Graduation Rate")) %>%
    mutate(year = as.character(year))

success_rates_all <- bind_rows(success_rates_1yr, success_rates_3yr, ACT_grad) %>%
    mutate(pct_prof_adv = round5(100 * (n_prof + n_adv)/valid_tests, 1))

# AMOs
AMOs <- success_rates_all %>%
    filter(year == "2014") %>%
    transmute(year = "2015", system, system_name, school, school_name, subject, subgroup,
        valid_tests_prior = valid_tests, pct_prof_adv_prior = pct_prof_adv,
        AMO_target_PA = amo_target(valid_tests, pct_prof_adv),
        AMO_target_PA_4 = amo_target(valid_tests, pct_prof_adv, double = TRUE))

# School Composite TVAAS
TVAAS_prior <- read_csv("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2013-14/URM School Value-Added and Composites.csv") %>%
    filter(Test == "TCAP/EOC", Subject == "Overall", Year == "One-Year Trend") %>%
    transmute(system = as.numeric(`District Number`), school = as.numeric(`School_Code`),
        TVAAS_level_lag = `District vs State Avg`)

TVAAS <- read_excel("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2014-15/URM School Value-Added and Composites.xlsx") %>%
    filter(Test == "TCAP/EOC", Subject == "Overall") %>%
    transmute(system = as.numeric(`District Number`), school = as.numeric(`School_Code`), subject = "Success Rate",
        subgroup = "All Students", TVAAS_level = `District vs State Avg`) %>%
    left_join(TVAAS_prior, by = c("system", "school"))

# School accountability file
school_accountability <- success_rates_all %>%
    filter(year != "2014") %>%
    left_join(AMOs, by = c("year", "system", "system_name", "school", "school_name", "subgroup", "subject")) %>%
    left_join(TVAAS, by = c("system", "school", "subject", "subgroup")) %>%
    mutate(upper_bound_ci_PA = ci_upper_bound(valid_tests, pct_prof_adv)) %>%
    group_by(year, subject, subgroup, designation_ineligible, pool) %>%
    mutate(rank_PA = if_else(valid_tests >= 30, rank(pct_prof_adv, ties.method = "max"), NA_integer_),
        denom = sum(valid_tests >= 30, na.rm = TRUE),
        pctile_rank_PA = if_else(!designation_ineligible, round5(100 * rank_PA/denom, 1), NA_real_)) %>%
    ungroup() %>%
    select(year, system, system_name, school, school_name, pool, subject, subgroup, designation_ineligible,
        valid_tests_prior, valid_tests, pct_prof_adv_prior, pct_prof_adv, upper_bound_ci_PA,
        AMO_target_PA, AMO_target_PA_4, pctile_rank_PA, TVAAS_level, TVAAS_level_lag) %>%
    arrange(system, system_name, school, school_name, subject, subgroup)

# Output file
write_csv(school_accountability, path = paste0("data/school_accountability_file_", format(Sys.Date(), "%b%d"), ".csv"), na = "")
