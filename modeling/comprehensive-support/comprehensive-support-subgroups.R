library(readr)
library(tidyr)
library(dplyr)

# Aggregate success rates by pool
priority_cusp_2015 <- readstata13::read.dta13("K:/ORP_accountability/projects/2015_Priority_Cusp/priority_cusp_ap.dta") %>%
    arrange(desc(pool), pctile) %>%
    filter(pool == "K8" & round(pctile, 1) <= 5.1 | pool == "HS" & round(pctile, 1) <= 5.2) %>%
    group_by(pool) %>%
    summarise(n_PA = sum(n_PA), valid_tests = sum(valid_tests)) %>%
    ungroup() %>%
    mutate(pool_success_rate = round(100 * n_PA/valid_tests, 1)) %>%
    select(-valid_tests, -n_PA)

## Priority success rates for all subgroups
# Read in grade pools
grade_pools <- readstata13::read.dta13("K:/ORP_accountability/projects/2015_school_coding/Output/grade_pools_immune_ap.dta") %>%
    select(system, school, designation_ineligible, pool, Title_1)

# School base + merge on grade pools
school_base <- read_csv("K:/ORP_accountability/data/2015_sas_accountability/ASD included grades/school_base_2015_19jul2015.csv") %>%
    mutate(grade = ifelse(subject == "Graduation Rate", 12, grade)) %>%
    filter(grade != "All Grades" & grade != "Missing Grade") %>%
    filter(!(subject == "Chemistry")) %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "grad_cohort", "grad_count")) %>%
    mutate(n_prof = ifelse(subject == "Graduation Rate", grad_count, n_prof),
        valid_tests = ifelse(subject == "Graduation Rate", grad_cohort, valid_tests)) %>%
    mutate(grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "RLA", subject),
        subject = ifelse(subject == "Biology I" & grade <= 8, "Science", subject),
        n_adv = ifelse(is.na(n_adv), 0, n_adv),
        n_PA = n_prof + n_adv) %>%
    inner_join(grade_pools, by = c("system", "school")) %>%
    filter(subgroup %in% c("Asian", "Black", "Black/Hispanic/Native American", "Economically Disadvantaged", "English Language Learners",
        "English Language Learners with T1/T2", "Hispanic", "Native American", "Students with Disabilities", "White"))

# Priority success rates
subgroups_below_priority <- school_base %>%
    group_by(year, system, system_name, school, school_name, pool, subgroup, subject, Title_1, designation_ineligible) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(temp = (subgroup == "English Language Learners" & valid_tests >= 30)) %>%
    group_by(system, system_name, school, school_name, pool, subject, Title_1, designation_ineligible) %>%
    ungroup() %>%
    mutate(ell_30 = max(temp)) %>%
    filter(!(subgroup == "English Language Learners with T1/T2" & ell_30 == 0)) %>%
    filter(!(subgroup == "English Language Learners")) %>%
    mutate(valid_tests = ifelse(valid_tests < 30, 0, valid_tests),
        n_PA = ifelse(valid_tests < 30, 0, n_PA)) %>%
    group_by(system, system_name, school, school_name, pool, subgroup, Title_1, designation_ineligible) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(success_rate_3yr = ifelse(valid_tests != 0, round(100 * n_PA/valid_tests, 1), NA)) %>%
    inner_join(priority_cusp_2015, by = "pool") %>% 
    filter(Title_1 == 1) %>%
    mutate(subgroup_below_pool_success_rate = ifelse(valid_tests != 0, success_rate_3yr <= pool_success_rate, NA))

write_csv(subgroups_below_priority, "subgroups_below_priority.csv")

schools_w_subgroups_below_priority <- subgroups_below_priority %>%
    group_by(system, system_name, school, school_name) %>%
    summarise(subgroups_below_priority = sum(subgroup_below_pool_success_rate, na.rm = TRUE))

write_csv(schools_w_subgroups_below_priority, "schools_w_subgroups_below_priority.csv")
