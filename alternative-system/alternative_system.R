library(readr)
library(tidyr)
library(dplyr)

rm(list = ls())

# Read in grade pools
grade_pools <- readstata13::read.dta13("K:/ORP_accountability/projects/2015_school_coding/Output/grade_pools_immune_ap.dta") %>%
    select(system, school, designation_ineligible, pool)

# School base + merge on grade pools
school_base <- read_csv("K:/ORP_accountability/data/2015_sas_accountability/ASD included grades/school_base_2015_19jul2015.csv") %>%
    mutate(grade = ifelse(subject == "Graduation Rate", 12, grade)) %>%
    filter(grade != "All Grades" & grade != "Missing Grade") %>%
    filter(subject != "Chemistry") %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "grad_cohort", "grad_count")) %>%
    inner_join(grade_pools, by = c("system", "school"))

# School TVAAs for priority safe harbors
tvaas_school2015 <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/SAS-NIET School-Wide.xlsx") %>%
    filter(!is.na(`District Number`)) %>%
    rename(system = `District Number`, school = `School Number`, tvaas2015 = `School-Wide: Composite`) %>%
    select(system, school, tvaas2015) %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "school"))

tvaas_school2014 <- readxl::read_excel("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2013-14/SAS-NIET School-Wide.xls") %>%
    rename(system = `District Number`, school = `School Number`, tvaas2014 = `School-Wide: Composite`) %>%
    select(system, school, tvaas2014) %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "school"))

## Priority schools
# Priority success rates
priority_success_rates <- school_base %>%
    mutate(n_prof = ifelse(subject == "Graduation Rate", grad_count, n_prof),
           valid_tests = ifelse(subject == "Graduation Rate", grad_cohort, valid_tests)) %>%
    filter(subgroup == "All Students") %>%
    mutate(grade = as.numeric(grade),
           subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
           subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "RLA", subject),
           subject = ifelse(subject %in% c("Biology I") & grade <= 8, "Science", subject),
           n_adv = ifelse(is.na(n_adv), 0, n_adv),
           n_PA = n_prof + n_adv) %>%
    group_by(year, system, system_name, school, school_name, pool, subgroup, subject, designation_ineligible) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(valid_tests = ifelse(valid_tests < 30, 0, valid_tests),
           n_PA = ifelse(valid_tests < 30, 0, n_PA)) %>%
    group_by(system, system_name, school, school_name, pool, subgroup, designation_ineligible) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(success_rate_3yr = round(100 * n_PA/valid_tests, 1)) %>%
    left_join(tvaas_school2015, by = c("system", "school")) %>%
    left_join(tvaas_school2014, by = c("system", "school")) %>%
    mutate(priority_sh = tvaas2015 %in% c(4, 5) & tvaas2014 %in% c(4, 5)) %>%
    mutate(priority_school = FALSE)

hs <- ceiling(0.05 * sum(priority_success_rates$pool == "HS"))
k8 <- ceiling(0.05 * sum(priority_success_rates$pool == "K8"))

# Designate priority high schools
priority_success_rates <- priority_success_rates %>%
    arrange(pool, designation_ineligible, priority_sh, success_rate_3yr)

priority_success_rates[1:priority_hs, ]$priority_school <- TRUE

# Designate priority K8 schools
priority_success_rates <- priority_success_rates %>%
    arrange(desc(pool), designation_ineligible, priority_sh, success_rate_3yr)

priority_success_rates[1:priority_k8, ]$priority_school <- TRUE

rm(tvaas_school2014, tvaas_school2015)
