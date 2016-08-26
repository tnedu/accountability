## Build School Accountability File with Super Subgroup and Science

library(readr)
library(dplyr)

# Read in grade pools
grade_pools <- readstata13::read.dta13("K:/ORP_accountability/projects/2016_pre_coding/Output/grade_pools_designation_immune_2016.dta") %>%
    select(system, school, designation_ineligible, pool)

# School base + merge on grade pools
school_base <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015)) %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    mutate(grade = ifelse(subject == "Graduation Rate", "12", grade),
        grade = ifelse(subject == "ACT Composite", "11", grade)) %>%
    filter(!(grade == "All Grades" | grade == "Missing Grade")) %>%
    mutate(n_prof = ifelse(subject == "Graduation Rate", grad_count, n_prof),
        valid_tests = ifelse(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_prof = ifelse(subject == "ACT Composite", n_21_and_above, n_prof),
        n_below_bsc = ifelse(subject == "ACT Composite", n_below_19, n_below_bsc)) %>%
    inner_join(grade_pools, by = c("system", "school"))

school_accountability <- school_base %>%
    mutate(grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject),
        n_adv = ifelse(is.na(n_adv), 0, n_adv)) %>%
    rowwise() %>%
    mutate(n_PA = sum(c(n_prof, n_adv), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(subject = ifelse(grade %in% c(3, 4, 5), paste("3-5", subject), subject),
        subject = ifelse(grade %in% c(6, 7, 8), paste("6-8", subject), subject),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II"), "HS Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III"), "HS English", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry"), "HS Science", subject)) %>%
    group_by(year, system, system_name, school, school_name, subgroup, subject, designation_ineligible, pool) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_below_bsc = sum(n_below_bsc, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct_below_bsc = ifelse(valid_tests != 0, round(100 * n_below_bsc/valid_tests, 1), NA),
        pct_prof_adv = ifelse(valid_tests != 0, round(100 * n_PA/valid_tests, 1), NA))

amos <- school_accountability %>%
    filter(year == 2014) %>%
    mutate(AMO_target_PA = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/16, 1), NA),
        AMO_target_PA_4 = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/8, 1), NA),
        AMO_target_BB = ifelse(valid_tests >= 30, round(pct_below_bsc - pct_below_bsc/8, 1), NA),
        AMO_target_BB_4 = ifelse(valid_tests >= 30, round(pct_below_bsc - pct_below_bsc/4, 1), NA),
        year = year + 1) %>%
    select(year, system, system_name, school, school_name, subject, subgroup, valid_tests, pct_below_bsc, pct_prof_adv, AMO_target_PA, AMO_target_PA_4, AMO_target_BB, AMO_target_BB_4) %>%
    rename(valid_tests_prior = valid_tests, pct_below_bsc_prior = pct_below_bsc, pct_prof_adv_prior = pct_prof_adv)

tvaas_all <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/School-Level Intra-Year NCE MRM and URM Results (All Students).xlsx") %>%
    filter(Test %in% c("TCAP", "EOC") | (Test == "ACT" & Subject == "Composite")) %>%
    mutate(Subject = ifelse(Subject == "Math" & Grade == "4-5", "3-5 Math", Subject),
        Subject = ifelse(Subject == "Math" & Grade == "6-8", "6-8 Math", Subject),
        Subject = ifelse(Subject == "Reading/Language" & Grade == "4-5", "3-5 ELA", Subject),
        Subject = ifelse(Subject == "Reading/Language" & Grade == "6-8", "6-8 ELA", Subject),
        Subject = ifelse(Test == "ACT" & Subject == "Composite", "ACT Composite", Subject),
        Year = as.numeric(Year),
        `System Number` = as.numeric(`System Number`),
        `School Number` = as.numeric(`School Number`),
        subgroup = "All Students") %>%
    rename(year = Year, system = `System Number`, school = `School Number`, subject = Subject, TVAAS_level = Level) %>%
    select(year, system, school, subject, subgroup, TVAAS_level)

tvaas_subgroups <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/School-Level Intra-Year NCE MRM and URM Results (Subgroups).xlsx") %>%
    filter(Test %in% c("TCAP", "EOC") | (Test == "ACT" & Subject == "Composite")) %>%
    mutate(Subject = ifelse(Subject == "Math" & Grade == "4-5", "3-5 Math", Subject),
        Subject = ifelse(Subject == "Math" & Grade == "6-8", "6-8 Math", Subject),
        Subject = ifelse(Subject == "Reading/Language" & Grade == "4-5", "3-5 ELA", Subject),
        Subject = ifelse(Subject == "Reading/Language" & Grade == "6-8", "6-8 ELA", Subject),
        Subject = ifelse(Test == "ACT" & Subject == "Composite", "ACT Composite", Subject),
        Year = as.numeric(Year),
        `System Number` = as.numeric(`System Number`),
        `School Number` = as.numeric(`School Number`)) %>%
    rename(year = Year, system = `System Number`, school = `School Number`, subject = Subject, TVAAS_level = Level, subgroup = Subgroup) %>%
    select(year, system, school, subject, subgroup, TVAAS_level)

tvaas_all <- bind_rows(tvaas_all, tvaas_subgroups)

school_accountability <- school_accountability %>%
    filter(year == 2015) %>%
    left_join(amos, by = c("year", "system", "system_name", "school", "subgroup", "school_name", "subject")) %>%
    left_join(tvaas_all, by = c("year", "system", "school", "subject", "subgroup")) %>%
    mutate(pct_prof_adv = pct_prof_adv/100,
        upper_bound_ci_PA = round(100 * valid_tests/(valid_tests + qnorm(0.975)^2) * (pct_prof_adv + ((qnorm(0.975)^2)/(2 * valid_tests)) + 
            qnorm(0.975) * sqrt((pct_prof_adv * (1 - pct_prof_adv))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_prof_adv = 100 * pct_prof_adv,
        pct_below_bsc = pct_below_bsc/100,
        lower_bound_ci_BB = round(100 * valid_tests/(valid_tests + qnorm(0.975)^2) * (pct_below_bsc + ((qnorm(0.975)^2)/(2 * valid_tests)) - 
            qnorm(0.975) * sqrt((pct_below_bsc * (1 - pct_below_bsc))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_below_bsc = 100 * pct_below_bsc,
        eligible = (valid_tests >= 30 & valid_tests_prior >= 30)) %>%
    group_by(designation_ineligible, subject, subgroup, eligible) %>%
    mutate(rank = ifelse(eligible == 1, rank(pct_prof_adv, na.last = FALSE, ties.method = "average"), NA), 
        rank_prior = ifelse(eligible == 1, rank(pct_prof_adv_prior, na.last = FALSE, ties.method = "average"), NA), 
        denom = sum(eligible),
        percentile_rank = round(100 * rank/denom, 1),
        percentile_rank_prior = round(100 * rank_prior/denom, 1)) %>%
    ungroup() %>%
    select(-(n_below_bsc:n_PA), -(eligible:denom))

rm(amos, grade_pools, school_base, tvaas_all, tvaas_subgroups)

# Output file
write_csv(school_accountability, path = "data/school_accountability_file.csv", na = "")
