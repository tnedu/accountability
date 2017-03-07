## School Accountability File with Super Subgroup and Science

library(tidyverse)

# Grade pools
grade_pools <- haven::read_dta("K:/ORP_accountability/projects/2016_pre_coding/Output/grade_pools_designation_immune_2016.dta") %>%
    transmute(system = as.numeric(system), school = as.numeric(school), designation_ineligible, pool)

# School base + merge on grade pools
school_base <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    mutate(grade = ifelse(subject == "Graduation Rate", "12", grade),
        grade = ifelse(subject == "ACT Composite", "11", grade)) %>%
    filter(!(grade == "All Grades" | grade == "Missing Grade")) %>%
    mutate(n_below_bsc = ifelse(subject == "Graduation Rate", dropout_count, n_below_bsc),
        n_prof = ifelse(subject == "Graduation Rate", grad_count, n_prof),
        valid_tests = ifelse(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_prof = ifelse(subject == "ACT Composite", n_21_and_above, n_prof),
        n_below_bsc = ifelse(subject == "ACT Composite", n_below_19, n_below_bsc),
        grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject)) %>%
    inner_join(grade_pools, by = c("system", "school"))

# One year success rates without ACT/Grad Rate
success_rates_1yr <- school_base %>%
    filter(!(subject == "Graduation Rate" | subject == "ACT Composite")) %>%
    rowwise() %>%
    mutate(n_PA = sum(n_prof, n_adv, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!(pool == "K8" & subject %in% c("Algebra I", "Algebra II", "Biology I", "Chemistry", "English I", "English II", "English III", "Graduation Rate"))) %>%
    mutate(subject = ifelse(subject %in% c("Algebra I", "Algebra II"), "HS Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III"), "HS English", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry"), "HS Science", subject)) %>% 
    group_by(year, system, system_name, school, school_name, pool, subgroup, subject, designation_ineligible) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    ungroup() %>%
    mutate_each(funs(ifelse(valid_tests < 30, 0, .)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>%
    rowwise() %>%
    mutate(n_PA = sum(n_prof, n_adv, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, system, system_name, school, school_name, pool, subgroup, designation_ineligible) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv, n_PA) %>%
    ungroup() %>%
    mutate(subject = "Success Rate",
        pct_bsc = ifelse(valid_tests != 0, round(100 * n_bsc/valid_tests, 1), NA),
        pct_prof = ifelse(valid_tests != 0, round(100 * n_prof/valid_tests, 1), NA),
        pct_adv = ifelse(valid_tests != 0, round(100 * n_adv/valid_tests, 1), NA),
        pct_below_bsc = ifelse(valid_tests != 0, round(100 - pct_bsc - pct_prof - pct_adv, 1), NA),
        pct_prof_adv = ifelse(valid_tests != 0, round(100 * n_PA/valid_tests, 1), NA),
        pct_below_bsc = ifelse(n_below_bsc == 0 & pct_below_bsc != 0, 0, pct_below_bsc)) %>%
    select(-pct_bsc, -pct_prof)

success_rates_3yr <- success_rates_1yr %>%
    group_by(system, system_name, school, school_name, pool, subgroup, designation_ineligible) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv, n_PA) %>%
    ungroup() %>%
    mutate(subject = "Success Rate",
        year = "3 Year",
        pct_bsc = ifelse(valid_tests != 0, round(100 * n_bsc/valid_tests, 1), NA),
        pct_prof = ifelse(valid_tests != 0, round(100 * n_prof/valid_tests, 1), NA),
        pct_adv = ifelse(valid_tests != 0, round(100 * n_adv/valid_tests, 1), NA),
        pct_below_bsc = ifelse(valid_tests != 0, round(100 - pct_bsc - pct_prof - pct_adv, 1), NA),
        pct_prof_adv = ifelse(valid_tests != 0, round(100 * n_PA/valid_tests, 1), NA),
        pct_below_bsc = ifelse(n_below_bsc == 0 & pct_below_bsc != 0, 0, pct_below_bsc)) %>%
    select(-pct_bsc, -pct_prof)

# School accountability subjects
school_accountability <- school_base %>%
    mutate(subject = ifelse(grade %in% c(3, 4, 5, 6, 7, 8), paste("3-8", subject), subject),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II"), "HS Math", subject),
        subject = ifelse(subject %in% c("English I", "English II", "English III"), "HS English", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry"), "HS Science", subject)) %>%
    group_by(year, system, system_name, school, school_name, subgroup, subject, designation_ineligible, pool) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, n_below_bsc, n_bsc, n_prof, n_adv) %>% 
    ungroup() %>%
    rowwise() %>%
    mutate(n_PA = sum(n_prof, n_adv, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct_bsc = ifelse(valid_tests != 0, round(100 * n_bsc/valid_tests, 1), NA),
        pct_prof = ifelse(valid_tests != 0, round(100 * n_prof/valid_tests, 1), NA),
        pct_adv = ifelse(valid_tests != 0, round(100 * n_adv/valid_tests, 1), NA),
        pct_below_bsc = ifelse(subject != "Graduation Rate" & subject != "ACT Composite" & valid_tests != 0, round(100 - pct_bsc - pct_prof - pct_adv, 1), NA),
        pct_below_bsc = ifelse(subject == "Graduation Rate" & valid_tests != 0, round(100 * n_below_bsc/valid_tests, 1), pct_below_bsc),
        pct_below_bsc = ifelse(subject == "ACT Composite" & valid_tests != 0, round(100 * n_below_bsc/valid_tests, 1), pct_below_bsc),
        pct_prof_adv = ifelse(valid_tests != 0, round(100 * n_PA/valid_tests, 1), NA),
        pct_below_bsc = ifelse(n_below_bsc == 0 & pct_below_bsc != 0, 0, pct_below_bsc)) %>%
    select(-pct_bsc, -pct_prof) %>%
    bind_rows(success_rates_1yr) %>%
    mutate(year = as.character(year)) %>%
    bind_rows(success_rates_3yr)

amos <- school_accountability %>%
    filter(year == "2014") %>%
    mutate(AMO_target_PA = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/16, 1), NA),
        AMO_target_PA_4 = ifelse(valid_tests >= 30, round(pct_prof_adv + (100 - pct_prof_adv)/8, 1), NA),
        AMO_target_BB = ifelse(valid_tests >= 30, round(pct_below_bsc - pct_below_bsc/8, 1), NA),
        AMO_target_BB_4 = ifelse(valid_tests >= 30, round(pct_below_bsc - pct_below_bsc/4, 1), NA),
        year = "2015") %>%
    select(year, system, system_name, school, school_name, subject, subgroup, valid_tests_prior = valid_tests,
        pct_below_bsc_prior = pct_below_bsc, pct_prof_adv_prior = pct_prof_adv,
        AMO_target_PA, AMO_target_PA_4, AMO_target_BB, AMO_target_BB_4)

# School Composite TVAAS
tvaas_2014 <- read_csv("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2013-14/URM School Value-Added and Composites.csv") %>%
    rename(system = `District Number`, school = `School_Code`, TVAAS_level_lag = `District vs State Avg`) %>%
    mutate_each(funs(as.numeric), system, school) %>%
    filter(Test == "TCAP/EOC", Subject == "Overall", Year == "One-Year Trend") %>%
    select(system, school, TVAAS_level_lag)

tvaas_2015 <- readxl::read_excel("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2014-15/URM School Value-Added and Composites.xlsx") %>%
    rename(system = `District Number`, school = `School_Code`, TVAAS_level = `District vs State Avg`) %>%
    mutate_each(funs(as.numeric), system, school) %>%
    filter(Test == "TCAP/EOC", Subject == "Overall") %>%
    left_join(tvaas_2014, by = c("system", "school")) %>%
    transmute(system, school, subject = "Success Rate", subgroup = "All Students", TVAAS_level, TVAAS_level_lag)

# Subject Specific TVAAS
tvaas_subjects <- readxl::read_excel("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2014-15/URM School Value-Added and Composites.xlsx") %>%
    filter(Test %in% c("TCAP", "EOC"), Subject %in% c("Numeracy", "Literacy", "Science"),
        Year == "One-Year Trend") %>%
    mutate(Subject = ifelse(Subject == "Numeracy", "Math", Subject),
        Subject = ifelse(Subject == "Literacy", "ELA", Subject),
        Subject = ifelse(Test == "TCAP", paste("3-8", Subject), Subject),
        Subject = ifelse(Test == "EOC", paste("HS", Subject), Subject)) %>% 
    transmute(system = as.numeric(`District Number`),
        school = as.numeric(`School_Code`),
        subject = Subject,
        subgroup = "All Students",
        TVAAS_level = `District vs State Avg`)
    
# ACT TVAAS
tvaas_act <- read_csv("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2014-15/School_ACT.csv") %>%
    filter(Year == "2014", Subject == "Composite") %>%
    transmute(system = `District Number`, school = `School Number`, subject = "ACT Composite", subgroup = "All Students", TVAAS_level = `School vs State Avg`)

tvaas_all <- bind_rows(tvaas_2015, tvaas_subjects, tvaas_act)

school_accountability %<>%
    filter(year != "2014") %>%
    left_join(amos, by = c("year", "system", "system_name", "school", "school_name", "subgroup", "subject")) %>%
    left_join(tvaas_all, by = c("system", "school", "subject", "subgroup")) %>%
    mutate(pct_prof_adv = pct_prof_adv/100,
        upper_bound_ci_PA = round(100 * (valid_tests/(valid_tests + qnorm(0.975)^2)) * (pct_prof_adv + ((qnorm(0.975)^2)/(2 * valid_tests)) + 
            qnorm(0.975) * sqrt((pct_prof_adv * (1 - pct_prof_adv))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_prof_adv = 100 * pct_prof_adv,
        pct_below_bsc = pct_below_bsc/100,
        lower_bound_ci_BB = round(100 * (valid_tests/(valid_tests + qnorm(0.975)^2)) * (pct_below_bsc + ((qnorm(0.975)^2)/(2 * valid_tests)) - 
            qnorm(0.975) * sqrt((pct_below_bsc * (1 - pct_below_bsc))/valid_tests + (qnorm(0.975)^2)/(4 * valid_tests^2))), 1),
        pct_below_bsc = 100 * pct_below_bsc,
        eligible = valid_tests >= 30) %>%
    group_by(year, subject, subgroup, pool, eligible) %>%
    mutate(rank_PA = ifelse(eligible, rank(pct_prof_adv, na.last = FALSE, ties.method = "average"), NA),
        denom = sum(eligible),
        pctile_rank_PA = round(100 * rank_PA/denom, 1)) %>%
    ungroup() %>%
    select(year, system, system_name, school, school_name, subject, subgroup, pool, designation_ineligible, valid_tests:pctile_rank_PA) %>%
    arrange(system, system_name, school, school_name, subject, subgroup)

rm(success_rates_1yr, success_rates_3yr, amos, grade_pools, school_base, tvaas_subjects, tvaas_2014, tvaas_2015, tvaas_act, tvaas_all)

# Output file
write_csv(school_accountability, path = "data/school_accountability_file.csv", na = "")
