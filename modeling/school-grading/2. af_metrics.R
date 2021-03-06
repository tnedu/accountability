## School Accountability Model 2: A-F Grades Assigned to Metrics

library(readr)
library(tidyr)
library(dplyr)

# Read in grade pools
grade_pools <- readstata13::read.dta13("K:/ORP_accountability/projects/2016_pre_coding/Output/grade_pools_designation_immune_2016.dta") %>%
    select(system, school, designation_ineligible, pool)

# School success rates and percentile ranks by subgroup
school_success_rates <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year == 2015) %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    filter(!(subject == "Graduation Rate" | subject == "ACT Composite")) %>%
    filter(!(grade == "All Grades" | grade == "Missing Grade")) %>%
    inner_join(grade_pools, by = c("system", "school")) %>%
    mutate(grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject)) %>%
    rowwise() %>%
    mutate(n_PA = sum(c(n_prof, n_adv), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(year, system, system_name, school, school_name, subgroup, subject, designation_ineligible, pool) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(n_PA = ifelse(valid_tests < 30, 0, n_PA),
        valid_tests = ifelse(valid_tests < 30, 0, valid_tests)) %>%
    group_by(system, system_name, school, school_name, subgroup, designation_ineligible, pool) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(success_rate = ifelse(valid_tests >= 30, round(100 * n_PA/valid_tests, 1), NA)) %>%
    select(-(valid_tests:n_PA)) %>%
    group_by(subgroup, designation_ineligible, pool) %>%
    mutate(rank_PA = ifelse(designation_ineligible == 0, rank(success_rate, na.last = "keep", ties.method = "min"), NA),
        denom = ifelse(designation_ineligible == 0, sum(!is.na(success_rate), na.rm = TRUE), NA),
        pctile_rank_PA = round(100 * rank_PA/denom, 1)) %>%
    select(-denom, -rank_PA)

BB_reduction <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year %in% c(2014, 2015)) %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    filter(!(subject == "Graduation Rate" | subject == "ACT Composite")) %>%
    filter(!(grade == "All Grades" | grade == "Missing Grade")) %>%
    inner_join(grade_pools, by = c("system", "school")) %>%
    mutate(grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject)) %>%
    group_by(year, system, system_name, school, school_name, subgroup, subject, designation_ineligible, pool) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_below_bsc = sum(n_below_bsc, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(n_below_bsc = ifelse(valid_tests < 30, 0, n_below_bsc),
        valid_tests = ifelse(valid_tests < 30, 0, valid_tests)) %>%
    group_by(year, system, system_name, school, school_name, subgroup, designation_ineligible, pool) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_below_bsc = sum(n_below_bsc, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(BB_rate = ifelse(valid_tests >= 30, round(100 * n_below_bsc/valid_tests, 1), NA)) %>%
    select(-(valid_tests:n_below_bsc)) %>%
    mutate(temp = ifelse(year == 2014, "Pct_BB2014", NA),
        temp = ifelse(year == 2015, "Pct_BB2015", temp)) %>%
    select(-year) %>%
    spread(temp, BB_rate) %>%
    mutate(BB_rate_reduction = round(Pct_BB2014 - Pct_BB2015, 1)) %>%
    group_by(subgroup, designation_ineligible, pool) %>%
    mutate(rank_BB = ifelse(designation_ineligible == 0, rank(BB_rate_reduction, na.last = "keep", ties.method = "min"), NA),
        denom = ifelse(designation_ineligible == 0, sum(!is.na(BB_rate_reduction), na.rm = TRUE), NA),
        pctile_rank_BB_reduction = round(100 * rank_BB/denom, 1)) %>%
    ungroup() %>%
    select(system, school, subgroup, pctile_rank_BB_reduction)

# Graduation Rate
grad <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year == 2015) %>%
    filter(subject == "Graduation Rate") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    mutate(grad_rate = ifelse(grad_cohort < 30, NA, grad_rate)) %>%
    select(system, system_name, school, school_name, subgroup, grad_rate)

# ACT
ACT <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(year == 2015) %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    filter(subject == "ACT Composite") %>%
    rename(act_21_and_above = pct_21_and_above) %>%
    mutate(act_21_and_above = ifelse(valid_tests < 30, NA, act_21_and_above)) %>%
    select(system, system_name, school, school_name, subgroup, act_21_and_above)

# TVAAS
TVAAS <- readxl::read_excel("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2014-15/URM School Value-Added and Composites.xlsx") %>%
    rename(system = `District Number`, school = `School_Code`, tvaas_composite = `District vs State Avg`) %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "school")) %>%
    filter(Test == "TCAP/EOC" & Subject == "Overall") %>%
    mutate(subgroup = "All Students") %>%
    select(system, school, subgroup, tvaas_composite)

# A-F Grades
AF_grades_metrics <- school_success_rates %>%
    left_join(BB_reduction, by = c("system", "school", "subgroup")) %>%
    left_join(TVAAS, by = c("system", "school", "subgroup")) %>%
    left_join(grad, by = c("system", "system_name", "school", "school_name", "subgroup")) %>%
    left_join(ACT, by = c("system", "system_name", "school", "school_name", "subgroup")) %>%
    mutate(grade_relative_achievement = ifelse(pctile_rank_PA >= 80, "A", NA),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 60 & pctile_rank_PA < 80, "B", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 40 & pctile_rank_PA < 60, "C", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 20 & pctile_rank_PA < 40, "D", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA < 20, "F", grade_relative_achievement),
        grade_below_bsc = ifelse(pctile_rank_BB_reduction >= 80, "A", NA),
        grade_below_bsc = ifelse(pctile_rank_BB_reduction >= 60 & pctile_rank_BB_reduction < 80, "B", grade_below_bsc),
        grade_below_bsc = ifelse(pctile_rank_BB_reduction >= 40 & pctile_rank_BB_reduction < 60, "C", grade_below_bsc),
        grade_below_bsc = ifelse(pctile_rank_BB_reduction >= 20 & pctile_rank_BB_reduction <= 40, "D", grade_below_bsc),
        grade_below_bsc = ifelse(pctile_rank_BB_reduction < 20, "F", grade_below_bsc),
        grade_tvaas = ifelse(tvaas_composite == "Level 5", "A", NA),
        grade_tvaas = ifelse(tvaas_composite == "Level 4", "B", grade_tvaas),
        grade_tvaas = ifelse(tvaas_composite == "Level 3", "C", grade_tvaas),
        grade_tvaas = ifelse(tvaas_composite == "Level 2", "D", grade_tvaas),
        grade_tvaas = ifelse(tvaas_composite == "Level 1", "F", grade_tvaas),
        grade_grad = ifelse(grad_rate >= 95, "A", NA),
        grade_grad = ifelse(grad_rate >= 90 & grad_rate < 95, "B", grade_grad),
        grade_grad = ifelse(grad_rate >= 80 & grad_rate < 90, "C", grade_grad),
        grade_grad = ifelse(grad_rate >= 67 & grad_rate < 80, "D", grade_grad),
        grade_grad = ifelse(grad_rate < 67, "F", grade_grad),
        grade_ACT = ifelse(act_21_and_above >= 50, "A", NA),
        grade_ACT = ifelse(act_21_and_above >= 40 & act_21_and_above < 50, "B", grade_ACT),
        grade_ACT = ifelse(act_21_and_above >= 30 & act_21_and_above < 40, "C", grade_ACT),
        grade_ACT = ifelse(act_21_and_above >= 15 & act_21_and_above < 30, "D", grade_ACT),
        grade_ACT = ifelse(act_21_and_above < 15, "F", grade_ACT))

# Final Summative Grades
AF_grades_final <- AF_grades_metrics

AF_grades_final[AF_grades_final == "A"] <- "4"
AF_grades_final[AF_grades_final == "B"] <- "3"
AF_grades_final[AF_grades_final == "C"] <- "2"
AF_grades_final[AF_grades_final == "D"] <- "1"
AF_grades_final[AF_grades_final == "F"] <- "0"

AF_grades_final %<>%
    mutate_each_(funs(as.numeric(.)), vars = c("grade_relative_achievement", "grade_below_bsc", "grade_tvaas", "grade_grad", "grade_ACT")) %>%
    rowwise() %>%
    mutate(subgroup_average = mean(c(grade_relative_achievement, grade_below_bsc, grade_tvaas, grade_grad, grade_ACT), na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(system, system_name, school, school_name, designation_ineligible, pool) %>%
    summarise(score = round(mean(subgroup_average, na.rm = TRUE), 2)) %>%
    ungroup() %>%
    mutate(final_grade = ifelse(score > 3, "A", NA),
        final_grade = ifelse(score > 2 & score <= 3, "B", final_grade),
        final_grade = ifelse(score > 1 & score <= 2, "C", final_grade),
        final_grade = ifelse(score > 0 & score <= 1, "D", final_grade),
        final_grade = ifelse(score == 0, "F", final_grade),
        final_grade = ifelse(designation_ineligible, NA, final_grade))

# Output files
write_csv(AF_grades_metrics, path = "data/AF_grades_metrics_by_subgroup.csv", na = "")
write_csv(AF_grades_final, path = "data/AF_grades_final.csv", na = "")
