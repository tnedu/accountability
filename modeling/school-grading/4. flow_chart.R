## School Accountability Model 4: Flow chart model

library(readr)
library(tidyr)
library(dplyr)

# Read in grade pools
grade_pools <- readstata13::read.dta13("K:/ORP_accountability/projects/2016_pre_coding/Output/grade_pools_designation_immune_2016.dta") %>%
    select(system, school, designation_ineligible, pool)

hs <- ceiling(0.05 * sum(grade_pools$pool == "HS"))
k8 <- ceiling(0.05 * sum(grade_pools$pool == "K8"))

# School success rates by subgroup
success_rates <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Language Learners with T1/T2", "Super Subgroup")) %>%
    mutate(grade = ifelse(subject == "Graduation Rate", "12", grade),
        grade = ifelse(subject == "ACT Composite", "11", grade)) %>%
    filter(!(grade == "All Grades" | grade == "Missing Grade")) %>%
    mutate(n_prof = ifelse(subject == "Graduation Rate", grad_count, n_prof),
        n_below_bsc = ifelse(subject == "Graduation Rate", dropout_count, n_below_bsc),
        valid_tests = ifelse(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_prof = ifelse(subject == "ACT Composite", n_21_and_above, n_prof),
        n_below_bsc = ifelse(subject == "ACT Composite", n_below_19, n_below_bsc),
        grade = as.numeric(grade),
        subject = ifelse(subject %in% c("Algebra I", "Algebra II") & grade <= 8, "Math", subject), 
        subject = ifelse(subject %in% c("English I", "English II", "English III") & grade <= 8, "ELA", subject),
        subject = ifelse(subject %in% c("Biology I", "Chemistry") & grade <= 8, "Science", subject)) %>%
    rowwise() %>%
    mutate(n_PA = sum(c(n_prof, n_adv), na.rm = TRUE)) %>%
    ungroup() %>%
    inner_join(grade_pools, by = c("system", "school")) %>%
    filter(!(pool == "K8" & subject %in% c("Algebra I", "Algebra II", "Biology I", "Chemistry", "English I", "English II", "English III", "Graduation Rate"))) %>%
    group_by(year, system, system_name, school, school_name, pool, subgroup, subject, designation_ineligible) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(valid_tests = ifelse(valid_tests < 30, 0, valid_tests),
        n_PA = ifelse(valid_tests < 30, 0, n_PA)) %>%
    group_by(system, system_name, school, school_name, pool, subgroup, designation_ineligible) %>%
    summarise(valid_tests = sum(valid_tests, na.rm = TRUE), n_PA = sum(n_PA, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(success_rate_3yr = ifelse(valid_tests > 0, round(100 * n_PA/valid_tests, 1), NA))

# School TVAAS for priority safe harbors
tvaas_school2015 <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/SAS-NIET School-Wide.xlsx") %>%
    filter(!is.na(`District Number`)) %>%
    rename(system = `District Number`, school = `School Number`, tvaas2015 = `School-Wide: Composite`) %>%
    select(system, school, tvaas2015) %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "school"))

tvaas_school2014 <- readxl::read_excel("K:/Research and Policy/ORP_Data/Educator_Evaluation/TVAAS/Raw_Files/2013-14/SAS-NIET School-Wide.xls") %>%
    rename(system = `District Number`, school = `School Number`, tvaas2014 = `School-Wide: Composite`) %>%
    select(system, school, tvaas2014) %>%
    mutate_each_(funs(as.numeric(.)), vars = c("system", "school"))

# Priority schools
priority_schools <- success_rates %>%
    filter(subgroup == "All Students") %>%
    left_join(tvaas_school2015, by = c("system", "school")) %>%
    left_join(tvaas_school2014, by = c("system", "school")) %>%
    mutate(priority_sh = tvaas2015 %in% c(4, 5) & tvaas2014 %in% c(4, 5)) %>%
    group_by(pool, designation_ineligible, priority_sh) %>%
    mutate(rank = rank(success_rate_3yr, na.last = "keep", ties.method = "min")) %>%
    ungroup() %>%
    arrange(pool, designation_ineligible, priority_sh, success_rate_3yr, rank) %>%
    mutate(priority = ifelse(rank <= hs & pool == "HS" & designation_ineligible == 0 & priority_sh == FALSE, TRUE, FALSE)) %>%
    arrange(desc(pool), designation_ineligible, priority_sh, success_rate_3yr, rank) %>%
    mutate(priority = ifelse(rank <= k8 & pool == "K8" & designation_ineligible == 0 & priority_sh == FALSE, TRUE, priority)) %>%
    filter(priority == TRUE) %>%
    select(system, school, priority)

rm(k8, hs, tvaas_school2014)

# Percentiles by subgroup
percentiles <- success_rates %>%
    group_by(pool, subgroup) %>%
    mutate(eligible = valid_tests >= 30,
        rank = ifelse(eligible, rank(success_rate_3yr, na.last = "keep", ties.method = "max"), NA), 
        denom = sum(eligible),
        percentile_rank = round(100 * rank/denom, 1)) %>%
    ungroup() %>%
    select(system, system_name, school, school_name, pool, subgroup, designation_ineligible, percentile_rank) %>%
    spread(subgroup, percentile_rank) %>%
    rename(pctile_rank_All = `All Students`, pctile_rank_BHN = `Black/Hispanic/Native American`,
        pctile_rank_ED = `Economically Disadvantaged`, pctile_rank_EL = `English Language Learners with T1/T2`,
        pctile_rank_SWD = `Students with Disabilities`, pctile_rank_Super = `Super Subgroup`)

# AMOs met by subgroup
amos <- read_csv("data/school_accountability_file.csv") %>%
    select(system, system_name, school, school_name, subgroup, subject, valid_tests, upper_bound_ci_PA, AMO_target_PA) %>%
    mutate(met_amo = ifelse(valid_tests >= 30, upper_bound_ci_PA >= AMO_target_PA, NA)) %>%
    group_by(system, system_name, school, school_name, subgroup) %>%
    summarise(pct_amos_met = round(100 * mean(met_amo, na.rm = TRUE), 1)) %>%
    ungroup() %>%
    mutate(pct_amos_met = ifelse(is.na(pct_amos_met), NA, pct_amos_met)) %>%
    spread(subgroup, pct_amos_met) %>%
    rename(pct_amos_met_All = `All Students`, pct_amos_met_BHN = `Black/Hispanic/Native American`,
        pct_amos_met_ED = `Economically Disadvantaged`, pct_amos_met_EL = `English Language Learners with T1/T2`,
        pct_amos_met_SWD = `Students with Disabilities`, pct_amos_met_Super = `Super Subgroup`)

# Final Designations
grades <- percentiles %>%
    left_join(priority_schools, by = c("system", "school")) %>%
    left_join(tvaas_school2015, by = c("system", "school")) %>%
    left_join(amos, by = c("system", "system_name", "school", "school_name")) %>%
    rowwise() %>%
    mutate(all_pctile_top_5 = (mean((c(pctile_rank_All, pctile_rank_BHN, pctile_rank_ED, pctile_rank_EL, pctile_rank_SWD, pctile_rank_Super) >= 95), na.rm = TRUE)) == 1,
        all_pctile_top_15 = (mean((c(pctile_rank_All, pctile_rank_BHN, pctile_rank_ED, pctile_rank_EL, pctile_rank_SWD, pctile_rank_Super) >= 85), na.rm = TRUE)) == 1,
        all_pctile_top_45 = (mean((c(pctile_rank_All, pctile_rank_BHN, pctile_rank_ED, pctile_rank_EL, pctile_rank_SWD, pctile_rank_Super) >= 55), na.rm = TRUE)) == 1,
        all_met_75_pct_amos = (mean((c(pct_amos_met_All, pct_amos_met_BHN, pct_amos_met_ED, pct_amos_met_EL, pct_amos_met_SWD, pct_amos_met_Super) >= 75), na.rm = TRUE)) == 1,
        all_met_majority_amos = (mean((c(pct_amos_met_All, pct_amos_met_BHN, pct_amos_met_ED, pct_amos_met_EL, pct_amos_met_SWD, pct_amos_met_Super) > 50), na.rm = TRUE)) == 1,
        all_met_one_third_amos = (mean((c(pct_amos_met_All, pct_amos_met_BHN, pct_amos_met_ED, pct_amos_met_EL, pct_amos_met_SWD, pct_amos_met_Super) >= 33), na.rm = TRUE)) == 1) %>%
    ungroup() %>%
    mutate(priority = ifelse(is.na(priority), FALSE, priority),
        grade = ifelse(priority, "F", NA),
        grade = ifelse(is.na(grade), ifelse(tvaas2015 >= 3 & all_pctile_top_5, "A", NA), grade),
        grade = ifelse(is.na(grade), ifelse(tvaas2015 >= 3 & all_met_75_pct_amos, "A", NA), grade),
        grade = ifelse(is.na(grade), ifelse(all_pctile_top_15, "B", NA), grade),
        grade = ifelse(is.na(grade), ifelse(all_met_majority_amos, "B", NA), grade),
        grade = ifelse(is.na(grade), ifelse(all_pctile_top_45, "C", NA), grade),
        grade = ifelse(is.na(grade), ifelse(all_met_one_third_amos, "C", NA), grade),
        grade = ifelse(is.na(grade), "D", grade),
        grade = ifelse(designation_ineligible == 1, NA, grade))

# Output files
write_csv(grades, path = "data/flow_chart_grades.csv", na = "")