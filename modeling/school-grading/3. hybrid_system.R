## School Accountability Model 3: Hybrid of current district/school accountability

library(readr)
library(dplyr)

# Read in grade pools
grade_pools <- readstata13::read.dta13("K:/ORP_accountability/projects/2016_pre_coding/Output/grade_pools_designation_immune_2016.dta") %>%
    select(system, school, designation_ineligible, pool)

hs <- ceiling(0.05 * sum(grade_pools$pool == "HS"))
k8 <- ceiling(0.05 * sum(grade_pools$pool == "K8"))

# School base + merge on grade pools
school_base <- read_csv("K:/ORP_accountability/projects/2016_pre_coding/Output/school_base_with_super_subgroup_2016.csv") %>%
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
    filter(!(pool == "K8" & subject %in% c("Algebra I", "Algebra II", "Biology I", "Chemistry", "English I", "English II", "English III", "Graduation Rate")))

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
priority_schools <- school_base %>%
    filter(subgroup == "All Students") %>%
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
    group_by(pool, designation_ineligible, priority_sh) %>%
    mutate(rank = rank(success_rate_3yr, na.last = "keep", ties.method = "min")) %>%
    ungroup() %>%
    arrange(pool, designation_ineligible, priority_sh, success_rate_3yr, rank) %>%
    mutate(priority = ifelse(rank <= hs & pool == "HS" & designation_ineligible == 0 & priority_sh == FALSE, TRUE, FALSE)) %>%
    arrange(desc(pool), designation_ineligible, priority_sh, success_rate_3yr, rank) %>%
    mutate(priority = ifelse(rank <= k8 & pool == "K8" & designation_ineligible == 0 & priority_sh == FALSE, TRUE, priority)) %>%
    filter(priority == TRUE) %>%
    select(system, school, priority)

rm(k8, hs, tvaas_school2014, tvaas_school2015)

# 2015 Reward schools
reward_schools <- readstata13::read.dta13("K:/ORP_accountability/projects/2015_school_coding/Output/reward_2015_ap.dta") %>%
    filter(reward == 1) %>%
    select(system, school, reward) %>%
    mutate(reward = as.logical(reward))

# Achievement heat map

school_accountability <- read_csv("data/school_accountability_file.csv")

ach_heat_map <- school_accountability %>%
    filter(subgroup == "All Students") %>%
    mutate(eligible = (valid_tests >= 30 & valid_tests_prior >= 30),
        amo_targets_goal = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, 0, NA),
        amo_targets_goal = ifelse(upper_bound_ci_PA >= pct_prof_adv_prior & upper_bound_ci_PA <= AMO_target_PA, 1, amo_targets_goal),
        amo_targets_goal = ifelse(upper_bound_ci_PA >= AMO_target_PA, 2, amo_targets_goal),
        amo_targets_goal = ifelse(pct_prof_adv >= AMO_target_PA & pct_prof_adv <= AMO_target_PA_4, 3, amo_targets_goal),
        amo_targets_goal = ifelse(pct_prof_adv >= AMO_target_PA_4, 4, amo_targets_goal),
        relative_performance_goal = ifelse(percentile_rank < (percentile_rank_prior - 10), 0, NA),
        relative_performance_goal = ifelse(percentile_rank >= (percentile_rank_prior - 10) & percentile_rank < (percentile_rank_prior - 2), 1, relative_performance_goal),
        relative_performance_goal = ifelse(percentile_rank >= (percentile_rank_prior - 2) & percentile_rank <= percentile_rank_prior, 2, relative_performance_goal),
        relative_performance_goal = ifelse(percentile_rank > percentile_rank_prior & percentile_rank < (percentile_rank_prior + 10), 3, relative_performance_goal),
        relative_performance_goal = ifelse(percentile_rank >= (percentile_rank_prior + 10), 4, relative_performance_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 1", 0, NA),
        tvaas_goal = ifelse(TVAAS_level == "Level 2", 1, tvaas_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 3", 2, tvaas_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 4", 3, tvaas_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 5", 4, tvaas_goal)) %>%
    rowwise() %>%
    mutate(average_score = ifelse(eligible, mean(c(amo_targets_goal, relative_performance_goal, tvaas_goal), na.rm = TRUE), NA)) %>%
    ungroup() %>%
    select(system, system_name, school, school_name, subject, pool, eligible, amo_targets_goal, relative_performance_goal, tvaas_goal, average_score)

# Subgroup Heat Maps
subgroup_heat_maps <- school_accountability %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged", "Students with Disabilities", "English Language Learners with T1/T2")) %>%
    mutate(eligible = (valid_tests >= 30 & valid_tests_prior >= 30),
        amo_targets_goal = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, 0, NA),
        amo_targets_goal = ifelse(upper_bound_ci_PA >= pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, 1, amo_targets_goal),
        amo_targets_goal = ifelse(upper_bound_ci_PA >= AMO_target_PA, 2, amo_targets_goal),
        amo_targets_goal = ifelse(pct_prof_adv >= AMO_target_PA & pct_prof_adv < AMO_target_PA_4, 3, amo_targets_goal),
        amo_targets_goal = ifelse(pct_prof_adv >= AMO_target_PA_4, 4, amo_targets_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 1", 0, NA),
        tvaas_goal = ifelse(TVAAS_level == "Level 2", 1, tvaas_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 3", 2, tvaas_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 4", 3, tvaas_goal),
        tvaas_goal = ifelse(TVAAS_level == "Level 5", 4, tvaas_goal),
        bb_reduction_goal = ifelse(lower_bound_ci_BB >= pct_below_bsc_prior, 0, NA),
        bb_reduction_goal = ifelse(lower_bound_ci_BB < pct_below_bsc_prior & lower_bound_ci_BB > AMO_target_BB, 1, bb_reduction_goal),
        bb_reduction_goal = ifelse(lower_bound_ci_BB <= AMO_target_BB, 2, bb_reduction_goal),
        bb_reduction_goal = ifelse(pct_below_bsc <= AMO_target_BB & pct_below_bsc > AMO_target_BB_4, 3, bb_reduction_goal),
        bb_reduction_goal = ifelse(pct_below_bsc <= AMO_target_BB_4, 4, bb_reduction_goal)) %>%
    rowwise() %>%
    mutate(average_score = ifelse(eligible, mean(c(amo_targets_goal, tvaas_goal), na.rm = TRUE), NA)) %>%
    ungroup() %>%
    select(system, system_name, school, school_name, subgroup, pool, subject, eligible, amo_targets_goal, tvaas_goal, bb_reduction_goal, average_score)

# Achievement Scores
ach_scores <- ach_heat_map %>%
    group_by(system, system_name, school, school_name, pool) %>%
    summarise(achievement_score = mean(average_score, na.rm = TRUE)) %>%
    mutate(achievement_determination = ifelse(achievement_score < 2, "D", NA),
        achievement_determination = ifelse(achievement_score >= 2 & achievement_score < 3, "C", achievement_determination),
        achievement_determination = ifelse(achievement_score >= 3, "B", achievement_determination))

# Overall Gap Scores
gap_scores <- subgroup_heat_maps %>%
    group_by(system, system_name, school, school_name, subgroup, pool) %>%
    summarise(subgroup_average = mean(average_score, na.rm = TRUE)) %>%
    group_by(system, system_name, school, school_name, pool) %>%
    summarise(gap_score = mean(subgroup_average, na.rm = TRUE)) %>%
    mutate(gap_determination = ifelse(gap_score < 2, "D", NA),
        gap_determination = ifelse(gap_score >= 2 & gap_score < 3, "C", gap_determination),
        gap_determination = ifelse(gap_score >= 3, "B", gap_determination))

# Final Determinations
final_determinations <- full_join(ach_scores, gap_scores, by = c("system", "system_name", "school", "school_name", "pool")) %>%
    left_join(grade_pools, by = c("system", "school", "pool")) %>%
    left_join(priority_schools, by = c("system", "school")) %>%
    left_join(reward_schools, by = c("system", "school")) %>%
    rowwise() %>%
    mutate(overall_average = mean(c(achievement_score, gap_score), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(reward = ifelse(is.na(reward), FALSE, reward),
        priority = ifelse(is.na(priority), FALSE, priority),
        grade = ifelse(overall_average < 2, "D", NA),
        grade = ifelse(is.na(grade), ifelse(overall_average >= 2 & overall_average < 3, "C", NA), grade),
        grade = ifelse(is.na(grade), ifelse(overall_average >= 3, "B", NA), grade),
        grade = ifelse(reward, "A", grade),
        grade = ifelse(priority, "F", grade),
        grade = ifelse(designation_ineligible, NA, grade))

# Output file
write_csv(final_determinations, path = "data/hybrid_system_grades.csv", na = "")
