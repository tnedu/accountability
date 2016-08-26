## School Accountability Model 1: Rolldown of District Accountability

library(readr)
library(dplyr)

school_accountability <- read_csv("data/school_accountability_file.csv")

# Minimum Performance Goal
performance_goal <- school_accountability %>%
    mutate(eligible = (valid_tests >= 30 & valid_tests_prior >= 30),
        achievement_key = ifelse(eligible, ifelse(upper_bound_ci_PA >= pct_prof_adv_prior, "Met", "Missed"), NA),
        tvaas_key = ifelse(eligible, ifelse(!is.na(TVAAS_level), ifelse(TVAAS_level %in% c("Level 3", "Level 4", "Level 5"), "Met", "Missed"), NA), NA),
        below_bsc_reduction = ifelse(!is.na(pct_below_bsc) & !is.na(pct_below_bsc_prior), round(100 * (pct_below_bsc_prior - pct_below_bsc)/pct_below_bsc_prior, 1), NA),
        gap_closure_bb = ifelse(subgroup == "Super Subgroup", ifelse(eligible, ifelse(below_bsc_reduction >= 25, "Met", "Missed"), NA), NA),
        gap_closure_tvaas = ifelse(subgroup == "Super Subgroup", ifelse(eligible, ifelse(!is.na(TVAAS_level), ifelse(TVAAS_level %in% c("Level 3", "Level 4", "Level 5"), "Met", "Missed"), NA), NA), NA))

performance_goal_all <- performance_goal %>%
    filter(subgroup == "All Students") %>%
    select(system, system_name, school, school_name, subject, designation_ineligible, pool, achievement_key, tvaas_key)

performance_goal_super <- performance_goal %>%
    filter(subgroup == "Super Subgroup") %>%
    select(system, system_name, school, school_name, subject, designation_ineligible, pool, gap_closure_bb, gap_closure_tvaas) %>%
    mutate(gap_closure_either = ifelse((gap_closure_bb == "Met" | gap_closure_tvaas == "Met"), "Met", "Missed"),
        gap_closure_either = ifelse(gap_closure_bb == "Missed" & is.na(gap_closure_tvaas), "Missed", gap_closure_either),
        gap_closure_either = ifelse(is.na(gap_closure_bb) & gap_closure_tvaas == "Missed", "Missed", gap_closure_either))

minimum_performance_goal <- full_join(performance_goal_all, performance_goal_super,
        by = c("system", "system_name", "school", "school_name", "subject", "designation_ineligible", "pool")) %>%
    group_by(system, system_name, school, school_name, designation_ineligible, pool) %>%
    summarise(achievement_met = sum(achievement_key == "Met", na.rm = TRUE),
        achievement_eligible = sum(!is.na(achievement_key)),
        tvaas_met = sum(tvaas_key == "Met", na.rm = TRUE),
        tvaas_eligible = sum(!is.na(tvaas_key)),
        gap_met = sum(gap_closure_either == "Met", na.rm = TRUE),
        gap_eligible = sum(!is.na(gap_closure_either))) %>%
    mutate(achievement_key = ifelse(achievement_eligible != 0, ifelse(achievement_met/achievement_eligible >= 0.25, "Met", "Missed"), NA),
        tvaas_key = ifelse(tvaas_eligible != 0, ifelse(tvaas_met/tvaas_eligible >= 0.25, "Met", "Missed"), NA),
        gap_closure_key = ifelse(gap_eligible != 0, ifelse(gap_met/gap_eligible >= 0.25, "Met", "Missed"), NA)) %>%
    ungroup() %>%
    mutate(minimum_performance_goal = ifelse(achievement_key == "Missed" | tvaas_key == "Missed" | gap_closure_key == "Missed", "Missed", "Met"),
        minimum_performance_goal = ifelse(is.na(minimum_performance_goal), "Met", minimum_performance_goal),
        minimum_performance_goal = ifelse(is.na(achievement_key) & is.na(tvaas_key) & is.na(gap_closure_key), NA, minimum_performance_goal),
        minimum_performance_goal = ifelse(designation_ineligible == 1, NA, minimum_performance_goal))

rm(performance_goal, performance_goal_all, performance_goal_super)

# Achievement Heat Map
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
    mutate(average_score = ifelse(eligible == TRUE, mean(c(amo_targets_goal, relative_performance_goal, tvaas_goal), na.rm = TRUE), NA)) %>%
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
    mutate(average_score = ifelse(eligible == TRUE, mean(c(amo_targets_goal, tvaas_goal), na.rm = TRUE), NA)) %>%
    ungroup() %>%
    select(system, system_name, school, school_name, subgroup, pool, subject, eligible, amo_targets_goal, tvaas_goal, bb_reduction_goal, average_score)

# Achievement Scores
ach_scores <- ach_heat_map %>%
    group_by(system, system_name, school, school_name, pool) %>%
    summarise(achievement_score = mean(average_score, na.rm = TRUE)) %>%
    mutate(achievement_determination = ifelse(achievement_score < 2, "Progressing", NA),
        achievement_determination = ifelse(achievement_score >= 2 & achievement_score < 3, "Achieving", achievement_determination),
        achievement_determination = ifelse(achievement_score >= 3, "Exemplary", achievement_determination))

# Overall Gap Scores
gap_scores <- subgroup_heat_maps %>%
    group_by(system, system_name, school, school_name, subgroup, pool) %>%
    summarise(subgroup_average = mean(average_score, na.rm = TRUE)) %>%
    group_by(system, system_name, school, school_name, pool) %>%
    summarise(gap_score = mean(subgroup_average, na.rm = TRUE)) %>%
    mutate(gap_determination = ifelse(gap_score < 2, "Progressing", NA),
        gap_determination = ifelse(gap_score >= 2 & gap_score < 3, "Achieving", gap_determination),
        gap_determination = ifelse(gap_score >= 3, "Exemplary", gap_determination))

# Final Determinations
final_determinations <- minimum_performance_goal %>%
    select(system, system_name, school, school_name, designation_ineligible, pool, achievement_key, tvaas_key, gap_closure_key, minimum_performance_goal) %>%
    full_join(ach_scores, by = c("system", "system_name", "school", "school_name", "pool")) %>%
    full_join(gap_scores, by = c("system", "system_name", "school", "school_name", "pool")) %>%
    rowwise() %>%
    mutate(overall_average = mean(c(achievement_score, gap_score), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(final_determination = ifelse(minimum_performance_goal == "Missed", "In Need of Improvement", NA),
        final_determination = ifelse(is.na(final_determination), ifelse(overall_average < 2, "Progressing", final_determination), final_determination),
        final_determination = ifelse(is.na(final_determination), ifelse(overall_average >= 2 & overall_average < 3, "Achieving", final_determination), final_determination),
        final_determination = ifelse(is.na(final_determination), ifelse(overall_average >= 3, "Exemplary", final_determination), final_determination),
        final_determination = ifelse(designation_ineligible == 1, NA, final_determination))

write_csv(final_determinations, path = "data/district_rolldown_determinations.csv", na = "")