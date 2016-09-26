## School Accountability Model 2b: F Assigned to Bottom 5%; A-D Grades Assigned to Metrics with AMOs

library(readr)
library(tidyr)
library(dplyr)

school_accountability <- read_csv("data/school_accountability_file.csv")

hs <- school_accountability %>%
    filter(subject == "Success Rate" & subgroup == "All Students" & pool == "HS") %>%
    nrow()
k8 <- school_accountability %>%
    filter(subject == "Success Rate" & subgroup == "All Students" & pool == "K8") %>%
    nrow()

# F schools
F_schools <- school_accountability %>%
    filter(subject == "Success Rate") %>%
    filter(subgroup == "All Students") %>%
    mutate(tvaas_sh = TVAAS_level %in% c("Level 4", "Level 5") & TVAAS_level_lag %in% c("Level 4", "Level 5")) %>%
    group_by(pool, designation_ineligible, tvaas_sh) %>%
    mutate(rank = rank(pct_prof_adv, na.last = "keep", ties.method = "min")) %>%
    ungroup() %>%
    arrange(pool, designation_ineligible, tvaas_sh, pct_prof_adv, rank) %>%
    mutate(final_grade = ifelse(rank <= ceiling(0.05 * hs) & pool == "HS" & designation_ineligible == 0 & tvaas_sh == FALSE, "F", NA)) %>%
    arrange(desc(pool), designation_ineligible, tvaas_sh, pct_prof_adv, rank) %>%
    mutate(final_grade = ifelse(rank <= ceiling(0.05 * k8) & pool == "K8" & designation_ineligible == 0 & tvaas_sh == FALSE, "F", final_grade)) %>%
    select(system, school, final_grade)

# ACT Heat Map
ACT <- school_accountability %>%
    filter(subject == "ACT Composite") %>%
    mutate(grade_ACT_absolute = ifelse(pct_prof_adv >= 50, "A", NA),
        grade_ACT_absolute = ifelse(pct_prof_adv >= 40 & pct_prof_adv < 50, "B", grade_ACT_absolute),
        grade_ACT_absolute = ifelse(pct_prof_adv >= 30 & pct_prof_adv < 40, "C", grade_ACT_absolute),
        grade_ACT_absolute = ifelse(pct_prof_adv >= 15 & pct_prof_adv < 30, "D", grade_ACT_absolute),
        grade_ACT_absolute = ifelse(pct_prof_adv < 15, "F", grade_ACT_absolute),
        grade_ACT_target = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_ACT_target = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_ACT_target),
        grade_ACT_target = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_ACT_target),
        grade_ACT_target = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_ACT_target),
        grade_ACT_target = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_ACT_target)) %>%
    select(system, school, subgroup, grade_ACT_absolute, grade_ACT_target)

# Grad Heat Map
grad <- school_accountability %>%
    filter(subject == "Graduation Rate") %>%
    mutate(grade_grad_absolute = ifelse(pct_prof_adv >= 95, "A", NA),
        grade_grad_absolute = ifelse(pct_prof_adv >= 90 & pct_prof_adv < 95, "B", grade_grad_absolute),
        grade_grad_absolute = ifelse(pct_prof_adv >= 80 & pct_prof_adv < 90, "C", grade_grad_absolute),
        grade_grad_absolute = ifelse(pct_prof_adv >= 67 & pct_prof_adv < 80, "D", grade_grad_absolute),
        grade_grad_absolute = ifelse(pct_prof_adv < 67, "F", grade_grad_absolute),
        grade_grad_target = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_grad_target = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_grad_target),
        grade_grad_target = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_grad_target),
        grade_grad_target = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_grad_target),
        grade_grad_target = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_grad_target)) %>%
    select(system, school, subgroup, grade_grad_absolute, grade_grad_target)

# All Students Heat Map
all_students <- school_accountability %>%
    filter(subgroup == "All Students" & subject == "Success Rate") %>%
    mutate(grade_relative_achievement = ifelse(pctile_rank_PA >= 80, "A", NA),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 60 & pctile_rank_PA < 80, "B", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 40 & pctile_rank_PA < 60, "C", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 20 & pctile_rank_PA < 40, "D", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA < 20, "F", grade_relative_achievement),
        grade_continuous_improvement = ifelse(pct_adv >= AMO_target_adv_4, "A", NA),
        grade_continuous_improvement = ifelse(pct_adv > AMO_target_adv & pct_adv < AMO_target_adv_4, "B", grade_continuous_improvement),
        grade_continuous_improvement = ifelse(upper_bound_ci_adv >= AMO_target_adv & pct_adv <= AMO_target_adv, "C", grade_continuous_improvement),
        grade_continuous_improvement = ifelse(upper_bound_ci_adv > pct_prof_adv_prior & upper_bound_ci_adv < AMO_target_PA, "D", grade_continuous_improvement),
        grade_continuous_improvement = ifelse(upper_bound_ci_adv <= pct_adv_prior, "F", grade_continuous_improvement),
        grade_achievement_amo = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_achievement_amo = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_achievement_amo),
        grade_tvaas = ifelse(TVAAS_level == "Level 5", "A", NA),
        grade_tvaas = ifelse(TVAAS_level == "Level 4", "B", grade_tvaas),
        grade_tvaas = ifelse(TVAAS_level == "Level 3", "C", grade_tvaas),
        grade_tvaas = ifelse(TVAAS_level == "Level 2", "D", grade_tvaas),
        grade_tvaas = ifelse(TVAAS_level == "Level 1", "F", grade_tvaas)) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible, 
        grade_relative_achievement, grade_continuous_improvement, grade_achievement_amo, grade_tvaas)

# Subgroup Heat Map
subgroups <- school_accountability %>%
    filter(subgroup != "All Students" & subject == "Success Rate") %>%
    mutate(grade_relative_achievement = ifelse(pctile_rank_PA >= 80, "A", NA),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 60 & pctile_rank_PA < 80, "B", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 40 & pctile_rank_PA < 60, "C", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 20 & pctile_rank_PA < 40, "D", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA < 20, "F", grade_relative_achievement),
        grade_continuous_improvement = ifelse(pct_adv >= AMO_target_adv_4, "A", NA),
        grade_continuous_improvement = ifelse(pct_adv > AMO_target_adv & pct_adv < AMO_target_adv_4, "B", grade_continuous_improvement),
        grade_continuous_improvement = ifelse(upper_bound_ci_adv >= AMO_target_adv & pct_adv <= AMO_target_adv, "C", grade_continuous_improvement),
        grade_continuous_improvement = ifelse(upper_bound_ci_adv > pct_prof_adv_prior & upper_bound_ci_adv < AMO_target_PA, "D", grade_continuous_improvement),
        grade_continuous_improvement = ifelse(upper_bound_ci_adv <= pct_adv_prior, "F", grade_continuous_improvement),
        grade_achievement_amo = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_achievement_amo = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_achievement_amo),  
        grade_BB_reduction = ifelse(pct_below_bsc <= AMO_target_BB_4, "A", NA),
        grade_BB_reduction = ifelse(pct_below_bsc < AMO_target_BB & pct_below_bsc > AMO_target_BB_4, "B", grade_BB_reduction),
        grade_BB_reduction = ifelse(lower_bound_ci_BB <= AMO_target_BB & pct_below_bsc >= AMO_target_BB, "C", grade_BB_reduction),
        grade_BB_reduction = ifelse(lower_bound_ci_BB < pct_below_bsc_prior & lower_bound_ci_BB > AMO_target_BB, "D", grade_BB_reduction),
        grade_BB_reduction = ifelse(lower_bound_ci_BB >= pct_below_bsc_prior, "F", grade_BB_reduction)) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
        grade_relative_achievement, grade_continuous_improvement, grade_achievement_amo, grade_BB_reduction)

# Full Heat Map
full_heat_map <- all_students %>%
    bind_rows(subgroups) %>%
    left_join(ACT, by = c("system", "school", "subgroup")) %>%
    left_join(grad, by = c("system", "school", "subgroup")) %>%
    rowwise() %>%
    mutate(grade_achievement = min(c(grade_relative_achievement, grade_achievement_amo), na.rm = TRUE),
        grade_ACT = min(c(grade_ACT_absolute, grade_ACT_target), na.rm = TRUE),
        grade_grad = min(c(grade_grad_absolute, grade_grad_target), na.rm = TRUE)) %>%
    ungroup() %>%
    select(system:designation_ineligible, grade_achievement, grade_continuous_improvement, grade_tvaas, grade_BB_reduction, grade_ACT, grade_grad)
