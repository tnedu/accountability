## Dashboard Model: F Assigned to Bottom 5%; A-D Grades Assigned to Metrics with AMOs; Alternative Weights

library(tidyverse)

school_accountability <- read_csv("data/school_accountability_file.csv")

hs <- school_accountability %>%
    filter(subject == "Success Rate" & subgroup == "All Students" & pool == "HS") %>%
    nrow()
k8 <- school_accountability %>%
    filter(subject == "Success Rate" & subgroup == "All Students" & pool == "K8") %>%
    nrow()

# Absenteeism Grade
absenteeism14 <- readstata13::read.dta13("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2013-14 Chronic Absenteeism by Subgroup.dta") %>%
    filter(subgroup %in% c("All Students", "BHN", "ED", "EL/T1/T2", "SWD", "Super")) %>%
    mutate(subgroup = ifelse(subgroup == "BHN", "Black/Hispanic/Native American", subgroup),
        subgroup = ifelse(subgroup == "ED", "Economically Disadvantaged", subgroup),
        subgroup = ifelse(subgroup == "EL/T1/T2", "English Language Learners with T1/T2", subgroup),
        subgroup = ifelse(subgroup == "SWD", "Students with Disabilities", subgroup),
        subgroup = ifelse(subgroup == "Super", "Super Subgroup", subgroup),
        pct_chronically_absent_prior = round((num_chronic + num_severe)/total_students_w_abs, 1),
        AMO_target = ifelse(total_students_w_abs >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/16, 1), NA),
        AMO_target_4 = ifelse(total_students_w_abs >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/8, 1), NA),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(1, 6, 5, 195), 793, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(3, 20, 30, 90, 150, 155, 7, 33, 95, 170, 25), 794, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(8, 55, 60, 63, 65, 168, 183, 190), 795, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(111, 109, 100, 70, 160), 796, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber == 116, 797, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(130, 133, 123, 78), 798, districtnumber)) %>%
    rename(system = districtnumber, school = schoolnumber) %>%
    select(system, school, subgroup, pct_chronically_absent_prior, AMO_target, AMO_target_4)

absenteeism <- readstata13::read.dta13("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2014-15 Chronic Absenteeism by Subgroup.dta") %>%
    filter(subgroup %in% c("All Students", "BHN", "ED", "EL/T1/T2", "SWD", "Super")) %>%
    mutate(subgroup = ifelse(subgroup == "BHN", "Black/Hispanic/Native American", subgroup),
        subgroup = ifelse(subgroup == "ED", "Economically Disadvantaged", subgroup),
        subgroup = ifelse(subgroup == "EL/T1/T2", "English Language Learners with T1/T2", subgroup),
        subgroup = ifelse(subgroup == "SWD", "Students with Disabilities", subgroup),
        subgroup = ifelse(subgroup == "Super", "Super Subgroup", subgroup),
        pct_chronically_absent = pct_chronic + pct_severe) %>%
    rename(system = districtnumber, school = schoolnumber, enrolled = total_students_w_abs) %>%
    select(system, school, subgroup, enrolled, pct_chronically_absent) %>%
    left_join(absenteeism14, by = c("system", "school", "subgroup")) %>%
    mutate(grade_absenteeism_absolute = ifelse(pct_chronically_absent <= 8, "A", NA),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent > 8 & pct_chronically_absent <= 12, "B", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent > 12 & pct_chronically_absent <= 17, "C", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent > 17 & pct_chronically_absent <= 24, "D", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent > 24, "F", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(enrolled < 30, NA, grade_absenteeism_absolute),
        pct_chronically_absent = pct_chronically_absent/100,
        lower_bound_ci = round(100 * (enrolled/(enrolled + qnorm(0.975)^2)) * (pct_chronically_absent + ((qnorm(0.975)^2)/(2 * enrolled)) - 
            qnorm(0.975) * sqrt((pct_chronically_absent * (1 - pct_chronically_absent))/enrolled + (qnorm(0.975)^2)/(4 * enrolled^2))), 1),
        pct_chronically_absent = 100 * pct_chronically_absent,
        grade_absenteeism_reduction = ifelse(pct_chronically_absent <= AMO_target_4, "A", NA),
        grade_absenteeism_reduction = ifelse(pct_chronically_absent < AMO_target & pct_chronically_absent > AMO_target_4, "B", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci <= AMO_target & pct_chronically_absent >= AMO_target, "C", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci < pct_chronically_absent_prior & lower_bound_ci > AMO_target, "D", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci >= pct_chronically_absent_prior, "F", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(enrolled < 30, NA, grade_absenteeism_reduction)) %>%
    select(system, school, subgroup, grade_absenteeism_absolute, grade_absenteeism_reduction)

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

# ACT and Grad
act <- school_accountability %>%
    filter(subject == "ACT Composite") %>%
    select(system, school, subgroup, valid_tests, pct_prof_adv, valid_tests_prior, pct_prof_adv_prior) %>%
    rename(valid_tests_ACT = valid_tests, pct_21_ACT = pct_prof_adv, valid_tests_prior_ACT = valid_tests_prior,
        pct_21_prior_ACT = pct_prof_adv_prior)

act_grad <- school_accountability %>%
    filter(subject == "Graduation Rate") %>%
    select(system, school, subgroup, valid_tests, pct_prof_adv, valid_tests_prior, pct_prof_adv_prior) %>%
    rename(grad_cohort = valid_tests, grad_rate = pct_prof_adv, grad_cohort_prior = valid_tests_prior,
        grad_rate_prior = pct_prof_adv_prior) %>%
    full_join(act, by = c("system", "school", "subgroup")) %>%
    mutate(act_grad_prior = round(grad_rate_prior * pct_21_prior_ACT/100, 1),
        AMO_target = ifelse(valid_tests_prior_ACT >= 30 & grad_cohort_prior >= 30, round(act_grad_prior + (100 - act_grad_prior)/16, 1), NA),
        AMO_target_4 = ifelse(valid_tests_prior_ACT >= 30 & grad_cohort_prior >= 30, round(act_grad_prior + (100 - act_grad_prior)/8, 1), NA),
        act_grad = round(grad_rate * pct_21_ACT/100, 1),
        grade_readiness_absolute = ifelse(act_grad >= 50, "A", NA),
        grade_readiness_absolute = ifelse(act_grad > 35 & act_grad < 50, "B", grade_readiness_absolute),
        grade_readiness_absolute = ifelse(act_grad > 28 & act_grad <= 35, "C", grade_readiness_absolute),
        grade_readiness_absolute = ifelse(act_grad > 16 & act_grad <= 28, "D", grade_readiness_absolute),
        grade_readiness_absolute = ifelse(act_grad <= 16, "F", grade_readiness_absolute),
        grade_readiness_target = ifelse(act_grad < act_grad_prior, "F", NA),
        grade_readiness_target = ifelse(act_grad == act_grad_prior, "D", grade_readiness_target),
        grade_readiness_target = ifelse(act_grad > act_grad_prior & act_grad < AMO_target, "C", grade_readiness_target),
        grade_readiness_target = ifelse(act_grad >= AMO_target & act_grad < AMO_target_4, "B", grade_readiness_target),
        grade_readiness_target = ifelse(act_grad >= AMO_target_4, "A", grade_readiness_target))

# All Students Heat Map
all_students <- school_accountability %>%
    filter(subgroup == "All Students" & subject == "Success Rate") %>%
    mutate(grade_relative_achievement = ifelse(pctile_rank_PA >= 80, "A", NA),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 60 & pctile_rank_PA < 80, "B", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 40 & pctile_rank_PA < 60, "C", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 20 & pctile_rank_PA < 40, "D", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA < 20, "F", grade_relative_achievement),
        grade_achievement_amo = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_achievement_amo = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_achievement_amo),
        grade_achievement_amo = ifelse(valid_tests < 30, NA, grade_achievement_amo),
        grade_maximizing_success = ifelse(pct_adv >= AMO_target_adv_4, "A", NA),
        grade_maximizing_success = ifelse(pct_adv > AMO_target_adv & pct_adv < AMO_target_adv_4, "B", grade_maximizing_success),
        grade_maximizing_success = ifelse(upper_bound_ci_adv >= AMO_target_adv & pct_adv <= AMO_target_adv, "C", grade_maximizing_success),
        grade_maximizing_success = ifelse(upper_bound_ci_adv > pct_adv_prior & upper_bound_ci_adv < AMO_target_adv, "D", grade_maximizing_success),
        grade_maximizing_success = ifelse(upper_bound_ci_adv <= pct_adv_prior, "F", grade_maximizing_success),
        grade_maximizing_success = ifelse(valid_tests < 30, NA, grade_maximizing_success),
        grade_tvaas = ifelse(TVAAS_level == "Level 5", "A", NA),
        grade_tvaas = ifelse(TVAAS_level == "Level 4", "B", grade_tvaas),
        grade_tvaas = ifelse(TVAAS_level == "Level 3", "C", grade_tvaas),
        grade_tvaas = ifelse(TVAAS_level == "Level 2", "D", grade_tvaas),
        grade_tvaas = ifelse(TVAAS_level == "Level 1", "F", grade_tvaas)) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible, 
        grade_relative_achievement, grade_maximizing_success, grade_achievement_amo, grade_tvaas)

# Subgroup Heat Map
subgroups <- school_accountability %>%
    filter(subgroup != "All Students" & subject == "Success Rate") %>%
    mutate(grade_relative_achievement = ifelse(pctile_rank_PA >= 80, "A", NA),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 60 & pctile_rank_PA < 80, "B", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 40 & pctile_rank_PA < 60, "C", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 20 & pctile_rank_PA < 40, "D", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA < 20, "F", grade_relative_achievement),
        grade_achievement_amo = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_achievement_amo = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_achievement_amo),
        grade_achievement_amo = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_achievement_amo),
        grade_achievement_amo = ifelse(valid_tests < 30, NA, grade_achievement_amo),
        grade_maximizing_success = ifelse(pct_adv >= AMO_target_adv_4, "A", NA),
        grade_maximizing_success = ifelse(pct_adv > AMO_target_adv & pct_adv < AMO_target_adv_4, "B", grade_maximizing_success),
        grade_maximizing_success = ifelse(upper_bound_ci_adv >= AMO_target_adv & pct_adv <= AMO_target_adv, "C", grade_maximizing_success),
        grade_maximizing_success = ifelse(upper_bound_ci_adv > pct_adv_prior & upper_bound_ci_adv < AMO_target_adv, "D", grade_maximizing_success),
        grade_maximizing_success = ifelse(upper_bound_ci_adv <= pct_adv_prior, "F", grade_maximizing_success),
        grade_maximizing_success = ifelse(valid_tests < 30, NA, grade_maximizing_success),
        grade_BB_reduction = ifelse(pct_below_bsc <= AMO_target_BB_4, "A", NA),
        grade_BB_reduction = ifelse(pct_below_bsc < AMO_target_BB & pct_below_bsc > AMO_target_BB_4, "B", grade_BB_reduction),
        grade_BB_reduction = ifelse(lower_bound_ci_BB <= AMO_target_BB & pct_below_bsc >= AMO_target_BB, "C", grade_BB_reduction),
        grade_BB_reduction = ifelse(lower_bound_ci_BB < pct_below_bsc_prior & lower_bound_ci_BB > AMO_target_BB, "D", grade_BB_reduction),
        grade_BB_reduction = ifelse(lower_bound_ci_BB >= pct_below_bsc_prior, "F", grade_BB_reduction),
        grade_BB_reduction = ifelse(valid_tests < 30, NA, grade_BB_reduction)) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
        grade_relative_achievement, grade_maximizing_success, grade_achievement_amo, grade_BB_reduction)

# Full Heat Map
full_heat_map <- all_students %>%
    bind_rows(subgroups) %>%
    left_join(act_grad, by = c("system", "school", "subgroup")) %>%
    left_join(absenteeism, by = c("system", "school", "subgroup")) %>%
    rowwise() %>%
    mutate(grade_achievement = min(c(grade_relative_achievement, grade_achievement_amo), na.rm = TRUE),
        grade_readiness = min(c(grade_readiness_absolute, grade_readiness_target), na.rm = TRUE),
        grade_absenteeism = min(c(grade_absenteeism_absolute, grade_absenteeism_reduction), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(priority_grad = ifelse(subgroup == "All Students", designation_ineligible == 0 & grad_cohort >= 30 & grad_rate < 67, NA)) %>%
    select(system:designation_ineligible, priority_grad, grade_achievement, grade_maximizing_success, grade_tvaas, grade_BB_reduction, 
        grade_readiness, grade_absenteeism)

AF_grades_metrics <- full_heat_map

AF_grades_metrics[AF_grades_metrics == "A"] <- "4"
AF_grades_metrics[AF_grades_metrics == "B"] <- "3"
AF_grades_metrics[AF_grades_metrics == "C"] <- "2"
AF_grades_metrics[AF_grades_metrics == "D"] <- "1"
AF_grades_metrics[AF_grades_metrics == "F"] <- "0"

AF_grades_metrics %<>%
    mutate_each_(funs(as.numeric(.)), vars = c("grade_achievement", "grade_maximizing_success", "grade_tvaas",
        "grade_BB_reduction", "grade_readiness", "grade_absenteeism")) %>%
    mutate(
        # All Students Weights
        weight_achievement = ifelse(!is.na(grade_achievement) & subgroup == "All Students", 0.5, NA),
        weight_growth = ifelse(!is.na(grade_tvaas) & subgroup == "All Students", 0.2, NA),
        weight_maximizing_success = ifelse(!is.na(grade_maximizing_success) & subgroup == "All Students", 0.05, NA),
        weight_readiness = ifelse(!is.na(grade_readiness) & subgroup == "All Students" & pool == "HS", 0.15, NA),
        weight_opportunity = ifelse(!is.na(grade_absenteeism) & subgroup == "All Students" & pool == "K8", 0.25, NA),
        weight_opportunity = ifelse(!is.na(grade_absenteeism) & subgroup == "All Students" & pool == "HS", 0.1, weight_opportunity),
        # Subgroup Weights
        weight_achievement = ifelse(!is.na(grade_achievement) & subgroup != "All Students", 0.4, weight_achievement),
        weight_growth = ifelse(!is.na(grade_BB_reduction) & subgroup != "All Students", 0.2, weight_growth),
        weight_maximizing_success = ifelse(!is.na(grade_maximizing_success) & subgroup != "All Students", 0.05, weight_maximizing_success),
        weight_readiness = ifelse(!is.na(grade_readiness) & subgroup == "All Students" & pool == "HS", 0.15, grade_readiness),
        weight_opportunity = ifelse(!is.na(grade_absenteeism) & subgroup != "All Students" & pool == "K8", 0.25, weight_opportunity),
        weight_opportunity = ifelse(!is.na(grade_absenteeism) & subgroup != "All Students" & pool == "HS", 0.1, weight_opportunity),
        weight_ELPA = NA) %>%
    rowwise() %>%
    mutate(total_weight = sum(c(weight_achievement, weight_growth, weight_opportunity, weight_maximizing_success, weight_readiness, weight_ELPA), na.rm = TRUE),
        subgroup_average = sum(c(weight_achievement * grade_achievement, 
            weight_growth * grade_tvaas,
            weight_growth * grade_BB_reduction,
            weight_opportunity * grade_absenteeism, 
            weight_maximizing_success * grade_maximizing_success,
            weight_readiness * grade_readiness), na.rm = TRUE)/total_weight) %>%
    ungroup()

all_students_grades_final <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    select(system, system_name, school, school_name, pool, designation_ineligible, priority_grad, subgroup_average) %>%
    rename(achievement_average = subgroup_average) %>%
    mutate(achievement_grade = ifelse(achievement_average == 0, "F", NA),
        achievement_grade = ifelse(achievement_average > 0 & achievement_average <= 1, "D", achievement_grade),
        achievement_grade = ifelse(achievement_average > 1 & achievement_average <= 2, "C", achievement_grade),
        achievement_grade = ifelse(achievement_average > 2 & achievement_average <= 3, "B", achievement_grade),
        achievement_grade = ifelse(achievement_average > 3, "A", achievement_grade))

# Drop Super Subgroup observation if other subgroups are present
subgroup_grades_final <- AF_grades_metrics %>%
    filter(subgroup != "All Students") %>%
    mutate(temp = ifelse(!is.na(subgroup_average), 1, NA)) %>%
    group_by(system, system_name, school, school_name) %>%
    mutate(subgroups_count = sum(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!(subgroup == "Super Subgroup" & subgroups_count > 1)) %>%
    group_by(system, system_name, school, school_name) %>%
    summarise(gap_closure_average = mean(subgroup_average, na.rm = TRUE)) %>%
    mutate(gap_closure_grade = ifelse(gap_closure_average == 0, "F", NA),
        gap_closure_grade = ifelse(gap_closure_average > 0 & gap_closure_average <= 1, "D", gap_closure_grade),
        gap_closure_grade = ifelse(gap_closure_average > 1 & gap_closure_average <= 2, "C", gap_closure_grade),
        gap_closure_grade = ifelse(gap_closure_average > 2 & gap_closure_average <= 3, "B", gap_closure_grade),
        gap_closure_grade = ifelse(gap_closure_average > 3, "A", gap_closure_grade))

AF_grades_final <- all_students_grades_final %>%
    full_join(subgroup_grades_final, by = c("system", "system_name", "school", "school_name")) %>%
    left_join(F_schools, by = c("system", "school")) %>%
    mutate(final_grade = ifelse(is.na(final_grade) & priority_grad == TRUE, "F", final_grade)) %>%
    rowwise() %>%
    mutate(overall_average = mean(c(achievement_average, gap_closure_average), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(final_grade = ifelse(is.na(final_grade) & overall_average > 3, "A", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 2 & overall_average <= 3, "B", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 1 & overall_average <= 2, "C", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average <= 1, "D", final_grade),
        final_grade = ifelse(designation_ineligible, NA, final_grade)) %>%
    select(system:gap_closure_grade, overall_average, final_grade)

# Output files
write_csv(AF_grades_metrics, path = "data/AF_bottom_five_amos_metrics_alternate_weights.csv", na = "")
write_csv(AF_grades_final, path = "data/AF_bottom_five_amos_final_grades_alternate_weights.csv", na = "")
