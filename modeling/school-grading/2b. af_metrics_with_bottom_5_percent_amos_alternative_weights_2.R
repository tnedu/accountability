## Dashboard Model: F Assigned to Bottom 5%; A-D Grades Assigned to Metrics with AMOs; Alternative Weights 2

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

# Absenteeism Grade
absenteeism14 <- readxl::read_excel("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/School Level Results 2014 inc counts.xlsx") %>%
    filter(`Grade Level` == "All") %>%
    rename(system = `District Number`, school = `School Number`, enrolled = `Number of students in grade`) %>%
    mutate(`10%-20% Absent` = as.numeric(`10%-20% Absent`),
        `+20% Absent` = as.numeric(`+20% Absent`),
        pct_chronically_absent_prior = `10%-20% Absent` + `+20% Absent`,
        AMO_target = ifelse(enrolled >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/16), NA),
        AMO_target_4 = ifelse(enrolled >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/8), NA)) %>%
    mutate(system = ifelse(system == 792 & (school %in% c(1, 6, 5, 195)), 793, system),
        system = ifelse(system == 792 & school %in% c(3, 20, 30, 90, 150, 155, 7, 33, 95, 170, 25), 794, system),
        system = ifelse(system == 792 & school %in% c(8, 55, 60, 63, 65, 168, 183, 190), 795, system),
        system = ifelse(system == 792 & school %in% c(111, 109, 100, 70, 160), 796, system),
        system = ifelse(system == 792 & school == 116, 797, system),
        system = ifelse(system == 792 & school %in% c(130, 133, 123, 78), 798, system)) %>%
    select(system, school, pct_chronically_absent_prior, AMO_target, AMO_target_4)

absenteeism <- readxl::read_excel("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/School Level Results 2015 inc counts.xlsx") %>%
    filter(`Grade Level` == "All") %>%
    rename(system = `District Number`, school = `School Number`, enrolled = `Number of students in grade`) %>%
    mutate(`10%-20% Absent` = as.numeric(`10%-20% Absent`),
        `+20% Absent` = as.numeric(`+20% Absent`),
        pct_chronically_absent = `10%-20% Absent` + `+20% Absent`) %>%
    select(system, school, enrolled, pct_chronically_absent) %>%
    left_join(absenteeism14, by = c("system", "school")) %>%
    mutate(pct_chronically_absent = pct_chronically_absent/100,
        lower_bound_ci = round(100 * (enrolled/(enrolled + qnorm(0.975)^2)) * (pct_chronically_absent + ((qnorm(0.975)^2)/(2 * enrolled)) - 
            qnorm(0.975) * sqrt((pct_chronically_absent * (1 - pct_chronically_absent))/enrolled + (qnorm(0.975)^2)/(4 * enrolled^2))), 1),
        pct_chronically_absent = 100 * pct_chronically_absent,
        grade_absenteeism_reduction = ifelse(pct_chronically_absent <= AMO_target_4, "A", NA),
        grade_absenteeism_reduction = ifelse(pct_chronically_absent < AMO_target & pct_chronically_absent > AMO_target_4, "B", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci <= AMO_target & pct_chronically_absent >= AMO_target, "C", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci < pct_chronically_absent_prior & lower_bound_ci > AMO_target, "D", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci >= pct_chronically_absent_prior, "F", grade_absenteeism_reduction),
        subgroup = "All Students") %>%
    select(system, school, subgroup, grade_absenteeism_reduction)

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
        grade_ACT_absolute = ifelse(valid_tests < 30, NA, grade_ACT_absolute),
        grade_ACT_target = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_ACT_target = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_ACT_target),
        grade_ACT_target = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_ACT_target),
        grade_ACT_target = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_ACT_target),
        grade_ACT_target = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_ACT_target),
        grade_ACT_target = ifelse(valid_tests < 30, NA, grade_ACT_target)) %>%
    select(system, school, subgroup, grade_ACT_absolute, grade_ACT_target)

# Grad Heat Map
grad <- school_accountability %>%
    filter(subject == "Graduation Rate") %>%
    mutate(grade_grad_absolute = ifelse(pct_prof_adv >= 95, "A", NA),
        grade_grad_absolute = ifelse(pct_prof_adv >= 90 & pct_prof_adv < 95, "B", grade_grad_absolute),
        grade_grad_absolute = ifelse(pct_prof_adv >= 80 & pct_prof_adv < 90, "C", grade_grad_absolute),
        grade_grad_absolute = ifelse(pct_prof_adv >= 67 & pct_prof_adv < 80, "D", grade_grad_absolute),
        grade_grad_absolute = ifelse(pct_prof_adv < 67, "F", grade_grad_absolute),
        grade_grad_absolute = ifelse(valid_tests < 30, NA, grade_grad_absolute),
        grade_grad_target = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", NA),
        grade_grad_target = ifelse(pct_prof_adv > AMO_target_PA & pct_prof_adv < AMO_target_PA_4, "B", grade_grad_target),
        grade_grad_target = ifelse(upper_bound_ci_PA >= AMO_target_PA & pct_prof_adv <= AMO_target_PA, "C", grade_grad_target),
        grade_grad_target = ifelse(upper_bound_ci_PA > pct_prof_adv_prior & upper_bound_ci_PA < AMO_target_PA, "D", grade_grad_target),
        grade_grad_target = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", grade_grad_target),
        grade_grad_target = ifelse(valid_tests < 30, NA, grade_grad_target)) %>%
    select(system, school, subgroup, grade_grad_absolute, grade_grad_target)

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
    left_join(ACT, by = c("system", "school", "subgroup")) %>%
    left_join(grad, by = c("system", "school", "subgroup")) %>%
    left_join(absenteeism, by = c("system", "school", "subgroup")) %>%
    rowwise() %>%
    mutate(grade_achievement = min(c(grade_relative_achievement, grade_achievement_amo), na.rm = TRUE),
        grade_ACT = min(c(grade_ACT_absolute, grade_ACT_target), na.rm = TRUE),
        grade_grad = min(c(grade_grad_absolute, grade_grad_target), na.rm = TRUE)) %>%
    ungroup() %>%
    select(system:designation_ineligible, grade_achievement, grade_maximizing_success, grade_tvaas, grade_BB_reduction, 
        grade_grad, grade_ACT, grade_absenteeism_reduction)

AF_grades_metrics <- full_heat_map

AF_grades_metrics[AF_grades_metrics == "A"] <- "4"
AF_grades_metrics[AF_grades_metrics == "B"] <- "3"
AF_grades_metrics[AF_grades_metrics == "C"] <- "2"
AF_grades_metrics[AF_grades_metrics == "D"] <- "1"
AF_grades_metrics[AF_grades_metrics == "F"] <- "0"

AF_grades_metrics %<>%
    mutate_each_(funs(as.numeric(.)), vars = c("grade_achievement", "grade_maximizing_success", "grade_tvaas",
        "grade_BB_reduction", "grade_grad", "grade_ACT", "grade_absenteeism_reduction")) %>%
    rowwise() %>%
    mutate(grade_postsecondary_readiness = ifelse(pool == "HS", mean(c(grade_ACT, grade_grad), na.rm = TRUE), NA)) %>%
    ungroup() %>%
    mutate(
        # All Students Weights
        weight_achievement = ifelse(!is.na(grade_achievement) & subgroup == "All Students", 0.35, NA),
        weight_growth = ifelse(!is.na(grade_tvaas) & subgroup == "All Students" & pool == "K8", 0.45, NA),
        weight_growth = ifelse(!is.na(grade_tvaas) & subgroup == "All Students" & pool == "HS", 0.35, weight_growth),
        weight_postsecondary_readiness = ifelse(!is.na(grade_postsecondary_readiness) & subgroup == "All Students" & pool == "HS", 0.2, NA),
        weight_opportunity = ifelse(!is.na(grade_absenteeism_reduction) & subgroup == "All Students" & pool == "K8", 0.2, NA),
        weight_opportunity = ifelse(!is.na(grade_absenteeism_reduction) & subgroup == "All Students" & pool == "HS", 0.1, weight_opportunity),
        # Subgroup Weights
        weight_achievement = ifelse(!is.na(grade_achievement) & subgroup != "All Students", 0.35, weight_achievement),
        weight_growth = ifelse(!is.na(grade_BB_reduction) & subgroup != "All Students" & pool == "K8", 0.35, weight_growth),
        weight_growth = ifelse(!is.na(grade_BB_reduction) & subgroup != "All Students" & pool == "HS", 0.25, weight_growth),
        weight_postsecondary_readiness = ifelse(!is.na(grade_postsecondary_readiness) & subgroup == "All Students" & pool == "HS", 0.2, grade_postsecondary_readiness),
        weight_opportunity = ifelse(!is.na(grade_absenteeism_reduction) & subgroup != "All Students" & pool == "K8", 0.2, weight_opportunity),
        weight_opportunity = ifelse(!is.na(grade_absenteeism_reduction) & subgroup != "All Students" & pool == "HS", 0.1, weight_opportunity),
        weight_ELPA = NA) %>%
    rowwise() %>%
    mutate(total_weight = ifelse(pool == "K8", sum(c(weight_achievement, weight_growth, weight_opportunity, weight_ELPA), na.rm = TRUE), NA),
        total_weight = ifelse(pool == "HS", sum(c(weight_achievement, weight_growth, weight_opportunity, weight_postsecondary_readiness, weight_ELPA), na.rm = TRUE), total_weight),
        subgroup_average = sum(c(weight_achievement * grade_achievement, 
            weight_growth * grade_tvaas,
            weight_growth * grade_BB_reduction,
            weight_opportunity * grade_absenteeism_reduction,
            weight_postsecondary_readiness * grade_postsecondary_readiness), na.rm = TRUE)/total_weight,
        # Bonus points for maximizing success
        subgroup_average = ifelse(grade_maximizing_success == 4, subgroup_average + 0.2, subgroup_average),
        subgroup_average = ifelse(grade_maximizing_success == 3, subgroup_average + 0.1, subgroup_average),
        subgroup_average = ifelse(grade_maximizing_success == 2, subgroup_average + 0.05, subgroup_average)) %>%
    ungroup()

all_students_grades_final <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    select(system, system_name, school, school_name, pool, designation_ineligible, grade_grad, subgroup_average) %>%
    rename(achievement_average = subgroup_average)

# Drop Super Subgroup observation if other subgroups are present
subgroup_grades_final <- AF_grades_metrics %>%
    filter(subgroup != "All Students") %>%
    mutate(temp = ifelse(!is.na(subgroup_average), 1, NA)) %>%
    group_by(system, system_name, school, school_name) %>%
    mutate(subgroups_count = sum(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!(subgroup == "Super Subgroup" & subgroups_count > 1)) %>%
    select(-temp, -subgroups_count) %>%
    group_by(system, system_name, school, school_name) %>%
    summarise(gap_closure_average = mean(subgroup_average, na.rm = TRUE))

AF_grades_final <- all_students_grades_final %>%
    full_join(subgroup_grades_final, by = c("system", "system_name", "school", "school_name")) %>%
    left_join(F_schools, by = c("system", "school")) %>%
    mutate(final_grade = ifelse(is.na(final_grade) & grade_grad == 0, "F", final_grade)) %>%
    rowwise() %>%
    mutate(overall_average = mean(c(achievement_average, gap_closure_average), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(final_grade = ifelse(is.na(final_grade) & overall_average > 3, "A", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 2 & overall_average <= 3, "B", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 1 & overall_average <= 2, "C", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average >= 0 & overall_average <= 1, "D", final_grade),
        final_grade = ifelse(designation_ineligible, NA, final_grade))

# Output files
write_csv(AF_grades_metrics, path = "data/AF_bottom_five_amos_metrics_alternate_weights_2.csv", na = "")
write_csv(AF_grades_final, path = "data/AF_bottom_five_amos_final_grades_alternate_weights_2.csv", na = "")
