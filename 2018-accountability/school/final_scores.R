library(acct)
library(tidyverse)

pools <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

bottom_five <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/bottom_five.csv") %>%
    filter(bottom_five == 1) %>%
    transmute(system, school, final_grade = "F")

school_accountability <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv",
        col_types = "icicciccddddddiii")

ach_scores <- school_accountability %>%
    filter(indicator == "Achievement") %>%
    select(system, school, subgroup, score_achievement_abs = score_abs, score_achievement_target = score_target, score_achievement = score)

growth_scores <- school_accountability %>%
    filter(indicator == "Growth") %>%
    select(system, school, subgroup, score_growth = score)

grad_scores <- school_accountability %>%
    filter(indicator == "Graduation Rate") %>%
    select(system, school, subgroup, score_grad_abs = score_abs, score_grad_target = score_target, score_grad = score)

ready_grad_scores <- school_accountability %>%
    filter(indicator == "Ready Graduates") %>%
    select(system, school, subgroup, score_ready_grad_abs = score_abs, score_ready_grad_target = score_target, score_ready_grad = score)

absenteeism_grades <- school_accountability %>%
    filter(indicator == "Chronic Absenteeism") %>%
    select(system, school, subgroup, score_absenteeism_abs = score_abs, score_absenteeism_reduction = score_target, score_absenteeism = score)

elpa_scores <- school_accountability %>%
    filter(indicator == "ELPA Growth Standard") %>%
    select(system, school, subgroup, score_elpa = score_abs)

AF_grades_metrics <- pools %>%
    inner_join(ach_scores, by = c("system", "school")) %>%
    left_join(growth_scores, by = c("system", "school", "subgroup")) %>%
    left_join(grad_scores, by = c("system", "school", "subgroup")) %>%
    left_join(ready_grad_scores, by = c("system", "school", "subgroup")) %>%
    left_join(absenteeism_grades, by = c("system", "school", "subgroup")) %>%
    left_join(elpa_scores, by = c("system", "school", "subgroup")) %>%
# Weights
    mutate(
        weight_achievement = if_else(!is.na(score_achievement) & pool == "K8", 0.45, NA_real_),
        weight_achievement = if_else(!is.na(score_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = if_else(!is.na(score_growth) & pool == "K8", 0.35, NA_real_),
        weight_growth = if_else(!is.na(score_growth) & pool == "HS", 0.25, weight_growth),
        weight_grad = if_else(!is.na(score_grad) & pool == "HS", 0.05, NA_real_),
        weight_ready_grad = if_else(!is.na(score_ready_grad) & pool == "HS", 0.2, NA_real_),
        weight_opportunity = if_else(!is.na(score_absenteeism), 0.1, NA_real_),
        weight_elpa = if_else(!is.na(score_elpa), 0.1, NA_real_),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = if_else(is.na(score_elpa) & !is.na(score_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = if_else(is.na(score_elpa) & !is.na(score_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = if_else(is.na(score_elpa) & !is.na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = if_else(is.na(score_elpa) & !is.na(weight_growth) & pool == "HS", 0.3, weight_growth)
    ) %>%
    rowwise() %>%
    mutate(total_weight = sum(weight_achievement, weight_growth, weight_opportunity,
            weight_grad, weight_ready_grad, weight_elpa, na.rm = TRUE),
        subgroup_average = round5(sum(weight_achievement * score_achievement,
            weight_growth * score_growth,
            weight_opportunity * score_absenteeism,
            weight_grad * score_grad,
            weight_ready_grad * score_ready_grad,
            weight_elpa * score_elpa, na.rm = TRUE)/total_weight, 1)) %>%
    ungroup()

# Targeted support schools
targeted_support <- AF_grades_metrics %>%
    mutate(subgroup_average = if_else(pool == "K8" & total_weight < 0.6, NA_real_, subgroup_average),
        subgroup_average = if_else(pool == "HS" & total_weight < 0.7, NA_real_, subgroup_average)) %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged", "English Learners with Transitional 1-4",
        "Students with Disabilities", "American Indian or Alaska Native", "Asian", "Black or African American",
        "Hispanic", "Native Hawaiian or Other Pacific Islander", "White")) %>%
    select(system, school, subgroup, designation_ineligible, subgroup_average) %>%
    full_join(bottom_five, by = c("system", "school")) %>%
    group_by(subgroup) %>%
    mutate(denom = sum(!is.na(subgroup_average))) %>%
    group_by(subgroup, designation_ineligible, final_grade) %>%
    mutate(rank = rank(subgroup_average, ties.method = "min"),
        targeted_support = if_else(is.na(final_grade) & !designation_ineligible, as.integer(rank <= ceiling(0.05 * denom)), NA_integer_)) %>%
    ungroup() %>%
    select(system, school, subgroup, final_grade, targeted_support) %>%
    spread(subgroup, targeted_support) %>%
    transmute(system, school,
        targeted_support_BHN = `Black/Hispanic/Native American`,
        targeted_support_ED = `Economically Disadvantaged`,
        targeted_support_SWD = `Students with Disabilities`,
        targeted_support_EL = `English Learners with Transitional 1-4`,
        targeted_support_Native = `American Indian or Alaska Native`,
        targeted_support_Asian = Asian,
        targeted_support_Black = `Black or African American`,
        targeted_support_Hispanic = Hispanic,
        targeted_support_HPI = `Native Hawaiian or Other Pacific Islander`,
        targeted_support_White = White,
        targeted_support = if_else(is.na(final_grade),
            pmax(targeted_support_BHN, targeted_support_ED, targeted_support_SWD, targeted_support_EL,
                targeted_support_Black, targeted_support_Hispanic, targeted_support_Native,
                targeted_support_HPI, targeted_support_Asian, targeted_support_White, na.rm = TRUE), NA_integer_)
    )

subgroup_grades <- AF_grades_metrics %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners with Transitional 1-4", "Super Subgroup")) %>%
    group_by(system, school) %>%
    mutate(subgroup_count = sum(!is.na(subgroup_average))) %>%
    ungroup() %>%
    filter(!(subgroup == "Super Subgroup" & subgroup_count > 1)) %>%
    mutate(subgroup = "Subgroups") %>%
    group_by(system, school, pool) %>%
    summarise_at(c("grade_achievement", "grade_growth", "grade_grad", "grade_ready_grad", "grade_absenteeism", "grade_elpa"),
        mean, na.rm = TRUE) %>%
    ungroup() %>%
    rename(
        grade_achievement_subgroups = score_achievement,
        grade_growth_subgroups = score_growth,
        grade_grad_subgroups = score_grad,
        grade_ready_grad_subgroups = score_ready_grad,
        grade_absenteeism_subgroups = score_absenteeism,
        grade_elpa_subgroups = score_elpa
    )

AF_grades_metrics <- subgroup_grades %>%
    rename(
        grade_achievement = grade_achievement_subgroups,
        grade_growth = grade_growth_subgroups,
        grade_grad = grade_grad_subgroups,
        grade_ready_grad = grade_ready_grad_subgroups,
        grade_absenteeism = grade_absenteeism_subgroups,
        grade_elpa = grade_elpa_subgroups
    ) %>%
    mutate(subgroup = "Subgroups") %>%
    bind_rows(AF_grades_metrics, .)

all_students_grades <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school, pool, designation_ineligible, 
        priority_grad = if_else(pool == "HS", as.integer(!designation_ineligible & grade_grad_abs == 0), NA_integer_),
        grade_achievement_all = score_achievement,
        grade_growth_all = score_growth,
        grade_grad_all = score_grad,
        grade_ready_grad_all = score_ready_grad,
        grade_absenteeism_all = score_absenteeism,
        grade_elpa_all = score_elpa) %>%
    left_join(subgroup_grades, by = c("system", "school", "pool")) %>%
    mutate(
        grade_achievement = case_when(
            !is.na(grade_achievement_subgroups) ~ 0.6 * grade_achievement_all + 0.4 * grade_achievement_subgroups,
            is.na(grade_achievement_subgroups) ~ grade_achievement_all
        ),
        grade_growth = case_when(
            !is.na(grade_growth_subgroups) ~ 0.6 * grade_growth_all + 0.4 * grade_growth_subgroups,
            is.na(grade_growth_subgroups) ~ grade_growth_all
        ),
        grade_grad = case_when(
            !is.na(grade_grad_subgroups) ~ 0.6 * grade_grad_all + 0.4 * grade_grad_subgroups,
            is.na(grade_grad_subgroups) ~ grade_grad_all
        ),
        grade_ready_grad = case_when(
            !is.na(grade_ready_grad_subgroups) ~ 0.6 * grade_ready_grad_all + 0.4 * grade_ready_grad_subgroups,
            is.na(grade_ready_grad_subgroups) ~ grade_ready_grad_all
        ),
        grade_absenteeism = case_when(
            !is.na(grade_absenteeism_subgroups) ~ 0.6 * grade_absenteeism_all + 0.4 * grade_absenteeism_subgroups,
            is.na(grade_absenteeism_subgroups) ~ grade_absenteeism_all
        ),
        grade_elpa = case_when(
            !is.na(grade_elpa_subgroups) ~ 0.6 * grade_elpa_all + 0.4 * grade_elpa_subgroups,
            is.na(grade_elpa_subgroups) ~ grade_elpa_all
        )
    ) %>%
# Weights
    mutate(
        weight_achievement = if_else(!is.na(score_achievement) & pool == "K8", 0.45, NA_real_),
        weight_achievement = if_else(!is.na(score_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = if_else(!is.na(score_growth) & pool == "K8", 0.35, NA_real_),
        weight_growth = if_else(!is.na(score_growth) & pool == "HS", 0.25, weight_growth),
        weight_grad = if_else(!is.na(score_grad) & pool == "HS", 0.05, NA_real_),
        weight_ready_grad = if_else(!is.na(score_ready_grad) & pool == "HS", 0.2, NA_real_),
        weight_absenteeism = if_else(!is.na(score_absenteeism), 0.1, NA_real_),
        weight_elpa = if_else(!is.na(score_elpa), 0.1, NA_real_),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = if_else(is.na(score_elpa) & !is.na(score_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = if_else(is.na(score_elpa) & !is.na(score_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = if_else(is.na(score_elpa) & !is.na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = if_else(is.na(score_elpa) & !is.na(weight_growth) & pool == "HS", 0.3, weight_growth)
    ) %>%
    rowwise() %>%
    mutate(total_weight = sum(weight_achievement, weight_growth, weight_absenteeism,
            weight_grad, weight_ready_grad, weight_elpa, na.rm = TRUE),
        final_average = round5(sum(weight_achievement * score_achievement,
            weight_growth * score_growth,
            weight_absenteeism * score_absenteeism,
            weight_grad * score_grad,
            weight_ready_grad * score_ready_grad,
            weight_elpa * score_elpa, na.rm = TRUE)/total_weight, 1)
    )

AF_grades_final <- all_students_grades %>%
    full_join(bottom_five, by = c("system", "school")) %>%
    full_join(targeted_support, by = c("system", "school")) %>%
    mutate(final_grade = case_when(
        designation_ineligible == 1 ~ NA_character_,
        final_grade == "F" ~ "F",
        priority_grad == 1L ~ "F",
        final_average > 3 ~ "A",
        final_average > 2 ~ "B",
        final_average > 1 ~ "C",
        final_average >= 0 ~ "D",
        TRUE ~ final_grade
    ),
    targeted_support = if_else(final_grade == "D", 1L, targeted_support),
    targeted_support = if_else(designation_ineligible == 1, NA_integer_, targeted_support),
    targeted_support = if_else(!is.na(priority_grad) & priority_grad == 1L, NA_integer_, targeted_support),
    targeted_support = if_else(is.na(targeted_support), 0L, targeted_support),
    final_grade = if_else(targeted_support == 1L & final_grade %in% c("A", "B", "C"), paste0(final_grade, "-"), final_grade)) %>%
    select(system, school, pool, designation_ineligible, priority_grad,
        score_achievement, score_growth, score_absenteeism, score_grad, score_ready_grad, score_elpa,
        contains("targeted_support"), final_average, final_grade)

write_csv(AF_grades_metrics, "N:/ORP_accountability/projects/2018_school_accountability/school_grading_metrics.csv")
write_csv(AF_grades_final, "N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv")
