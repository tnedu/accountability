library(acct)
library(tidyverse)

pools <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

priority <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/priority_exit.csv") %>%
    mutate(priority_csi = if_else(system != 985 & priority_csi == 1 & not_na(priority_exit) & priority_exit == 1, 0, priority_csi)) %>%
    filter(priority_csi == 1) %>%
    transmute(system, school, priority_csi)

school_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv")

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

absenteeism_scores <- school_accountability %>%
    filter(indicator == "Chronic Absenteeism") %>%
    select(system, school, subgroup, score_absenteeism_abs = score_abs, score_absenteeism_reduction = score_target, score_absenteeism = score)

elpa_scores <- school_accountability %>%
    filter(indicator == "ELPA Growth Standard") %>%
    select(system, school, subgroup, score_elpa = score)

AF_grades_metrics <- list(ach_scores, growth_scores, absenteeism_scores, grad_scores, ready_grad_scores, elpa_scores) %>%
    reduce(full_join, by = c("system", "school", "subgroup")) %>%
    full_join(pools, by = c("system", "school")) %>%
# Weights
    mutate(
        weight_achievement = if_else(not_na(score_achievement) & pool == "K8", 0.45, NA_real_),
        weight_achievement = if_else(not_na(score_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = if_else(not_na(score_growth) & pool == "K8", 0.35, NA_real_),
        weight_growth = if_else(not_na(score_growth) & pool == "HS", 0.25, weight_growth),
        weight_grad = if_else(not_na(score_grad) & pool == "HS", 0.05, NA_real_),
        weight_ready_grad = if_else(not_na(score_ready_grad) & pool == "HS", 0.2, NA_real_),
        weight_opportunity = if_else(not_na(score_absenteeism), 0.1, NA_real_),
        weight_elpa = if_else(not_na(score_elpa), 0.1, NA_real_),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = if_else(is.na(score_elpa) & not_na(score_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = if_else(is.na(score_elpa) & not_na(score_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = if_else(is.na(score_elpa) & not_na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = if_else(is.na(score_elpa) & not_na(weight_growth) & pool == "HS", 0.3, weight_growth)
    ) %>%
    rowwise() %>%
    mutate(
        total_weight = sum(weight_achievement, weight_growth, weight_opportunity, weight_grad, weight_ready_grad, weight_elpa, na.rm = TRUE),
        subgroup_average = round5(digits = 1,
            sum(
                weight_achievement * score_achievement,
                weight_growth * score_growth,
                weight_opportunity * score_absenteeism,
                weight_grad * score_grad,
                weight_ready_grad * score_ready_grad,
                weight_elpa * score_elpa, 
                na.rm = TRUE
            )/total_weight
        )
    ) %>%
    ungroup() %>%
    filter(not_na(designation_ineligible), not_na(pool))

# Targeted support schools
targeted_support_ranks <- AF_grades_metrics %>%
    mutate(subgroup_average = if_else(total_weight < 1, NA_real_, subgroup_average)) %>%
    filter(
        subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
            "English Learners with Transitional 1-4", "Students with Disabilities",
            "American Indian or Alaska Native", "Asian", "Black or African American",
            "Hispanic", "Native Hawaiian or Other Pacific Islander", "White")
    ) %>%
    select(system, school, subgroup, designation_ineligible, subgroup_average) %>%
    full_join(priority, by = c("system", "school")) %>%
    group_by(subgroup) %>%
    mutate(denom = sum(not_na(subgroup_average))) %>%
    group_by(subgroup, designation_ineligible, priority_csi) %>%
    mutate(
        rank = if_else(is.na(priority_csi) & !designation_ineligible & not_na(subgroup_average), rank(subgroup_average, ties.method = "min"), NA_integer_),
        targeted_support = if_else(is.na(priority_csi) & !designation_ineligible, as.integer(rank <= ceiling(0.05 * denom)), NA_integer_)
    ) %>%
    ungroup()

write_csv(targeted_support_ranks, "N:/ORP_accountability/projects/2019_school_accountability/targeted_support_ranks.csv", na = "")

targeted_support <- targeted_support_ranks %>%
    select(system, school, subgroup, priority_csi, targeted_support) %>%
    spread(subgroup, targeted_support) %>%
    transmute(
        system, school,
        priority_csi,
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
        targeted_support = if_else(is.na(priority_csi),
            true = pmax(targeted_support_BHN, targeted_support_ED, targeted_support_SWD, targeted_support_EL,
                targeted_support_Black, targeted_support_Hispanic, targeted_support_Native,
                targeted_support_HPI, targeted_support_Asian, targeted_support_White, na.rm = TRUE),
            false = NA_integer_
        )
    )

subgroup_grades <- AF_grades_metrics %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners with Transitional 1-4", "Super Subgroup")) %>%
    group_by(system, school) %>%
    mutate(subgroup_count = sum(not_na(subgroup_average))) %>%
    ungroup() %>%
    filter(!(subgroup == "Super Subgroup" & subgroup_count > 1)) %>%
    mutate(subgroup = "Subgroups") %>%
    group_by(system, school, pool) %>%
    summarise_at(
        .vars = vars(score_achievement, score_growth, score_grad, score_ready_grad, score_absenteeism, score_elpa),
        .f = ~ mean(., na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate_at(
        .vars = vars(score_achievement, score_growth, score_grad, score_ready_grad, score_absenteeism, score_elpa),
        .f = ~ if_else(is.nan(.), NA_real_, .)
    ) %>%
    rename(
        score_achievement_subgroups = score_achievement,
        score_growth_subgroups = score_growth,
        score_grad_subgroups = score_grad,
        score_ready_grad_subgroups = score_ready_grad,
        score_absenteeism_subgroups = score_absenteeism,
        score_elpa_subgroups = score_elpa
    )

AF_grades_metrics <- subgroup_grades %>%
    rename(
        score_achievement = score_achievement_subgroups,
        score_growth = score_growth_subgroups,
        score_grad = score_grad_subgroups,
        score_ready_grad = score_ready_grad_subgroups,
        score_absenteeism = score_absenteeism_subgroups,
        score_elpa = score_elpa_subgroups
    ) %>%
    mutate(subgroup = "Subgroups") %>%
    bind_rows(AF_grades_metrics, .)

all_students_grades <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    transmute(
        system, school, 
        pool, designation_ineligible, 
        score_achievement_all = score_achievement,
        score_growth_all = score_growth,
        score_grad_all = score_grad,
        score_ready_grad_all = score_ready_grad,
        score_absenteeism_all = score_absenteeism,
        score_elpa_all = score_elpa
    ) %>%
    left_join(subgroup_grades, by = c("system", "school", "pool")) %>%
    mutate(
        score_achievement = case_when(
            not_na(score_achievement_subgroups) ~ 0.6 * score_achievement_all + 0.4 * score_achievement_subgroups,
            is.na(score_achievement_subgroups) ~ score_achievement_all
        ),
        score_growth = case_when(
            not_na(score_growth_subgroups) ~ 0.6 * score_growth_all + 0.4 * score_growth_subgroups,
            is.na(score_growth_subgroups) ~ score_growth_all
        ),
        score_grad = case_when(
            not_na(score_grad_subgroups) ~ 0.6 * score_grad_all + 0.4 * score_grad_subgroups,
            is.na(score_grad_subgroups) ~ score_grad_all
        ),
        score_ready_grad = case_when(
            not_na(score_ready_grad_subgroups) ~ 0.6 * score_ready_grad_all + 0.4 * score_ready_grad_subgroups,
            is.na(score_ready_grad_subgroups) ~ score_ready_grad_all
        ),
        score_absenteeism = case_when(
            not_na(score_absenteeism_subgroups) ~ 0.6 * score_absenteeism_all + 0.4 * score_absenteeism_subgroups,
            is.na(score_absenteeism_subgroups) ~ score_absenteeism_all
        ),
        score_elpa = case_when(
            not_na(score_elpa_subgroups) ~ 0.6 * score_elpa_all + 0.4 * score_elpa_subgroups,
            is.na(score_elpa_subgroups) ~ score_elpa_all
        )
    ) %>%
# Weights
    mutate(
        weight_achievement = if_else(not_na(score_achievement) & pool == "K8", 0.45, NA_real_),
        weight_achievement = if_else(not_na(score_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = if_else(not_na(score_growth) & pool == "K8", 0.35, NA_real_),
        weight_growth = if_else(not_na(score_growth) & pool == "HS", 0.25, weight_growth),
        weight_grad = if_else(not_na(score_grad) & pool == "HS", 0.05, NA_real_),
        weight_ready_grad = if_else(not_na(score_ready_grad) & pool == "HS", 0.2, NA_real_),
        weight_absenteeism = if_else(not_na(score_absenteeism), 0.1, NA_real_),
        weight_elpa = if_else(not_na(score_elpa), 0.1, NA_real_),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = if_else(is.na(score_elpa) & not_na(score_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = if_else(is.na(score_elpa) & not_na(score_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = if_else(is.na(score_elpa) & not_na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = if_else(is.na(score_elpa) & not_na(weight_growth) & pool == "HS", 0.3, weight_growth)
    ) %>%
    rowwise() %>%
    mutate(
        total_weight = sum(weight_achievement, weight_growth, weight_absenteeism, weight_grad, weight_ready_grad, weight_elpa, na.rm = TRUE),
        final_average = round5(digits = 1,
            sum(
                weight_achievement * score_achievement,
                weight_growth * score_growth,
                weight_absenteeism * score_absenteeism,
                weight_grad * score_grad,
                weight_ready_grad * score_ready_grad,
                weight_elpa * score_elpa,
                na.rm = TRUE
            )/total_weight
        )
    ) %>%
    ungroup()

# Source atsi.R file to avoid having to re-run atsi.R and final_grades.R files out of order (circularity)
source(
    file = "N:/ORP_accountability/projects/Alex/accountability/2019-accountability/school/atsi.R",
    local = T,
    echo = T
)

subgroup_below_priority <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/subgroup_below_priority.csv")

AF_grades_final <- all_students_grades %>%
    full_join(targeted_support, by = c("system", "school")) %>%
    left_join(subgroup_below_priority, by = c("system", "school")) %>%
    mutate(
        priority_csi = if_else(is.na(priority_csi), 0L, priority_csi),
        targeted_support = if_else(designation_ineligible == 1, NA_integer_, targeted_support),
        additional_targeted_support_BHN = targeted_support_BHN == 1 & subgroup_below_BHN == 1,
        additional_targeted_support_ED = targeted_support_ED == 1 & subgroup_below_ED == 1,
        additional_targeted_support_SWD = targeted_support_SWD == 1 & subgroup_below_SWD == 1,
        additional_targeted_support_EL = targeted_support_EL == 1 & subgroup_below_EL == 1,
        additional_targeted_support_Native = targeted_support_Native == 1 & subgroup_below_Native == 1,
        additional_targeted_support_Asian = targeted_support_Asian == 1 & subgroup_below_Asian == 1,
        additional_targeted_support_Black = targeted_support_Black == 1 & subgroup_below_Black == 1,
        additional_targeted_support_Hispanic = targeted_support_Hispanic == 1 & subgroup_below_Hispanic == 1,
        additional_targeted_support_HPI = targeted_support_HPI == 1 & subgroup_below_HPI == 1,
        additional_targeted_support_White = targeted_support_White == 1 & subgroup_below_White == 1,
        additional_targeted_support = if_else(designation_ineligible == 0 & (priority_csi == 0 | is.na(priority_csi)),
            true = pmax(
                additional_targeted_support_BHN, additional_targeted_support_ED, additional_targeted_support_SWD,
                additional_targeted_support_EL, additional_targeted_support_Black, additional_targeted_support_Hispanic, 
                additional_targeted_support_Native, additional_targeted_support_HPI, additional_targeted_support_Asian,
                additional_targeted_support_White,
                na.rm = TRUE),
            false = NA_integer_
        ),
        reward = if_else(
            designation_ineligible == 0 & priority_csi == 0 & targeted_support == 0 & additional_targeted_support == 0 & final_average > 3,
            true = 1L,
            false = 0L
        )
    ) %>%
    mutate_at(vars(starts_with("additional_targeted_support")), as.integer) %>%
    select(system, school, pool, designation_ineligible, priority_csi, additional_targeted_support, targeted_support, reward,
        score_achievement, score_growth, score_absenteeism, score_grad, score_ready_grad, score_elpa,
        starts_with("targeted_support"), starts_with("additional_targeted_support"), final_average)

school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

AF_grades_metrics %>%
    mutate(subgroup_average = if_else(is.nan(subgroup_average), NA_real_, subgroup_average)) %>%
    arrange(system, school, subgroup) %>%
    left_join(school_names, by = c("system", "school")) %>%
    filter(not_na(school_name)) %>%
    select(system, system_name, school, school_name, everything()) %>%
    write_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_metrics.csv", na = "")

AF_grades_final %>%
    arrange(system, school) %>%
    left_join(school_names, by = c("system", "school")) %>%
    filter(not_na(school_name)) %>%
    select(system, system_name, school, school_name, pool, designation_ineligible, everything()) %>%
    write_csv("N:/ORP_accountability/projects/2019_school_accountability/school_grading_grades.csv", na = "")
