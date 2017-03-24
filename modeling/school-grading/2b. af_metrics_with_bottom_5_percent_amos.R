## Dashboard Model: F Assigned to Bottom 5%; A-D Grades Assigned to Metrics with AMOs

library(tidyverse)

school_accountability <- read_csv("data/school_accountability_file.csv")

hs <- school_accountability %>%
    filter(year == "2015", subject == "Success Rate", subgroup == "All Students", pool == "HS") %>%
    nrow()
k8 <- school_accountability %>%
    filter(year == "2015", subject == "Success Rate", subgroup == "All Students", pool == "K8") %>%
    nrow()

# Absenteeism Grade
absenteeism <- read_csv("data/absenteeism_indicator.csv") %>%
    select(system, school, subgroup, grade_absenteeism_absolute, grade_absenteeism_reduction)

# Students advancing in proficiency for subgroup growth
subgroup_growth <- read_csv("data/student_match_ranks.csv") %>%
    select(system, school, subgroup, grade_growth = grade)

# ELPA
ELPA <- read_csv("data/elpa_indicator.csv") %>%
    select(system, school, subgroup, grade_elpa)

# F schools
F_schools <- school_accountability %>%
    filter(year == "3 Year", subject == "Success Rate", subgroup == "All Students") %>%
    mutate(TVAAS_sh = TVAAS_level %in% c("Level 4", "Level 5") & TVAAS_level_lag %in% c("Level 4", "Level 5")) %>%
    group_by(pool, designation_ineligible, TVAAS_sh) %>%
    mutate(rank = rank(pct_prof_adv, ties.method = "min")) %>%
    ungroup() %>%
    transmute(system, school,
        final_grade = ifelse(rank <= ceiling(0.05 * hs) & pool == "HS" & !designation_ineligible & !TVAAS_sh, "F", NA),
        final_grade = ifelse(rank <= ceiling(0.05 * k8) & pool == "K8" & !designation_ineligible & !TVAAS_sh, "F", final_grade))

# ACT and Grad
ACT <- school_accountability %>%
    filter(subject == "ACT Composite") %>%
    select(system, school, subgroup, valid_tests_ACT = valid_tests, pct_21_ACT = pct_prof_adv,
        valid_tests_prior_ACT = valid_tests_prior, pct_21_prior_ACT = pct_prof_adv_prior)

ACT_grad <- school_accountability %>%
    filter(subject == "Graduation Rate") %>%
    select(system, school, subgroup, grad_cohort = valid_tests, grad_rate = pct_prof_adv,
        grad_cohort_prior = valid_tests_prior, grad_rate_prior = pct_prof_adv_prior) %>%
    full_join(ACT, by = c("system", "school", "subgroup")) %>%
    mutate(ACT_grad_prior = ifelse(valid_tests_prior_ACT >= 30 & grad_cohort_prior >= 30,
            round(grad_rate_prior * pct_21_prior_ACT/100, 1), NA),
        AMO_target = round(ACT_grad_prior + (100 - ACT_grad_prior)/16, 1),
        AMO_target_4 = round(ACT_grad_prior + (100 - ACT_grad_prior)/8, 1),
        ACT_grad = ifelse(valid_tests_ACT >= 30 & grad_cohort >= 30, round(grad_rate * pct_21_ACT/100, 1), NA),
        grade_readiness_absolute = ifelse(ACT_grad <= 16, "F", NA),
        grade_readiness_absolute = ifelse(ACT_grad > 16, "D", grade_readiness_absolute),
        grade_readiness_absolute = ifelse(ACT_grad > 28, "C", grade_readiness_absolute),
        grade_readiness_absolute = ifelse(ACT_grad > 35, "B", grade_readiness_absolute),
        grade_readiness_absolute = ifelse(ACT_grad >= 50, "A", grade_readiness_absolute),
        grade_readiness_target = ifelse(ACT_grad < ACT_grad_prior, "F", NA),
        grade_readiness_target = ifelse(ACT_grad == ACT_grad_prior, "D", grade_readiness_target),
        grade_readiness_target = ifelse(ACT_grad > ACT_grad_prior, "C", grade_readiness_target),
        grade_readiness_target = ifelse(ACT_grad >= AMO_target, "B", grade_readiness_target),
        grade_readiness_target = ifelse(ACT_grad >= AMO_target_4, "A", grade_readiness_target))

# Achievement and growth
ach_growth <- school_accountability %>%
    filter(year == "2015", subject == "Success Rate") %>%
    mutate(grade_relative_achievement = ifelse(pctile_rank_PA < 20, "F", NA),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 20, "D", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 40, "C", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 60, "B", grade_relative_achievement),
        grade_relative_achievement = ifelse(pctile_rank_PA >= 80, "A", grade_relative_achievement),
        grade_achievement_AMO = ifelse(upper_bound_ci_PA <= pct_prof_adv_prior, "F", NA),
        grade_achievement_AMO = ifelse(upper_bound_ci_PA > pct_prof_adv_prior, "D", grade_achievement_AMO),
        grade_achievement_AMO = ifelse(upper_bound_ci_PA >= AMO_target_PA, "C", grade_achievement_AMO),
        grade_achievement_AMO = ifelse(pct_prof_adv >= AMO_target_PA, "B", grade_achievement_AMO),
        grade_achievement_AMO = ifelse(pct_prof_adv >= AMO_target_PA_4, "A", grade_achievement_AMO),
        grade_achievement_AMO = ifelse(valid_tests < 30, NA, grade_achievement_AMO),
        grade_TVAAS = ifelse(TVAAS_level == "Level 5", "A", NA),
        grade_TVAAS = ifelse(TVAAS_level == "Level 4", "B", grade_TVAAS),
        grade_TVAAS = ifelse(TVAAS_level == "Level 3", "C", grade_TVAAS),
        grade_TVAAS = ifelse(TVAAS_level == "Level 2", "D", grade_TVAAS),
        grade_TVAAS = ifelse(TVAAS_level == "Level 1", "F", grade_TVAAS)) %>%
    left_join(subgroup_growth, by = c("system", "school", "subgroup")) %>%
    left_join(ELPA, by = c("system", "school", "subgroup")) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
        grade_relative_achievement, grade_achievement_AMO, grade_TVAAS, grade_growth, grade_elpa)

# Full Heat Map
full_heat_map <- ach_growth %>%
    left_join(ACT_grad, by = c("system", "school", "subgroup")) %>%
    left_join(absenteeism, by = c("system", "school", "subgroup")) %>%
# Not setting na.rm = TRUE so that schools are only evaluated if they have absolute and AMO pathways
    mutate(grade_achievement = pmin(grade_relative_achievement, grade_achievement_AMO),
        grade_readiness = pmin(grade_readiness_absolute, grade_readiness_target),
        grade_absenteeism = pmin(grade_absenteeism_absolute, grade_absenteeism_reduction),
        priority_grad = ifelse(subgroup == "All Students", !designation_ineligible & grad_cohort >= 30 & grad_rate < 67, NA)) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible, priority_grad, 
        grade_achievement, grade_TVAAS, grade_growth, grade_readiness, grade_absenteeism, grade_elpa)

AF_grades_metrics <- full_heat_map %>%
    mutate_at(vars(starts_with("grade_")), funs(recode(., "A" = 4, "B" = 3, "C" = 2, "D" = 1, "F" = 0))) %>%
    mutate(
    # Weights
        weight_achievement = ifelse(!is.na(grade_achievement) & pool == "K8", 0.45, NA),
        weight_achievement = ifelse(!is.na(grade_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = ifelse((!is.na(grade_TVAAS) | !is.na(grade_growth)) & pool == "K8", 0.35, NA),
        weight_growth = ifelse((!is.na(grade_TVAAS) | !is.na(grade_growth)) & pool == "HS", 0.25, weight_growth),
        weight_readiness = ifelse(!is.na(grade_readiness) & pool == "HS", 0.25, NA),
        weight_opportunity = ifelse(!is.na(grade_absenteeism), 0.1, NA),
        weight_elpa = ifelse(!is.na(grade_elpa), 0.1, NA),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = ifelse(is.na(grade_elpa) & !is.na(grade_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = ifelse(is.na(grade_elpa) & !is.na(grade_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = ifelse(is.na(grade_elpa) & !is.na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = ifelse(is.na(grade_elpa) & !is.na(weight_growth) & pool == "HS", 0.3, weight_growth)) %>%
    rowwise() %>%
    mutate(total_weight = sum(weight_achievement, weight_growth, weight_opportunity, weight_readiness, weight_elpa, na.rm = TRUE),
        subgroup_average = sum(weight_achievement * grade_achievement,
            weight_growth * grade_TVAAS,
            weight_growth * grade_growth,
            weight_opportunity * grade_absenteeism,
            weight_readiness * grade_readiness,
            weight_elpa * grade_elpa, na.rm = TRUE)/total_weight) %>%
    ungroup()

# Achievement grades
all_students_grades_final <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    transmute(system, system_name, school, school_name, pool, designation_ineligible, priority_grad,
        achievement_average = subgroup_average,
        achievement_grade = ifelse(achievement_average == 0, "F", NA),
        achievement_grade = ifelse(achievement_average > 0, "D", achievement_grade),
        achievement_grade = ifelse(achievement_average > 1, "C", achievement_grade),
        achievement_grade = ifelse(achievement_average > 2, "B", achievement_grade),
        achievement_grade = ifelse(achievement_average > 3, "A", achievement_grade))

# Targeted support schools
targeted_support <- AF_grades_metrics %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "English Learners", "Students with Disabilities")) %>%
    select(system, school, subgroup, designation_ineligible, subgroup_average) %>%
    full_join(F_schools, by = c("system", "school")) %>%
    group_by(subgroup) %>%
    mutate(denom = sum(!is.na(subgroup_average))) %>%
    group_by(subgroup, designation_ineligible, final_grade) %>%
    mutate(rank = rank(subgroup_average, ties.method = "min"),
        targeted_support = ifelse(is.na(final_grade) & !designation_ineligible, rank <= ceiling(0.05 * denom), NA)) %>%
    ungroup() %>%
    select(system, school, subgroup, final_grade, targeted_support) %>%
    spread(subgroup, targeted_support) %>%
    transmute(system, school,
        targeted_support_BHN = `Black/Hispanic/Native American`,
        targeted_support_ED = `Economically Disadvantaged`,
        targeted_support_SWD = `Students with Disabilities`,
        targeted_support_EL = `English Learners`,
        targeted_support = ifelse(is.na(final_grade), pmax(targeted_support_BHN, targeted_support_ED,
            targeted_support_SWD, targeted_support_EL, na.rm = TRUE), NA))

# Gap closure grades
subgroup_grades_final <- AF_grades_metrics %>%
    filter(subgroup != "All Students") %>%
    # Drop Super Subgroup observation if other subgroups are present
    mutate(temp = !is.na(subgroup_average)) %>%
    group_by(system, system_name, school, school_name) %>%
    mutate(subgroups_count = sum(temp)) %>%
    filter(!(subgroup == "Super Subgroup" & subgroups_count > 1)) %>%
    mutate(subgroup_average_weighted = total_weight * subgroup_average) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), total_weight, subgroup_average_weighted) %>%
    ungroup() %>%
    transmute(system, system_name, school, school_name, gap_closure_average = subgroup_average_weighted/total_weight,
        gap_closure_grade = ifelse(gap_closure_average == 0, "F", NA),
        gap_closure_grade = ifelse(gap_closure_average > 0, "D", gap_closure_grade),
        gap_closure_grade = ifelse(gap_closure_average > 1, "C", gap_closure_grade),
        gap_closure_grade = ifelse(gap_closure_average > 2, "B", gap_closure_grade),
        gap_closure_grade = ifelse(gap_closure_average > 3, "A", gap_closure_grade))

AF_grades_final <- all_students_grades_final %>%
    full_join(subgroup_grades_final, by = c("system", "system_name", "school", "school_name")) %>%
    full_join(F_schools, by = c("system", "school")) %>%
    full_join(targeted_support, by = c("system", "school")) %>%
    mutate(final_grade = ifelse(is.na(final_grade) & priority_grad == TRUE, "F", final_grade)) %>%
    rowwise() %>%
    mutate(overall_average = ifelse(!is.na(achievement_average) & !is.na(gap_closure_average),
            sum(0.6 * achievement_average, 0.4 * gap_closure_average, na.rm = TRUE), NA),
        overall_average = ifelse(!is.na(achievement_average) & is.na(gap_closure_average),
            achievement_average, overall_average),
        overall_average = ifelse(is.na(achievement_average) & !is.na(gap_closure_average),
            gap_closure_average, overall_average),
        overall_average = round(overall_average, 1)) %>%
    ungroup() %>%
    mutate(final_grade = ifelse(is.na(final_grade) & overall_average <= 1, "D", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 1 & overall_average <= 2, "C", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 2 & overall_average <= 3, "B", final_grade),
        final_grade = ifelse(is.na(final_grade) & overall_average > 3, "A", final_grade),
        final_grade = ifelse(designation_ineligible, NA, final_grade),
        # targeted_support = ifelse(final_grade == "D", 1, targeted_support),
        targeted_support = ifelse(designation_ineligible, NA, targeted_support),
        priority_grad = ifelse(is.na(priority_grad), FALSE, priority_grad),
        targeted_support = ifelse(priority_grad, NA, targeted_support),
        targeted_support = ifelse(is.na(targeted_support), FALSE, targeted_support)) %>%
    select(system, system_name, school, school_name, pool, designation_ineligible, priority_grad,
        achievement_average, achievement_grade, gap_closure_average, gap_closure_grade, overall_average,
        contains("targeted_support"), final_grade)

# Merge Title 1 Status
title_1 <- readxl::read_excel("data/2014-15 Title I Schools List.xlsx") %>%
    transmute(system = as.numeric(substr(`School ID`, 1, 3)),
        school = as.numeric(substr(`School ID`, 5, 8)),
        title_1 = 1)

# Output files
write_csv(AF_grades_metrics, path = paste0("data/AF_bottom_five_amos_metrics_", format(Sys.Date(), "%b%d"), ".csv"), na = "")

AF_grades_final %>%
    left_join(title_1, by = c("system", "school")) %>%
    mutate(title_1 = ifelse(is.na(title_1), 0, title_1)) %>%
    write_csv(path = paste0("data/AF_bottom_five_amos_final_grades_", format(Sys.Date(), "%b%d"), ".csv"), na = "")

# Create presentation
options(java.home = "C:/Program Files/Java/jre1.8.0_51", "ReporteRs-default-font" = "Calibri")
library(ReporteRs)
library(ggthemes)

slides <- pptx(template = "template.pptx") %>%
    addSlide("Body - Red") %>%
    addTitle("Distribution of Final Grades")

final_distribution <- FlexTable(table(AF_grades_final$pool, AF_grades_final$final_grade),
    add.rownames = TRUE,
    header.par.props = parProperties(text.align = "center"),
    body.par.props = parProperties(text.align = "center"),
    header.text.props = textProperties(font.size = 18, font.weight = "bold"),
    body.text.props = textProperties(font.size = 18))

plot_sr_3yr <- school_accountability %>%
    filter(subgroup == "All Students", subject == "Success Rate", year == "3 Year") %>%
    full_join(AF_grades_final, by = c("system", "system_name", "school", "school_name", "designation_ineligible", "pool")) %>%
    ggplot(aes(x = final_grade, y = pct_prof_adv)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(position = position_jitter(width = .2, height = 0), size = 1) +
        theme_hc() +
        scale_x_discrete(limits = c("F", "D", "C", "B", "A")) +
        scale_y_continuous(limits = c(0, 100), breaks = 10 * (0:10)) +
        xlab("Final Grade") +
        ylab("All Students Success Rate")

slides <- addFlexTable(slides, final_distribution) %>%
    addSlide("Body - TN Mark") %>%
    addTitle("Distribution of 3-Year Success Rates by Final Grade") %>%
    addPlot(fun = print, x = plot_sr_3yr)

plot_sr_1yr <- school_accountability %>%
    filter(subgroup == "All Students" & subject == "Success Rate" & year == "2015") %>%
    full_join(AF_grades_final, by = c("system", "system_name", "school", "school_name", "designation_ineligible", "pool")) %>%
    ggplot(aes(x = final_grade, y = pct_prof_adv)) +
        geom_boxplot(outlier.shape = NA) +
        geom_jitter(position = position_jitter(width = .2, height = 0), size = 1) +
        theme_hc() +
        scale_x_discrete(limits = c("F", "D", "C", "B", "A")) +
        scale_y_continuous(limits = c(0, 100), breaks = 10 * (0:10)) +
        xlab("Final Grade") +
        ylab("All Students Success Rate")
    
slides <- addSlide(slides, "Body - TN Mark") %>%
    addTitle("Distribution of 1-Year Success Rates by Final Grade") %>%
    addPlot(fun = print, x = plot_sr_1yr)

plot_tvaas <- AF_grades_metrics %>%
    filter(!is.na(grade_TVAAS),
        subgroup == "All Students") %>%
    mutate(grade_TVAAS = paste("Level", 1 + grade_TVAAS),
        grade_TVAAS = factor(grade_TVAAS, levels = c("Level 5", "Level 4", "Level 3", "Level 2", "Level 1"))) %>%
    inner_join(AF_grades_final, by = c("system", "system_name", "school", "school_name", "designation_ineligible", "pool")) %>%
    filter(!is.na(final_grade)) %>%
    count(final_grade, grade_TVAAS) %>%
    ggplot(aes(x = final_grade, y = n, fill = grade_TVAAS, label = n)) + 
        geom_bar(stat = "identity") +
        geom_text(position = position_stack(vjust = 0.5)) + 
        theme_hc() + 
        scale_x_discrete(limits = c("F", "D", "C", "B", "A")) +
        scale_fill_manual(values = c("#00b050", "#92d050", "#f6f7dd", "#ff9f99", "#ff0000")) + 
        labs(fill = "TVAAS", y = "Count") +
        theme(axis.title.x = element_blank())

slides <- addSlide(slides, "Body - TN Mark") %>%
    addTitle("Distribution of TVAAS by Final Grade") %>%
    addPlot(fun = print, x = plot_tvaas)

writeDoc(slides, paste0("school_grading_", format(Sys.Date(), "%b%d"), ".pptx"))
