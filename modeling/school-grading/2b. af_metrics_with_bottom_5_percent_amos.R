## Dashboard Model: F Assigned to Bottom 5%; A-D Grades Assigned to Metrics with AMOs

library(tidyverse)

school_accountability <- read_csv("data/school_accountability_file_Aug31.csv", col_types = c("cicicccciddddddddcc"))

hs <- school_accountability %>%
    filter(year == "2015", subject == "Success Rate", subgroup == "All Students", pool == "HS") %>%
    nrow()
k8 <- school_accountability %>%
    filter(year == "2015", subject == "Success Rate", subgroup == "All Students", pool == "K8") %>%
    nrow()

# Absenteeism Grade
absenteeism <- read_csv("data/absenteeism_indicator.csv") %>%
    select(system, school, subgroup, grade_absenteeism, grade_absenteeism_reduction)

# Students advancing in proficiency for subgroup growth
subgroup_growth <- read_csv("data/student_match_ranks.csv") %>%
    transmute(system, school, subgroup, grade_growth)

# ELPA
ELPA <- read_csv("data/elpa_indicator.csv") %>%
    select(system, school, subgroup, grade_exit, grade_growth_standard)

# Ready Graduate
ready_grad <- read_csv("data/ready_graduate.csv") %>%
    select(system, school, subgroup, grade_grad, grade_grad_target, grade_ready_grad, grade_ready_grad_target)

# F schools
F_schools <- school_accountability %>%
    filter(year == "3 Year", subject == "Success Rate", subgroup == "All Students") %>%
    mutate(TVAAS_sh = TVAAS_level %in% c("Level 4", "Level 5") & TVAAS_level_lag %in% c("Level 4", "Level 5")) %>%
    group_by(pool, designation_ineligible, TVAAS_sh) %>%
    mutate(rank = rank(pct_prof_adv, ties.method = "min")) %>%
    ungroup() %>%
    transmute(system, system_name, school, school_name,
        final_grade = case_when(
            rank <= ceiling(0.05 * hs) & pool == "HS" & !designation_ineligible & !TVAAS_sh ~ "F",
            rank <= ceiling(0.05 * k8) & pool == "K8" & !designation_ineligible & !TVAAS_sh ~ "F"
        )
    )

# Achievement and growth
ach_growth <- school_accountability %>%
    filter(year == "2015", subject == "Success Rate") %>%
    mutate(grade_relative_achievement = case_when(
            pctile_rank_PA >= 80 ~ "A",
            pctile_rank_PA >= 60 ~ "B",
            pctile_rank_PA >= 40 ~ "C",
            pctile_rank_PA >= 20 ~ "D",
            pctile_rank_PA < 20 ~ "F"),
        grade_achievement_AMO = case_when(
            valid_tests < 30 ~ NA_character_,
            pct_prof_adv >= AMO_target_PA_4 ~ "A",
            pct_prof_adv > AMO_target_PA ~ "B",
            upper_bound_ci_PA >= AMO_target_PA ~ "C",
            upper_bound_ci_PA > pct_prof_adv_prior ~ "D",
            upper_bound_ci_PA <= pct_prof_adv_prior ~ "F"),
        grade_TVAAS = case_when(
            TVAAS_level == "Level 5" ~ "A",
            TVAAS_level == "Level 4" ~ "B",
            TVAAS_level == "Level 3" ~ "C",
            TVAAS_level == "Level 2" ~ "D",
            TVAAS_level == "Level 1" ~ "F")
        ) %>%
    left_join(subgroup_growth, by = c("system", "school", "subgroup")) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
        grade_relative_achievement, grade_achievement_AMO, grade_TVAAS, grade_growth)

# Full Heat Map
AF_grades_metrics <- ach_growth %>%
    left_join(ready_grad, by = c("system", "school", "subgroup")) %>%
    left_join(absenteeism, by = c("system", "school", "subgroup")) %>%
    left_join(ELPA, by = c("system", "school", "subgroup")) %>%
    transmute(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
        priority_grad = if_else(subgroup == "All Students", !designation_ineligible & grade_grad == "F", NA),
    # Not setting na.rm = TRUE for pmin so that only schools with both absolute and AMO receive a grade
        grade_achievement = pmin(grade_relative_achievement, grade_achievement_AMO),
        grade_growth = if_else(subgroup == "All Students", grade_TVAAS, grade_growth),
        grade_grad = pmin(grade_grad, grade_grad_target),
        grade_ready_grad = pmin(grade_ready_grad, grade_ready_grad_target),
        grade_absenteeism = pmin(grade_absenteeism, grade_absenteeism_reduction),
        grade_ELPA = pmin(grade_exit, grade_growth_standard)) %>%
    mutate_at(vars(starts_with("grade_")), funs(recode(., "A" = 4, "B" = 3, "C" = 2, "D" = 1, "F" = 0))) %>%
# Weights
    mutate(weight_achievement = if_else(!is.na(grade_achievement) & pool == "K8", 0.45, NA_real_),
        weight_achievement = if_else(!is.na(grade_achievement) & pool == "HS", 0.3, weight_achievement),
        weight_growth = if_else(!is.na(grade_growth) & pool == "K8", 0.35, NA_real_),
        weight_growth = if_else(!is.na(grade_growth) & pool == "HS", 0.25, weight_growth),
        weight_grad = if_else(!is.na(grade_grad) & pool == "HS", 0.05, NA_real_),
        weight_ready_grad = if_else(!is.na(grade_ready_grad) & pool == "HS", 0.2, NA_real_),
        weight_opportunity = if_else(!is.na(grade_absenteeism), 0.1, NA_real_),
        weight_ELPA = if_else(!is.na(grade_ELPA), 0.1, NA_real_),
    # If no ELPA, adjust achievement and growth weights accordingly
        weight_achievement = if_else(is.na(grade_ELPA) & !is.na(grade_achievement) & pool == "K8", 0.5, weight_achievement),
        weight_achievement = if_else(is.na(grade_ELPA) & !is.na(grade_achievement) & pool == "HS", 0.35, weight_achievement),
        weight_growth = if_else(is.na(grade_ELPA) & !is.na(weight_growth) & pool == "K8", 0.4, weight_growth),
        weight_growth = if_else(is.na(grade_ELPA) & !is.na(weight_growth) & pool == "HS", 0.3, weight_growth)) %>%
    rowwise() %>%
    mutate(total_weight = sum(weight_achievement, weight_growth, weight_opportunity, weight_grad, weight_ready_grad, weight_ELPA, na.rm = TRUE),
        subgroup_average = sum(weight_achievement * grade_achievement,
            weight_growth * grade_growth,
            weight_opportunity * grade_absenteeism,
            weight_grad * grade_grad,
            weight_ready_grad * grade_ready_grad,
            weight_ELPA * grade_ELPA, na.rm = TRUE)/total_weight) %>%
    ungroup()

# Achievement grades
all_students_grades_final <- AF_grades_metrics %>%
    filter(subgroup == "All Students") %>%
    transmute(system, system_name, school, school_name, pool, designation_ineligible, priority_grad,
        achievement_average = subgroup_average,
        achievement_grade = case_when(
            achievement_average > 3 ~ "A",
            achievement_average > 2 ~ "B",
            achievement_average > 1 ~ "C",
            achievement_average > 0 ~ "D",
            achievement_average == 0 ~ "F")
    )

# Targeted support schools
targeted_support <- AF_grades_metrics %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "English Learners", "Students with Disabilities",
        "Black", "Hispanic", "Native American", "Asian", "White")) %>%
    select(system, school, subgroup, designation_ineligible, subgroup_average) %>%
    full_join(F_schools, by = c("system", "school")) %>%
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
        targeted_support_EL = `English Learners`,
        targeted_support_Black = Black,
        targeted_support_Hispanic = Hispanic,
        targeted_support_Native = `Native American`,
        targeted_support_Asian = Asian,
        targeted_support_White = White,
        targeted_support = if_else(is.na(final_grade), 
            pmax(targeted_support_BHN, targeted_support_ED, targeted_support_SWD, targeted_support_EL,
                targeted_support_Black, targeted_support_Hispanic, targeted_support_Native,
                targeted_support_Asian, targeted_support_White, na.rm = TRUE), NA_integer_))

# Gap closure grades
subgroup_grades_final <- AF_grades_metrics %>%
    filter(subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
        "Students with Disabilities", "English Learners", "Super Subgroup")) %>%
# Drop Super Subgroup observation if other subgroups are present
    mutate(temp = !is.na(subgroup_average)) %>%
    group_by(system, system_name, school, school_name) %>%
    mutate(subgroups_count = sum(temp)) %>%
    filter(!(subgroup == "Super Subgroup" & subgroups_count > 1)) %>%
# Weight by total weight of the indicators represented by each subgroup
    mutate(subgroup_average_weighted = total_weight * subgroup_average) %>%
    summarise_at(c("total_weight", "subgroup_average_weighted"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(system, system_name, school, school_name,
        gap_closure_average = subgroup_average_weighted/total_weight,
        gap_closure_grade = case_when(
            gap_closure_average > 3 ~ "A",
            gap_closure_average > 2 ~ "B",
            gap_closure_average > 1 ~ "C",
            gap_closure_average > 0 ~ "D",
            gap_closure_average == 0 ~ "F",
            is.na(gap_closure_average) ~ NA_character_)
    )

AF_grades_final <- all_students_grades_final %>%
    full_join(subgroup_grades_final, by = c("system", "system_name", "school", "school_name")) %>%
    full_join(F_schools, by = c("system", "system_name", "school", "school_name")) %>%
    full_join(targeted_support, by = c("system", "school")) %>%
    mutate(overall_average = round(0.6 * achievement_average + 0.4 * gap_closure_average, 1),
        overall_average = if_else(is.na(overall_average), achievement_average, overall_average),
        final_grade = case_when(
            designation_ineligible == 1 ~ NA_character_,
            is.na(final_grade) & priority_grad ~ "F",
            is.na(final_grade) & overall_average > 3 ~ "A",
            is.na(final_grade) & overall_average > 2 ~ "B",
            is.na(final_grade) & overall_average > 1 ~ "C",
            is.na(final_grade) & overall_average > 0 ~ "D",
            TRUE ~ final_grade),
        targeted_support = if_else(final_grade == "D", 1L, targeted_support),
        targeted_support = if_else(designation_ineligible == 1, NA_integer_, targeted_support),
        priority_grad = if_else(is.na(priority_grad), FALSE, priority_grad),
        targeted_support = if_else(priority_grad, NA_integer_, targeted_support),
        targeted_support = if_else(is.na(targeted_support), 0L, targeted_support)) %>%
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
    mutate(title_1 = if_else(is.na(title_1), 0, title_1)) %>%
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
    filter(subgroup == "All Students", subject == "Success Rate", year == "2015") %>%
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

plot_TVAAS <- AF_grades_metrics %>%
    filter(subgroup == "All Students", !is.na(grade_growth)) %>%
    mutate(grade_growth = paste("Level", 1 + grade_growth),
        grade_growth = factor(grade_growth, levels = c("Level 5", "Level 4", "Level 3", "Level 2", "Level 1"))) %>%
    inner_join(AF_grades_final, by = c("system", "system_name", "school", "school_name", "designation_ineligible", "pool")) %>%
    filter(!is.na(final_grade)) %>%
    count(final_grade, grade_growth) %>%
    ggplot(aes(x = final_grade, y = n, fill = grade_growth, label = n)) +
        geom_bar(stat = "identity") +
        geom_text(position = position_stack(vjust = 0.5)) +
        theme_hc() +
        scale_x_discrete(limits = c("F", "D", "C", "B", "A")) +
        scale_fill_manual(values = c("#00b050", "#92d050", "#f6f7dd", "#ff9f99", "#ff0000")) +
        labs(fill = "TVAAS", y = "Count") +
        theme(axis.title.x = element_blank())

slides <- addSlide(slides, "Body - TN Mark") %>%
    addTitle("Distribution of TVAAS by Final Grade") %>%
    addPlot(fun = print, x = plot_TVAAS)

writeDoc(slides, paste0("school_grading_", format(Sys.Date(), "%b%d"), ".pptx"))
