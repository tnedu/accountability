library(acct)
library(zeallot)
library(tidyverse)

district_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file.csv")

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

minimum_performance <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
    filter(
        residential_facility == 0,
        enrolled_50_pct_district == "Y",
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
    mutate(
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        subject = case_when(
            subject %in% math_eoc ~ "HS Math",
            subject %in% english_eoc ~ "HS English",
        )
    ) %>%
# Aggregate HS Math/English
    group_by(system, subject) %>%
    summarise_at(c("valid_test", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_test", "ot_m"), ~ if_else(valid_test < 30, 0L, as.integer(.))) %>%
    group_by(system) %>%
    summarise_at(c("valid_test", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
        metric = if_else(valid_test != 0, round5(100 * ot_m/valid_test, 1), NA_real_),
        rank = if_else(!is.na(metric), rank(metric, ties.method = "min"), NA_integer_),
        denom = sum(!is.na(metric)),
        percentile = round(100 * rank/denom, 1),
        met_minimum_performance = as.integer(percentile > 5)
    ) %>%
    select(system, met_minimum_performance)

c(subgroups, all_students) %<-% (district_accountability %>% split(.$subgroup == "All Students"))

all_students_overall <- all_students %>%
    group_by(system) %>%
    summarise(achievement_average = mean(overall_score, na.rm = TRUE))

subgroups_overall <- subgroups %>%
    group_by(system, subgroup) %>%
    summarise_at("overall_score", mean, na.rm = TRUE) %>%
    group_by(system) %>%
    summarise(subgroup_average = mean(overall_score, na.rm = TRUE))

districts_final <- minimum_performance %>%
    left_join(all_students_overall, by = "system") %>%
    left_join(subgroups_overall, by = "system") %>%
    transmute(
        system, 
        met_minimum_performance,
        achievement_average,
        achievement_determination = case_when(
            achievement_average >= 3 ~ "Exemplary",
            achievement_average >= 2 ~ "Advancing",
            achievement_average >= 1 ~ "Satisfactory",
            achievement_average < 1 ~ "Marginal"
        ),
        subgroup_average,
        subgroup_determination = case_when(
            subgroup_average >= 3 ~ "Exemplary",
            subgroup_average >= 2 ~ "Advancing",
            subgroup_average >= 1 ~ "Satisfactory",
            subgroup_average < 1 ~ "Marginal"
        ),
        overall_average = round5(0.6 * achievement_average + 0.4 * subgroup_average, 1),
        final_determination = case_when(
            met_minimum_performance == 0 ~ "In Need of Improvement",
            overall_average >= 3 ~ "Exemplary",
            overall_average >= 2 ~ "Advancing",
            overall_average >= 1 ~ "Satisfactory",
            overall_average < 1 ~ "Marginal"
        )
    ) %>%
    mutate_at(c("achievement_average", "subgroup_average", "overall_average"), ~ if_else(is.nan(.), NA_real_, .))

write_csv(districts_final, path = "N:/ORP_accountability/data/2019_final_accountability_files/district_determinations.csv", na = "")
