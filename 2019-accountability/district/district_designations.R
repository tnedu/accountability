library(acct)
library(zeallot)
library(tidyverse)

district_accountability <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_accountability_file.csv")

c(subgroups, all_students) %<-% (district_accountability %>% split(.$subgroup == "All Students"))

all_students_overall <- all_students %>%
    group_by(system) %>%
    summarise(achievement_average = mean(overall_score, na.rm = TRUE))

subgroups_overall <- subgroups %>%
    group_by(system, subgroup) %>%
    summarise_at("overall_score", mean, na.rm = TRUE) %>%
    group_by(system) %>%
    summarise(subgroup_average = mean(overall_score, na.rm = TRUE))

final <- left_join(all_students_overall, subgroups_overall, by = "system") %>%
    transmute(
        system,
        achievement_average,
        achievement_determination = case_when(
            achievement_average >= 3.1 ~ "Exemplary",
            achievement_average >= 2.1 ~ "Advancing",
            achievement_average >= 1.1 ~ "Satisfactory",
            achievement_average < 1.1 ~ "Marginal"
        ),
        subgroup_average,
        subgroup_determination = case_when(
            subgroup_average >= 3.1 ~ "Exemplary",
            subgroup_average >= 2.1 ~ "Advancing",
            subgroup_average >= 1.1 ~ "Satisfactory",
            subgroup_average < 1.1 ~ "Marginal"
        ),
        overall_average = round5(0.6 * achievement_average + 0.4 * subgroup_average, 1),
        rank = if_else(not_na(overall_average), rank(overall_average, ties.method = "min"), NA_integer_),
        denom = sum(not_na(overall_average)),
        percentile = round(100 * rank/denom, 1),
        met_minimum_performance = as.integer(percentile > 5),
        final_determination = case_when(
            met_minimum_performance == 0 ~ "In Need of Improvement",
            overall_average >= 3.1 ~ "Exemplary",
            overall_average >= 2.1 ~ "Advancing",
            overall_average >= 1.1 ~ "Satisfactory",
            overall_average < 1.1 ~ "Marginal"
        )
    ) %>%
    mutate_at(c("achievement_average", "subgroup_average", "overall_average"), ~ if_else(is.nan(.), NA_real_, .))

write_csv(final, path = "N:/ORP_accountability/data/2019_final_accountability_files/district_designations.csv", na = "")
