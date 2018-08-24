library(zeallot)
library(tidyverse)

district_accountability <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_district_accountability_file_JW.csv") %>%
    mutate(subgroup = if_else(grepl("English Language Learners", subgroup), "English Learners", subgroup))

act_participation <- district_accountability %>%
    filter(indicator == "ACT Participation", subgroup == "All Students") %>%
    transmute(system, act_participation = as.integer(metric >= 95))

minimum_achievement <- district_accountability %>%
    filter(indicator == "Success Rate" & subgroup == "All Students") %>%
    transmute(system,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
        ),
        achievement_key = as.integer(metric >= metric_prior)
    )

minimum_tvaas <- district_accountability %>%
    filter(indicator == "Success Rate" & subgroup == "All Students") %>%
    transmute(system,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
            ),
        tvaas_key = NA_integer_
    )

minimum_super <- district_accountability %>%
    filter(indicator == "Super Subgroup Performance") %>%
    transmute(system,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
        ),
        subgroup_key = as.integer(pct_below_bsc <= pct_below_bsc2017)
    )

minimum_performance <- minimum_achievement %>%
    full_join(minimum_tvaas, by = c("system", "content_area")) %>%
    full_join(minimum_super, by = c("system", "content_area")) %>%
    group_by(system) %>%
    summarise_at(c("achievement_key", "tvaas_key", "subgroup_key"), max) %>%
    ungroup() %>%
    left_join(act_participation, by = "system") %>% 
    mutate(met_minimum_performance = pmin(achievement_key, tvaas_key, subgroup_key, act_participation, na.rm = TRUE))

achievement <- district_accountability %>%
    filter(indicator == "Success Rate") %>%
    transmute(system, subgroup,
              content_area = case_when(
                  grade == "3rd through 5th" ~ "3-5 Success Rate",
                  grade == "6th through 8th" ~ "6-8 Success Rate",
                  grade == "9th through 12th" ~ "9-12 Success Rate"
              ),
              absolute_pathway = case_when(
                  metric >= 50 ~ 4,
                  metric >= 45 ~ 3,
                  metric >= 35 ~ 2,
                  metric >= 25 ~ 1,
                  metric < 25 ~ 0
              ),
              amo_pathway = case_when(
                  metric >= amo_target_double ~ 4,
                  metric > amo_target ~ 3,
                  upper_CI >= amo_target ~ 2,
                  upper_CI > metric_prior ~ 1,
                  upper_CI <= metric_prior ~ 0
              )
    )

absenteeism_va <- district_accountability %>%
    filter(indicator == "Chronic Absenteeism", pathway == "Value Added") %>%
    transmute(system, subgroup,
              content_area = "Chronic Absenteeism",
              value_added_pathway = case_when(
                  percentile >= 80 ~ 4,
                  percentile >= 60 ~ 3,
                  percentile >= 40 ~ 2,
                  percentile >= 20 ~ 1,
                  percentile < 20 ~ 0
              )
    )

absenteeism <- district_accountability %>%
    filter(indicator == "Chronic Absenteeism") %>%
    transmute(system, subgroup,
              content_area = "Chronically Out of School",
              absolute_pathway = case_when(
                  metric <= 8 ~ 4,
                  metric <= 11.5 ~ 3,
                  metric <= 16.5 ~ 2,
                  metric <= 25 ~ 1,
                  metric > 25 ~ 0
              ),
              amo_pathway = case_when(
                  metric >= amo_target_double ~ 4,
                  metric > amo_target ~ 3,
                  lower_CI >= amo_target ~ 2,
                  lower_CI > metric_prior ~ 1,
                  lower_CI <= metric_prior ~ 0
              )
    ) %>%
    left_join(absenteeism_va, by = c("system", "subgroup", "content_area"))

ELPA_va <- district_accountability %>%
    filter(indicator == "ELPA", pathway == "Value Added") %>%
    transmute(system, subgroup,
        content_area = "ELPA",
        value_added_pathway = case_when(
            percentile >= 80 ~ 4,
            percentile >= 60 ~ 3,
            percentile >= 40 ~ 2,
            percentile >= 20 ~ 1,
            percentile < 20 ~ 0
        )
    )

ELPA <- district_accountability %>%
    filter(indicator == "ELPA", pathway == "AMO/Absolute") %>%
    transmute(system, subgroup,
        content_area = "ELPA",
        absolute_pathway = case_when(
            metric >= 60 ~ 4,
            metric >= 50 ~ 3,
            metric >= 40 ~ 2,
            metric >= 25 ~ 1,
            metric < 25 ~ 0
        ),
        amo_pathway = case_when(
            metric >= amo_target_double ~ 4,
            metric > amo_target ~ 3,
            upper_CI >= amo_target ~ 2,
            upper_CI > metric_prior ~ 1,
            upper_CI <= metric_prior ~ 0
        )
    ) %>%
    left_join(ELPA_va, by = c("system", "subgroup", "content_area"))

grad_va <- district_accountability %>%
    filter(indicator == "Graduation Rate", pathway == "Value Added") %>%
    transmute(system, subgroup,
        content_area = "Graduation Rate",
        value_added_pathway = case_when(
            percentile >= 80 ~ 4,
            percentile >= 60 ~ 3,
            percentile >= 40 ~ 2,
            percentile >= 20 ~ 1,
            percentile < 20 ~ 0
        )
    )

grad <- district_accountability %>%
    filter(indicator == "Graduation Rate", pathway == "AMO/Absolute") %>%
    transmute(system, subgroup,
        content_area = "Graduation Rate",
        absolute_pathway = case_when(
            metric >= 95 ~ 4,
            metric >= 90 ~ 3,
            metric >= 80 ~ 2,
            metric >= 67 ~ 1,
            metric < 67 ~ 0
        ),
        amo_pathway = case_when(
            metric >= amo_target_double ~ 4,
            metric > amo_target ~ 3,
            upper_CI >= amo_target ~ 2,
            upper_CI > metric_prior ~ 1,
            upper_CI <= metric_prior ~ 0
        )
    ) %>%
    left_join(grad_va, by = c("system", "subgroup", "content_area"))


c(subgroups, all_students) %<-% (bind_rows(achievement, absenteeism, ELPA, grad) %>%
    mutate(overall_score = case_when(
        !is.na(value_added_pathway) ~ (pmax(absolute_pathway, amo_pathway, na.rm = TRUE) + value_added_pathway)/2,
        is.na(value_added_pathway) ~ pmax(absolute_pathway, amo_pathway, na.rm = TRUE)
        )
    ) %>%
    split(.$subgroup == "All Students"))

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
        system, achievement_key, tvaas_key, subgroup_key, act_participation, met_minimum_performance,
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
        overall_average = 0.6 * achievement_average + 0.4 * subgroup_average,
        final_determination = case_when(
            met_minimum_performance == 0 ~ "In Need of Improvement",
            overall_average >= 3 ~ "Exemplary",
            overall_average >= 2 ~ "Advancing",
            overall_average >= 1 ~ "Satisfactory",
            overall_average < 1 ~ "Marginal"
        )
    )

write_csv(districts_final, path = "N:/ORP_accountability/data/2018_final_accountability_files/district_determinations.csv", na = "")
