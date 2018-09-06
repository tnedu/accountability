library(acct)
library(zeallot)
library(tidyverse)

district_accountability <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_district_accountability_file_JW.csv") %>%
    mutate(subgroup = if_else(grepl("English Language Learners", subgroup), "English Learners", subgroup))

act_participation <- district_accountability %>%
    filter(indicator == "ACT Participation", subgroup == "All Students") %>%
    transmute(system, act_participation = if_else(n_count >= 30, as.integer(metric >= 95), NA_integer_))

minimum_achievement <- district_accountability %>%
    filter(indicator == "Success Rate" & subgroup == "All Students" & pathway == "AMO/Absolute") %>%
    transmute(system,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
        ),
        achievement_key = if_else(n_count >= 30 & n_count_prior >= 30, as.integer(metric >= metric_prior), NA_integer_)
    )

minimum_tvaas <- district_accountability %>%
    filter(indicator == "TVAAS" & subgroup == "All Students" & pathway == "Minimum Performance Goal") %>%
    transmute(system,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
            ),
        tvaas_key = as.integer(metric >= 3)
    )

minimum_super <- district_accountability %>%
    filter(indicator == "Super Subgroup Performance") %>%
    transmute(system,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
        ),
        subgroup_key = if_else(n_count >= 30 & n_count_prior >= 30, as.integer(pct_below_bsc <= pct_below_bsc2017), NA_integer_)
    )

minimum_performance <- minimum_achievement %>%
    full_join(minimum_tvaas, by = c("system", "content_area")) %>%
    full_join(minimum_super, by = c("system", "content_area"))

write_csv(minimum_performance, "N:/ORP_accountability/data/2018_final_accountability_files/minimum_performance.csv")

minimum_performance <- minimum_performance %>%
    group_by(system) %>%
    summarise_at(c("achievement_key", "tvaas_key", "subgroup_key"), max, na.rm = TRUE) %>%
    ungroup() %>%
    mutate_at(c("achievement_key", "subgroup_key"), ~ if_else(. == -Inf, NA_real_, .)) %>%
    left_join(act_participation, by = "system") %>%
    mutate(
        met_minimum_performance = pmin(
            pmax(achievement_key, tvaas_key, na.rm = TRUE),
            subgroup_key,
            act_participation,
        na.rm = TRUE)
    )

achievement_va <- district_accountability %>%
    filter(indicator == "Success Rate" & pathway == "Value Added") %>%
    transmute(system, subgroup,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
        ),
        value_added_pathway = case_when(
            metric == 5 ~ 4,
            metric == 4 ~ 3,
            metric == 3 ~ 2,
            metric == 2 ~ 1,
            metric == 1 ~ 0
        )
    )

achievement <- district_accountability %>%
    filter(indicator == "Success Rate" & pathway == "AMO/Absolute") %>%
    transmute(system, subgroup,
        content_area = case_when(
            grade == "3rd through 5th" ~ "3-5 Success Rate",
            grade == "6th through 8th" ~ "6-8 Success Rate",
            grade == "9th through 12th" ~ "9-12 Success Rate"
        ),
        absolute_pathway = case_when(
            n_count < 30 ~ NA_real_,
            metric >= 50 ~ 4,
            metric >= 45 ~ 3,
            metric >= 35 ~ 2,
            metric >= 25 ~ 1,
            metric < 25 ~ 0
        ),
        amo_pathway = case_when(
            n_count < 30 | n_count_prior < 30 ~ NA_real_,
            metric >= amo_target_double ~ 4,
            metric >= amo_target ~ 3,
            upper_CI >= amo_target ~ 2,
            upper_CI > metric_prior ~ 1,
            upper_CI <= metric_prior ~ 0
        )
    ) %>%
    left_join(achievement_va, by = c("system", "subgroup", "content_area"))

absenteeism_va <- district_accountability %>%
    filter(indicator == "Chronic Absenteeism", pathway == "Value Added") %>%
    transmute(system, subgroup,
        content_area = "Chronically Out of School",
        value_added_pathway = case_when(
            percentile >= 80 ~ 4,
            percentile >= 60 ~ 3,
            percentile >= 40 ~ 2,
            percentile >= 20 ~ 1,
            percentile < 20 ~ 0
        )
    )

absenteeism <- district_accountability %>%
    filter(indicator == "Chronic Absenteeism", pathway == "AMO/Absolute") %>%
    transmute(system, subgroup,
        content_area = "Chronically Out of School",
        absolute_pathway = case_when(
            n_count < 30 ~ NA_real_,
            metric <= 8 ~ 4,
            metric <= 11.5 ~ 3,
            metric <= 16.5 ~ 2,
            metric <= 25 ~ 1,
            metric > 25 ~ 0
        ),
        amo_pathway = case_when(
            n_count < 30 | n_count_prior < 30 ~ NA_real_,
            metric <= amo_target_double ~ 4,
            metric <= amo_target ~ 3,
            lower_CI <= amo_target ~ 2,
            lower_CI < metric_prior ~ 1,
            lower_CI >= metric_prior ~ 0
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
            n_count < 30 ~ NA_real_,
            metric >= 60 ~ 4,
            metric >= 50 ~ 3,
            metric >= 40 ~ 2,
            metric >= 25 ~ 1,
            metric < 25 ~ 0
        ),
        amo_pathway = case_when(
            n_count < 30 | n_count_prior < 30 ~ NA_real_,
            metric >= amo_target_double ~ 4,
            metric >= amo_target ~ 3,
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
            n_count < 30 ~ NA_real_,
            metric >= 95 ~ 4,
            metric >= 90 ~ 3,
            metric >= 80 ~ 2,
            metric >= 67 ~ 1,
            metric < 67 ~ 0
        ),
        amo_pathway = case_when(
            n_count < 30 | n_count_prior < 30 ~ NA_real_,
            metric >= amo_target_double ~ 4,
            metric >= amo_target ~ 3,
            upper_CI >= amo_target ~ 2,
            upper_CI > metric_prior ~ 1,
            upper_CI <= metric_prior ~ 0
        )
    ) %>%
    left_join(grad_va, by = c("system", "subgroup", "content_area"))

heat_map <- bind_rows(achievement, absenteeism, ELPA, grad)

write_csv(heat_map, "N:/ORP_accountability/data/2018_final_accountability_files/heat_map.csv")

c(subgroups, all_students) %<-% (heat_map %>%
    mutate(overall_score = case_when(
        !is.na(absolute_pathway) & !is.na(amo_pathway) & !is.na(value_added_pathway) ~ (pmax(absolute_pathway, amo_pathway) + value_added_pathway)/2,
        !is.na(absolute_pathway) & !is.na(amo_pathway) & is.na(value_added_pathway) ~ pmax(absolute_pathway, amo_pathway),
        is.na(absolute_pathway) | is.na(amo_pathway) & !is.na(value_added_pathway) ~ value_added_pathway
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

schools <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv") %>%
    mutate(temp = 1) %>%
    group_by(system) %>%
    summarise(number_of_schools = sum(temp), reward = sum(reward), pct_reward_schools = 100 * reward/number_of_schools) %>%
    ungroup()

district_names <- readxl::read_excel("N:/ORP_accountability/data/2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>%
    janitor::clean_names() %>%
    transmute(system = as.integer(dg_4_lea_id_state), system_name = extra_item_lea_name) %>%
    distinct() %>%
    bind_rows(
        tribble(
            ~system, ~system_name,
            970, "Department of Children's Services"
        )
    )

districts_final <- minimum_performance %>%
    left_join(all_students_overall, by = "system") %>% 
    left_join(subgroups_overall, by = "system") %>%
    left_join(schools, by = "system") %>%
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
        number_of_schools, reward, pct_reward_schools,
        final_determination = case_when(
            met_minimum_performance == 0 ~ "In Need of Improvement",
            overall_average >= 3 ~ "Exemplary",
            overall_average >= 2 ~ "Advancing",
            overall_average >= 1 ~ "Satisfactory",
            overall_average < 1 ~ "Marginal"
        ),
        final_determination = if_else(
            final_determination == "In Need of Improvement" & achievement_determination == "Exemplary" & subgroup_determination == "Exemplary", "Exemplary", final_determination
        ),
        final_determination = case_when(
            final_determination == "In Need of Improvement" & pct_reward_schools >= 100/3 & overall_average >= 3 ~ "Exemplary",
            final_determination == "In Need of Improvement" & pct_reward_schools >= 100/3 & overall_average >= 2 ~ "Advancing",
            final_determination == "In Need of Improvement" & pct_reward_schools >= 100/3 & overall_average >= 1 ~ "Satisfactory",
            final_determination == "In Need of Improvement" & pct_reward_schools >= 100/3 & overall_average < 1 ~ "Marginal",
            TRUE ~ final_determination
        )
    ) %>%
    mutate_at(c("achievement_average", "subgroup_average", "overall_average"), ~ if_else(is.nan(.), NA_real_, .)) %>%
    left_join(district_names, by = "system") %>%
    select(system, system_name, everything())

write_csv(districts_final, path = "N:/ORP_accountability/data/2018_final_accountability_files/district_determinations.csv", na = "")
