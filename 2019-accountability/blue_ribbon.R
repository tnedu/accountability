library(acct)
library(tidyverse)

grad <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>%
    filter(
        not_na(pool),
        indicator == "Graduation Rate",
        subgroup == "All Students"
    ) %>%
    select(system, school, grad_rate = metric) %>%
    mutate(
        rank = if_else(not_na(grad_rate), rank(grad_rate, ties.method = "max"), NA_integer_),
        denom = sum(not_na(grad_rate)),
        grad_percentile = round5(100 * rank/denom, 1)
    ) %>%
    select(-rank, -denom)

sw <- read_delim("N:/ORP_accountability/data/2019_tdoe_provided_files/title_1_school_wide.txt", delim = ";")
ta <- read_delim("N:/ORP_accountability/data/2019_tdoe_provided_files/title_1_targeted_assistance.txt", delim = ";")

title_1 <- bind_rows(sw, ta) %>%
    transmute(system = as.numeric(` District No`), school = as.numeric(` School No`), title_1 = "Yes")

blue_ribbon <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>%
    filter(
        not_na(pool),
        indicator == "Achievement",
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged", "Students with Disabilities", "English Learners with Transitional 1-4")
    ) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible, success_rate = metric) %>%
    group_by(subgroup, pool) %>%
    mutate(
        rank = if_else(not_na(success_rate), rank(success_rate, ties.method = "max"), NA_integer_),
        denom = sum(not_na(success_rate)),
        percentile = round5(100 * rank/denom, 1)
    ) %>%
    ungroup() %>%
    select(-rank, -denom) %>%
    mutate(
        subgroup = case_when(
            subgroup == "All Students" ~ "All",
            subgroup == "Black/Hispanic/Native American" ~ "BHN",
            subgroup == "Economically Disadvantaged" ~ "ED",
            subgroup == "English Learners with Transitional 1-4" ~ "EL",
            subgroup == "Students with Disabilities" ~ "SWD"
        )
    ) %>%
    pivot_wider(names_from = subgroup, values_from = c(success_rate, percentile)) %>%
    rowwise() %>%
    mutate(
        subgroups_in_top_40 = sum(percentile_BHN >= 60, percentile_ED >= 60, percentile_SWD >= 60, percentile_EL >= 60, na.rm = TRUE),
        temp = mean(c(percentile_BHN >= 60, percentile_ED >= 60, percentile_SWD >= 60, percentile_EL >= 60), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(temp == 1 | is.nan(temp), percentile_All >= 85) %>%
    select(-temp) %>%
    left_join(grad, by = c("system", "school")) %>%
    filter(grad_percentile >= 85 | is.na(grad_percentile)) %>%
    left_join(title_1, by = c("system", "school")) %>%
    arrange(desc(success_rate_All)) %>%
    transmute(
        `District Number` = system,
        `District Name` = system_name,
        `School Number` = school,
        `School Name` = school_name,
        `Grade Pool` = pool,
        `Designation Ineligible` = designation_ineligible,
        `Success Rate` = success_rate_All,
        `Success Rate Percentile` = percentile_All,
        `BHN Success Rate` = success_rate_BHN,
        `BHN Success Rate Percentile` = percentile_BHN,
        `ED Success Rate` = success_rate_ED,
        `ED Success Rate Percentile` = percentile_ED,
        `SWD Success Rate` = success_rate_SWD,
        `SWD Success Rate Percentile` = percentile_SWD,
        `EL Success Rate` = success_rate_EL,
        `EL Success Rate Percentile` = percentile_EL,
        `Graduation Rate (Lagged)` = grad_rate,
        `Graduation Rate (Lagged) Percentile` = grad_percentile,
        `Exemplary High Performing` = 1L,
        `Number of Subgroups in Top 40 Pct` = subgroups_in_top_40,
        `Title I` = `title_1`,
        Comments = ""
    )

write_csv(blue_ribbon, "N:/ORP_accountability/projects/Data Requests/Blue Ribbon/blue_ribbon.csv", na = "")
