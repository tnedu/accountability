## District Accountability - Final Designations

library(tidyverse)

# Achievement and Subgroup Averages
achievement <- read_csv("data/achievement_scores.csv")
subgroup <- read_csv("data/subgroup_scores.csv")

# Final Designations
final_designations <- read_csv("data/minimum_performance.csv") %>%
    select(system, system_name, met_minimum_performance_goal) %>%
    left_join(achievement, by = "system") %>%
    left_join(subgroup, by = "system") %>%
    rowwise() %>%
    mutate(overall_average = sum(0.6 * achievement_average, 0.4 * subgroup_average, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(final_designation = ifelse(overall_average <= 1, "Marginal", NA),
        final_designation = ifelse(overall_average > 1, "Satisfactory", final_designation),
        final_designation = ifelse(overall_average > 2, "Advancing", final_designation),
        final_designation = ifelse(overall_average > 3, "Exemplary", final_designation),
        final_designation = ifelse(!met_minimum_performance_goal, "In Need of Improvement", final_designation))

write_csv(final_designations, path = "data/final_designations.csv", na = "")
