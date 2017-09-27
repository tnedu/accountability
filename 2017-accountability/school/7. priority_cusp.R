# Priority Cusp Schools - Bottom 10% of Schools Based on a 1 Year Success Rate using 2018 rules

library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools_immune <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

high_schools <- ceiling(0.1 * sum(pools_immune$pool == "HS", na.rm = TRUE))
k8_schools <- ceiling(0.1 * sum(pools_immune$pool == "K8", na.rm = TRUE))

one_year_success <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_sep26.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    mutate(grade = if_else(subject == "ACT Composite", "12", grade)) %>%
    filter(year == 2017, subgroup == "All Students", grade %in% as.character(3:12)) %>%
    filter(subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "ACT Composite")) %>%
    mutate(grade = as.numeric(grade),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, pool, designation_ineligible, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
# One year success rates
    group_by(year, system, school, pool, designation_ineligible, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(pct_on_mastered = round5(100 * (n_on_track + n_mastered)/valid_tests, 1))

# Priority cusp schools are schools at or below 10th percentile in each school based on one year success rate
priority_cusp <- one_year_success %>%
    group_by(pool, designation_ineligible) %>%
    mutate(rank_OM = if_else(valid_tests >= 30, rank(pct_on_mastered, ties.method = "min"), NA_integer_)) %>%
    ungroup() %>%
    mutate(priority_cusp = if_else(designation_ineligible == 0 & pool == "HS" & rank_OM <= high_schools, 1, 0),
        priority_cusp = if_else(designation_ineligible == 0 & pool == "K8" & rank_OM <= k8_schools, 1, priority_cusp))

write_csv(priority_cusp, path = "K:/ORP_accountability/projects/2017_school_accountability/priority_cusp.csv", na = "")
