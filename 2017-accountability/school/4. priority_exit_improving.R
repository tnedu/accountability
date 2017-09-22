library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

priority_schools <- read_csv("K:/ORP_accountability/projects/2015_school_coding/Output/priority_schools_not_exiting_ap.csv") %>%
    select(system, school, priority)

pools_immune <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

school_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_sep18.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    left_join(pools_immune, by = c("system", "school")) %>%
    filter(year == 2017, subgroup == "All Students", grade %in% as.character(3:12)) %>%
    filter(subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate")) %>%
    mutate(grade = as.numeric(grade),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject)
    ) %>%
    group_by(year, system, school, pool, designation_ineligible, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    # Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(year, system, school, pool, designation_ineligible, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(pct_on_mastered = round(100 * (n_on_track + n_mastered)/valid_tests + 1e-10, 1))

priority_exit_improving <- school_base %>%
    group_by(pool, designation_ineligible) %>%
    mutate(rank_OM = if_else(valid_tests >= 30, rank(pct_on_mastered, ties.method = "max"), NA_integer_),
        denom = sum(valid_tests >= 30, na.rm = TRUE),
        pctile_rank_OM = round(100 * rank_OM/denom + 1e-10, 1)
    ) %>%
    ungroup() %>%
    left_join(priority_schools, by = c("system", "school")) %>%
    mutate(priority = if_else(is.na(priority), 0L, priority),
        priority_improving = as.integer(priority == 1 & pctile_rank_OM > 10 & pctile_rank_OM <= 15),
        priority_exit = as.integer(priority == 1 & pctile_rank_OM > 15)
    )

write_csv(priority_exit_improving, path = "K:/ORP_accountability/projects/2017_school_accountability/priority_exit_improving.csv", na = "")
