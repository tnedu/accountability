library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool)

school_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_sep27.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    inner_join(pools, by = c("system", "school")) %>%
    mutate(grade = if_else(subject == "ACT Composite", "12", grade)) %>%
    filter(year == 2017,
        !grade %in% c("All Grades", "Missing Grade"),
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "ACT Composite"),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "English Learners", "Students with Disabilities", "Super Subgroup"),
        !(subject == "Science" & grade %in% c("3", "4"))
    ) %>%
    mutate(grade = as.numeric(grade),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 5:8 ~ "Science",
            TRUE ~ subject
           )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, pool, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .)))

# One year success rates
one_year_success <- school_base %>%
    group_by(year, system, school, pool, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_))

AMO_targets <- one_year_success %>%
    mutate(year = 2018,
        AMO_target = amo_target(valid_tests, pct_on_mastered),
        AMO_target_4 = amo_target(valid_tests, pct_on_mastered, double = TRUE))

write_csv(AMO_targets, path = "K:/ORP_accountability/projects/2018_amo/school_success_rates.csv", na = "")
