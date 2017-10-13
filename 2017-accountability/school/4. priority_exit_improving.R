library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

# Priority schools not exiting in 2015
priority_schools <- read_csv("K:/ORP_accountability/projects/2015_school_coding/Output/priority_schools_not_exiting_ap.csv") %>%
    select(system, school, priority)

# Pools/Immune ----------------------------------------------------------------------------------------------------
pools_immune <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible) %>%
    filter(!is.na(pool))

high_schools <- sum(pools_immune$pool == "HS", na.rm = TRUE)
k8_schools <- sum(pools_immune$pool == "K8", na.rm = TRUE)

# Success Rates with Grad -----------------------------------------------------------------------------------------
one_year_success <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade)) %>%
    filter(year == 2017, subgroup == "All Students", grade %in% as.character(3:12),
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate")) %>%
    mutate(grade = as.numeric(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_on_track = if_else(subject == "Graduation Rate", grad_count, n_on_track),
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
    group_by(year, system, school, pool, designation_ineligible, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# One year success rates
    mutate(success_rate_2017 = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_))

# Priority Exit/Improving -----------------------------------------------------------------------------------------
priority_exit_improving <- one_year_success %>%
    group_by(pool, designation_ineligible) %>%
    mutate(rank_OM = if_else(valid_tests >= 30 & designation_ineligible == 0, 
            rank(success_rate_2017, ties.method = "max"), NA_integer_),
        denom = case_when(
            pool == "HS" ~ high_schools,
            pool == "K8" ~ k8_schools
        ),
        pctile_rank_OM = round5(100 * rank_OM/denom, 1)) %>%
    ungroup() %>%
    left_join(priority_schools, by = c("system", "school")) %>%
# Priority schools exit if one year success rate is greater than 15%
# Priority schools improve if one year success rate is greater than 10%
    mutate(priority = if_else(is.na(priority), 0L, priority),
        priority_improving = case_when(
            designation_ineligible == 1 ~ NA_integer_,
            priority == 1 & pctile_rank_OM > 10 & pctile_rank_OM <= 15 ~ 1L,
            TRUE ~ 0L
        ),
        priority_exit = case_when(
            designation_ineligible == 1 ~ NA_integer_,
            priority == 1 & pctile_rank_OM > 15 ~ 1L,
            TRUE ~ 0L
        )
    )

write_csv(priority_exit_improving, path = "K:/ORP_accountability/projects/2017_school_accountability/priority_exit_improving.csv", na = "")

# Priority not exiting for reward
priority_not_exiting <- filter(priority_exit_improving, priority == 1 & (priority_exit == 0L | is.na(priority_exit)))

write_csv(priority_not_exiting, path = "K:/ORP_accountability/projects/2017_school_accountability/priority_schools_not_exiting.csv", na = "")
