library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

gap_subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged",
    "Students with Disabilities", "English Learners with T1/T2")
comparison_subgroups <- c("All Students", "Non-Economically Disadvantaged",
    "Non-Students with Disabilities", "Non-English Learners/T1 or T2")

# Priority and Focus Schools --------------------------------------------------------------------------------------
focus_schools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/focus_schools_not_exiting.csv")
priority_schools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/priority_schools_not_exiting.csv")

priority_focus <- bind_rows(focus_schools, priority_schools) %>%
    transmute(system, school, priority_focus = 1)

# Pools/Immune ----------------------------------------------------------------------------------------------------
pools_immune <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

high_schools <- sum(pools_immune$pool == "HS", na.rm = TRUE)
k8_schools <- sum(pools_immune$pool == "K8", na.rm = TRUE)

grad_only_BHN <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    transmute(system, school, subgroup = "Black/Hispanic/Native American",
        grad_only_target = grad_only_BHN, grad_only_comparison = grad_only_All)

grad_only_ED <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    transmute(system, school, subgroup = "Economically Disadvantaged",
        grad_only_target = grad_only_ED, grad_only_comparison = grad_only_Non_ED)

grad_only_SWD <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    transmute(system, school, subgroup = "Students with Disabilities",
        grad_only_target = grad_only_SWD, grad_only_comparison = grad_only_Non_SWD)

grad_only_EL <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    transmute(system, school, subgroup = "English Learners with T1/T2",
        grad_only_target = grad_only_EL, grad_only_comparison = grad_only_Non_EL)

grad_only <- bind_rows(grad_only_BHN, grad_only_ED, grad_only_SWD, grad_only_EL)

# TVAAS for Reward Progress ---------------------------------------------------------------------------------------
tvaas <- readxl::read_excel("K:/ORP_accountability/data/2017_tvaas/Schoolwide Composite Index.xlsx") %>%
    transmute(system = as.integer(`District Number`), school = as.integer(`School Number`),
        tvaas_index = `TCAP/EOC: School-Wide: Composite`)

# Success Rates for Reward Performance ----------------------------------------------------------------------------
one_year_success <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade)) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    filter(year == 2017,
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate"),
        grade %in% as.character(3:12),
        subgroup %in% c(gap_subgroups, comparison_subgroups)) %>%
    mutate(grade = as.integer(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_below = if_else(subject == "Graduation Rate", dropout_count, n_below),
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
    mutate(pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_))

# Reward Exemption ------------------------------------------------------------------------------------------------
gaps_comparison <- one_year_success %>%
    filter(subgroup %in% comparison_subgroups) %>%
    transmute(year, system, school, pool, designation_ineligible,
        subgroup = case_when(subgroup == "All Students" ~ "Black/Hispanic/Native American",
            subgroup == "Non-Economically Disadvantaged" ~ "Economically Disadvantaged",
            subgroup == "Non-Students with Disabilities" ~ "Students with Disabilities",
            subgroup == "Non-English Learners/T1 or T2" ~ "English Learners with T1/T2"
        ),
    pct_on_mastered_comparison = pct_on_mastered)

reward_exemption <- one_year_success %>%
    filter(subgroup %in% gap_subgroups) %>%
    select(year, system, school, pool, designation_ineligible, subgroup, pct_on_mastered_target = pct_on_mastered) %>%
    full_join(gaps_comparison, by = c("year", "system", "school", "pool", "designation_ineligible", "subgroup")) %>%
    mutate(gap = pct_on_mastered_comparison - pct_on_mastered_target) %>%
    left_join(grad_only, by = c("system", "school", "subgroup")) %>%
# Blank out gaps if either subgroup is grad only
    mutate(gap = if_else(grad_only_target == 1 | grad_only_comparison == 1, NA_real_, gap)) %>%
    group_by(designation_ineligible, pool, subgroup) %>%
    mutate(rank_gap = if_else(designation_ineligible == 0 & !is.na(gap), rank(gap, ties.method = "max"), NA_integer_),
        denom = if_else(designation_ineligible == 0, sum(!is.na(gap)), NA_integer_)) %>%
    ungroup() %>%
    mutate(pctile_rank_gap = round5(100 * rank_gap/denom, 1),
        reward_exemption = as.integer(pctile_rank_gap >= 75)) %>%
    group_by(system, school) %>%
    summarise(reward_exemption = max(reward_exemption, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(reward_exemption = if_else(reward_exemption == -Inf, 0, reward_exemption))

# Reward Performance and Progress ---------------------------------------------------------------------------------
reward <- one_year_success %>%
    filter(subgroup == "All Students") %>%
    left_join(tvaas, by = c("system", "school")) %>%
    full_join(reward_exemption, by = c("system", "school")) %>%
    full_join(priority_focus, by = c("system", "school")) %>%
    mutate(priority_focus = if_else(is.na(priority_focus), 0, priority_focus)) %>%
    group_by(pool, designation_ineligible, priority_focus, reward_exemption) %>%
    mutate(rank_performance = if_else(
            designation_ineligible == 0 & !is.na(pool) & priority_focus == 0 & reward_exemption == 0, 
                rank(-pct_on_mastered, ties = "min"), NA_integer_),
        rank_progress = if_else(
            designation_ineligible == 0 & !is.na(pool) & priority_focus == 0 & reward_exemption == 0,
                rank(-tvaas_index, ties = "min"), NA_integer_),
    # Reward performance are 5 percent of non-exempt schools by pool with highest one-year success rate
        reward_performance = if_else(pool == "HS", rank_performance <= ceiling(0.05 * high_schools), NA),
        reward_performance = if_else(pool == "K8", rank_performance <= ceiling(0.05 * k8_schools), reward_performance),
        reward_progress = FALSE) %>%
    ungroup()

# Reward progress are 5 percent of non-exempt schools by pool with highest TVAAS composite index
reward_hs_count <- nrow(reward[reward$reward_progress == TRUE & reward$pool == "HS" & is.na(reward$reward_performance), ])
    
while (reward_hs_count < ceiling(0.05 * high_schools)) {

    reward <- reward %>%
        arrange(pool, designation_ineligible, reward_exemption, priority_focus, reward_progress, desc(tvaas_index))

    reward[1, ]$reward_progress <- TRUE

# Update count if selected a unique reward progress school
    reward_hs_count <- nrow(reward[reward$reward_progress == TRUE & reward$pool == "HS" & reward$reward_performance == FALSE, ])

}

reward_k8_count <- nrow(reward[reward$reward_progress == TRUE & reward$pool == "K8" & is.na(reward$reward_performance), ])

while (reward_k8_count < ceiling(0.05 * k8_schools)) {
    
    reward <- reward %>%
        arrange(desc(pool), designation_ineligible, reward_exemption, priority_focus, reward_progress, desc(tvaas_index))
    
    reward[1, ]$reward_progress <- TRUE
    
# Update count if selected a unique reward progress school
    reward_k8_count <- nrow(reward[reward$reward_progress == TRUE & reward$pool == "K8" & reward$reward_performance == FALSE, ])
    
}

output <- reward %>%
    transmute(system, school, pool, designation_ineligible, pct_on_mastered, tvaas_index, reward_exemption,
        reward_performance = if_else(reward_performance, 1L, 0L),
        reward_progress = if_else(reward_progress, 1L, 0L))

write_csv(output, "K:/ORP_accountability/projects/2017_school_accountability/reward.csv")


# School Summary File Metrics -------------------------------------------------------------------------------------
reward_exemption_output <- one_year_success %>%
    filter(subgroup %in% gap_subgroups) %>%
    select(year, system, school, pool, designation_ineligible, subgroup, pct_on_mastered_target = pct_on_mastered) %>%
    full_join(gaps_comparison, by = c("year", "system", "school", "pool", "designation_ineligible", "subgroup")) %>%
    mutate(gap = pct_on_mastered_comparison - pct_on_mastered_target) %>%
    left_join(grad_only, by = c("system", "school", "subgroup")) %>%
# Blank out gaps if either subgroup is grad only
    mutate(gap = if_else(grad_only_target == 1 | grad_only_comparison == 1, NA_real_, gap)) %>%
    group_by(designation_ineligible, pool, subgroup) %>%
    mutate(rank_gap = if_else(designation_ineligible == 0 & !is.na(gap), rank(gap, ties.method = "max"), NA_integer_),
        denom = if_else(designation_ineligible == 0, sum(!is.na(gap)), NA_integer_)) %>%
    ungroup() %>%
    transmute(system, school, pool, subgroup, gap,
        pctile_rank_gap = round5(100 * rank_gap/denom, 1),
        reward_exemption = as.integer(pctile_rank_gap >= 75))

write_csv(reward_exemption_output, path = "K:/ORP_accountability/projects/2017_school_accountability/reward_metrics.csv", na = "")
