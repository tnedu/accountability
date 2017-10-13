library(tidyverse)

# District and School Names ---------------------------------------------------------------------------------------
system_names <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/system_name_crosswalk.csv")

school_names <- readxl::read_excel("K:/ORP_accountability/data/2017_final_accountability_files/school_name_crosswalk.xlsx") %>%
    select(system, school, school_name) %>%
    inner_join(system_names, by = "system") %>%
    select(system, system_name, school, school_name)

# Pools/Immune ----------------------------------------------------------------------------------------------------
pools <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible) %>%
    filter(!is.na(pool))

# Summary File Metrics --------------------------------------------------------------------------------------------
priority <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/priority_exit_improving.csv",
        col_types = c("iiicicdiidiidiii")) %>%
    select(system, school, subgroup, success_rate_2017, success_rate_2017_pctile = pctile_rank_OM)

focus <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/focus_metrics.csv")

reward <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/reward_metrics.csv")

summary <- priority %>%
    bind_rows(focus) %>%
    full_join(reward, by = c("system", "school", "subgroup")) %>%
    mutate(success_rate_2017 = if_else(
            subgroup %in% c("Black/Hispanic/Native American", "Economically Disadvantaged",
                "Students with Disabilities", "English Learners with T1/T2"),
            pct_on_mastered_target, success_rate_2017)) %>%
    right_join(pools, by = c("system", "school")) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, subgroup, pool, designation_ineligible,
        success_rate_2017, success_rate_2017_pctile, pct_below_2015, pct_below_2016, pct_below_2017,
        pctile_below_2015, pctile_below_2017, pctile_below_reduction, pct_below_reduction,
        success_rate_comparison = pct_on_mastered_comparison, gap, pctile_rank_gap) %>%
    arrange(system, school, subgroup) %>%
# Fill in missing names
    mutate(system_name = if_else(system == 792, "Shelby County", system_name),
        system_name = if_else(system == 794, "Bartlett City", system_name),
        system_name = if_else(system == 985, "Achievement School District", system_name),
        school_name = if_else(system == 792 & school == 2075, "Carnes Elementary", school_name),
        school_name = if_else(system == 792 & school == 2535, "Northside High School", school_name),
        school_name = if_else(system == 794 & school == 170, "Bartlett 9th Grade Academy", school_name),
        school_name = if_else(system == 985 & school == 8035, "Klondike Perparatory Academy", school_name),
        school_name = if_else(system == 985 & school == 8080, "KIPP Memphis University Middle", school_name))

write_csv(summary, path = "K:/ORP_accountability/projects/2017_school_accountability/school_summary_file.csv", na = "")
