library(tidyverse)

# System Numeric --------------------------------------------------------------------------------------------------
system_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/system_base_2017_oct17.csv",
        col_types = c("ddccccddddddddddddddddddddddddd")) %>%
    filter(year == 2017,
        grade == "All Grades",
        !subject %in% c("Graduation Rate", "ACT Composite", "ACT Math", "ACT Reading"))
    
system_numeric <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/Non-official files/system_numeric_2017_JW_10232017.csv") %>%
    filter(year == 2017,
        subject %in% c("HS Math", "HS ELA", "HS Science"),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
        pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv,
        pct_on_mastered = pct_ontrack_prof_adv) %>%
    bind_rows(system_base) %>%
    transmute(Level = "System", year, system, system_name = "", school = 0, school_name = "",
        subject = if_else(subject == "HS ELA", "HS English", subject),
        grade, subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup),
        participation_rate_1yr = NA, participation_rate_2yr = NA, participation_rate_3yr = NA,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered,
        pct_below_approaching = 100 - pct_on_mastered, pct_on_mastered,
        amo_target = NA, gap_size = NA, gap_target = NA, grad_cohort = NA, grad_rate = NA,
        tvaas = NA, upper_bound_ci = NA, red_perc_below_or_bsc_1yr = NA, red_perc_below_or_bsc_2yr = NA,
        red_perc_below_or_bsc_3yr = NA, year_to_year_diff = NA, maas_adjusted_amo_target = NA,
        maas_adjusted_gap_target = NA, maas_adjusted_prior_pct_prof_adv = NA, maas_adj_year_to_year_diff = NA)

write_csv(system_numeric, "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ReportCard_Numeric_Part_Prof.csv", na = "")

# State -----------------------------------------------------------------------------------------------------------
state_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/state_base_2017_oct17.csv",
        col_types = c("iiccccddddddddddddddddddddddddd")) %>%
    filter(year == 2017,
        grade == "All Grades",
        !subject %in% c("Graduation Rate", "ACT Composite", "ACT Math", "ACT Reading"))

state_numeric <- haven::read_dta("K:/ORP_accountability/data/2017_final_accountability_files/Non-official files/state_numeric_2017_JW_final_10172017.dta") %>%
    filter(year == 2017,
        subject %in% c("HS Math", "HS ELA", "HS Science"),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
        pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv,
        pct_on_mastered = pct_ontrack_prof_adv) %>%
    bind_rows(state_base) %>%
    transmute(year, subject = if_else(subject == "HS ELA", "HS English", subject),
        grade, subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup),
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        system = "", pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

write_csv(state_numeric, "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ReportCard_state_complete.csv", na = "")


# District --------------------------------------------------------------------------------------------------------
system_numeric %>%
    select(year, system, subject, grade, subgroup, valid_tests,
        n_below, n_approaching, n_on_track, n_mastered,
        pct_on_track, pct_mastered, pct_on_mastered, pct_approaching, pct_below) %>%
    write_csv("K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ReportCard_district_complete.csv", na = "")


# School ----------------------------------------------------------------------------------------------------------
school_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_oct17.csv",
        col_types = c("ddccccddddddddddddddddddddddddd")) %>%
    filter(year == 2017,
        grade == "All Grades",
        !subject %in% c("Graduation Rate", "ACT Composite", "ACT Math", "ACT Reading")) %>%
    mutate(school = as.numeric(school))

school_numeric <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/Non-official files/school_numeric_2017_JW_wHSScience_11012017.csv") %>%
    filter(year == 2017,
        subject %in% c("HS Math", "HS ELA", "HS Science"),
        subgroup %in% c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "English Language Learners with T1/T2", "Students with Disabilities")) %>%
    rename(n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
        pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv,
        pct_on_mastered = pct_ontrack_prof_adv) %>%
    bind_rows(school_base) %>%
    transmute(year, system, school,
        subject = if_else(subject == "HS ELA", "HS English", subject),
        grade, subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup),
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_on_track, pct_mastered, pct_on_mastered, pct_approaching, pct_below, tab = "B")

write_csv(school_numeric, "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ReportCard_school_complete.csv", na = "")
