library(tidyverse)

base_subjects <- c("Math", "ELA", "Science", "Algebra I", "Algebra II", "Biology I", "Chemistry",
    "Integrated Math I", "Integrated Math II", "Integrated Math III", "Geometry",
    "English I", "English II", "English III", "US History")

numeric_subjects <- c("Math", "ELA", "Science", "HS Math", "HS English", "HS Science")
numeric_subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "Students with Disabilities", "English Learners", "Super Subgroup")

system_names <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/system_name_crosswalk.csv")

# Data download files are suppressed for n < 10 (*) or any individual proficiency band < 1% or > 99% (**)
suppress <- function(file, threshold = 1) {
    file %>%
        rowwise() %>% 
        mutate(temp = any(pct_below < threshold, pct_below > (100 - threshold),
            pct_approaching < threshold, pct_approaching > (100 - threshold),
            pct_on_track < threshold, pct_on_track > (100 - threshold),
            pct_mastered < threshold, pct_mastered > (100 - threshold))) %>%
        ungroup() %>%
        mutate_at(c("n_below", "n_approaching", "n_on_track", "n_mastered",
                "pct_below", "pct_approaching", "pct_on_track", "pct_mastered"),
            funs(if_else(temp, "**", as.character(.)))
        ) %>%
        select(-temp) %>%
        mutate_at(c("n_below", "n_approaching", "n_on_track", "n_mastered",
                "pct_below", "pct_approaching", "pct_on_track", "pct_mastered", "pct_on_mastered"),
            funs(if_else(valid_tests < 10, "*", as.character(.))))
}

# State Base
state_base_suppressed <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/state_base_2017_oct17.csv",
        col_types = c("iiccccddddddddddddddddddddddddd")) %>%
    filter(year == 2017, subject %in% base_subjects, grade != "Missing Grade") %>%
    select(year, system, system_name, subject, grade, subgroup,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>%
    arrange(grade, subject, subgroup) %>%
    suppress() %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 1 | as.numeric(pct_on_mastered) > 99) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(state_base_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/state_base_suppressed.csv", na = "")

# State Numeric
state_numeric_suppressed <- haven::read_dta("N:/ORP_accountability/data/2017_final_accountability_files/state_numeric_2017_JW_final_10242017.dta") %>%
    mutate(subject = if_else(subject == "HS ELA", "HS English", subject),
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup)) %>%
    filter(year == 2017, subject %in% numeric_subjects, subgroup %in% numeric_subgroups) %>%
    transmute(year, system = 0, system_name = "State of Tennessee", subject, grade, subgroup,
        valid_tests, n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
        pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv,
        pct_on_mastered = pct_ontrack_prof_adv) %>%
    arrange(grade, subject, subgroup) %>%
    suppress() %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 1 | as.numeric(pct_on_mastered) > 99) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(state_numeric_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/state_numeric_suppressed.csv", na = "")

# System Base
system_base_suppressed <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/system_base_2017_oct17.csv",
        col_types = c("iiccccddddddddddddddddddddddddd")) %>%
    filter(year == 2017, subject %in% base_subjects, grade != "Missing Grade") %>%
    select(year, system, system_name, subject, grade, subgroup,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>%
    arrange(system, grade, subject, subgroup) %>%
    suppress() %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 1 | as.numeric(pct_on_mastered) > 99) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(system_base_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/system_base_suppressed.csv", na = "")

# System Numeric
system_numeric_suppressed <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/Non-official files/system_numeric_2017_JW_10232017.csv") %>%
    mutate(subject = if_else(subject == "HS ELA", "HS English", subject),
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup)) %>%
    filter(year == 2017, subject %in% numeric_subjects, subgroup %in% numeric_subgroups) %>%
    left_join(system_names, by = "system") %>%
    select(year, system, system_name, subject, grade, subgroup,
        valid_tests, 
        n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
        pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv,
        pct_on_mastered = pct_ontrack_prof_adv) %>%
    arrange(system, grade, subject, subgroup) %>%
    suppress() %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 1 | as.numeric(pct_on_mastered) > 99) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(system_numeric_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/system_numeric_suppressed.csv", na = "")

## School files are suppressed for < 5% or > 95%
# School Base
school_base_suppressed <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_oct17.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(year == 2017, subject %in% base_subjects, grade != "Missing Grade") %>%
    select(year, system, school, subject, grade, subgroup,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>%
    arrange(system, school, grade, subject, subgroup) %>%
    suppress(threshold = 5) %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 5 | as.numeric(pct_on_mastered) > 95) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(school_base_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/school_base_suppressed.csv", na = "")

school_base_60_pct_suppressed <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(year == 2017, subject %in% base_subjects, grade != "Missing Grade") %>%
    select(year, system, school, subject, grade, subgroup,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered) %>%
    arrange(system, school, grade, subject, subgroup) %>%
    suppress(threshold = 5) %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 5 | as.numeric(pct_on_mastered) > 95) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(school_base_60_pct_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/school_base_60_pct_suppressed.csv", na = "")

# School Numeric
school_numeric_suppressed <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/Non-official files/school_numeric_2017_JW_wHSScience_11012017.csv") %>%
    mutate(subject = if_else(subject == "HS ELA", "HS English", subject),
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners", subgroup)) %>%
    filter(year == 2017, subject %in% numeric_subjects, subgroup %in% numeric_subgroups) %>%
    select(year, system, school, subject, grade, subgroup,
        valid_tests, n_below = n_below_bsc, n_approaching = n_approach_bsc, n_on_track = n_ontrack_prof, n_mastered = n_mastered_adv,
        pct_below = pct_below_bsc, pct_approaching = pct_approach_bsc, pct_on_track = pct_ontrack_prof, pct_mastered = pct_mastered_adv,
        pct_on_mastered = pct_ontrack_prof_adv) %>%
    arrange(system, school, grade, subject, subgroup) %>%
    suppress(threshold = 5) %>%
    mutate(pct_on_mastered = if_else((as.numeric(pct_on_mastered) < 5 | as.numeric(pct_on_mastered) > 95) & !is.na(as.numeric(pct_on_mastered)), 
        "**", as.character(pct_on_mastered)))

write_csv(school_numeric_suppressed, path = "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/school_numeric_suppressed.csv", na = "")
