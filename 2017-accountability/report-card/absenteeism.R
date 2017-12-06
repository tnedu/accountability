library(tidyverse)

# Data download files are suppressed for n < 10 (*) or < 1% or > 99% (**)
suppress <- function(file, threshold = 1) {
    file %>%
        rowwise() %>% 
        mutate(temp = any(pct_chronically_absent < threshold, pct_chronically_absent > (100 - threshold))) %>%
        ungroup() %>%
        mutate_at(c("n_chronically_absent", "pct_chronically_absent"),
            funs(if_else(temp, "**", as.character(.)))
        ) %>%
        select(-temp) %>%
        mutate_at(c("n_chronically_absent", "pct_chronically_absent"),
            funs(if_else(n_students < 10, "*", as.character(.))))
}

state_abs <- read_csv("K:/ORP_accountability/data/2017_chronic_absenteeism/state_chronic_absenteeism.csv") %>%
    transmute(year, system, system_name, subgroup,
        grade_band = case_when(
            grade_band == "9-12" ~ "9th through 12th",
            grade_band == "K-8" ~ "K through 8th",
            TRUE ~ grade_band
        ),
        n_students, n_chronically_absent, pct_chronically_absent) %>%
    suppress()

write_csv(state_abs, "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/state_absenteeism_suppressed.csv", na = "")

system_abs <- read_csv("K:/ORP_accountability/data/2017_chronic_absenteeism/system_chronic_absenteeism.csv",
    col_types = c("iicccddddddd")) %>%
    transmute(year, system, system_name, subgroup,
        grade_band = case_when(
            grade_band == "9-12" ~ "9th through 12th",
            grade_band == "K-8" ~ "K through 8th",
            TRUE ~ grade_band
        ),
        n_students, n_chronically_absent, pct_chronically_absent) %>%
    suppress()

write_csv(system_abs, "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/system_absenteeism_suppressed.csv", na = "")

school_abs <- read_csv("K:/ORP_accountability/data/2017_chronic_absenteeism/school_chronic_absenteeism.csv",
    col_types = c("iicicccddddddd")) %>%
    transmute(year, system, system_name, school, school_name, subgroup,
        grade_band = case_when(
            grade_band == "9-12" ~ "9th through 12th",
            grade_band == "K-8" ~ "K through 8th",
            TRUE ~ grade_band
        ),
        n_students, n_chronically_absent, pct_chronically_absent) %>%
    suppress()

write_csv(school_abs, "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/school_absenteeism_suppressed.csv", na = "")
