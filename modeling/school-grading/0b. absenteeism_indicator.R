## Absenteeism Indicator for A-F School Grading

library(acct)
library(haven)
library(tidyverse)

absenteeism14 <- read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2013-14 Chronic Absenteeism by Subgroup.dta") %>%
    filter(subgroup %in% c("All Students", "BHN", "ED", "EL/T1/T2", "SWD", "Super")) %>%
    rename(system = districtnumber, school = schoolnumber) %>%
    mutate(subgroup = case_when(
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL/T1/T2" ~ "English Learners",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            TRUE ~ subgroup
        ),
        AMO_target = amo_target(total_students_w_abs, pct_chronically_absent_prior),
        AMO_target_4 = amo_target(total_students_w_abs, pct_chronically_absent_prior, double = TRUE),
        system = case_when(
            system == 792 & school %in% c(1, 6, 5, 195) ~ 793,
            system == 792 & school %in% c(3, 20, 30, 90, 150, 155, 7, 33, 95, 170, 25) ~ 794,
            system == 792 & school %in% c(8, 55, 60, 63, 65, 168, 183, 190) ~ 795,
            system == 792 & school %in% c(111, 109, 100, 70, 160) ~ 796,
            system == 792 & school == 116 ~ 797,
            system == 792 & school %in% c(130, 133, 123, 78) ~ 798,
            TRUE ~ system
        )
    ) %>%
    select(system, school, subgroup, pct_chronically_absent_prior, AMO_target, AMO_target_4)

absenteeism <- read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2014-15 Chronic Absenteeism by Subgroup.dta") %>%
    filter(subgroup %in% c("All Students", "BHN", "ED", "EL/T1/T2", "SWD", "Super")) %>%
    transmute(system = districtnumber, school = schoolnumber,
        subgroup = case_when(
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL/T1/T2" ~ "English Learners",
            subgroup == "SWD" ~ "Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            TRUE ~ subgroup
        ), 
        enrolled = total_students_w_abs,
        pct_chronically_absent = round5(100 * (num_chronic + num_severe)/total_students_w_abs, 1)) %>%
    left_join(absenteeism14, by = c("system", "school", "subgroup")) %>%
    mutate(lower_bound_ci = ci_lower_bound(enrolled, pct_chronically_absent),
        grade_absenteeism = case_when(
            enrolled < 30 ~ NA_character_,
            pct_chronically_absent <= 8 ~ "A",
            pct_chronically_absent <= 12 ~ "B",
            pct_chronically_absent <= 17 ~ "C",
            pct_chronically_absent <= 24 ~ "D",
            pct_chronically_absent > 24 ~ "F"
        ),
        grade_absenteeism_reduction = case_when(
            enrolled < 30 ~ NA_character_,
            pct_chronically_absent <= AMO_target_4 ~ "A",
            pct_chronically_absent < AMO_target ~ "B",
            lower_bound_ci <= AMO_target ~ "C",
            lower_bound_ci < pct_chronically_absent_prior ~ "D",
            lower_bound_ci >= pct_chronically_absent_prior ~ "F"
        )
    )

write_csv(absenteeism, path = "data/absenteeism_indicator.csv", na = "")
