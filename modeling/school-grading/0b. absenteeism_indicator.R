## Absenteeism Indicator for A-F School Grading

library(haven)
library(tidyverse)

absenteeism14 <- read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2013-14 Chronic Absenteeism by Subgroup.dta") %>%
    filter(subgroup %in% c("All Students", "BHN", "ED", "EL/T1/T2", "SWD", "Super")) %>%
    rename(system = districtnumber, school = schoolnumber) %>%
    mutate_at("subgroup", funs(recode(.,
        "BHN" = "Black/Hispanic/Native American",
        "ED" = "Economically Disadvantaged",
        "EL/T1/T2" = "English Learners",
        "SWD" = "Students with Disabilities",
        "Super" = "Super Subgroup"))) %>%
    mutate(pct_chronically_absent_prior = round(100 * (num_chronic + num_severe)/total_students_w_abs, 1),
        AMO_target = ifelse(total_students_w_abs >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/16, 1), NA),
        AMO_target_4 = ifelse(total_students_w_abs >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/8, 1), NA),
        system = ifelse(system == 792 & school %in% c(1, 6, 5, 195), 793, system),
        system = ifelse(system == 792 & school %in% c(3, 20, 30, 90, 150, 155, 7, 33, 95, 170, 25), 794, system),
        system = ifelse(system == 792 & school %in% c(8, 55, 60, 63, 65, 168, 183, 190), 795, system),
        system = ifelse(system == 792 & school %in% c(111, 109, 100, 70, 160), 796, system),
        system = ifelse(system == 792 & school == 116, 797, system),
        system = ifelse(system == 792 & school %in% c(130, 133, 123, 78), 798, system)) %>%
    select(system, school, subgroup, pct_chronically_absent_prior, AMO_target, AMO_target_4)

absenteeism <- read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2014-15 Chronic Absenteeism by Subgroup.dta") %>%
    filter(subgroup %in% c("All Students", "BHN", "ED", "EL/T1/T2", "SWD", "Super")) %>%
    mutate_at("subgroup", funs(recode(.,
        "BHN" = "Black/Hispanic/Native American",
        "ED" = "Economically Disadvantaged",
        "EL/T1/T2" = "English Learners",
        "SWD" = "Students with Disabilities",
        "Super" = "Super Subgroup"))) %>%
    transmute(system = districtnumber, school = schoolnumber, subgroup, enrolled = total_students_w_abs,
        pct_chronically_absent = round(100 * (num_chronic + num_severe)/total_students_w_abs, 1)) %>%
    left_join(absenteeism14, by = c("system", "school", "subgroup")) %>%
    mutate(grade_absenteeism_absolute = ifelse(pct_chronically_absent > 24, "F", NA),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent <= 24, "D", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent <= 17, "C", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent <= 12, "B", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(pct_chronically_absent <= 8, "A", grade_absenteeism_absolute),
        grade_absenteeism_absolute = ifelse(enrolled < 30, NA, grade_absenteeism_absolute),
        pct_chronically_absent = pct_chronically_absent/100,
        lower_bound_ci = round(100 * (enrolled/(enrolled + qnorm(0.975)^2)) * (pct_chronically_absent + ((qnorm(0.975)^2)/(2 * enrolled)) - 
            qnorm(0.975) * sqrt((pct_chronically_absent * (1 - pct_chronically_absent))/enrolled + (qnorm(0.975)^2)/(4 * enrolled^2))), 1),
        pct_chronically_absent = 100 * pct_chronically_absent,
        grade_absenteeism_reduction = ifelse(lower_bound_ci >= pct_chronically_absent_prior, "F", NA),
        grade_absenteeism_reduction = ifelse(lower_bound_ci < pct_chronically_absent_prior, "D", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(lower_bound_ci <= AMO_target, "C", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(pct_chronically_absent < AMO_target, "B", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(pct_chronically_absent <= AMO_target_4, "A", grade_absenteeism_reduction),
        grade_absenteeism_reduction = ifelse(enrolled < 30, NA, grade_absenteeism_reduction))

write_csv(absenteeism, path = "data/absenteeism_indicator.csv", na = "")
