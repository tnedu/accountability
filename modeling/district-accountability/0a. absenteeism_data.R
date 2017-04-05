## Chronic Absenteeism for District Accountability

library(haven)
library(tidyverse)

# Cohort Absenteeism
absenteeism_prior <- read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2013-14 Chronic Absenteeism by Subgroup.dta") %>%
    rename(system = districtnumber, school = schoolnumber) %>%
    filter(!system == 0) %>%
    mutate(system = ifelse(system == 792 & school %in% c(1, 6, 5, 195), 793, system),
        system = ifelse(system == 792 & school %in% c(3, 20, 30, 90, 150, 155, 7, 33, 95, 170, 25), 794, system),
        system = ifelse(system == 792 & school %in% c(8, 55, 60, 63, 65, 168, 183, 190), 795, system),
        system = ifelse(system == 792 & school %in% c(111, 109, 100, 70, 160), 796, system),
        system = ifelse(system == 792 & school == 116, 797, system),
        system = ifelse(system == 792 & school %in% c(130, 133, 123, 78), 798, system)) %>%
    mutate_at("subgroup", funs(recode(.,
        "BHN" = "Black/Hispanic/Native American",
        "ED" = "Economically Disadvantaged",
        "EL/T1/T2" = "English Learners",
        "SWD" = "Students with Disabilities",
        "Super" = "Super Subgroup"))) %>%
    group_by(system, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), n_students_prior = total_students_w_abs,
        n_chronic_prior = num_chronic, n_severe_prior = num_severe) %>%
    ungroup() %>%
    transmute(system, subgroup,
        pct_chronically_absent_prior = round(100 * (n_chronic_prior + n_severe_prior)/n_students_prior, 1),
        AMO_target = ifelse(n_students_prior >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/16, 1), NA),
        AMO_target_4 = ifelse(n_students_prior >= 30, round(pct_chronically_absent_prior - pct_chronically_absent_prior/8, 1), NA))

absenteeism <- read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2014-15 Chronic Absenteeism by Subgroup.dta") %>%
    rename(system = districtnumber) %>%
    filter(!system == 0) %>%
    mutate_at("subgroup", funs(recode(.,
        "BHN" = "Black/Hispanic/Native American",
        "ED" = "Economically Disadvantaged",
        "EL/T1/T2" = "English Learners",
        "SWD" = "Students with Disabilities",
        "Super" = "Super Subgroup"))) %>%
    filter(!(grepl("non-", subgroup))) %>%
    group_by(system, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), n_students = total_students_w_abs, n_chronic = num_chronic, n_severe = num_severe) %>%
    ungroup() %>%
    mutate(pct_chronically_absent = ifelse(n_students >= 30, round((n_chronic + n_severe)/n_students, 3), NA),
        lower_bound_ci = round(100 * (n_students/(n_students + qnorm(0.975)^2)) * (pct_chronically_absent + ((qnorm(0.975)^2)/(2 * n_students)) -
            qnorm(0.975) * sqrt((pct_chronically_absent * (1 - pct_chronically_absent))/n_students + (qnorm(0.975)^2)/(4 * n_students^2))), 1),
        pct_chronically_absent = 100 * pct_chronically_absent) %>%
    left_join(absenteeism_prior, by = c("system", "subgroup")) %>%
    select(-n_chronic, -n_severe) %>%
    group_by(subgroup) %>%
    mutate(rank_CA = ifelse(n_students >= 30, rank(pct_chronically_absent, ties.method = "min"), NA),
        CA_denom = sum(n_students >= 30, na.rm = TRUE),
        CA_quintile = ifelse(rank_CA/CA_denom <= 0.2, 4, NA),
        CA_quintile = ifelse(rank_CA/CA_denom > 0.2, 3, CA_quintile),
        CA_quintile = ifelse(rank_CA/CA_denom > 0.4, 2, CA_quintile),
        CA_quintile = ifelse(rank_CA/CA_denom > 0.6, 1, CA_quintile),
        CA_quintile = ifelse(rank_CA/CA_denom > 0.8, 0, CA_quintile),
        CA_AMO = ifelse(lower_bound_ci >= pct_chronically_absent_prior, 0, NA),
        CA_AMO = ifelse(lower_bound_ci < pct_chronically_absent_prior, 1, CA_AMO),
        CA_AMO = ifelse(lower_bound_ci <= AMO_target, 2, CA_AMO),
        CA_AMO = ifelse(pct_chronically_absent < AMO_target, 3, CA_AMO),
        CA_AMO = ifelse(pct_chronically_absent <= AMO_target_4, 4, CA_AMO),
        CA_AMO = ifelse(n_students < 30, NA, CA_AMO)) %>%
    ungroup()

write_csv(absenteeism, path = "data/cohort_absenteeism.csv", na = "")

# Student Match Absenteeism
student_absenteeism_14 <- read_dta("K:/Research and Policy/ORP_Data/Student_Information/Attendance/data_attendance/IT Files - Enrollment and Demographic/2014 CA enr race.dta") %>%
    transmute(studentid, chronic = gt10lt20 + gt20) %>%
    filter(chronic == 1) %>%
    group_by(studentid) %>%
    mutate(temp_chronic = max(chronic)) %>%
    ungroup() %>%
# If duplicates on student id, keep if chronic
    filter(chronic == temp_chronic) %>%
# Force drop duplicates
    mutate(temp = duplicated(studentid)) %>%
    filter(!temp) %>%
    select(studentid, chronic_prior = chronic)

district_CA_reduction <- read_dta("K:/Research and Policy/ORP_Data/Student_Information/Attendance/data_attendance/IT Files - Enrollment and Demographic/2015 CA enr race.dta") %>%
    transmute(studentid, system = districtnumber, system_name = districtname, school = schoolnumber, school_name = schoolname,
        n_days = totaldaysenrolled, chronic = gt10lt20 + gt20) %>%
    left_join(student_absenteeism_14, by = "studentid") %>%
    mutate(no_longer_chronic = chronic_prior == 1 & chronic == 0) %>%
    group_by(system, system_name) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), chronic_prior, no_longer_chronic) %>%
    ungroup() %>%
    mutate(pct_no_longer_chronic = ifelse(chronic_prior >= 30, round(100 * no_longer_chronic/chronic_prior, 1), NA),
        rank_CA_reduction = ifelse(chronic_prior >= 30, rank(pct_no_longer_chronic, ties.method = "max"), NA),
        CA_reduction_denom = sum(chronic_prior >= 30, na.rm = TRUE),
        CA_reduction_quintile = ifelse(rank_CA_reduction/CA_reduction_denom < 0.2, 0, NA),
        CA_reduction_quintile = ifelse(rank_CA_reduction/CA_reduction_denom >= 0.2, 1, CA_reduction_quintile),
        CA_reduction_quintile = ifelse(rank_CA_reduction/CA_reduction_denom >= 0.4, 2, CA_reduction_quintile),
        CA_reduction_quintile = ifelse(rank_CA_reduction/CA_reduction_denom >= 0.6, 3, CA_reduction_quintile),
        CA_reduction_quintile = ifelse(rank_CA_reduction/CA_reduction_denom >= 0.8, 4, CA_reduction_quintile))

write_csv(district_CA_reduction, path = "data/student_match_absenteeism.csv", na = "")

# Check how chronic reduction maps against percent ED
profile <- readxl::read_excel("data/data_2015_district_profile.xlsx") %>%
    select(system = DISTRICT, pct_ED = ECONOMICALLY_DISADVANTAGED_PCT)

left_join(district_CA_reduction, profile, by = "system") %>%
    ggplot(aes(x = pct_ED, y = pct_no_longer_chronic)) +
    geom_point()
