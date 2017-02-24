library(tidyverse)

district_absenteeism_14 <- haven::read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2013-14 Chronic Absenteeism by Subgroup.dta") %>%
    mutate(districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(1, 6, 5, 195), 793, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(3, 20, 30, 90, 150, 155, 7, 33, 95, 170, 25), 794, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(8, 55, 60, 63, 65, 168, 183, 190), 795, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(111, 109, 100, 70, 160), 796, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber == 116, 797, districtnumber),
        districtnumber = ifelse(districtnumber == 792 & schoolnumber %in% c(130, 133, 123, 78), 798, districtnumber)) %>%
    transmute(system = districtnumber, subgroup, n_students_prior = total_students_w_abs,
        n_chronic_prior = num_chronic + num_severe, n_severe_prior = num_severe) %>%
    group_by(system, subgroup) %>%
    summarise_each(funs(sum), starts_with("n_")) %>%
    filter(!grepl("non-", subgroup)) %>% 
    mutate(pct_chronic_prior = 100 * n_chronic_prior/n_students_prior,
        pct_severe_prior = 100 * n_severe_prior/n_students_prior)

district_absenteeism_15 <- haven::read_dta("K:/Research and Policy/data/data_attendance/IT Files - Enrollment and Demographic/For Alex/2014-15 Chronic Absenteeism by Subgroup.dta") %>%
    transmute(system = districtnumber, system_name = districtname, subgroup, n_students = total_students_w_abs,
        n_chronic = num_chronic + num_severe, n_severe = num_severe) %>%
    group_by(system, system_name, subgroup) %>%
    summarise_each(funs(sum), starts_with("n_")) %>%
    filter(!grepl("non-", subgroup)) %>%
    mutate(pct_chronic = 100 * n_chronic/n_students,
        pct_severe = 100 * n_severe/n_students) %>%
    left_join(district_absenteeism_14, by = c("system", "subgroup"))

write_csv(district_absenteeism_15, path = "data/cohort_absenteeism.csv", na = "")

# Student Match
student_absenteeism_14 <- haven::read_dta("K:/Research and Policy/ORP_Data/Student_Information/Attendance/data_attendance/IT Files - Enrollment and Demographic/2014 CA enr race.dta") %>%
    transmute(studentid, chronic = gt10lt20 + gt20, severe = gt20) %>%
    group_by(studentid) %>%
    mutate(temp_chronic = max(chronic), temp_severe = max(severe)) %>%
    ungroup() %>%
    # If duplicates on student id, keep if severe or chronic
    filter(severe == temp_severe) %>%
    filter(chronic == temp_chronic) %>%
    # Force drop duplicates
    mutate(temp = duplicated(studentid)) %>%
    filter(!temp) %>%
    transmute(studentid, chronic_prior = chronic, severe_prior = severe)

student_match <- haven::read_dta("K:/Research and Policy/ORP_Data/Student_Information/Attendance/data_attendance/IT Files - Enrollment and Demographic/2015 CA enr race.dta") %>%
    transmute(studentid, system = districtnumber, system_name = districtname, school = schoolnumber, school_name = schoolname,
        n_days = totaldaysenrolled, chronic = gt10lt20 + gt20, severe = gt20) %>%
    left_join(student_absenteeism_14, by = "studentid") %>%
    filter(chronic_prior == 1) %>%
    mutate(no_longer_chronic = chronic_prior == 1 & chronic == 0) %>%
    group_by(system, system_name) %>%
    summarise_each(funs(sum), no_longer_chronic, chronic_prior) %>%
    ungroup() %>%
    mutate(pct_no_longer_chronic = ifelse(chronic_prior >= 30, 100 * no_longer_chronic/chronic_prior, NA),
        pct_no_longer_chronic_scaled = scale(pct_no_longer_chronic))

write_csv(student_match, path = "data/student_match_absenteeism.csv", na = "")
