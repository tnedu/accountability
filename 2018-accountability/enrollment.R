library(tidyverse)

student_absenteeism <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/student_chronic_absenteeism_grainger_correction.csv")

school_enrollment <- student_absenteeism %>%
    select(system, school, student_id, isp_days, instructional_calendar_days) %>%
    distinct() %>%
    group_by(system, school, student_id) %>%
    summarise(isp_days = sum(isp_days), instructional_calendar_days = max(instructional_calendar_days)) %>%
    ungroup() %>%
    mutate(enrolled_50_pct_school = if_else(isp_days/instructional_calendar_days >= 0.5, "Y", "N"))

district_enrollment <- student_absenteeism %>%
    select(system, school, student_id, isp_days, instructional_calendar_days) %>%
    distinct() %>%
    group_by(system, student_id) %>%
    summarise(isp_days = sum(isp_days), instructional_calendar_days = max(instructional_calendar_days)) %>%
    ungroup() %>%
    mutate(enrolled_50_pct_district = if_else(isp_days/instructional_calendar_days >= 0.5, "Y", "N"))

enrollment <- left_join(
    select(school_enrollment, system, school, student_id, enrolled_50_pct_school),
    select(district_enrollment, system, student_id, enrolled_50_pct_district),
    by = c("system", "student_id")
) %>%
    distinct()

write_csv(enrollment, path = "N:/ORP_accountability/data/2018_final_accountability_files/enrollment.csv")
