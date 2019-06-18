library(tidyverse)

dupes <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/student_chronic_absenteeism_Jun17.csv") %>%
    filter(str_length(student_id) == 7) %>%
# Remove duplicates on all columns
    distinct() %>%
# Aggregate records for students by school
    group_by(system, school, student_id) %>%
    summarise(isp_days = sum(isp_days, na.rm = TRUE), instructional_calendar_days = first(instructional_calendar_days)) %>%
    ungroup() %>%
# Look for students with more than one enrollment
    add_count(student_id) %>%
    filter(n > 1) %>%
    select(-n) %>%
    group_by(student_id) %>%
# Identify school with greatest number of ISP days
    mutate(temp = max(isp_days)) %>%
    ungroup() %>%
# Keep only record with greatest number of ISP days if >= half of school year
    filter(temp == isp_days, isp_days/instructional_calendar_days >= .5) %>%
# If multiple records exist with >= half of school year enrollment, use system and school from assessment record
    add_count(student_id) %>%
    filter(n == 1) %>%
    select(acct_system = system, acct_school = school, state_student_id = student_id)

write_csv(dupes, "N:/ORP_accountability/data/2019_final_accountability_files/enrollment.csv")
