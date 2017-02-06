library(tidyverse)

time_in_esl <- read_csv("K:/ORP_accountability/projects/2016_acct_modeling/time_in_esl.csv") %>%
    rename(student_id = id) %>%
    filter(!is.na(student_id)) %>%
    filter(year == 2016) %>%
    select(system, school, student_id, time_in_esl) %>%
# Dedup first by max time in ESL
    mutate(dup = duplicated(student_id)) %>%
    group_by(student_id) %>%
    mutate(temp = max(time_in_esl)) %>%
    ungroup() %>%
    filter(!time_in_esl != temp) %>%
    select(-temp, -dup) %>%
# Force drop duplicates on system, school, student ID, time in ESL
    filter(!duplicated(.))

elpa15 <- haven::read_dta("K:/ORP_accountability/data/2015_WIDA_Access/2015_State_Student_Data_File_ACH.dta") %>%
    transmute(student_id = unique_student_id, composite_prior = as.numeric(compositeproficiencylevel))
    filter(!is.na(student_id)) %>%
    group_by(student_id) %>%
# Dedup first by max prior composite score
    mutate(max = max(composite_prior)) %>%
    ungroup() %>%
    filter(!composite_prior != max) %>%
    select(-max) %>%
# Force drop duplicates on student ID, proficiency level
    filter(!duplicated(.))

elpa16 <- haven::read_dta("K:/ORP_accountability/data/2016_WIDA_Access/2016_State_Student_Data_File_ACH.dta") %>%
    transmute(system = as.numeric(substr(districtcode, 3, length(districtcode))), school = schoolcode, 
        student_id = statestudentid, swd = iepstatus, 
        hispanic = ethnicityhispaniclatino, native = raceamericanindianalaskanative, black = raceblack, 
        literacy = as.numeric(literacyperformancelevel), composite = as.numeric(performancelevelcomposite)) %>%
# Drop missing student ids and records with missing literacy and composite scores
    filter(!is.na(student_id)) %>%
    filter(!(is.na(literacy) & is.na(composite))) %>%
    group_by(system, school, student_id) %>%
# Dedup first by max composite score
    mutate(max = max(composite)) %>%
    ungroup() %>%
    filter(composite == max | is.na(max)) %>%
    select(-max) %>%
# Unique on system, school, id at this point
# Merge on time in esl and prior proficiency
    left_join(time_in_esl, by = c("system", "school", "student_id")) %>%
    left_join(elpa15, by = "student_id")

# All Students entries
elpa_all <- elpa16 %>%
    mutate(subgroup = "All Students")

elpa_bhn <- elpa16 %>% 
    filter(hispanic == "H" | native == "Y" | black == "Y") %>%
    mutate(subgroup = "Black/Hispanic/Native American")
    
elpa_swd <- elpa16 %>%
    filter(swd == "Y") %>% 
    mutate(subgroup = "Students with Disabilities")

elpa_el <- elpa16 %>% 
    mutate(subgroup = "English Language Learners with T1/T2")

elpa_indicator <- bind_rows(elpa_all, elpa_bhn, elpa_swd, elpa_el) %>% 
    mutate(valid_tests = as.numeric(!is.na(literacy) & !is.na(composite)),
        exit_count = ifelse(valid_tests, as.numeric(literacy >= 5.0 & composite >= 5.0), NA),
        exit_denom = ifelse(time_in_esl == 1, 0.2, NA),
        exit_denom = ifelse(time_in_esl == 2, 0.4, exit_denom),
        exit_denom = ifelse(time_in_esl == 3, 0.6, exit_denom),
        exit_denom = ifelse(time_in_esl == 4, 0.8, exit_denom),
        exit_denom = ifelse(time_in_esl == 5, 1.0, exit_denom),
        exit_denom = ifelse(time_in_esl >= 6 | is.na(time_in_esl), 1.2, exit_denom),
        growth_standard_denom = as.numeric(!is.na(composite) & !is.na(composite_prior)),
        met_growth_standard = as.numeric((composite - composite_prior) >= 0.7)) %>%
    group_by(system, school, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, exit_count, exit_denom, growth_standard_denom, met_growth_standard) %>%
    ungroup() %>%
    mutate(exit_percent = 100 * exit_count/exit_denom,
        exit_points = ifelse(exit_percent < 6, 0, NA),
        exit_points = ifelse(exit_percent >= 6 & exit_percent < 12, 1, exit_points),
        exit_points = ifelse(exit_percent >= 12 & exit_percent < 24, 2, exit_points),
        exit_points = ifelse(exit_percent >= 24 & exit_percent < 36, 3, exit_points),
        exit_points = ifelse(exit_percent >= 36, 4, exit_points),
        exit_points = ifelse(valid_tests < 10, NA, exit_points),
        met_growth_percent = 100 * met_growth_standard/growth_standard_denom,
        growth_points = ifelse(met_growth_percent < 30, 0, NA),
        growth_points = ifelse(met_growth_percent >= 30 & met_growth_percent < 45, 1, growth_points),
        growth_points = ifelse(met_growth_percent >= 45 & met_growth_percent < 60, 2, growth_points),
        growth_points = ifelse(met_growth_percent >= 60 & met_growth_percent < 70, 3, growth_points),
        growth_points = ifelse(met_growth_percent >= 70, 4, growth_points),
        growth_points = ifelse(growth_standard_denom < 10, NA, growth_points)) %>%
    rowwise() %>%
    mutate(points = mean(c(exit_points, growth_points), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(grade_elpa = ifelse(points < 0.5, "F", NA),
        grade_elpa = ifelse(points >= 0.5 & points < 1.5, "D", grade_elpa),
        grade_elpa = ifelse(points >= 1.5 & points < 2.5, "C", grade_elpa),
        grade_elpa = ifelse(points >= 2.5 & points < 3.5, "B", grade_elpa),
        grade_elpa = ifelse(points >= 3.5, "A", grade_elpa))

write_csv(elpa_indicator, path = "data/elpa_indicator.csv", na = "")
