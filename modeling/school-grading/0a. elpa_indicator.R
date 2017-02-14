library(tidyverse)

elpa15 <- haven::read_dta("K:/ORP_accountability/data/2015_WIDA_Access/2015_State_Student_Data_File_ACH.dta") %>%
    transmute(student_id = unique_student_id, composite_prior = as.numeric(compositeproficiencylevel)) %>%
    filter(!is.na(student_id)) %>%
    group_by(student_id) %>%
# Dedup first by max prior composite score
    mutate(max = max(composite_prior)) %>%
    ungroup() %>%
    filter(!composite_prior != max) %>%
    select(-max) %>%
# Force drop duplicates on student ID, proficiency level
    filter(!duplicated(.))

econ_dis <- read_csv("K:/ORP_accountability/projects/2016_acct_modeling/sc_ed_lw.csv") %>% 
    rename(student_id = state_id)

elpa16 <- haven::read_dta("K:/ORP_accountability/data/2016_WIDA_Access/2016_State_Student_Data_File_ACH.dta") %>%
    mutate(timeinlepellinus = ifelse(timeinlepellinus %in% c("<1", "0.1", "0.3", "0.5", "0.7", "3m", "8M"), "0", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("1y", "1+"), "1", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("2y", "2Y", "2+"), "2", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("3y", "3+"), "3", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("4y", "4+"), "4", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("5y", "5+"), "5", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "6y", "6", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "7y", "7", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "8y", "8", timeinlepellinus),
        timeinlepellinus = ifelse(is.na(timeinlepellinus), grade, timeinlepellinus)) %>% 
    transmute(system = as.numeric(substr(districtcode, 3, length(districtcode))), school = schoolcode, 
        student_id = statestudentid, swd = iepstatus, time_in_esl = timeinlepellinus,
        hispanic = ethnicityhispaniclatino, native = raceamericanindianalaskanative, black = raceblack, 
        literacy = as.numeric(literacyperformancelevel), composite = as.numeric(performancelevelcomposite)) %>%
    left_join(econ_dis, by = "student_id") %>% 
# Drop missing student ids and records with missing literacy and composite scores
    filter(!is.na(student_id)) %>%
    filter(!(is.na(literacy) & is.na(composite))) %>%
    group_by(system, school, student_id) %>%
# Dedup first by max composite score
    mutate(max = max(composite)) %>%
    ungroup() %>%
    filter(composite == max | is.na(max)) %>%
    select(-max) %>%
# Merge on prior proficiency
    left_join(elpa15, by = "student_id")

# All Students entries
elpa_all <- elpa16 %>%
    mutate(subgroup = "All Students")

elpa_ed <- elpa16 %>%
    filter(ed == 1) %>% 
    mutate(subgroup = "Economically Disadvantaged")

elpa_bhn <- elpa16 %>%
    filter(hispanic == "H" | native == "Y" | black == "Y") %>%
    mutate(subgroup = "Black/Hispanic/Native American")

elpa_swd <- elpa16 %>%
    filter(swd == "Y") %>% 
    mutate(subgroup = "Students with Disabilities")

elpa_el <- elpa16 %>% 
    mutate(subgroup = "English Language Learners with T1/T2")

elpa_indicator <- bind_rows(elpa_all, elpa_ed, elpa_bhn, elpa_swd, elpa_el) %>% 
    mutate(valid_tests = !is.na(literacy) & !is.na(composite),
        exit_count = ifelse(valid_tests, literacy >= 5.0 & composite >= 5.0, NA),
        exit_denom = ifelse(time_in_esl == 0, 0.2, NA),
        exit_denom = ifelse(time_in_esl == 1, 0.4, NA),
        exit_denom = ifelse(time_in_esl == 2, 0.5, exit_denom),
        exit_denom = ifelse(time_in_esl == 3, 0.6, exit_denom),
        exit_denom = ifelse(time_in_esl == 4, 1.0, exit_denom),
        exit_denom = ifelse(time_in_esl >= 5 | is.na(time_in_esl), 1.2, exit_denom),
        growth_standard_denom = !is.na(composite) & !is.na(composite_prior),
        met_growth_standard = (composite - composite_prior) >= 0.7) %>%
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
