library(tidyverse)

# Percent Meeting Growth Standard
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
    transmute(system = as.numeric(substr(districtcode, 3, length(districtcode))), student_id = statestudentid, 
        swd = iepstatus, time_in_esl = as.numeric(timeinlepellinus),
        hispanic = ethnicityhispaniclatino, native = raceamericanindianalaskanative, black = raceblack, 
        composite = as.numeric(performancelevelcomposite)) %>%
    left_join(econ_dis, by = "student_id") %>% 
# Drop records with missing student ids or composite scores
    filter(!is.na(student_id), !is.na(composite)) %>%
    group_by(system, student_id) %>%
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

growth_standard <- bind_rows(elpa_all, elpa_ed, elpa_bhn, elpa_swd, elpa_el) %>% 
    mutate(valid_tests = !is.na(composite),
        growth_standard_denom = !is.na(composite) & !is.na(composite_prior),
        met_growth_standard = (composite - composite_prior) >= 0.7) %>%
    group_by(system, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, growth_standard_denom, met_growth_standard) %>%
    ungroup() %>%
    mutate(pct_met_growth_standard = 100 * met_growth_standard/growth_standard_denom)

write_csv(growth_standard, path = "data/elpa_growth_standard.csv")

rm(elpa15, elpa16, elpa_all, elpa_bhn, elpa_ed, elpa_el, elpa_swd, growth_standard)

# Long Term ELs
long_term_15 <- haven::read_dta("K:/ORP_accountability/data/2015_WIDA_Access/2015_State_Student_Data_File_ACH.dta") %>%
    transmute(system = systemnumber, student_id = unique_student_id, composite_prior = as.numeric(compositeproficiencylevel),
        long_term_el = as.numeric(lengthoftimeinlepprogram) > 5) %>%
    filter(!is.na(student_id)) %>%
    group_by(student_id) %>%
    # Dedup first by max prior composite score
    mutate(max = max(composite_prior)) %>%
    ungroup() %>%
    filter(!composite_prior != max) %>%
    # Keep duplicates on student ID, proficiency level
    mutate(count = 1) %>%
    group_by(system) %>%
    summarise(n_long_term_el_prior = sum(long_term_el), n_students_prior = sum(count)) %>%
    mutate(pct_long_term_el_prior = 100 * n_long_term_el_prior/n_students_prior)

long_term_els <- haven::read_dta("K:/ORP_accountability/data/2016_WIDA_Access/2016_State_Student_Data_File_ACH.dta") %>%
    mutate(timeinlepellinus = ifelse(timeinlepellinus %in% c("<1", "0.1", "0.3", "0.5", "0.7", "3m", "8M"), "0", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("1y", "1+"), "1", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("2y", "2Y", "2+"), "2", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("3y", "3+"), "3", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("4y", "4+"), "4", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("5y", "5+"), "5", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "6y", "6", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "7y", "7", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "8y", "8", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "  ", grade, timeinlepellinus)) %>%
    transmute(system = as.numeric(substr(districtcode, 3, length(districtcode))), student_id = statestudentid,
        swd = iepstatus, hispanic = ethnicityhispaniclatino, native = raceamericanindianalaskanative, black = raceblack,
        composite = as.numeric(performancelevelcomposite), long_term_el = as.numeric(timeinlepellinus) > 5) %>%
    left_join(econ_dis, by = "student_id") %>%
# Drop missing student ids and records with missing literacy and composite scores
    filter(!is.na(student_id), !is.na(composite)) %>%
    group_by(system, student_id) %>%
# Dedup first by max composite score
    mutate(max = max(composite)) %>%
    ungroup() %>%
    filter(composite == max | is.na(max)) %>%
    select(-max) %>%
    mutate(count = 1) %>%
    group_by(system) %>%
    summarise(n_long_term_el = sum(long_term_el, na.rm = TRUE), n_students = sum(count, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(pct_long_term_el = 100 * n_long_term_el/n_students) %>%
    left_join(long_term_15, by = "system")

write_csv(long_term_els, path = "data/long_term_els.csv")
