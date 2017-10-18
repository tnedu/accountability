## ELPA Indicator for A-F School Grading

library(acct)
library(haven)
library(tidyverse)

elpa15 <- read_dta("K:/ORP_accountability/data/2015_WIDA_Access/2015_State_Student_Data_File_ACH.dta") %>%
    transmute(student_id = unique_student_id, composite_prior = as.numeric(compositeproficiencylevel)) %>%
    filter(!is.na(student_id)) %>%
    group_by(student_id) %>%
# Dedup first by max prior composite score
    mutate(max = max(composite_prior)) %>%
    ungroup() %>%
    filter(composite_prior == max) %>%
    select(-max) %>%
# Force drop duplicates on student ID, proficiency level
    filter(!duplicated(.))

econ_dis <- read_csv("K:/ORP_accountability/projects/2016_acct_modeling/sc_ed_lw.csv") %>%
    rename(student_id = state_id)

elpa16 <- read_dta("K:/ORP_accountability/data/2016_WIDA_Access/old/2016_State_Student_Data_File_ACH.dta") %>%
    transmute(system = as.numeric(substr(districtcode, 3, length(districtcode))), school = schoolcode,
        student_id = statestudentid,
        time_in_esl = case_when(
            timeinlepellinus %in% c("<1", "0.1", "0.3", "0.5", "0.7", "3m", "8M") ~ 0,
            timeinlepellinus %in% c("1y", "1+") ~ 1,
            timeinlepellinus %in% c("2y", "2Y", "2+") ~ 2,
            timeinlepellinus %in% c("3y", "3+") ~ 3,
            timeinlepellinus %in% c("4y", "4+") ~ 4,
            timeinlepellinus %in% c("5y", "5+") ~ 5,
            timeinlepellinus == "6y" ~ 6,
            timeinlepellinus == "7y" ~ 7,
            timeinlepellinus == "8y" ~ 8,
            timeinlepellinus == "  " ~ grade
        ),
        black = raceblack, hispanic = ethnicityhispaniclatino, native = raceamericanindianalaskanative,
        hpi = racepacificislander, asian = raceasian, white = racewhite, swd = iepstatus,
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

# Observations by subgroup
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
    mutate(subgroup = "English Learners")

elpa_black <- elpa16 %>%
    filter(black == "Y") %>%
    mutate(subgroup = "Black")

elpa_hispanic <- elpa16 %>%
    filter(hispanic == "H") %>%
    mutate(subgroup = "Black")

elpa_native <- elpa16 %>%
    filter(native == "Y") %>%
    mutate(subgroup = "Native American")

elpa_hpi <- elpa16 %>%
    filter(hpi == "Y") %>%
    mutate(subgroup = "Hawaiian/Pacific Islander")

elpa_asian <- elpa16 %>%
    filter(asian == "Y") %>%
    mutate(subgroup = "Asian")

elpa_white <- elpa16 %>%
    filter(white == "Y") %>%
    mutate(subgroup = "White")

elpa_indicator <- bind_rows(elpa_all, elpa_ed, elpa_bhn, elpa_swd, elpa_el,
        elpa_black, elpa_hispanic, elpa_native, elpa_hpi, elpa_asian, elpa_white) %>% 
    mutate(valid_tests = !is.na(literacy) & !is.na(composite),
        exit_count = if_else(valid_tests, literacy >= 5.0 & composite >= 5.0, NA),
        exit_denom = case_when(
            time_in_esl == 0 ~ 0.2,
            time_in_esl == 1 ~ 0.4,
            time_in_esl == 2 ~ 0.6,
            time_in_esl == 3 ~ 0.8,
            time_in_esl == 4 ~ 1.0,
            time_in_esl >= 5 | is.na(time_in_esl) ~ 1.2,
            !valid_tests ~ NA_real_
        ),
        growth_standard_denom = !is.na(composite) & !is.na(composite_prior),
        met_growth_standard = composite - composite_prior >= 0.7) %>%
    group_by(system, school, subgroup) %>%
    summarise_at(c("valid_tests", "exit_count", "exit_denom", "growth_standard_denom", "met_growth_standard"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(exit_percent = if_else(valid_tests >= 10, round5(100 * exit_count/exit_denom, 1), NA_real_),
        grade_exit = case_when(
            exit_percent >= 36 ~ "A",
            exit_percent >= 24 ~ "B",
            exit_percent >= 12 ~ "C",
            exit_percent >= 6 ~ "D",
            exit_percent < 6 ~ "F"
        ),
        met_growth_percent = if_else(growth_standard_denom >= 10, round5(100 * met_growth_standard/growth_standard_denom, 1), NA_real_),
        grade_growth_standard = case_when(
            met_growth_percent >= 70 ~ "A",
            met_growth_percent >= 60 ~ "B",
            met_growth_percent >= 45 ~ "C",
            met_growth_percent >= 30 ~ "D",
            met_growth_percent < 30 ~ "F"
        )
    )

write_csv(elpa_indicator, path = "data/elpa_indicator.csv", na = "")
