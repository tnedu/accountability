library(acct)
library(haven)
library(tidyverse)

xwalk <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Helpful Documents/ACT_xwalkAug2018rev.csv")

grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/student_level.csv",
        col_types = "dccccTccTiccccccciciicciicTccciccccccdcTccciTc") %>%
    filter(included_in_cohort == "Y" & (completion_type %in% c(1, 11, 12, 13)))

closed <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/closed_schools.csv") %>%
    transmute(system = as.integer(SYSTEM), school = as.integer(SCHOOL))

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
    transmute(system = as.integer(DISTRICT_NUMBER), school = as.integer(SCHOOL_NUMBER))

junior_2018 <- read_dta("N:/Assessment_Data Returns/ACT/2017-18/2018 Spring/Final Spring Files/20180717_ACT_JuniorDayResults_SY2017-18_Whalen_v1.dta") %>%
    filter(!is.na(state_stud_id), test_location != "M") %>%
    add_count(state_stud_id) %>% # No duplicates
    select(unique_student_id = state_stud_id, acthscode, 
        composite = act_composite,
        math = act_math,
        reading = act_read,
        english = act_eng,
        science = act_sci)

junior_2017 <- read_dta("N:/Assessment_Data Returns/ACT/2016-17/Junior Day File/20170713_ACT_JuniorDayResults_SY2016-17_Whalen_v1.dta") %>%
    filter(!is.na(state_stud_id), test_location != "M") %>%
    group_by(state_stud_id) %>%
# Dedup by highest composite score
    mutate(temp = max(act_composite, na.rm = TRUE)) %>%
    filter(act_composite == temp) %>%
# Dedup by highest math score
    mutate(temp = max(act_math, na.rm = TRUE)) %>%
    filter(act_math == temp) %>%
# Dedup by highest reading score
    mutate(temp = max(act_read, na.rm = TRUE)) %>%
    filter(act_read == temp) %>%
# Dedup by highest English score
    mutate(temp = max(act_eng, na.rm = TRUE)) %>%
    filter(act_eng == temp) %>%
# Dedup by highest science score
    mutate(temp = max(act_sci, na.rm = TRUE)) %>%
    filter(act_sci == temp) %>%
    add_count(state_stud_id) %>%
    ungroup() %>%
    select(unique_student_id = state_stud_id, acthscode, 
        composite = act_composite,
        math = act_math,
        reading = act_read,
        english = act_eng,
        science = act_sci)

cohort <- read_dta("N:/Assessment_Data Returns/ACT/2017-18/2018 Cohort Highest Score/20180925_ACT_GraduatingCohortHighestScore_SY2017-18_Whalen_V1_Tae.dta") %>%
    filter(!is.na(unique_student_id)) %>%
    group_by(unique_student_id) %>%
# Dedup by highest composite score
    mutate(temp = max(act_composite_highest, na.rm = TRUE)) %>%
    filter(act_composite_highest == temp) %>%
# Dedup by highest math score
    mutate(temp = max(act_math_highest, na.rm = TRUE)) %>%
    filter(act_math_highest == temp) %>%
# Dedup by highest reading score
    mutate(temp = max(act_reading_highest, na.rm = TRUE)) %>%
    filter(act_reading_highest == temp) %>%
# Dedup by highest English score
    mutate(temp = max(act_english_highest, na.rm = TRUE)) %>%
    filter(act_english_highest == temp) %>%
# Dedup by highest science score
    mutate(temp = max(act_science_highest, na.rm = TRUE)) %>%
    filter(act_science_highest == temp) %>%
    ungroup() %>%
    add_count(unique_student_id)  %>%
    select(unique_student_id, acthscode,
        composite = act_composite_highest,
        math = act_math_highest,
        reading = act_reading_highest,
        english = act_english_highest,
        science = act_science_highest)

cohort2 <- read_dta("N:/Assessment_Data Returns/ACT/2017-18/2018 Cohort Highest Score/20180925_ACT_GraduatingCohortHighestScore_SY2017-18_Whalen_v1.dta") %>%
    filter(!is.na(unique_student_id)) %>%
    group_by(unique_student_id) %>%
# Dedup by highest composite score
    mutate(temp = max(act_composite_highest, na.rm = TRUE)) %>%
    filter(act_composite_highest == temp) %>%
# Dedup by highest math score
    mutate(temp = max(act_math_highest, na.rm = TRUE)) %>%
    filter(act_math_highest == temp) %>%
# Dedup by highest reading score
    mutate(temp = max(act_reading_highest, na.rm = TRUE)) %>%
    filter(act_reading_highest == temp) %>%
# Dedup by highest English score
    mutate(temp = max(act_english_highest, na.rm = TRUE)) %>%
    filter(act_english_highest == temp) %>%
# Dedup by highest science score
    mutate(temp = max(act_science_highest, na.rm = TRUE)) %>%
    filter(act_science_highest == temp) %>%
    ungroup() %>%
    add_count(unique_student_id) %>%
    select(unique_student_id, acthscode,
        composite = act_composite_highest,
        math = act_math_highest,
        reading = act_reading_highest,
        english = act_english_highest,
        science = act_science_highest)

student_level <- bind_rows(junior_2018, junior_2017, cohort, cohort2) %>%
    group_by(unique_student_id) %>%
# Dedup by highest composite score
    mutate(temp = max(composite, na.rm = TRUE)) %>%
    filter(composite == temp) %>%
# Dedup by highest math score
    mutate(temp = max(math, na.rm = TRUE)) %>%
    filter(math == temp) %>%
# Dedup by highest reading score
    mutate(temp = max(reading, na.rm = TRUE)) %>%
    filter(reading == temp) %>%
# Dedup by highest English score
    mutate(temp = max(english, na.rm = TRUE)) %>%
    filter(english == temp) %>%
# Dedup by highest science score
    mutate(temp = max(science, na.rm = TRUE)) %>%
    filter(science == temp) %>%
    ungroup() %>%
    select(-temp) %>%
    distinct() %>%
    left_join(xwalk, by = "acthscode") %>%
    anti_join(closed, by = c("system", "school")) %>%
    anti_join(cte_alt_adult, by = c("system", "school")) %>%
    select(-system, -school) %>%
    left_join(grad, ., by = c("student_key" = "unique_student_id")) %>%
    mutate(
        enrolled = 1,
        tested = !is.na(composite),
        valid_test = !is.na(composite),
        n_21_or_higher = composite >= 21,
        n_below_19 = composite < 19,
        n_21_or_higher_male = composite >= 21 & gender == "M",
        n_below_19_male = composite < 19 & gender == "M",
        n_21_or_higher_female = composite >= 21 & gender == "F",
        n_below_19_female = composite < 19 & gender == "F",
        met_CRB_english = english >= 18,
        met_CRB_math = math >= 22,
        met_CRB_reading = reading >= 22,
        met_CRB_science = science >= 23,
        met_CRB_all = met_CRB_english & met_CRB_math & met_CRB_reading & met_CRB_science,
        ED = ed == "Y",
        BHN = race_ethnicity %in% c("B", "H", "I"),
        EL = el == "Y",
        SWD = swd == "Y",
        Asian = race_ethnicity == "A",
        Black = race_ethnicity == "B",
        Hispanic = race_ethnicity == "H",
        Native = race_ethnicity == "I",
        HPI = race_ethnicity == "P",
        White = race_ethnicity == "W",
        Non_ED = ed == "N",
        Non_EL = el == "N",
        Non_SWD = swd == "N"
    )

write_csv(student_level, "N:/ORP_accountability/data/2018_ACT/ACT_student.csv", na = "")

act_state <- map_dfr(
    .x = list(
        mutate(student_level, subgroup = "All Students"),
        filter(student_level, Native) %>% mutate(subgroup = "American Indian or Alaska Native"),
        filter(student_level, Asian) %>% mutate(subgroup = "Asian"),
        filter(student_level, Black) %>% mutate(subgroup = "Black or African American"),
        filter(student_level, BHN) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(student_level, ED) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(student_level, EL) %>% mutate(subgroup = "English Learners"),
        filter(student_level, Hispanic) %>% mutate(subgroup = "Hispanic"),
        filter(student_level, HPI) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
        filter(student_level, Non_ED) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
        filter(student_level, Non_EL) %>% mutate(subgroup = "Non-English Learners"),
        filter(student_level, Non_SWD) %>% mutate(subgroup = "Non-Students with Disabilities"),
        filter(student_level, SWD) %>% mutate(subgroup = "Students with Disabilities"),
        filter(student_level, BHN | ED | EL | SWD) %>% mutate(subgroup = "Super Subgroup"),
        filter(student_level, White) %>% mutate(subgroup = "White")
    ),
    .f = function(x) {
        sums <- x %>% 
            group_by(subgroup) %>%
            summarise_at(c("enrolled", "tested", "valid_test", "n_21_or_higher", "n_below_19",
                "met_CRB_english", "met_CRB_math", "met_CRB_reading", "met_CRB_science", "met_CRB_all"), sum, na.rm = TRUE) %>%
            ungroup()
        
        averages <- x %>% 
            group_by(subgroup) %>%
            summarise_at(c("composite", "math", "reading", "english", "science"), mean, na.rm = TRUE) %>%
            mutate_at(c("composite", "math", "reading", "english", "science"), round5, 1) %>%
            ungroup()
        
        inner_join(sums, averages, by = "subgroup")
    }
) %>%
    arrange(subgroup) %>%
    mutate(
        participation_rate = round5(100 * tested/enrolled),
        pct_21_or_higher = if_else(valid_test != 0, round5(100 * n_21_or_higher/valid_test, 1), NA_real_),
        pct_below_19 = if_else(valid_test != 0, round5(100 * n_below_19/valid_test, 1), NA_real_),
        pct_met_CRB_english = if_else(valid_test != 0, round5(100 * met_CRB_english/valid_test, 1), NA_real_),
        pct_met_CRB_math = if_else(valid_test != 0, round5(100 * met_CRB_math/valid_test, 1), NA_real_),
        pct_met_CRB_reading = if_else(valid_test != 0, round5(100 * met_CRB_reading/valid_test, 1), NA_real_),
        pct_met_CRB_science = if_else(valid_test != 0, round5(100 * met_CRB_science/valid_test, 1), NA_real_),
        pct_met_CRB_all = if_else(valid_test != 0, round5(100 * met_CRB_all/valid_test, 1), NA_real_)
    ) %>%
    select(subgroup, participation_rate, everything())

write_csv(act_state, "N:/ORP_accountability/data/2018_ACT/ACT_state.csv", na = "")

act_system <- map_dfr(
    .x = list(
        mutate(student_level, subgroup = "All Students"),
        filter(student_level, Native) %>% mutate(subgroup = "American Indian or Alaska Native"),
        filter(student_level, Asian) %>% mutate(subgroup = "Asian"),
        filter(student_level, Black) %>% mutate(subgroup = "Black or African American"),
        filter(student_level, BHN) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(student_level, ED) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(student_level, EL) %>% mutate(subgroup = "English Learners"),
        filter(student_level, Hispanic) %>% mutate(subgroup = "Hispanic"),
        filter(student_level, HPI) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
        filter(student_level, Non_ED) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
        filter(student_level, Non_EL) %>% mutate(subgroup = "Non-English Learners"),
        filter(student_level, Non_SWD) %>% mutate(subgroup = "Non-Students with Disabilities"),
        filter(student_level, SWD) %>% mutate(subgroup = "Students with Disabilities"),
        filter(student_level, BHN | ED | EL | SWD) %>% mutate(subgroup = "Super Subgroup"),
        filter(student_level, White) %>% mutate(subgroup = "White")
    ),
    .f = function(x) {
        sums <- x %>%
            group_by(system, subgroup) %>%
            summarise_at(c("enrolled", "tested", "valid_test", "n_21_or_higher", "n_below_19",
                "met_CRB_english", "met_CRB_math", "met_CRB_reading", "met_CRB_science", "met_CRB_all"), sum, na.rm = TRUE) %>%
            ungroup()
        
        averages <- x %>%
            group_by(system, subgroup) %>%
            summarise_at(c("composite", "math", "reading", "english", "science"), mean, na.rm = TRUE) %>%
            mutate_at(c("composite", "math", "reading", "english", "science"), round5, 1) %>%
            ungroup()
        
        inner_join(sums, averages, by = c("system", "subgroup"))
    }
) %>%
    arrange(system, subgroup) %>%
    mutate(
        participation_rate = round5(100 * tested/enrolled),
        pct_21_or_higher = if_else(valid_test != 0, round5(100 * n_21_or_higher/valid_test, 1), NA_real_),
        pct_below_19 = if_else(valid_test != 0, round5(100 * n_below_19/valid_test, 1), NA_real_),
        pct_met_CRB_english = if_else(valid_test != 0, round5(100 * met_CRB_english/valid_test, 1), NA_real_),
        pct_met_CRB_math = if_else(valid_test != 0, round5(100 * met_CRB_math/valid_test, 1), NA_real_),
        pct_met_CRB_reading = if_else(valid_test != 0, round5(100 * met_CRB_reading/valid_test, 1), NA_real_),
        pct_met_CRB_science = if_else(valid_test != 0, round5(100 * met_CRB_science/valid_test, 1), NA_real_),
        pct_met_CRB_all = if_else(valid_test != 0, round5(100 * met_CRB_all/valid_test, 1), NA_real_)
    ) %>%
    select(system, subgroup, participation_rate, everything())

write_csv(act_system, "N:/ORP_accountability/data/2018_ACT/ACT_system.csv", na = "")

act_school <- map_dfr(
    .x = list(
        mutate(student_level, subgroup = "All Students"),
        filter(student_level, Native) %>% mutate(subgroup = "American Indian or Alaska Native"),
        filter(student_level, Asian) %>% mutate(subgroup = "Asian"),
        filter(student_level, Black) %>% mutate(subgroup = "Black or African American"),
        filter(student_level, BHN) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(student_level, ED) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(student_level, EL) %>% mutate(subgroup = "English Learners"),
        filter(student_level, Hispanic) %>% mutate(subgroup = "Hispanic"),
        filter(student_level, HPI) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
        filter(student_level, Non_ED) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
        filter(student_level, Non_EL) %>% mutate(subgroup = "Non-English Learners"),
        filter(student_level, Non_SWD) %>% mutate(subgroup = "Non-Students with Disabilities"),
        filter(student_level, SWD) %>% mutate(subgroup = "Students with Disabilities"),
        filter(student_level, BHN | ED | EL | SWD) %>% mutate(subgroup = "Super Subgroup"),
        filter(student_level, White) %>% mutate(subgroup = "White")
    ),
    .f = function(x) {
        sums <- x %>%
            group_by(system, school, subgroup) %>%
            summarise_at(c("enrolled", "tested", "valid_test", "n_21_or_higher", "n_below_19",
                "met_CRB_english", "met_CRB_math", "met_CRB_reading", "met_CRB_science", "met_CRB_all"), sum, na.rm = TRUE) %>%
            ungroup()
        
        averages <- x %>%
            group_by(system, school, subgroup) %>%
            summarise_at(c("composite", "math", "reading", "english", "science"), mean, na.rm = TRUE) %>%
            mutate_at(c("composite", "math", "reading", "english", "science"), round5, 1) %>%
            ungroup()
        
        inner_join(sums, averages, by = c("system", "school", "subgroup"))
    }
) %>%
    arrange(system, school, subgroup) %>%
    mutate(
        participation_rate = round5(100 * tested/enrolled),
        pct_21_or_higher = if_else(valid_test != 0, round5(100 * n_21_or_higher/valid_test, 1), NA_real_),
        pct_below_19 = if_else(valid_test != 0, round5(100 * n_below_19/valid_test, 1), NA_real_),
        pct_met_CRB_english = if_else(valid_test != 0, round5(100 * met_CRB_english/valid_test, 1), NA_real_),
        pct_met_CRB_math = if_else(valid_test != 0, round5(100 * met_CRB_math/valid_test, 1), NA_real_),
        pct_met_CRB_reading = if_else(valid_test != 0, round5(100 * met_CRB_reading/valid_test, 1), NA_real_),
        pct_met_CRB_science = if_else(valid_test != 0, round5(100 * met_CRB_science/valid_test, 1), NA_real_),
        pct_met_CRB_all = if_else(valid_test != 0, round5(100 * met_CRB_all/valid_test, 1), NA_real_)
    ) %>%
    select(system, school, subgroup, participation_rate, everything())

write_csv(act_school, "N:/ORP_accountability/data/2018_ACT/ACT_school.csv", na = "")
