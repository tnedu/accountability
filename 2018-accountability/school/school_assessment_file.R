library(acct)
library(tidyverse)

student_level <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(grade %in% c(0, 3:12), residential_facility == 0, homebound == 0) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(year = 2018,
        grade = if_else(is.na(grade), 0L, grade),
        test = if_else(test %in% c("MSAA", "Alt-Science/Social Studies"), "MSAA/Alt-Science/Social Studies", test),
        n_below = if_else(performance_level %in% c("Below", "Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("Approaching", "Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_),
        All = 1L,
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic/Latino",
        Hawaiian = race == "Native Hawaiian/Pac. Islander",
        Native = race == "American Indian/Alaska Native",
        White = race == "White",
        T1234 = EL_T1234,
        EL_T1234 = if_else(EL == 1, 1L, EL_T1234),
        Non_BHN = BHN == 0L,
        Non_ED = ED == 0L,
        Non_SWD = SWD == 0L,
        Non_EL = EL_T1234 == 0L,
        Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1234 == 1L)) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
        "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Super"), as.integer) %>%
# EL Excluded are counted as tested and enrolled but do not receive a proficiency level
    mutate(original_perfomance_level = if_else(el_recently_arrived == 1, "", original_performance_level))

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
    "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, test, original_subject) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(collapse, .)
    
    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, test, original_subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)

}

school_assessment <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(grade = if_else(grade == 0, "Missing Grade", as.character(grade)),
        pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O,
        pct_approaching = if_else(pct_below != 0 & n_below == 0, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(pct_below != 0 & n_below == 0, 0, pct_below),
        pct_on_track = if_else(pct_approaching != 0 & n_approaching == 0, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(pct_approaching != 0 & n_approaching == 0, 0, pct_approaching),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL" ~ "English Learners",
            subgroup == "T1234" ~ "English Learner Transitional 1-4",
            subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        )
    ) %>%
    select(year, system, school, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

historical_2017 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2017_school_assessment_file.csv",
    col_types = "iiiccccdddiiiiddddd")

historical_2016 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2016_school_assessment_file.csv",
    col_types = "iiiccccdddiiiiddddd")

school_names <- readxl::read_excel("N:/ORP_accountability/data/2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>% 
    transmute(
        system = as.integer(`DG 4 LEA ID (State)`), school = as.integer(`DG 5 School ID (State)`),
        system_name = `EXTRA ITEM - LEA Name`, school_name = `DG 7 School Name`
    ) %>%
    bind_rows(
        tribble(
            ~system, ~system_name, ~school, ~school_name,
            970, "Department of Children's Services", 25, "Gateway to Independence",
            970, "Department of Children's Services", 45, "Wilder Youth Development Center",
            970, "Department of Children's Services", 65, "Mountain View Youth Development Center",
            970, "Department of Children's Services", 140, "DCS Affiliated Schools"
        )
    )

school_assessment <- bind_rows(school_assessment, historical_2017, historical_2016) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(year, system, system_name, school, school_name, everything()) %>%
    arrange(system, school, subject, grade, subgroup, desc(year)) %>%
# Drop schools with only prior year data
    group_by(system, school) %>%
    mutate(temp = max(year)) %>%
    ungroup() %>%
    filter(temp == 2018) %>%
    select(-temp)

write_csv(school_assessment, "N:/ORP_accountability/data/2018_final_accountability_files/school_assessment_file.csv", na = "")
