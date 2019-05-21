library(acct)
library(tidyverse)

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
# Proficiency and subgroup indicators for collapse
    rename(
        BHN = bhn_group,
        ED = economically_disadvantaged,
        SWD = special_ed,
        EL = el,
        T1234 = t1234,
        Gifted = gifted,
        Migrant = migrant
    ) %>%
    mutate_at(vars(BHN, ED, SWD, EL, T1234, Gifted, Migrant), as.logical) %>%
    mutate(
        year = 2019,
        test = if_else(test %in% c("MSAA", "Alt-Social Studies"), "MSAA/Alt-Social Studies", test),
        n_below = if_else(performance_level %in% c("Below", "Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("Approaching", "Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_),
        All = TRUE,
        Asian = reported_race == "Asian",
        Black = reported_race == "Black or African American",
        Hawaiian = reported_race == "Native Hawaiian/Pac. Islander",
        Hispanic = reported_race == "Hispanic/Latino",
        Native = reported_race == "American Indian/Alaska Native",
        White = reported_race == "White",
        EL_T1234 = EL | T1234,
        Non_BHN = !BHN,
        Non_ED = !ED,
        Non_SWD = !SWD,
        Non_EL = !EL_T1234,
        Super = BHN | ED | SWD | EL_T1234,
        Male = gender == "M",
        Female = gender == "F"
    ) %>%
# EL Excluded are counted as tested and enrolled but do not receive a proficiency level
    mutate(original_perfomance_level = if_else(el_recently_arrived == 1, "", original_performance_level))

collapse <- function(g, ...) {

    g_quo <- enquo(g)

    all_grades <- student_level %>%
        filter(!!g_quo) %>%
        group_by(...) %>%
        summarise_at(vars(enrolled, tested, valid_test, n_below, n_approaching, n_on_track, n_mastered), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo), grade = "All Grades")

    by_grade <- student_level %>%
        filter(!!g_quo) %>%
        group_by(grade, ...) %>%
        summarise_at(vars(enrolled, tested, valid_test, n_below, n_approaching, n_on_track, n_mastered), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo), grade = as.character(grade))

    bind_rows(all_grades, by_grade)

}

# State assessment file
state <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(Black), quo(Hawaiian), quo(Hispanic), quo(Native), quo(White),
        quo(BHN), quo(ED), quo(SWD), quo(EL), quo(T1234), quo(EL_T1234), 
        quo(Non_BHN), quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Super),
        quo(Male), quo(Female), quo(Migrant), quo(Gifted)
    ),
    .f = ~ collapse(!!., year, test, original_subject)
) %>%
    rename(subject = original_subject, valid_tests = valid_test) %>%
    mutate(
        grade = if_else(grade == 0, "Missing Grade", grade),
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
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners",
            subgroup == "~T1234" ~ "English Learner Transitional 1-4",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        )
    ) %>%
    select(year, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

state_prior <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/state_assessment_file.csv") %>%
    filter(year %in% 2017:2018, subject %in% state$subject, !(subject == "Social Studies" & grade %in% 3:5))

state_assessment <- bind_rows(state, state_prior) %>%
    mutate(system = 0, system_name = "State of Tennessee") %>%
    select(year, system, system_name, everything()) %>%
    arrange(subject, grade, subgroup, desc(year))

write_csv(state_assessment, "N:/ORP_accountability/data/2019_final_accountability_files/state_assessment_file.csv", na = "")

# District assessment file
# Residential facility students are dropped at the district and school level
student_level <- filter(student_level, residential_facility == 0)

district <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(Black), quo(Hawaiian), quo(Hispanic), quo(Native), quo(White),
        quo(BHN), quo(ED), quo(SWD), quo(EL), quo(T1234), quo(EL_T1234), 
        quo(Non_BHN), quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Super),
        quo(Male), quo(Female), quo(Migrant), quo(Gifted)
    ),
    .f = ~ collapse(!!., year, system, system_name, test, original_subject)
) %>%
    rename(subject = original_subject, valid_tests = valid_test) %>%
    mutate(
        grade = if_else(grade == 0, "Missing Grade", grade),
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
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners",
            subgroup == "~T1234" ~ "English Learner Transitional 1-4",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        )
    ) %>%
    select(year, system, system_name, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

district_prior <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/district_assessment_file.csv") %>%
    filter(year %in% 2017:2018, subject %in% state$subject, !(subject == "Social Studies" & grade %in% 3:5))

district_assessment <- bind_rows(district, district_prior) %>%
    arrange(system, subject, grade, subgroup, desc(year))

write_csv(district_assessment, "N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv", na = "")

# School assessment file
school <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(Black), quo(Hawaiian), quo(Hispanic), quo(Native), quo(White),
        quo(BHN), quo(ED), quo(SWD), quo(EL), quo(T1234), quo(EL_T1234), 
        quo(Non_BHN), quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Super),
        quo(Male), quo(Female), quo(Migrant), quo(Gifted)
    ),
    .f = ~ collapse(!!., year, system, system_name, school, school_name, test, original_subject)) %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(
        grade = if_else(grade == 0, "Missing Grade", grade),
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
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners",
            subgroup == "~T1234" ~ "English Learner Transitional 1-4",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        )
    ) %>%
    select(year, system, system_name, school, school_name, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

school_prior <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/school_assessment_file.csv") %>%
    filter(year %in% 2017:2018, subject %in% state$subject, !(subject == "Social Studies" & grade %in% 3:5))

school_assessment <- bind_rows(school, school_prior) %>%
    arrange(system, school, subject, grade, subgroup, desc(year)) %>%
# Drop schools with only prior year data
    group_by(system, school) %>%
    mutate(temp = max(year)) %>%
    filter(temp == 2019) %>%
    ungroup() %>%
    select(-temp)

write_csv(school_assessment, "N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv", na = "")
