library(acct)
library(lubridate)
library(tidyverse)

grade_2 <- read_csv("N:/ORP_accountability/data/2019_cdf/2019_grade_2_cdf.csv") %>%
    mutate(
        ri_status = if_else(reason_not_tested == 1 & ri_status == 6, 0, ri_status),
        absent = reason_not_tested == 1,
        breach_adult = ri_status == 1,
        breach_student = ri_status == 2,
        irregular_admin = ri_status == 3,
        incorrect_grade_subject = ri_status == 4,
        refused_to_test = ri_status == 5,
        failed_attemptedness = ri_status == 6,
        original_subject = case_when(
            content_area_code == "ENG" ~ "ELA",
            content_area_code == "MAT" ~ "Math"
        )
    ) %>%
    transmute(
        system,
        school,
        test,
        original_subject,
        subject = original_subject,
        original_performance_level = performance_level,
        performance_level,
        scale_score,
        state_student_id = unique_student_id,
        last_name,
        first_name,
        grade,
        gender,
        reported_race,
        bhn_group = reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"),
        economically_disadvantaged,
        el,
        el_recently_arrived = (el_arrived_year_1 == 1 | el_arrived_year_2 == 1),
        t1234 = t1234 %in% 1:4,
        special_ed,
        functionally_delayed,
        gifted,
        migrant,
        enrolled_50_pct_district,
        enrolled_50_pct_school,
        teacher_of_record_tln,
        breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness,
        absent
    ) %>%
    mutate_at(vars(bhn_group, t1234, el_recently_arrived), as.integer) %>%
    rowwise() %>%
# Grade 2 are all considered enrolled
# Grade 2 are considered tested if performance level is not missing
    mutate(
        enrolled = 1L,
        tested = if_else(not_na(performance_level), 1L, 0L),
        performance_level = case_when(
            any(absent, breach_adult, breach_student, irregular_admin, incorrect_grade_subject, refused_to_test, failed_attemptedness) ~ NA_character_,
            el_recently_arrived == 1 ~ NA_character_,
            TRUE ~ performance_level
        ),
        valid_test = if_else(not_na(performance_level), 1L, 0L)
    ) %>%
    ungroup()

# Records from Alternative, CTE, Adult HS are dropped from student level
cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
    transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER))

dedup <- grade_2 %>%
    anti_join(cte_alt_adult, by = c("system", "school")) %>%
# For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    mutate(
        test_priority = case_when(
            test == "Grade 2 Alt" ~ 2,
            test == "Grade 2" ~ 1
        )
    ) %>%
    group_by(state_student_id, subject) %>%
    mutate(temp = max(test_priority, na.rm = TRUE)) %>%
    filter(test_priority == temp | temp == -Inf) %>%
    select(-test_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same test, take highest performance level
    mutate(
        prof_priority = case_when(
            performance_level %in% c("Below", "Below Basic") ~ 1,
            performance_level %in% c("Approaching", "Basic") ~ 2,
            performance_level %in% c("On Track", "Proficient") ~ 3,
            performance_level %in% c("Mastered", "Advanced") ~ 4
        )
    ) %>%
    group_by(state_student_id, original_subject, test) %>%
    mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
    filter(prof_priority == temp | temp == -Inf) %>%
    select(-prof_priority, -temp) %>%
    ungroup() %>%
# For students with multiple records within the same performance level, take highest scale score
    group_by(state_student_id, original_subject, test, performance_level) %>%
    mutate(temp = max(scale_score, na.rm = TRUE)) %>%
    filter(scale_score == temp | temp == -Inf) %>%
    select(-temp) %>%
    ungroup() %>%
# For students with multiple test records with the same original subject, performance level, scale score
# Deduplicate by missing race/ethnicity
    group_by(state_student_id, original_subject, test, performance_level, scale_score) %>%
    mutate(
        n = n(),                           # Tag duplicates by id, subject, test, performance level, scale score
        temp = mean(is.na(reported_race))  # Check whether one among duplicates has non-missing race/ethnicity
    ) %>%
    filter(!(n > 1 & temp != 0 & is.na(reported_race))) %>%
    ungroup() %>%
    select(-n, -temp) %>%
# For students multiple test records with the same original subject, performance level, scale score, demographics
# Deduplicate for non-missing grade
    group_by(state_student_id, original_subject, test, performance_level, scale_score, reported_race) %>%
    mutate(
        n = n(),                   # Tag duplicates by id, subject, test, performance level, scale score
        temp = mean(is.na(grade))  # Check whether one among duplicates has non-missing race/ethnicity
    ) %>%
    filter(!(n > 1 & temp != 0 & is.na(grade))) %>%
    ungroup() %>%
    select(-n, -temp)

# ELPA Students should be EL = 1
elpa <- read_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv") %>%
    select(student_id)

school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

student_level <- dedup %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(
        system, system_name, school, school_name, test, original_subject, subject,
        original_performance_level, performance_level, scale_score, enrolled, tested, valid_test,
        state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group, teacher_of_record_tln,
        functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
        enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test
    ) %>%
    mutate_at(vars(absent, refused_to_test), as.integer) %>%
# Percentiles by original subject
    group_by(test, original_subject, grade) %>%
    mutate(
        rank = if_else(not_na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
        denom = sum(not_na(scale_score)),
        percentile = if_else(test == "Grade 2", round5(100 * rank/denom, 1), NA_real_)
    ) %>%
    select(-rank, -denom) %>%
    arrange(system, school, state_student_id) %>%
# Assign EL = 1 if student tested ELPA
    mutate(
        el = if_else(state_student_id %in% elpa$student_id, 1, el)
    )

write_csv(student_level, "N:/ORP_accountability/projects/2019_grade_2_assessment/grade_2_student_level.csv", na = "")

# District and School Level Grade 2 Files
preprocess <- student_level %>%
    # Proficiency and subgroup indicators for aggregation
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
# Default missing ED, SWD, EL to FALSE, otherwise doesn't get captured in Non- aggregations
    mutate_at(vars(ED, SWD, EL), ~ if_else(is.na(.), FALSE, .)) %>%
    mutate(
        year = year(now()),
        n_below = if_else(performance_level == "Below", 1L, NA_integer_),
        n_approaching = if_else(performance_level == "Approaching", 1L, NA_integer_),
        n_on_track = if_else(performance_level == "On Track", 1L, NA_integer_),
        n_mastered = if_else(performance_level == "Mastered", 1L, NA_integer_),
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
        Female = gender == "F",
    # EL Recently Arrived are counted as tested and enrolled but do not receive a proficiency level
        perfomance_level = if_else(el_recently_arrived == 1, "", performance_level)
    )

collapse <- function(g, ...) {

    g_quo <- enquo(g)

    preprocess %>%
        filter(!!g_quo) %>%
        group_by(grade, ...) %>%
        summarise_at(vars(enrolled, tested, valid_test, n_below, n_approaching, n_on_track, n_mastered), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo))

}

district_prior <- read_csv("N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_district_level_file.csv") %>%
    mutate(
        test = "Grade 2",
        grade = 2
    )

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
            subgroup == "~Female" ~ "Female",
            subgroup == "~Gifted" ~ "Gifted",
            subgroup == "~T1234" ~ "English Learner Transitional 1-4",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Male" ~ "Male",
            subgroup == "~Migrant" ~ "Migrant",
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
    select(
        year, system, system_name, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered
    ) %>%
    bind_rows(district_prior) %>%
    arrange(system, test, subject, subgroup, desc(year))

write_csv(district, "N:/ORP_accountability/projects/2019_grade_2_assessment/grade_2_district_level.csv", na = "")

school_prior <- read_csv("N:/ORP_accountability/projects/2018_grade_2_assessment/2018_grade_2_school_level_file.csv") %>%
    mutate(
        test = "Grade 2",
        grade = 2
    )

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
        pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
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
            subgroup == "~Female" ~ "Female",
            subgroup == "~Gifted" ~ "Gifted",
            subgroup == "~T1234" ~ "English Learner Transitional 1-4",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Male" ~ "Male",
            subgroup == "~Migrant" ~ "Migrant",
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
    select(
        year, system, system_name, school, school_name, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered
    ) %>%
    bind_rows(school_prior) %>%
    arrange(system, school, test, subject, subgroup, desc(year))

write_csv(school, "N:/ORP_accountability/projects/2019_grade_2_assessment/grade_2_school_level.csv", na = "")

# Split district file
district_numbers <- sort(unique(grade_2$system))

district %>%
    # Drop districts with only prior year data
    group_by(system) %>%
    mutate(temp = max(year)) %>%
    filter(temp == year(now())) %>%
    ungroup() %>%
    select(-temp) %>%
    split(., .$system) %>%
    walk2(
        .x = .,
        .y = district_numbers,
        .f = ~ write_csv(.x, path = paste0(
            "N:/ORP_accountability/projects/", year(now()), "_grade_2_assessment/Split/", .y,
            "_Grade2DistrictFile_", sprintf("%02d", day(now())), month(now(), label = T), year(now()),
            ".csv"
        ), na = "")
    )

school %>%
    # Drop districts with only prior year data
    group_by(system) %>%
    mutate(temp = max(year)) %>%
    filter(temp == year(now())) %>%
    ungroup() %>%
    select(-temp) %>%
    split(., .$system) %>%
    walk2(
        .x = .,
        .y = district_numbers,
        .f = ~ write_csv(.x, path = paste0(
            "N:/ORP_accountability/projects/", year(now()), "_grade_2_assessment/Split/", .y,
            "_Grade2SchoolFile_", sprintf("%02d", day(now())), month(now(), label = T), year(now()), ".csv"
        ), na = "")
    )

student_level %>%
    split(., .$system) %>%
    walk2(
        .x = .,
        .y = district_numbers,
        .f = ~ write_csv(.x, path = paste0(
            "N:/ORP_accountability/projects/", year(now()), "_grade_2_assessment/Split/", .y,
            "_Grade2StudentFile_", sprintf("%02d", day(now())), month(now(), label = T), year(now()), ".csv"
        ), na = "")
    )
