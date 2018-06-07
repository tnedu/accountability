library(acct)
library(tidyverse)

student_level <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_eoc_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiicciiiiiiii") %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(year = 2018,
        grade = if_else(is.na(grade), 0L, grade),
        test = if_else(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test),
        n_below = if_else(performance_level %in% c("Below", "Below Basic"), 1L, NA_integer_),
        n_approaching = if_else(performance_level %in% c("Approaching", "Basic"), 1L, NA_integer_),
        n_on_track = if_else(performance_level %in% c("On track", "Proficient"), 1L, NA_integer_),
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
        "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Super"), as.integer)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD",
    "EL", "T1234", "EL_T1234", "Non_BHN", "Non_ED", "Non_SWD", "Non_EL", "Super")) {
    
    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, test, original_subject) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(collapse, .)
    
    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, test, original_subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

district_assessment <- collapse %>%
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
            subgroup == "Non_EL" ~ "Non-English Learners",
            subgroup == "Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        )
    ) %>%
    select(year, system, test, subject, grade, subgroup,
        enrolled, tested, valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

ACT <- haven::read_dta("N:/ORP_accountability/data/2017_ACT/ACT_district2018_appeals2.dta") %>%
    transmute(year = 2018, system = as.integer(system), subject = "ACT Composite", grade = "All Grades",
        subgroup = case_when(
            subgroup == "English Language Learners with T1/T2" ~ "English Learners with Transitional 1-2",
            subgroup == "HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native American" ~ "American Indian or Alaska Native",
            subgroup == "Non-English Language Learners" ~ "Non-English Learners",
            TRUE ~ subgroup
        ),
        enrolled, tested, valid_tests = valid_tests_wSAT, n_below = n_below19, n_on_track = n_21_orhigher,
        ACT_18_and_below = pct_below19, ACT_21_and_above = pct_21_orhigher
    )

grad <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_public_release_EK.csv") %>%
    filter(system != 0 & school == 0) %>%
    select(system, subject, subgroup, grad_cohort, grad_count, grad_rate)

historical_2017 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2017_district_assessment_file.csv",
        col_types = "iiccccddddddddddddddiidid") %>%
    mutate_at(c("grad_cohort", "grad_count", "dropout_count"), as.integer) %>%
    mutate_at(c("grad_rate", "dropout_rate"), as.numeric)

historical_2016 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2016_district_assessment_file.csv",
    col_types = "iiccccddddddddddddddiidid")

output <- bind_rows(district_assessment, ACT, grad, historical_2017, historical_2016) %>%
    arrange(system, subject, grade, subgroup)

write_csv(output, "N:/ORP_accountability/data/2018_final_accountability_files/district_assessment_file.csv", na = "")
