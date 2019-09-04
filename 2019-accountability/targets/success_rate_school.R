library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
    filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
# Fill in missing residential facility and enrolled 50%
# Otherwise will get dropped when checking residential facility = 0 and enrolled 50% = "Y"
    mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
    mutate_at("enrolled_50_pct_school", ~ if_else(is.na(.), "Y", .)) %>%
    filter(
        residential_facility == 0,
        (enrolled_50_pct_school == "Y" | (acct_system != system | acct_school != school)),
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, T1234 = t1234) %>%
    mutate_at(vars(BHN, ED, SWD, EL, T1234), as.logical) %>%
    mutate(
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        All = TRUE,
        Asian = reported_race == "Asian",
        Black = reported_race == "Black or African American",
        Hispanic = reported_race == "Hispanic/Latino",
        Hawaiian = reported_race == "Native Hawaiian/Pac. Islander",
        Native = reported_race == "American Indian/Alaska Native",
        White = reported_race == "White",
        EL_T1234 = EL | T1234,
        Super = BHN | ED | SWD | EL_T1234
    )

ACT_substitution <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_school.csv") %>%
    transmute(
        system, school,
        subject = "HS Math",
        grade = 11,
        subgroup = "~All",
        valid_tests,
        ot_m = n_met_benchmark
    )

collapse <- function(g) {

    g_quo <- enquo(g)

    student_level %>%
        filter(!!g_quo) %>%
        group_by(acct_system, acct_school, subject, grade) %>%
        summarise_at(c("valid_test", "ot_m"), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo))

}

school <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(Black), quo(Hawaiian), quo(Hispanic), quo(Native), quo(White),
        quo(BHN), quo(ED), quo(SWD), quo(EL_T1234), quo(Super)
    ),
    .f = ~ collapse(!!.)
) %>% 
    rename(system = acct_system, school = acct_school, valid_tests = valid_test) %>%
    bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        ),
        subject = case_when(
            subject %in% english_eoc ~ "HS English",
            subject %in% c(math_eoc, "HS Math") ~ "HS Math",
            TRUE ~ subject
        )
    ) %>%
# Aggregate HS Math/English
    group_by(system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 10
    mutate_at(c("valid_tests", "ot_m"), ~ if_else(valid_tests < 10, 0L, as.integer(.))) %>%
    group_by(system, school, subgroup) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(success_rate_prior = if_else(valid_tests != 0, round5(100 * ot_m/valid_tests, 1), NA_real_)) %>%
    transmute(
        system,
        school,
        subgroup,
        n_count = valid_tests,
        metric = success_rate_prior,
        AMO_target = amo_target(valid_tests, success_rate_prior, n_minimum = 10),
        AMO_target_double = amo_target(valid_tests, success_rate_prior, double = TRUE, n_minimum = 10)
    )

write_csv(school, "N:/ORP_accountability/projects/2020_amo/success_rate_targets_school.csv", na = "")
