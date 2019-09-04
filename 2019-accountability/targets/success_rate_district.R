library(acct)
library(janitor)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

student_level <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
# Fill in missing residential facility and enrolled 50%
# Otherwise will get dropped when checking residential facility = 0 and enrolled 50% = "Y"
    mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
    mutate_at("enrolled_50_pct_district", ~ if_else(is.na(.), "Y", .)) %>%
    filter(
        residential_facility == 0,
        (enrolled_50_pct_district == "Y" | system != acct_system),
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, T1234 = t1234) %>%
    mutate_at(vars(BHN, ED, SWD, EL, T1234), as.logical) %>%
    mutate(
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        All = TRUE,
        EL_T1234 = EL | T1234,
        grade = case_when(
            grade %in% 3:5 ~ "3rd through 5th",
            grade %in% 6:8 ~ "6th through 8th",
            grade %in% 9:12 | is.na(grade) ~ "9th through 12th"
        )
    )

ACT_substitution <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/act_substitution_district.csv") %>%
    transmute(
        system,
        subject = "HS Math",
        grade = "9th through 12th",
        subgroup = "~All",
        valid_tests,
        ot_m = n_met_benchmark
    )

collapse <- function(g) {
    
    g_quo <- enquo(g)
    
    student_level %>%
        filter(!!g_quo) %>%
        group_by(acct_system, subject, grade) %>%
        summarise_at(c("valid_test", "ot_m"), sum, na.rm = TRUE) %>%
        mutate(subgroup = deparse(g_quo)) %>%
        ungroup()
    
}

district <- map_dfr(
    .x = list(quo(All), quo(BHN), quo(ED), quo(SWD), quo(EL_T1234)),
    .f = ~ collapse(!!.)
) %>%
    rename(system = acct_system, valid_tests = valid_test) %>%
    bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~SWD" ~ "Students with Disabilities"
        ),
        subject = case_when(
            subject %in% english_eoc ~ "HS English",
            subject %in% c(math_eoc, "HS Math") ~ "HS Math",
            TRUE ~ subject
        )
    ) %>%
# Aggregate HS Math/English
    group_by(system, subject, subgroup, grade) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 10
    mutate_at(c("valid_tests", "ot_m"), ~ if_else(valid_tests < 10, 0L, as.integer(.))) %>%
    group_by(system, subgroup, grade) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(
        system,
        grade,
        subgroup,
        n_count = valid_tests,
        metric = if_else(valid_tests != 0, round5(100 * ot_m/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests, metric, n_minimum = 10),
        AMO_target_double = amo_target(valid_tests, metric, double = TRUE, n_minimum = 10)
    )

write_csv(district, "N:/ORP_accountability/projects/2020_amo/success_rate_targets_district.csv", na = "")
