library(acct)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")

pools <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

student_level <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(
        grade %in% 3:12, residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y",
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(
        original_subject = if_else(test == "MSAA", subject, original_subject),
        test = if_else(test %in% c("MSAA", "Alt-Science/Social Studies"), "MSAA/Alt-Science/Social Studies", test),
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_),
        All = 1L,
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic/Latino",
        Hawaiian = race == "Native Hawaiian/Pac. Islander",
        Native = race == "American Indian/Alaska Native",
        White = race == "White",
        EL_T1234 = if_else(EL == 1, 1L, EL_T1234),
        Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1234 == 1L)
    ) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD", "EL_T1234", "Super"), as.integer)

int_math_systems <- student_level %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

ACT_substitution <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_school.csv") %>%
    filter(subject == "ACT Math") %>%
    transmute(system, school,
        subject = case_when(
            system %in% int_math_systems ~ "Integrated Math III",
            !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, subgroup = "All", valid_tests, n_on_track = n_met_benchmark)

collapse_school <- map_dfr(

    .x = list(
        filter(student_level, All == 1L) %>% mutate(subgroup = "All"),
        filter(student_level, Asian == 1L) %>% mutate(subgroup = "Asian"),
        filter(student_level, Black == 1L) %>% mutate(subgroup = "Black"),
        filter(student_level, Hispanic == 1L) %>% mutate(subgroup = "Hispanic"),
        filter(student_level, Hawaiian == 1L) %>% mutate(subgroup = "Hawaiian"),
        filter(student_level, Native == 1L) %>% mutate(subgroup = "Native"),
        filter(student_level, White == 1L) %>% mutate(subgroup = "White"),
        filter(student_level, BHN == 1L) %>% mutate(subgroup = "BHN"),
        filter(student_level, ED == 1L) %>% mutate(subgroup = "ED"),
        filter(student_level, SWD == 1L) %>% mutate(subgroup = "SWD"),
        filter(student_level, EL_T1234 == 1L) %>% mutate(subgroup = "EL_T1234"),
        filter(student_level, Super == 1L) %>% mutate(subgroup = "Super")
    ),
    .f = function(x) {
        x %>%
            group_by(system, school, test, original_subject, grade, subgroup) %>%
            summarise_at(c("enrolled", "tested", "valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
            ungroup()
    }

)

subject_AMO_school <- collapse_school %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        ),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced 3-8 subjects
    group_by(system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
        success_rate_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests, success_rate_prior, n_minimum = 1),
        AMO_target_double = amo_target(valid_tests, success_rate_prior, double = TRUE, n_minimum = 1)
    )

subject_AMO_school %>%
    select(-n_on_track, -n_mastered) %>% 
    write_csv("N:/ORP_accountability/projects/2019_amo/subject_targets_school.csv", na = "")

subject_AMO_school <- collapse_school %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    bind_rows(ACT_substitution) %>%
    mutate(
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        ),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced 3-8 subjects
    group_by(system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Aggregate by HS Math/English
    mutate(
        subject = case_when(
            subject %in% math_eoc ~ "HS Math",
            subject %in% english_eoc ~ "HS English",
            TRUE ~ subject
        )
    ) %>%
    group_by(system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(
        system, school, subject, subgroup,
        valid_tests, n_on_track, n_mastered,
        success_rate_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests, success_rate_prior, n_minimum = 1),
        AMO_target_double = amo_target(valid_tests, success_rate_prior, double = TRUE, n_minimum = 1)
    )

subject_AMO_school %>%
    select(-n_on_track, -n_mastered) %>% 
    write_csv("N:/ORP_accountability/projects/2019_amo/subject_targets_school.csv", na = "")

success_AMO_school <- subject_AMO_school %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    group_by(system, school, subgroup) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(
        system, school, subgroup, valid_tests,
        success_rate_prior = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
        AMO_target = amo_target(valid_tests, success_rate_prior),
        AMO_target_double = amo_target(valid_tests, success_rate_prior, double = TRUE)
    )

write_csv(success_AMO_school, "N:/ORP_accountability/projects/2019_amo/success_rate_targets_school.csv", na = "")
