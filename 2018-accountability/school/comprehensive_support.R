library(acct)
library(readxl)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools_immune <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible) %>%
    filter(!is.na(pool))

high_schools <- ceiling(0.05 * sum(pools_immune$pool == "HS", na.rm = TRUE))
k8_schools <- ceiling(0.05 * sum(pools_immune$pool == "K8", na.rm = TRUE))

int_math_systems <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(grade %in% 3:12, residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y") %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

ACT_sub_2016 <- read_csv("N:/ORP_accountability/projects/2016_pre_coding/Output/school_act_substitution.csv") %>%
    transmute(system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, valid_tests, n_on_track = n_met_benchmark)

ACT_sub_2017 <- read_csv("N:/ORP_accountability/data/2017_ACT/school_act_substitution_2017.csv") %>%
    transmute(system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, valid_tests, n_on_track = n_met_benchmark)

ACT_sub_2018 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_school.csv") %>%
    transmute(system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
              ),
        grade = 11, valid_tests, n_on_track = n_met_benchmark)

success_2018 <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(grade %in% 3:12, residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y",
        original_subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc)) %>%
    mutate(
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_)
    ) %>%
    group_by(system, school, grade, original_subject) %>%
    summarise_at(c("valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    bind_rows(ACT_sub_2018) %>%
# Aggregate across grades and replaced subjects
    group_by(system, school, subject) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    group_by(system, school) %>%
# Aggregate across subjects with n >= 30
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE)

success_2016 <- haven::read_dta("N:/ORP_accountability/projects/2016_student_level_file/state_student_level_2016.dta") %>%
    filter(grade %in% 3:12, homebound == 0, greater_than_60_pct == "Y",
        original_subject %in% c(math_eoc, english_eoc, science_eoc)) %>%
    mutate(
        n_on_track = if_else(proficiency_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(proficiency_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_)
    ) %>%
    group_by(system, school, grade, original_subject) %>%
    summarise_at(c("valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    bind_rows(ACT_sub_2016) %>%
# Aggregate across grades and replaced subjects
    group_by(system, school, subject) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, as.integer(.)))) %>%
    group_by(system, school) %>%
# Aggregate across subjects with n >= 30
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE)

success_2017 <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(school != 0,
        subgroup == "All Students",
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc),
        grade %in% as.character(3:12)) %>%
    mutate(grade = as.numeric(grade),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    bind_rows(ACT_sub_2017) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, subject) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(system, school) %>%
# Aggregate across subjects with n >= 30
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE)

tvaas_2017 <- read_excel("N:/ORP_accountability/data/2017_tvaas/2017 School Composites.xlsx") %>%
    transmute(system = as.numeric(`District Number`), school = as.numeric(`School Number`), tvaas_2017 = `School-Wide: Composite`)

tvaas_2018 <- read_excel("N:/ORP_accountability/data/2018_tvaas/School Composite Level.xlsx") %>%
    transmute(system = as.numeric(`District Number`), school = as.numeric(`School Number`), tvaas_2018 = `School-Wide: Composite`)

school_names <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    select(system, system_name, school, school_name) %>%
    distinct()

comprehensive_support <- bind_rows(success_2016, success_2017, success_2018) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    group_by(system, school, pool, designation_ineligible) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    left_join(tvaas_2018, by = c("system", "school")) %>%
    left_join(tvaas_2017, by = c("system", "school")) %>%
# Two/Three-Year Success Rates
    transmute(system, school, pool, designation_ineligible, valid_tests,
        tvaas_sh = as.integer(tvaas_2018 %in% c(4, 5) & tvaas_2017 %in% c(4, 5)),
        tvaas_sh = if_else(is.na(tvaas_sh), 0L, tvaas_sh),
        pct_on_mastered = round5(100 * (n_on_track + n_mastered)/valid_tests, 1)) %>%
    group_by(pool, designation_ineligible, tvaas_sh) %>%
    mutate(rank_OM = if_else(valid_tests >= 30, rank(pct_on_mastered, ties.method = "min"), NA_integer_)) %>%
    ungroup() %>%
    mutate(
        comprehensive_support = if_else(tvaas_sh == 0 & designation_ineligible == 0 & pool == "HS" & rank_OM <= high_schools, 1L, 0L),
        comprehensive_support = if_else(tvaas_sh == 0 & designation_ineligible == 0 & pool == "K8" & rank_OM <= k8_schools, 1L, comprehensive_support)
    ) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, everything())

write_csv(comprehensive_support, path = "N:/ORP_accountability/projects/2018_school_accountability/comprehensive_support.csv", na = "")
