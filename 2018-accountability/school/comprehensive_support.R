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
    filter(
        grade %in% 3:12, residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y",
        original_subject %in% c("Algebra I", "Integrated Math I")
    ) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

ACT_sub_2016 <- haven::read_dta("N:/ORP_accountability/data/2016_ACT/school_act_substitution_2016_JP.dta") %>%
    transmute(
        year = 2016,
        system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, valid_tests, n_on_track = n_ontrack_prof)

ACT_sub_2017 <- read_csv("N:/ORP_accountability/data/2017_ACT/school_act_substitution_2017.csv") %>%
    transmute(
        year = 2017,
        system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
        ),
        grade = 11, valid_tests, n_on_track = n_met_benchmark)

ACT_sub_2018 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/act_substitution_school.csv") %>%
    transmute(
        year = 2018,
        system, school,
        subject = case_when(
            subject == "ACT Reading" ~ "English III",
            subject == "ACT Math" & system %in% int_math_systems ~ "Integrated Math III",
            subject == "ACT Math" & !system %in% int_math_systems ~ "Algebra II"
              ),
        grade = 11, valid_tests, n_on_track = n_met_benchmark)

success_2018 <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(grade %in% 3:12, residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y",
        original_subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc), !is.na(state_student_id)) %>%
    mutate(
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_)
    ) %>%
    group_by(system, school, grade, original_subject) %>%
    summarise_at(c("valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(
        subject = case_when(
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
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup()

success_17_16 <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(
        school != 0,
        subgroup == "All Students",
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc),
        grade %in% as.character(3:12)
    ) %>%
    mutate(
        grade = as.numeric(grade),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    bind_rows(ACT_sub_2017, ACT_sub_2016) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, subject) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(year, system, school) %>%
# Aggregate across subjects with n >= 30
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup()

tvaas_2017 <- read_excel("N:/ORP_accountability/data/2017_tvaas/2017 School Composites.xlsx") %>%
    transmute(system = as.numeric(`District Number`), school = as.numeric(`School Number`), tvaas_2017 = `School-Wide: Composite`)

tvaas_2018 <- read_excel("N:/ORP_accountability/data/2018_tvaas/School Composite Level.xlsx") %>%
    transmute(system = as.numeric(`District Number`), school = as.numeric(`School Number`), tvaas_2018 = `School-Wide: Composite`)

school_names <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    select(system, system_name, school, school_name) %>%
    distinct()

grad_less_than_67 <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/2018_school_accountability_file.csv",
        col_types = "icicciccddddddiii") %>%
    filter(indicator == "Graduation Rate", subgroup == "All Students", score_abs == 0) %>%
    transmute(system, school, grad_less_than_67 = 1L)

comprehensive_support <- bind_rows(success_17_16, success_2018) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    group_by(system, school, pool, designation_ineligible) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    left_join(tvaas_2018, by = c("system", "school")) %>%
    left_join(tvaas_2017, by = c("system", "school")) %>%
# Two/Three-Year Success Rates
    transmute(
        system, school, pool, designation_ineligible, valid_tests,
        tvaas_sh = as.integer(tvaas_2018 %in% c(4, 5) & tvaas_2017 %in% c(4, 5) | (tvaas_2018 %in% c(4, 5) & is.na(tvaas_2017))),
        tvaas_sh = if_else(is.na(tvaas_sh), 0L, tvaas_sh),
        pct_on_mastered = if_else(valid_tests > 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_)
    ) %>%
    group_by(pool, designation_ineligible, tvaas_sh) %>%
    mutate(rank_eligible = if_else(valid_tests >= 30 & tvaas_sh == 0L & designation_ineligible == 0L,
        rank(pct_on_mastered, ties.method = "min"), NA_integer_)) %>%
    ungroup() %>%
    mutate(
        comprehensive_support = if_else(tvaas_sh == 0 & designation_ineligible == 0 & pool == "HS" & rank_eligible <= high_schools, 1L, 0L),
        comprehensive_support = if_else(tvaas_sh == 0 & designation_ineligible == 0 & pool == "K8" & rank_eligible <= k8_schools, 1L, comprehensive_support),
        comprehensive_support = if_else(designation_ineligible == 0 & system == 985, 1L, comprehensive_support)
    ) %>%
    group_by(pool) %>%
    mutate(
        rank = if_else(valid_tests >= 30, rank(pct_on_mastered, ties.method = "min"), NA_integer_),
        count = sum(!is.na(pool))
    ) %>%
    ungroup() %>%
    left_join(grad_less_than_67, by = c("system", "school")) %>%
    mutate(comprehensive_support = if_else(designation_ineligible == 0 & !is.na(grad_less_than_67) & grad_less_than_67 == 1L, 1L, comprehensive_support)) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, everything())

write_csv(comprehensive_support, path = "N:/ORP_accountability/projects/2018_school_accountability/comprehensive_support.csv", na = "")

j <- haven::read_dta("N:/ORP_accountability/projects/Jessica/2018 Accountability Supporting Files/School Accountability/2018school_indicators.dta") %>%
    filter(subgroup == "All Students", comprehensive_support == 1)

mismatch <- comprehensive_support %>%
    filter(comprehensive_support == 1) %>%
    anti_join(j, by = c("system", "school"))

mismatch2 <- comprehensive_support %>%
    filter(comprehensive_support == 1) %>%
    anti_join(j, ., by = c("system", "school"))
