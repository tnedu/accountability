library(acct)
library(haven)
library(readxl)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

pools <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible)

high_schools <- ceiling(0.05 * sum(pools$pool == "HS", na.rm = TRUE))
k8_schools <- ceiling(0.05 * sum(pools$pool == "K8", na.rm = TRUE))

student_level_2017 <- haven::read_dta("N:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10192017.dta") %>%
# Combine two Bartlett schools that merged
    mutate(school = if_else(system == 794 & school == 170, 25, school)) %>%
    filter(
        grade %in% 3:12,
        greater_than_60_pct == "Y",
        homebound == 0 | is.na(homebound),
        residential_facility != 1 | is.na(residential_facility),
        original_subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc)
    ) %>%
    filter(!(grade %in% 3:4 & original_subject == "Science")) %>%
# Proficiency indicators for collapse
    mutate(
        year = 2017,
        valid_test = as.integer(valid_test),
        n_on_track = if_else(performance_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_)
    )

student_level_2018 <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(
        grade %in% 3:12,
        enrolled_50_pct_school == "Y",
        homebound == 0,
        residential_facility == 0,
        original_subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc)
    ) %>%
    filter(!(grade %in% 3:4 & original_subject == "Science")) %>%
# Proficiency indicators for collapse
    mutate(
        year = 2018,
        test = if_else(test %in% c("MSAA", "Alt-Science/Social Studies"), "MSAA/Alt-Science/Social Studies", test),
        n_on_track = if_else(performance_level %in% c("On Track", "Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("Mastered", "Advanced"), 1L, NA_integer_)
    )

int_math_systems <- student_level_2018 %>%
    filter(original_subject %in% c("Algebra I", "Integrated Math I")) %>%
    count(system, original_subject) %>%
    group_by(system) %>%
    mutate(temp = max(n)) %>%
    filter(n == temp, original_subject == "Integrated Math I") %>%
    magrittr::extract2("system")

ACT_sub_2017 <- read_csv("N:/ORP_accountability/data/2017_ACT/school_act_substitution_2017.csv") %>%
    filter(school != -9999) %>%
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

ach <- bind_rows(student_level_2018, student_level_2017) %>%
    group_by(year, system, school, test, original_subject, grade) %>%
    summarise_at(c("enrolled", "tested", "valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>% 
    rename(valid_tests = valid_test, subject = original_subject) %>%
    bind_rows(ACT_sub_2018, ACT_sub_2017) %>%
    mutate(
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, subject) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    # Suppress subjects with n < 30
    mutate_at(c("valid_tests", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0L, as.integer(.)))) %>%
    group_by(system, school) %>%
    # Aggregate across subjects with n >= 30
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Two-year success rates
    group_by(system, school) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(
        system, school,
        success_rate = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_)
    )

growth <- read_csv("N:/ORP_accountability/projects/Alex/accountability/2019-accountability/modeling/tvaas_multi_year.csv") %>%
    transmute(system, school,
        score_growth = case_when(
            level == 5 ~ 4,
            level == 4 ~ 3,
            level == 3 ~ 2,
            level == 2 ~ 1,
            level == 1 ~ 0
        )
    )

grad_lag <- read_dta("N:/ORP_accountability/data/2016_graduation_rate/School_grad_rate2017_JP.dta") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school,
        grad_cohort = if_else(grad_cohort < 30, 0L, as.integer(grad_cohort)),
        grad_count = if_else(grad_cohort < 30, 0L, as.integer(grad_count))
    )

grad <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>%
    filter(system != 0 & school != 0, system != 90, subgroup == "All Students") %>%
    mutate(
        grad_cohort = if_else(grad_cohort < 30, 0, grad_cohort),
        grad_count = if_else(grad_cohort < 30, 0, grad_count)
    ) %>%
    bind_rows(grad_lag) %>%
    group_by(system, school) %>%
    summarise_at(c("grad_cohort", "grad_count"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(grad_cohort != 0) %>%
    transmute(
        system, school,
        grad_rate = round5(100 * grad_count/grad_cohort, 1),
        score_grad = case_when(
            grad_rate >= 95 ~ 4,
            grad_rate >= 90 ~ 3,
            grad_rate >= 80 ~ 2,
            grad_rate >= 67 ~ 1,
            grad_rate < 67 ~ 0
        )
    )

ready_grad_lag <- haven::read_dta("N:/ORP_accountability/data/2018_final_accountability_files/ready_grad_school2018_JW.dta") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school,
        grad_cohort = if_else(grad_cohort < 30, 0L, as.integer(grad_cohort)),
        n_21_or_higher = if_else(grad_cohort < 30, 0L, as.integer(n_21_orhigher))
    )

ready_grad <- read_csv("N:/ORP_accountability/projects/2018_amo/school_readygrad_AMO_targets2018_JW_individualsubgroups.csv") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school,
        grad_cohort = if_else(grad_cohort < 30, 0, grad_cohort),
        n_21_or_higher = if_else(grad_cohort < 30, 0, n_21_orhigher)
    ) %>%
    bind_rows(ready_grad_lag) %>%
    group_by(system, school) %>%
    summarise_at(c("grad_cohort", "n_21_or_higher"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(grad_cohort != 0) %>%
    transmute(
        system, school,
        ready_grad_rate = round5(100 * n_21_or_higher/grad_cohort, 1),
        score_ready_grad = case_when(
            ready_grad_rate >= 40 ~ 4,
            ready_grad_rate >= 30 ~ 3,
            ready_grad_rate >= 25 ~ 2,
            ready_grad_rate >= 16 ~ 1,
            ready_grad_rate < 16 ~ 0
        )
    )
    
elpa_lag <- haven::read_dta("N:/ORP_accountability/data/2017_ELPA/school_level_elpa_JW.dta") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school = schoolnumber,
        denom = if_else(n_validtests_growth < 10, 0L, as.integer(n_validtests_growth)),
        met_growth_standard = if_else(n_validtests_growth < 10, 0L, as.integer(met_growth_standard))
    )

elpa <- read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_school.csv") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school,
        denom = if_else(growth_standard_denom < 10, 0, growth_standard_denom),
        met_growth_standard = if_else(denom < 10, 0, n_met_growth_standard)
    ) %>%
    bind_rows(elpa_lag) %>%
    group_by(system, school) %>%
    summarise_at(c("denom", "met_growth_standard"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    filter(denom != 0) %>%
    transmute(
        system, school,
        pct_met_growth_standard = round5(100 * met_growth_standard/denom, 1),
        score_elpa = case_when(
            pct_met_growth_standard >= 60 ~ 4,
            pct_met_growth_standard >= 50 ~ 3,
            pct_met_growth_standard >= 40 ~ 2,
            pct_met_growth_standard >= 25 ~ 1,
            pct_met_growth_standard < 25 ~ 0
        )
    )

absenteeism_lag <- read_csv("N:/ORP_accountability/data/2017_chronic_absenteeism/school_chronic_absenteeism.csv",
        col_types = "iicicccdddiiiddd") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school,
        n_students = if_else(n_students < 30, 0L, as.integer(n_students)),
        n_chronically_absent = if_else(n_students < 30, 0L, as.integer(n_chronically_absent))
    )

absenteeism <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/school_chronic_absenteeism.csv",
        col_types = "iicicccdid") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school,
        n_students = if_else(n_students < 30, 0L, as.integer(n_students)),
        n_chronically_absent = if_else(n_students < 30, 0L, as.integer(n_chronically_absent))
    ) %>%
    bind_rows(absenteeism_lag) %>%
    group_by(system, school) %>%
    summarise_at(c("n_students", "n_chronically_absent"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    left_join(pools, by = c("system", "school")) %>%
    transmute(system, school,
        pct_chronically_absent = if_else(n_students >= 30, round5(100 * n_chronically_absent/n_students, 1), NA_real_),
        score_absenteeism = case_when(
            pool == "K8" & pct_chronically_absent <= 6 ~ 4,
            pool == "K8" & pct_chronically_absent <= 9 ~ 3,
            pool == "K8" & pct_chronically_absent <= 13 ~ 2,
            pool == "K8" & pct_chronically_absent <= 20 ~ 1,
            pool == "K8" & pct_chronically_absent > 20 ~ 0,
            pool == "HS" & pct_chronically_absent <= 10 ~ 4,
            pool == "HS" & pct_chronically_absent <= 14 ~ 3,
            pool == "HS" & pct_chronically_absent <= 20 ~ 2,
            pool == "HS" & pct_chronically_absent <= 30 ~ 1,
            pool == "HS" & pct_chronically_absent > 30 ~ 0
        )
    )

comprehensive_support <- pools %>%
    filter(!is.na(pool)) %>%
    inner_join(ach, by = c("system", "school")) %>%
    left_join(growth, by = c("system", "school")) %>%
    left_join(grad, by = c("system", "school")) %>%
    left_join(ready_grad, by = c("system", "school")) %>%
    left_join(absenteeism, by = c("system", "school")) %>%
    left_join(elpa, by = c("system", "school")) %>%
    rowwise() %>%
    mutate(safe_harbor = map_lgl(
            .x = list(score_growth, score_grad, score_ready_grad, score_absenteeism, score_elpa), 
            .f = function(x) if_else(!is.na(x), x %in% 3:4, NA)) %>% all(na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
        comprehensive_support = if_else(system == 985, 1L, 0L)
    )

comprehensive_k8 <- comprehensive_support %>% filter(pool == "K8" & comprehensive_support == 1) %>% nrow()
comprehensive_hs <- comprehensive_support %>% filter(pool == "HS" & comprehensive_support == 1) %>% nrow()

while (comprehensive_k8 < k8_schools) {

    comprehensive_support <- comprehensive_support %>%
        arrange(desc(pool), designation_ineligible, safe_harbor, comprehensive_support, success_rate)
    
    comprehensive_support$comprehensive_support[1] <- 1L
    
    comprehensive_k8 <- comprehensive_support %>% filter(pool == "K8" & comprehensive_support == 1) %>% nrow()

}

while (comprehensive_hs < high_schools) {

    comprehensive_support <- comprehensive_support %>%
        arrange(pool, designation_ineligible, safe_harbor, comprehensive_support, success_rate)
    
    comprehensive_support$comprehensive_support[1] <- 1L
    
    comprehensive_hs <- comprehensive_support %>% filter(pool == "HS" & comprehensive_support == 1) %>% nrow()

}

comprehensive_support <- comprehensive_support %>%
    mutate(asd = as.integer(system == 985)) %>%
    group_by(pool, comprehensive_support, asd) %>%
    mutate(cutoff = if_else(comprehensive_support == 1L & asd == 0, max(success_rate, na.rm = 1), NA_real_)) %>%
    group_by(pool) %>%
    mutate(cutoff = max(cutoff, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(comprehensive_support = if_else(comprehensive_support == 0 & designation_ineligible == 0 & safe_harbor == 0 & success_rate == cutoff, 1L, comprehensive_support))

prior <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/comprehensive_support.csv") %>%
    select(system, school, prior = comprehensive_support)

comprehensive_support <- full_join(comprehensive_support, prior, by = c("system", "school"))

write_csv(comprehensive_support, "N:/ORP_accountability/projects/Alex/accountability/2019-accountability/modeling/comprehensive_support.csv", na = "")

# Matching
j <- read_csv("N:/ORP_accountability/projects/Jessica/Accountability/2017-18/2018 Accountability Supporting Files/School Accountability/comprehensive_supportNEW_JW_v3.csv") %>%
    mutate(comprehensive_support = if_else(is.na(comprehensive_support), 0L, as.integer(comprehensive_support)))

matching <- full_join(
    comprehensive_support %>% select(system, school, pool, designation_ineligible, comprehensive_support),
    j %>% select(system, school, pool, comprehensive_support),
    by = c("system", "school", "pool")
)

table(matching$comprehensive_support.x, matching$comprehensive_support.y, useNA = "always")

View_if(matching, map2_lgl(comprehensive_support.x, comprehensive_support.y, function(x, y) !identical(x, y)))
