library(readxl)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

school_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(!system %in% c(960, 963, 964, 970, 972)) %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade)) %>%
    filter(year == 2017, grade %in% as.character(3:12)) %>%
    filter(subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate"))

# High Schools are schools with a grad cohort of at least 30
high_schools <- school_base %>%
    filter(subject == "Graduation Rate", subgroup == "All Students") %>%
    transmute(system, school, pool = if_else(grad_cohort >= 30, "HS", NA_character_))

# K8 are all other schools, assuming 30 tests in any subject
school_pools <- school_base %>%
    filter(subgroup == "All Students") %>%
    left_join(high_schools, by = c("system", "school")) %>%
    mutate(grade = as.numeric(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_on_track = if_else(subject == "Graduation Rate", grad_count, n_on_track),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    # Aggregate by replaced subjects
    group_by(year, system, school, subject, pool) %>%
    summarise_at(c("valid_tests", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    group_by(system, school) %>%
    mutate(temp = sum(valid_tests >= 30)) %>%
    ungroup() %>%
    mutate(pool = if_else(is.na(pool) & temp >= 1, "K8", pool)) %>%
    select(system, school, pool) %>%
    distinct()

special_ed <- read_excel("K:/ORP_accountability/data/2017_tdoe_provided_files/List of Schools Acct 2016-17.xlsx",
        sheet = "SPED Schools") %>%
    transmute(system = DISTRICT_NUMBER, school = SCHOOL_NUMBER, special_ed = 1)

cte_alt_adult <- read_excel("K:/ORP_accountability/data/2017_tdoe_provided_files/List of Schools Acct 2016-17.xlsx",
        sheet = "CTE Adult and Alternative") %>%
    transmute(system = DISTRICT_NUMBER, school = SCHOOL_NUMBER, cte_alt_adult = 1)

closed_prior <- read_excel("K:/ORP_accountability/data/2017_tdoe_provided_files/List of Schools Acct 2016-17.xlsx",
        sheet = "Closed Schools prior 2016-17") %>%
    transmute(system = DISTRICT_NUMBER, school = SCHOOL_NUMBER, closed = 1)

closed <- read_excel("K:/ORP_accountability/data/2017_tdoe_provided_files/List of Schools Acct 2016-17.xlsx",
        sheet = "Closed Schools") %>%
    transmute(system = DISTRICT_NUMBER, school = SCHOOL_NUMBER, closed = 1) %>%
    bind_rows(closed_prior)

grad_only <- school_base %>%
    filter(subgroup %in% c("All Students", "Black/Hispanic/Native American",
            "Economically Disadvantaged", "Non-Economically Disadvantaged",
            "Students with Disabilities", "Non-Students with Disabilities",
            "English Learners with T1/T2", "Non-English Learners/T1 or T2"),
        !subject %in% c("ACT Math", "ACT Reading", "ACT Composite"),
        grade != "All Grades" | subject == "Graduation Rate") %>%
    mutate(valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        subject = case_when(
            subject %in% math_eoc & grade %in% as.character(3:8) ~ "Math",
            subject %in% english_eoc & grade %in% as.character(3:8) ~ "ELA",
            subject %in% science_eoc & grade %in% as.character(3:8) ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    group_by(system, school, subgroup, subject) %>%
    summarise_at("valid_tests", sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(tests_30 = valid_tests >= 30,
        tests_30_nograd = if_else(subject != "Graduation Rate", valid_tests >= 30, NA)) %>%
    group_by(system, school, subgroup) %>%
    mutate(temp = max(tests_30, na.rm = TRUE),
        temp2 = max(tests_30_nograd, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(grad_only = as.numeric(temp == 1 & (temp2 %in% c(0, -Inf)))) %>%
    select(system, school, subgroup, grad_only) %>%
    distinct() %>%
    spread(subgroup, grad_only) %>%
    rename(grad_only_All = `All Students`,
        grad_only_BHN = `Black/Hispanic/Native American`,
        grad_only_ED = `Economically Disadvantaged`,
        grad_only_SWD = `Students with Disabilities`,
        grad_only_EL = `English Learners with T1/T2`,
        grad_only_Non_ED = `Non-Economically Disadvantaged`,
        grad_only_Non_SWD = `Non-Students with Disabilities`,
        grad_only_Non_EL = `Non-English Learners/T1 or T2`)

grade_pools_designation_immune <- school_base %>%
    select(system, school) %>%
    distinct() %>%
    left_join(school_pools, by = c("system", "school")) %>%
    left_join(grad_only, by = c("system", "school")) %>%
    left_join(special_ed, by = c("system", "school")) %>%
    left_join(cte_alt_adult, by = c("system", "school")) %>%
    left_join(closed, by = c("system", "school")) %>%
    mutate_at(c("grad_only_All", "grad_only_BHN", "grad_only_ED", "grad_only_SWD", "grad_only_EL",
        "grad_only_Non_ED", "grad_only_Non_SWD", "grad_only_Non_EL",
        "special_ed", "cte_alt_adult", "closed"), funs(if_else(is.na(.), 0, .))) %>%
    mutate(designation_ineligible = as.numeric(grad_only_All == 1 | special_ed == 1 | cte_alt_adult == 1 | closed == 1))

write_csv(grade_pools_designation_immune, path = "K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv", na = "")
