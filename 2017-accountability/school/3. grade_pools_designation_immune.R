library(readxl)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

school_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_sep27.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    filter(!system %in% c(960, 963, 964, 970, 972)) %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade)) %>%
    filter(year == 2017, subgroup == "All Students", grade %in% as.character(3:12)) %>%
    filter(subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate"))

# High Schools are schools with a grad cohort of at least 30
high_schools <- school_base %>%
    filter(subject == "Graduation Rate", subgroup == "All Students") %>%
    transmute(system, school, pool = if_else(grad_cohort >= 30, "HS", NA_character_))

# K8 are all other schools, assuming 30 tests in any subject
school_pools <- school_base %>%
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
    filter(subgroup == "All Students", !subject %in% c("ACT Math", "ACT Reading", "ACT Composite")) %>%
    filter(grade != "All Grades" | subject == "Graduation Rate") %>%
    mutate(valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        subject = case_when(
            subject %in% math_eoc & grade %in% as.character(3:8) ~ "Math",
            subject %in% english_eoc & grade %in% as.character(3:8) ~ "ELA",
            subject %in% science_eoc & grade %in% as.character(3:8) ~ "Science",
            TRUE ~ subject
        )
    ) %>%
    group_by(system, school, subject) %>%
    summarise_at("valid_tests", sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(tests_30 = valid_tests >= 30,
        tests_30_nograd = if_else(subject != "Graduation Rate", valid_tests >= 30, NA)) %>%
    group_by(system, school) %>%
    mutate(temp = max(tests_30, na.rm = TRUE),
        temp2 = max(tests_30_nograd, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(grad_only = as.numeric(temp == 1 & (temp2 %in% c(0, -Inf)))) %>%
    select(system, school, grad_only) %>%
    distinct()

grade_pools_designation_immune <- school_base %>%
    select(system, school) %>%
    distinct() %>%
    left_join(school_pools, by = c("system", "school")) %>%
    left_join(grad_only, by = c("system", "school")) %>%
    left_join(special_ed, by = c("system", "school")) %>%
    left_join(cte_alt_adult, by = c("system", "school")) %>%
    left_join(closed, by = c("system", "school")) %>%
    mutate_at(c("grad_only", "special_ed", "cte_alt_adult", "closed"), funs(if_else(is.na(.), 0, .))) %>%
    mutate(designation_ineligible = as.numeric(grad_only == 1 | special_ed == 1 | cte_alt_adult == 1 | closed == 1))

write_csv(grade_pools_designation_immune, path = "K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv", na = "")
