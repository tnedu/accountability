library(acct)
library(readxl)
library(tidyverse)

subgroups <- c("All Students", "Asian", "Black or African American", "Hispanic", "Native Hawaiian or Other Pacific Islander",
    "American Indian or Alaska Native", "White", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "Students with Disabilities", "Super Subgroup")

school_assessment <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/school_assessment_file.csv",
    col_types = "iicicccccdddiiiiddddd") %>%
    filter(school != 0, !grade %in% c("All Grades", "Missing Grade"),
        subgroup %in% c(subgroups, "English Learners with Transitional 1-4"))

grad <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>%
    filter(
        system != 0 & school != 0, system != 90,
        subgroup %in% c(subgroups, "English Learners")
    ) %>%
    transmute(year = 2018, system, school, subject, 
        subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup),
        grad_count, grad_cohort, grad_rate)

# Counting years where schools have at least one subject with 30 or more valid tests
one_year_or_less <- school_assessment %>%
    filter(subgroup == "All Students") %>%
# Aggregate tests across grades
    group_by(year, system, school, subject, subgroup) %>%
    summarise_at("valid_tests", sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(temp = as.integer(valid_tests >= 30)) %>%
    group_by(year, system, school, subgroup) %>%
    summarise(temp = max(temp, na.rm = TRUE)) %>%
    group_by(system, school) %>%
    summarise(num_years = sum(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    transmute(system, school, one_year_or_less = as.integer(num_years %in% c(0, 1)))

# High Schools are schools with a grad cohort of at least 30
high_schools <- grad %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school, pool = if_else(grad_cohort >= 30, "HS", NA_character_))

# K8 are all other schools, assuming 30 tests in any subject
school_pools <- school_assessment %>%
    bind_rows(grad) %>%
    filter(subgroup == "All Students") %>%
    left_join(high_schools, by = c("system", "school")) %>%
    mutate(
        grade = as.numeric(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, as.integer(valid_tests))
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, subject, pool) %>%
    summarise_at(c("valid_tests"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    group_by(system, school) %>%
    mutate(temp = sum(valid_tests >= 30)) %>%
    ungroup() %>%
    mutate(pool = if_else(is.na(pool) & temp >= 1, "K8", pool)) %>%
    select(system, school, pool) %>%
    distinct()

special_ed <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/sped_schools.csv") %>%
    transmute(system = as.numeric(SYSTEM), school = as.numeric(SCHOOL), special_ed = 1)

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
    transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), cte_alt_adult = 1)

closed <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/closed_schools.csv") %>%
    transmute(system = as.numeric(SYSTEM), school = as.numeric(SCHOOL), closed = 1)

grad_only <- school_assessment %>%
    bind_rows(grad) %>%
    mutate(valid_tests = if_else(subject == "Graduation Rate", grad_cohort, as.integer(valid_tests))) %>%
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
    rename(
        grad_only_all = `All Students`,
        grad_only_native = `American Indian or Alaska Native`,
        grad_only_asian = Asian,
        grad_only_black = `Black or African American`,
        grad_only_BHN = `Black/Hispanic/Native American`,           
        grad_only_ED = `Economically Disadvantaged`,
        grad_only_EL = `English Learners with Transitional 1-4`,
        grad_only_hispanic = Hispanic,
        grad_only_hawaiian_pi = `Native Hawaiian or Other Pacific Islander`,
        grad_only_SWD = `Students with Disabilities`,
        grad_only_super = `Super Subgroup`,
        grad_only_white = White
    )

school_names <- school_assessment %>%
    filter(year == 2018) %>%
    select(system, system_name, school, school_name) %>%
    distinct()

grade_pools_designation_immune <- school_names %>%
    left_join(school_pools, by = c("system", "school")) %>%
    left_join(one_year_or_less, by = c("system", "school")) %>%
    left_join(grad_only, by = c("system", "school")) %>%
    left_join(special_ed, by = c("system", "school")) %>%
    left_join(cte_alt_adult, by = c("system", "school")) %>%
    left_join(closed, by = c("system", "school")) %>%
    mutate_at(c("special_ed", "cte_alt_adult", "closed"), funs(if_else(is.na(.), 0L, as.integer(.)))) %>%
    mutate_at(vars(starts_with("grad_only_")), funs(if_else(is.na(.), 0L, as.integer(.)))) %>%
    mutate(designation_ineligible = as.numeric(one_year_or_less == 1 | grad_only_all == 1 | special_ed == 1 | cte_alt_adult == 1 | closed == 1))

write_csv(grade_pools_designation_immune, path = "N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv", na = "")
