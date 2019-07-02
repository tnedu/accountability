library(acct)
library(readxl)
library(tidyverse)

school_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_assessment_file.csv") %>%
    filter(subgroup == "All Students", !grade %in% c("All Grades", "Missing Grade")) %>%
    filter(!(system == 964 & school == 964 | system == 970 & school == 970))

grad <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>%
    filter(subgroup == "All Students") %>%
    transmute(system, school, subgroup, grad_count, grad_cohort, grad_rate)

# Counting years where schools have at least one subject with 30 or more valid tests
one_year_or_less <- school_assessment %>%
    mutate(
        subject = if_else(subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III"), "HS Math", subject),
        subject = if_else(subject %in% c("English I", "English II"), "HS English", subject)
    ) %>%
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
    transmute(system, school, pool = if_else(grad_cohort >= 30, "HS", NA_character_))

# K8 are all other schools if 30 tests in any subject
school_pools <- school_assessment %>%
    filter(year == 2019) %>%
    full_join(high_schools, by = c("system", "school")) %>%
    mutate(
        subject = if_else(subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III"), "HS Math", subject),
        subject = if_else(subject %in% c("English I", "English II"), "HS English", subject)
    ) %>%
# Aggregate test counts across grades
    group_by(system, school, subject, pool) %>%
    summarise_at(c("valid_tests"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    group_by(system, school) %>%
    mutate(temp = sum(valid_tests >= 30)) %>%
    ungroup() %>%
    mutate(pool = if_else(is.na(pool) & temp >= 1, "K8", pool)) %>%
    select(system, school, pool) %>%
    distinct()

special_ed <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/sped_schools.csv") %>%
    transmute(system = as.numeric(SYSTEM), school = as.numeric(SCHOOL), special_ed = 1)

cte_alt_adult <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
    transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), cte_alt_adult = 1)

closed <- read_csv("N:/ORP_accountability/data/2019_tdoe_provided_files/closed_schools.csv") %>%
    transmute(system = as.numeric(SYSTEM), school = as.numeric(SCHOOL), closed = 1)

grade_pools_designation_immune <- school_pools %>%
    left_join(one_year_or_less, by = c("system", "school")) %>%
    left_join(special_ed, by = c("system", "school")) %>%
    left_join(cte_alt_adult, by = c("system", "school")) %>%
    left_join(closed, by = c("system", "school")) %>%
    mutate_at(c("special_ed", "cte_alt_adult", "closed"), ~ if_else(is.na(.), 0L, as.integer(.))) %>%
    mutate(designation_ineligible = as.numeric(one_year_or_less == 1 | special_ed == 1 | cte_alt_adult == 1 | closed == 1))

write_csv(grade_pools_designation_immune, path = "N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv", na = "")
