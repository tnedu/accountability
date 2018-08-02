library(acct)
library(readxl)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

student_level <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(grade %in% c(0, 3:12), residential_facility == 0, homebound == 0, enrolled_50_pct_school == "Y") %>%
    # Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate(year = 2018,
        grade = if_else(is.na(grade), 0L, grade),
        test = if_else(test %in% c("MSAA", "ALT_SCI"), "MSAA/Alt-Science", test),
        All = 1L,
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic/Latino",
        Hawaiian = race == "Native Hawaiian/Pac. Islander",
        Native = race == "American Indian/Alaska Native",
        White = race == "White",
        EL_T1234 = if_else(EL == 1, 1L, EL_T1234),
        Super = (BHN == 1L | ED == 1L | SWD == 1L | EL_T1234 == 1L)) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD", "EL_T1234", "Super"), as.integer)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("All", "Asian", "Black", "Hispanic", "Hawaiian", "Native", "White", "BHN", "ED", "SWD", "EL_T1234", "Super")) {

    collapse <- student_level %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, test, original_subject) %>%
        summarise_at(c("enrolled", "tested", "valid_test"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s, grade = "All Grades") %>%
        bind_rows(collapse, .)

    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, school, test, original_subject, grade) %>%
        summarise_at(c("enrolled", "tested", "valid_test"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)

}

grad <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>%
    filter(system != 0 & school != 0, system != 90,
        subgroup %in% c("All Students", "Asian", "Black or African American", "Hispanic", "Native Hawaiian or Other Pacific Islander",
            "American Indian or Alaska Native", "White", "Black/Hispanic/Native American", "Economically Disadvantaged",
            "Students with Disabilities", "English Learners", "Super Subgroup")
    ) %>%
    transmute(year = 2018, system, school, subject, grade = "All Grades",
        subgroup = case_when(
            subgroup == "English Learners" ~ "English Learners with Transitional 1-4",
            TRUE ~ subgroup
        ),
        grad_count, grad_cohort, grad_rate)

school_accountability <- collapse %>%
    rename(valid_tests = valid_test, subject = original_subject) %>%
    mutate(grade = if_else(grade == 0, "Missing Grade", as.character(grade)),
        subgroup = case_when(
            subgroup == "All" ~ "All Students",
            subgroup == "Black" ~ "Black or African American",
            subgroup == "BHN" ~ "Black/Hispanic/Native American",
            subgroup == "ED" ~ "Economically Disadvantaged",
            subgroup == "EL" ~ "English Learners",
            subgroup == "EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            subgroup == "Super" ~ "Super Subgroup",
            subgroup == "SWD" ~ "Students with Disabilities",
            TRUE ~ subgroup
        )
    ) %>%
    select(year, system, school, test, subject, grade, subgroup, enrolled, tested, valid_tests) %>%
    bind_rows(grad) %>%
    filter(!system %in% c(960, 963, 964, 970, 972)) %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade)) %>%
    filter(year == 2018, grade %in% as.character(3:12),
        subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate"))

# High Schools are schools with a grad cohort of at least 30
high_schools <- school_accountability %>%
    filter(subject == "Graduation Rate", subgroup == "All Students") %>%
    transmute(system, school, pool = if_else(grad_cohort >= 30, "HS", NA_character_)) %>%
    filter(!is.na(pool))

# K8 are all other schools, assuming 30 tests in any subject
school_pools <- school_accountability %>%
    filter(subgroup == "All Students") %>%
    left_join(high_schools, by = c("system", "school")) %>%
    mutate(grade = as.numeric(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
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

grad_only <- school_accountability %>%
    filter(grade != "All Grades" | subject == "Graduation Rate") %>%
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

school_names <- student_level %>%
    select(system, system_name, school, school_name) %>%
    distinct()

grade_pools_designation_immune <- school_names %>%
    left_join(school_pools, by = c("system", "school")) %>%
    left_join(grad_only, by = c("system", "school")) %>%
    left_join(special_ed, by = c("system", "school")) %>%
    left_join(cte_alt_adult, by = c("system", "school")) %>%
    left_join(closed, by = c("system", "school")) %>%
    mutate_at(c("special_ed", "cte_alt_adult", "closed"), funs(if_else(is.na(.), 0L, as.integer(.)))) %>%
    mutate_at(vars(starts_with("grad_only_")), funs(if_else(is.na(.), 0L, as.integer(.)))) %>%
    mutate(designation_ineligible = as.numeric(grad_only_all == 1 | special_ed == 1 | cte_alt_adult == 1 | closed == 1))

write_csv(grade_pools_designation_immune, path = "N:/ORP_accountability/projects/2018_school_accountability/grade_pools_designation_immune.csv", na = "")
