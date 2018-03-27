library(acct)
library(haven)
library(tidyverse)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

student_level <- read_dta("N:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_03212018_wgender.dta") %>%
    # Residential Facility students are dropped from system level
    filter(test %in% c("EOC", "TNReady"),
        residential_facility != 1 | is.na(residential_facility)) %>%
    # Proficiency and subgroup indicators for collapse
    mutate(year = 2017,
        grade = if_else(subject %in% c("Algebra I", "Algebra II", "Biology I", "Chemistry",
                "English I", "English II", "English III", "Geometry",
                "Integrated Math I", "Integrated Math II", "Integrated Math III", "US History"),
            "All Grades", as.character(grade)),
        n_on_track = if_else(performance_level %in% c("3. On Track", "3. Proficient"), 1L, NA_integer_),
        n_mastered = if_else(performance_level %in% c("4. Mastered", "4. Advanced"), 1L, NA_integer_),
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic",
        Hawaiian = race == "Native Hawaiian or Pacific Islander",
        Native = race == "American Indian or Alaskan Native",
        White = race == "White"
    ) %>%
    mutate_at(c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White"), as.integer)

collapse <- tibble()

# Collapse proficiency by subject and subgroup
for (s in c("Asian", "Black", "Hispanic", "Hawaiian", "Native", "White")) {
    
    collapse <- student_level %>%
        mutate(grade = as.character(grade)) %>%
        filter_(paste(s, "== 1L")) %>%
        group_by(year, system, subject, grade, gender) %>%
        summarise_at(c("valid_test", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
        mutate(subgroup = s) %>%
        bind_rows(collapse, .)
    
}

system_base <- collapse %>%
    transmute(year, system, subject, grade, gender,
        subgroup = case_when(
            subgroup == "Black" ~ "Black or African American",
            subgroup == "Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "Native" ~ "American Indian or Alaska Native",
            TRUE ~ subgroup
        ),
        valid_tests = valid_test,
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_)
    )

# Names crosswalk
system_names <- read_csv("N:/ORP_accountability/data/2017_final_accountability_files/system_name_crosswalk.csv")

# Create new rows (with 0 enrolled, tested, etc.) for missing year/grade/subject/subgroup combinations
all_combinations <- expand.grid(stringsAsFactors = FALSE,
    year = 2017,
    system = unique(system_names$system),
    subject = c("Algebra I", "Algebra II", "Biology I", "Chemistry", "English I", "English II", "English III",
        "Geometry", "US History", "Integrated Math I", "Integrated Math II", "Integrated Math III"),
    gender = c("M", "F"),
    grade = c("3", "4", "5", "6", "7", "8", "All Grades"),
    subgroup = c("American Indian or Alaska Native", "Asian", "Black or African American",
        "Hispanic", "Native Hawaiian or Other Pacific Islander", "White"))

# Output file
base_2017 <- system_base %>%
    # Add entries for missing subgroups (with 0 enrolled, valid tests, etc.)
    full_join(all_combinations, by = c("year", "system", "subject", "grade", "subgroup", "gender")) %>%
    group_by(system, subject, grade) %>%
    mutate(temp = sum(is.na(valid_tests))) %>%
    filter(temp != 12) %>%
    select(-temp) %>%
    ungroup() %>%
    mutate_at(c("valid_tests"), funs(if_else(is.na(.), 0, .))) %>%
    # Add ACT, grad, and system names
    left_join(system_names, by = "system") %>%
    arrange(desc(year), system, subject, grade, subgroup) %>%
    select(year, system, system_name, everything()) %>%
    mutate(pct_on_mastered = case_when(
        valid_tests == 0 ~ NA_character_,
        valid_tests < 10 ~ "*",
        pct_on_mastered < 1 ~ "**",
        TRUE ~ as.character(pct_on_mastered)
        )
    )

write_csv(base_2017, path = "N:/ORP_accountability/data/2017_final_accountability_files/system_base_race_gender.csv", na = "")
