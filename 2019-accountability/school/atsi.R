library(acct)
library(readxl)
library(tidyverse)

cutoffs <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/comprehensive_support.csv") %>%
    filter(designation_ineligible == 0, system != 985, comprehensive_support == 1, is.na(grad_less_than_67)) %>%
    group_by(pool) %>%
    summarise(cutoff = max(pct_on_mastered, na.rm = TRUE)) %>%
    ungroup()

pools <- read_csv("N:/ORP_accountability/projects/2019_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool)

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

student_2017 <- haven::read_dta("N:/ORP_accountability/projects/2017_student_level_file/state_student_level_2017_JP_final_10192017.dta") %>%
    filter(
        greater_than_60_pct == "Y",
        homebound == 0 | is.na(homebound),
        # residential_facility == 0 | is.na(residential_facility),  # These are all NA
        original_subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = ell, EL_T1_T2 = ell_t1t2) %>%
    mutate_at(c("BHN", "ED", "SWD", "EL", "EL_T1_T2"), as.logical) %>%
    mutate(
        year = 2017,
        ot_m = if_else(performance_level %in% c("3. On Track", "3. Proficient", "4. Mastered", "4. Advanced"), 1L, NA_integer_),
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic",
        Hawaiian = race == "Native Hawaiian or Pacific Islander",
        Native = race == "American Indian or Alaskan Native",
        White = race == "White",
        EL_T1234 = EL | EL_T1_T2
    )

grad_count_2017 <- read_csv("N:/ORP_accountability/data/2016/2016_graduation_rate/grad_rate_base_EK.csv") %>%
    mutate(
        subgroup = case_when(
            subgroup == "Asian" ~ "~Asian",
            subgroup == "Black or African American" ~ "~Black",
            subgroup == "Black/Hispanic/Native American" ~ "~BHN",
            subgroup == "Economically Disadvantaged" ~ "~ED",
            subgroup == "English Learners" ~ "~EL_T1234",
            subgroup == "Native Hawaiian or Other Pacific Islander" ~ "~Hawaiian",
            subgroup == "Hispanic" ~ "~Hispanic",
            subgroup == "American Indian or Alaska Native" ~ "~Native",
            subgroup == "Students with Disabilities" ~ "~SWD",
            subgroup == "White" ~ "~White"
        )
    ) %>% 
    filter(system != 0, school != 0, not_na(subgroup)) %>%
    select(system, school, subgroup, valid_tests = grad_count)

ACT_2017 <- haven::read_dta("N:/ORP_accountability/data/2016/2016_ACT/ACT_school2017_individualsubgroups.dta") %>%
    filter(subgroup %in% c("Asian", "Black or African American", "Black/Hispanic/Native American",
        "Economically Disadvantaged", "English Language Learners with T1/T2", "HPI", "Hispanic", 
        "Native American", "Students with Disabilities", "White")) %>%
    transmute(
        year = 2017,
        system, school,
        subject = "ACT",
        subgroup = case_when(
            subgroup == "Asian" ~ "~Asian",
            subgroup == "Black or African American" ~ "~Black",
            subgroup == "Black/Hispanic/Native American" ~ "~BHN",
            subgroup == "Economically Disadvantaged" ~ "~ED",
            subgroup == "English Language Learners with T1/T2" ~ "~EL_T1234",
            subgroup == "HPI" ~ "~Hawaiian",
            subgroup == "Hispanic" ~ "~Hispanic",
            subgroup == "Native American" ~ "~Native",
            subgroup == "Students with Disabilities" ~ "~SWD",
            subgroup == "White" ~ "~White"
        ),
        ot_m = n_21_orhigher
    ) %>%
    left_join(grad_count_2017, by = c("system", "school", "subgroup"))

student_2018 <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv",
        col_types = "iciccccccciiiidcciciiiiiiiicciiii") %>%
    filter(
        grade %in% 3:12,
        residential_facility == 0,
        homebound == 0,
        enrolled_50_pct_school == "Y",
        original_subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc)
    ) %>%
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, EL_T1234 = el_t1234) %>%
    mutate_at(c("BHN", "ED", "SWD", "EL", "EL_T1234"), as.logical) %>%
    mutate(
        year = 2018,
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        Asian = race == "Asian",
        Black = race == "Black or African American",
        Hispanic = race == "Hispanic/Latino",
        Hawaiian = race == "Native Hawaiian/Pac. Islander",
        Native = race == "American Indian/Alaska Native",
        White = race == "White",
        EL_T1234 = EL | EL_T1234,
    )

grad_count_2018 <- read_csv("N:/ORP_accountability/data/2017_graduation_rate/grad_rate_base_EK.csv") %>%
    mutate(
        subgroup = case_when(
            subgroup == "Asian" ~ "~Asian",
            subgroup == "Black or African American" ~ "~Black",
            subgroup == "Black/Hispanic/Native American" ~ "~BHN",
            subgroup == "Economically Disadvantaged" ~ "~ED",
            subgroup == "English Learners" ~ "~EL_T1234",
            subgroup == "Native Hawaiian or Other Pacific Islander" ~ "~Hawaiian",
            subgroup == "Hispanic" ~ "~Hispanic",
            subgroup == "American Indian or Alaska Native" ~ "~Native",
            subgroup == "Students with Disabilities" ~ "~SWD",
            subgroup == "White" ~ "~White"
        )
    ) %>%
    filter(system != 0, school != 0, not_na(subgroup)) %>%
    select(system, school, subgroup, valid_tests = grad_count)

ACT_2018 <- haven::read_dta("N:/ORP_accountability/data/2017_ACT/ACT_school2018_appeals2.dta") %>%
    filter(
        subgroup %in% c("Asian", "Black or African American", "Black/Hispanic/Native American",
            "Economically Disadvantaged", "English Language Learners with T1/T2", "HPI", "Hispanic", 
            "Native American", "Students with Disabilities", "White")
    ) %>%
    transmute(
        year = 2018,
        system, school,
        subject = "ACT",
        subgroup = case_when(
            subgroup == "Asian" ~ "~Asian",
            subgroup == "Black or African American" ~ "~Black",
            subgroup == "Black/Hispanic/Native American" ~ "~BHN",
            subgroup == "Economically Disadvantaged" ~ "~ED",
            subgroup == "English Language Learners with T1/T2" ~ "~EL_T1234",
            subgroup == "HPI" ~ "~Hawaiian",
            subgroup == "Hispanic" ~ "~Hispanic",
            subgroup == "Native American" ~ "~Native",
            subgroup == "Students with Disabilities" ~ "~SWD",
            subgroup == "White" ~ "~White"
        ),
        ot_m = n_21_orhigher,
    ) %>%
    mutate_at(c("system", "school"), as.numeric) %>%
    left_join(grad_count_2018, by = c("system", "school", "subgroup"))

student_2019 <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
    filter(!(system == 964 & school == 964 | system == 970 & school == 970)) %>%
# Fill in missing residential facility and enrolled 50%
# Otherwise will get dropped when checking residential facility = 0 and enrolled 50% = "Y"
    mutate_at("residential_facility", ~ if_else(is.na(.), 0, .)) %>%
    mutate_at("enrolled_50_pct_school", ~ if_else(is.na(.), "Y", .)) %>%
    filter(
        residential_facility == 0,
        (enrolled_50_pct_school == "Y" | (acct_system != system | acct_school != school)),
        original_subject %in% c("Math", "ELA", math_eoc, english_eoc)
    ) %>%
# Proficiency and subgroup indicators for collapse
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, EL = el, T1234 = t1234) %>%
    mutate_at(c("BHN", "ED", "SWD", "EL", "T1234"), as.logical) %>%
    mutate(
        year = 2019,
        ot_m = if_else(performance_level %in% c("On Track", "Proficient", "Mastered", "Advanced"), 1L, NA_integer_),
        Asian = reported_race == "Asian",
        Black = reported_race == "Black or African American",
        Hispanic = reported_race == "Hispanic/Latino",
        Hawaiian = reported_race == "Native Hawaiian/Pac. Islander",
        Native = reported_race == "American Indian/Alaska Native",
        White = reported_race == "White",
        EL_T1234 = EL | T1234,
    ) %>%
    select(-system, -school) %>%
    rename(system = acct_system, school = acct_school)

grad_count_2019 <- read_csv("N:/ORP_accountability/data/2018_graduation_rate/school_grad_rate.csv") %>%
    mutate(
        subgroup = case_when(
            subgroup == "Asian" ~ "~Asian",
            subgroup == "Black or African American" ~ "~Black",
            subgroup == "Black/Hispanic/Native American" ~ "~BHN",
            subgroup == "Economically Disadvantaged" ~ "~ED",
            subgroup == "English Learners with Transitional 1-4" ~ "~EL_T1234",
            subgroup == "Native Hawaiian or Other Pacific Islander" ~ "~Hawaiian",
            subgroup == "Hispanic" ~ "~Hispanic",
            subgroup == "American Indian or Alaska Native" ~ "~Native",
            subgroup == "Students with Disabilities" ~ "~SWD",
            subgroup == "White" ~ "~White"
        )
    ) %>%
    filter(system != 0, school != 0, not_na(subgroup)) %>%
    select(system, school, subgroup, valid_tests = grad_count)

ACT_2019 <- haven::read_dta("N:/ORP_accountability/data/2018_ACT/Post-Appeals/ACT_school2019_appeals.dta") %>%
    filter(
        subgroup %in% c("Asian", "Black or African American", "Black/Hispanic/Native American",
            "Economically Disadvantaged", "English Language Learners with T1/T2", "HPI", "Hispanic", 
            "Native American", "Students with Disabilities", "White")
    ) %>%
    transmute(
        year = 2019,
        system, school,
        subject = "ACT",
        subgroup = case_when(
            subgroup == "Asian" ~ "~Asian",
            subgroup == "Black or African American" ~ "~Black",
            subgroup == "Black/Hispanic/Native American" ~ "~BHN",
            subgroup == "Economically Disadvantaged" ~ "~ED",
            subgroup == "English Language Learners with T1/T2" ~ "~EL_T1234",
            subgroup == "HPI" ~ "~Hawaiian",
            subgroup == "Hispanic" ~ "~Hispanic",
            subgroup == "Native American" ~ "~Native",
            subgroup == "Students with Disabilities" ~ "~SWD",
            subgroup == "White" ~ "~White"
        ),
        ot_m = n_21_orhigher
    ) %>%
    left_join(grad_count_2019, by = c("system", "school", "subgroup"))

collapse <- function(g) {

    g_quo <- enquo(g)

    student %>%
        filter(!!g_quo) %>%
        group_by(year, system, school, subject) %>%
        summarise_at(c("valid_test", "ot_m"), sum, na.rm = TRUE) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo))

}

student <- bind_rows(student_2017, student_2018, student_2019)

three_year_subgroup_success <- map_dfr(
    .x = list(
        quo(Asian), quo(Black), quo(Hawaiian), quo(Hispanic), quo(Native), quo(White), 
        quo(BHN), quo(ED), quo(SWD), quo(EL_T1234)
    ),
    .f = ~ collapse(!!.)
) %>%
    rename(valid_tests = valid_test) %>%
    bind_rows(ACT_2017, ACT_2018, ACT_2019) %>%
# Drop schools that only appear in prior years
    group_by(system, school) %>%
    mutate(temp = max(year)) %>%
    ungroup() %>%
    filter(temp == 2019) %>%
    select(-temp) %>%
    mutate(
        subgroup = case_when(
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hawaiian" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        ),
        subject = case_when(
            subject %in% english_eoc ~ "HS English",
            subject %in% math_eoc ~ "HS Math",
            subject %in% science_eoc ~ "HS Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by subject (HS Math/English/Science for HS)
    group_by(year, system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress subjects with n < 30 by year
    mutate_at(c("valid_tests", "ot_m"), ~ if_else(valid_tests < 30, 0L, as.integer(.))) %>%
# Aggregate valid tests, OT/M across subjects
    group_by(year, system, school, subgroup) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
# Aggregate valid tests, OT/M across years
    group_by(system, school, subgroup) %>%
    summarise_at(c("valid_tests", "ot_m"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(success_rate = if_else(valid_tests != 0, round5(100 * ot_m/valid_tests, 1), NA_real_)) %>%
    left_join(pools, by = c("system", "school")) %>%
    inner_join(cutoffs, by = "pool")

write_csv(three_year_subgroup_success, "N:/ORP_accountability/projects/2019_school_accountability/atsi_metrics.csv", na = "")

subgroup_below_priority <- three_year_subgroup_success %>%
# Identify schools with a subgroup performing at or below the priority school cutoff by pool
    mutate(subgroup_below_priority = as.integer(success_rate <= cutoff)) %>%
    select(system, school, subgroup, subgroup_below_priority) %>%
    spread(subgroup, subgroup_below_priority) %>%
    select(
        system, school,
        subgroup_below_BHN = `Black/Hispanic/Native American`,
        subgroup_below_ED = `Economically Disadvantaged`,
        subgroup_below_SWD = `Students with Disabilities`,
        subgroup_below_EL = `English Learners with Transitional 1-4`,
        subgroup_below_Native = `American Indian or Alaska Native`,
        subgroup_below_Asian = Asian,
        subgroup_below_Black = `Black or African American`,
        subgroup_below_Hispanic = Hispanic,
        subgroup_below_HPI = `Native Hawaiian or Other Pacific Islander`,
        subgroup_below_White = White
    )

write_csv(subgroup_below_priority, "N:/ORP_accountability/projects/2019_school_accountability/subgroup_below_priority.csv", na = "")
