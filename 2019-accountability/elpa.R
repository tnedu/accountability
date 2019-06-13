library(acct)
library(janitor)
library(tidyverse)

prior <- read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_student_level.csv",
            col_types = "diiiiiiiiiiddddddddddiiiddiiiiiiiiiii") %>%
    select(student_id, prof_composite_18 = prof_composite, prof_composite_17)

# Demographic file
demographics <- read_csv("N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv") %>%
    # Student IDs should be 7 digits
    filter(str_length(student_key) == 7) %>%
    transmute(
        student_id = student_key,
        system = district_id,
        school = school_id,
        reported_race = case_when(
            ethnicity == "H" ~ "Hispanic/Latino",
            isblack == 1 ~ "Black or African American",
            isamericanindian == 1 ~ "American Indian/Alaska Native",
            ispacificislander == 1 ~ "Native Hawaiian/Pac. Islander",
            isasian == 1 ~ "Asian",
            iswhite == 1 ~ "White",
            TRUE ~ "Unknown"
        ),
        BHN = reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"),
        Hispanic = reported_race == "Hispanic/Latino",
        Black = reported_race == "Black or African American",
        Native = reported_race == "American Indian/Alaska Native",
        HPI = reported_race == "Native Hawaiian/Pac. Islander",
        Asian = reported_race == "Asian",
        White = reported_race == "White",
        SWD = specialeducation == 1,
        ED = codeab == 1,
        EL = TRUE
    )

wida <- read_csv("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2018-19/TN_Summative_StudRR_File_2019-06-07.csv", guess_max = 20000) %>%
    clean_names() %>%
    filter(
        reported_mode != "Mixed",
        !(grade == 0 & cluster_listening != 0 & not_na(cluster_listening)),
        !(grade == 1 & cluster_listening != 1 & not_na(cluster_listening)),
        !(grade == 2 & reported_mode == "Paper" & cluster_listening != 2 & not_na(cluster_listening)),
        !(grade == 3 & reported_mode == "Paper" & cluster_listening != 3 & not_na(cluster_listening)),
        !(grade %in% 2:3 & reported_mode == "Online" & cluster_listening != 2 & not_na(cluster_listening)),
        !(grade %in% 4:5 & cluster_listening != 4 & not_na(cluster_listening)),
        !(grade %in% 6:8 & cluster_listening != 6 & not_na(cluster_listening)),
        !(grade %in% 9:12 & cluster_listening != 9 & not_na(cluster_listening))
    ) %>%
    transmute(
        student_id = state_student_id,
        system = as.integer(str_sub(district_number, 3, 5)),
        school = school_number,
        grade,
        scale_score_listening = listening_scale_score,
        scale_score_reading = reading_scale_score,
        scale_score_speaking = speaking_scale_score,
        scale_score_writing = writing_scale_score,
        scale_score_comprehension = comprehension_scale_score,
        scale_score_oral = oral_scale_score,
        scale_score_literacy = literacy_scale_score,
        scale_score_composite = composite_overall_scale_score,
        prof_listening = listening_proficiency_level,
        prof_reading = reading_proficiency_level,
        prof_speaking = speaking_proficiency_level,
        prof_writing = writing_proficiency_level,
        prof_comprehension = comprehension_proficiency_level,
        prof_oral = oral_proficiency_level,
        prof_literacy = literacy_proficiency_level,
        prof_composite = composite_overall_proficiency_level
    ) %>%
    mutate_at(vars(starts_with("scale_score_"), starts_with("prof_")), as.numeric) %>%
    filter(not_na(student_id), not_na(scale_score_composite)) %>%
    left_join(demographics, by = c("student_id", "system", "school")) %>%
    mutate(EL = TRUE) %>%
# Deduplicate by keeping highest composite scale score, then highest literacy scale score
    group_by(student_id) %>%
    mutate(max_comp = max(scale_score_composite, na.rm = TRUE)) %>%
    filter(scale_score_composite == max_comp) %>%
    mutate(max_lit = max(scale_score_literacy, na.rm = TRUE)) %>%
    filter(scale_score_literacy == max_lit) %>%
    ungroup() %>%
# Deduplicate by non-missing race/ethnicity and grade
    group_by(student_id) %>%
    mutate(
        n = n(),                           # Tag duplicates by id, subject, test, performance level, scale_score, semester
        temp = mean(is.na(reported_race))  # Check whether one among duplicates has non-missing race/ethnicity
    ) %>%
    filter(!(n > 1 & temp != 0 & is.na(reported_race))) %>%
    ungroup() %>%
    select(-n, -temp) %>%
    group_by(student_id, reported_race) %>%
    mutate(
        n = n(),                   # Tag duplicates by id, subject, test, performance, leve, scale_score, semester
        temp = mean(is.na(grade))  # Check whether one among duplicates has non-missing race/ethnicity
    ) %>%
    filter(!(n > 1 & temp != 0 & is.na(grade))) %>%
    ungroup() %>%
    select(-n, -temp) %>%
    left_join(prior, by = "student_id") %>%
    mutate(
        exit_denom = not_na(prof_composite) & not_na(prof_literacy),
        exit = prof_composite >= 4.2 & prof_literacy >= 4,
        growth_standard_denom = not_na(prof_composite) & not_na(prof_composite_18),
        growth_standard_1yr = case_when(
            is.na(prof_composite_18) ~ NA_real_,
            prof_composite_18 <= 1.4 ~ 1.3,
            prof_composite_18 <= 1.9 ~ 0.7,
            prof_composite_18 <= 2.4 ~ 0.8,
            prof_composite_18 <= 2.9 ~ 0.7,
            prof_composite_18 <= 3.4 ~ 0.4,
            prof_composite_18 <= 3.9 ~ 0.5,
            prof_composite_18 <= 4.4 ~ 0.4,
            prof_composite_18 <= 4.9 ~ 0.2
        ),
        growth_standard_2yr = case_when(
            is.na(prof_composite_17) ~ NA_real_,
            prof_composite_17 <= 1.4 ~ round5(1.3 + prof_composite_17, 1),
            prof_composite_17 <= 1.9 ~ round5(0.7 + prof_composite_17, 1),
            prof_composite_17 <= 2.4 ~ round5(0.8 + prof_composite_17, 1),
            prof_composite_17 <= 2.9 ~ round5(0.7 + prof_composite_17, 1),
            prof_composite_17 <= 3.4 ~ round5(0.4 + prof_composite_17, 1),
            prof_composite_17 <= 3.9 ~ round5(0.5 + prof_composite_17, 1),
            prof_composite_17 <= 4.4 ~ round5(0.4 + prof_composite_17, 1),
            prof_composite_17 <= 4.9 ~ round5(0.2 + prof_composite_17, 1)
        ),
        growth_standard_2yr = case_when(
            is.na(growth_standard_2yr) ~ NA_real_,
            growth_standard_2yr <= 1.4 ~ round5(1.3 + growth_standard_2yr, 1),
            growth_standard_2yr <= 1.9 ~ round5(0.7 + growth_standard_2yr, 1),
            growth_standard_2yr <= 2.4 ~ round5(0.8 + growth_standard_2yr, 1),
            growth_standard_2yr <= 2.9 ~ round5(0.7 + growth_standard_2yr, 1),
            growth_standard_2yr <= 3.4 ~ round5(0.4 + growth_standard_2yr, 1),
            growth_standard_2yr <= 3.9 ~ round5(0.5 + growth_standard_2yr, 1),
            growth_standard_2yr <= 4.4 ~ round5(0.4 + growth_standard_2yr, 1),
            growth_standard_2yr <= 4.9 ~ round5(0.2 + growth_standard_2yr, 1)
        ),
        met_growth_standard = case_when(
            growth_standard_denom == 0 ~ NA_integer_,
            round5(prof_composite - prof_composite_18, 1) >= growth_standard_1yr | prof_composite >= growth_standard_2yr ~ 1L,
            TRUE ~ 0L
        ),
        met_growth_standard = if_else(growth_standard_denom & exit, 1L, met_growth_standard)
    )

# Export Student Level File
wida %>%
    mutate_at(vars(BHN, Hispanic, Black, Native, HPI, Asian, White, ED, SWD, EL, exit_denom, exit, growth_standard_denom), as.integer) %>%
    select(-max_comp, -max_lit, -reported_race) %>%
    write_csv("N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_student.csv", na = "")

# Split Student Level File
district_numbers <- unique(wida$system)

wida %>%
    mutate_at(vars(BHN, Hispanic, Black, Native, HPI, Asian, White, ED, SWD, EL, exit_denom, exit, growth_standard_denom), as.integer) %>%
    select(-max_comp, -max_lit, -reported_race) %>%
    split(., .$system) %>%
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_ELPA/split/", .y, "_ACCESSStudentLevelFile_13Jun2018.csv"), na = ""))

wida <- wida %>%
    mutate_at(c("ED", "SWD"), ~ if_else(is.na(.), FALSE, .))

# School Level File
growth_standard_school <- map_dfr(
    .x = list(
        mutate(wida, subgroup = "All Students"),
        filter(wida, Native) %>% mutate(subgroup = "American Indian or Alaska Native"),
        filter(wida, Asian) %>% mutate(subgroup = "Asian"),
        filter(wida, Black) %>% mutate(subgroup = "Black or African American"),
        filter(wida, BHN) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(wida, ED) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(wida, EL) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
        filter(wida, Hispanic) %>% mutate(subgroup = "Hispanic"),
        filter(wida, HPI) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
        filter(wida, !ED) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
        filter(wida, !SWD) %>% mutate(subgroup = "Non-Students with Disabilities"),
        filter(wida, SWD) %>% mutate(subgroup = "Students with Disabilities"),
        filter(wida, White) %>% mutate(subgroup = "White")
    ),
    .f = function(x) {
        group_by(x, system, school, subgroup) %>%
        summarise(
            exit_denom = sum(exit_denom, na.rm = TRUE),
            n_exit = sum(exit, na.rm = TRUE),
            growth_standard_denom = sum(growth_standard_denom, na.rm = TRUE),
            n_met_growth_standard = sum(met_growth_standard, na.rm = TRUE),
            literacy_average = round5(mean(prof_literacy, na.rm = TRUE), 1),
            composite_average = round5(mean(prof_composite, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        transmute(
            system, school, subgroup,
            exit_denom, n_exit,
            pct_exit = if_else(exit_denom != 0, round5(100 * n_exit/exit_denom, 1), NA_real_),
            growth_standard_denom, n_met_growth_standard,
            pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
            literacy_average, composite_average
        )
    }
) %>%
    arrange(system, school, subgroup)

# Export School Level File
write_csv(growth_standard_school, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_school.csv", na = "")

# Split School Level File
growth_standard_school %>%
    split(., .$system) %>%
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_ELPA/split/", .y, "_ACCESSSchoolLevelFile_13Jun2018.csv"), na = ""))

# District Level File
growth_standard_district <- map_dfr(
    .x = list(
        mutate(wida, subgroup = "All Students"),
        filter(wida, Native) %>% mutate(subgroup = "American Indian or Alaska Native"),
        filter(wida, Asian) %>% mutate(subgroup = "Asian"),
        filter(wida, Black) %>% mutate(subgroup = "Black or African American"),
        filter(wida, BHN) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(wida, ED) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(wida, EL) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
        filter(wida, Hispanic) %>% mutate(subgroup = "Hispanic"),
        filter(wida, HPI) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
        filter(wida, !ED) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
        filter(wida, !SWD) %>% mutate(subgroup = "Non-Students with Disabilities"),
        filter(wida, SWD) %>% mutate(subgroup = "Students with Disabilities"),
        filter(wida, White) %>% mutate(subgroup = "White")
    ),
    .f = function(x) {
        group_by(x, system, subgroup) %>%
        summarise(
            exit_denom = sum(exit_denom, na.rm = TRUE),
            n_exit = sum(exit, na.rm = TRUE),
            growth_standard_denom = sum(growth_standard_denom, na.rm = TRUE),
            n_met_growth_standard = sum(met_growth_standard, na.rm = TRUE),
            literacy_average = round5(mean(prof_literacy, na.rm = TRUE), 1),
            composite_average = round5(mean(prof_composite, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        transmute(
            system, subgroup,
            exit_denom, n_exit,
            pct_exit = if_else(exit_denom != 0, round5(100 * n_exit/exit_denom, 1), NA_real_),
            growth_standard_denom, n_met_growth_standard,
            pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
            literacy_average, composite_average
        )
    }
) %>%
    arrange(system, subgroup)

# Export District Level File
write_csv(growth_standard_district, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_district.csv", na = "")

# Split District Level File
growth_standard_district %>%
    split(., .$system) %>%
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_ELPA/split/", .y, "_ACCESSDistrictLevelFile_13Jun2018.csv"), na = ""))

# State Level File
growth_standard_state <- map_dfr(
    .x = list(
        mutate(wida, subgroup = "All Students"),
        filter(wida, Native) %>% mutate(subgroup = "American Indian or Alaska Native"),
        filter(wida, Asian) %>% mutate(subgroup = "Asian"),
        filter(wida, Black) %>% mutate(subgroup = "Black or African American"),
        filter(wida, BHN) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(wida, ED) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(wida, EL) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
        filter(wida, Hispanic) %>% mutate(subgroup = "Hispanic"),
        filter(wida, HPI) %>% mutate(subgroup = "Native Hawaiian or Other Pacific Islander"),
        filter(wida, !ED) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
        filter(wida, !SWD) %>% mutate(subgroup = "Non-Students with Disabilities"),
        filter(wida, SWD) %>% mutate(subgroup = "Students with Disabilities"),
        filter(wida, White) %>% mutate(subgroup = "White")
    ),
    .f = function(x) {
        group_by(x, subgroup) %>%
        summarise(
            exit_denom = sum(exit_denom, na.rm = TRUE),
            n_exit = sum(exit, na.rm = TRUE),
            growth_standard_denom = sum(growth_standard_denom, na.rm = TRUE),
            n_met_growth_standard = sum(met_growth_standard, na.rm = TRUE),
            literacy_average = round5(mean(prof_literacy, na.rm = TRUE), 1),
            composite_average = round5(mean(prof_composite, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        transmute(
            subgroup,
            exit_denom, n_exit,
            pct_exit = if_else(exit_denom != 0, round5(100 * n_exit/exit_denom, 1), NA_real_),
            growth_standard_denom, n_met_growth_standard,
            pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
            literacy_average, composite_average
        )
    }
) %>%
    arrange(subgroup)

# Export State Level File
write_csv(growth_standard_state, "N:/ORP_accountability/data/2019_ELPA/wida_growth_standard_state.csv", na = "")
