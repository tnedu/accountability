library(acct)
library(tidyverse)

prior <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Data/WIDA/2016-17/WIDA_student_level2017.csv") %>%
    janitor::clean_names() %>%
    select(student_id = unique_student_id,
        prof_composite_17 = composite_overall_proficiency_level,
        prof_composite_16 = prof_level_composite_lag)

wida <- haven::read_dta("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/WIDA/20180606_WIDA_AccessResults_SY2017-18_Whalen_v2.dta") %>%
    janitor::clean_names() %>%
    transmute(
        student_id = as.numeric(statestudentid),
        system = as.integer(str_sub(districtnumber, 3, 5)),
        school = schoolnumber,
        scale_score_listening = listeningscalescore,
        scale_score_reading = readingscalescore,
        scale_score_speaking = speakingscalescore,
        scale_score_writing = writingscalescore,
        scale_score_comprehension = comprehensionscalescore,
        scale_score_oral = oralscalescore,
        scale_score_literacy = literacyscalescore,
        scale_score_composite = compositeoverallscalescore,
        prof_listening = listeningproficiencylevel,
        prof_reading = readingproficiencylevel,
        prof_speaking = speakingproficiencylevel,
        prof_writing = writingproficiencylevel,
        prof_comprehension = comprehensionproficiencylevel,
        prof_oral = oralproficiencylevel,
        prof_literacy = literacyproficiencylevel,
        prof_composite = compositeoverallproficiencylevel
    ) %>%
    mutate_at(vars(starts_with("scale_score_"), starts_with("prof_")), as.numeric) %>%
    filter(!is.na(student_id), !is.na(scale_score_composite)) %>%
    group_by(student_id) %>%
# Dedup by keeping highest composite scale score, then highest literacy scale score by student
    mutate(max_comp = max(scale_score_composite, na.rm = TRUE)) %>%
    filter(scale_score_composite == max_comp) %>%
    mutate(max_lit = max(scale_score_literacy, na.rm = TRUE)) %>%
    filter(scale_score_literacy == max_lit) %>%
    ungroup() %>%
    left_join(prior, by = "student_id") %>%
    mutate(
        exit_denom = as.integer(!is.na(prof_composite) & !is.na(prof_literacy)),
        exit = as.integer(prof_composite >= 4.2 & prof_literacy >= 4),
        growth_standard_denom = as.integer(!is.na(prof_composite) & !is.na(prof_composite_17)),
        met_growth_standard_1yr = case_when(
            is.na(prof_composite_17) ~ NA_integer_,
            prof_composite_17 <= 1.4 ~ as.integer(prof_composite - prof_composite_17 >= 1.3),
            prof_composite_17 <= 1.9 ~ as.integer(prof_composite - prof_composite_17 >= 0.7),
            prof_composite_17 <= 2.4 ~ as.integer(prof_composite - prof_composite_17 >= 0.8),
            prof_composite_17 <= 2.9 ~ as.integer(prof_composite - prof_composite_17 >= 0.7),
            prof_composite_17 <= 3.4 ~ as.integer(prof_composite - prof_composite_17 >= 0.4),
            prof_composite_17 <= 3.9 ~ as.integer(prof_composite - prof_composite_17 >= 0.5),
            prof_composite_17 <= 4.4 ~ as.integer(prof_composite - prof_composite_17 >= 0.4),
            TRUE ~ 0L
        ),
        met_growth_standard_2yr = case_when(
            is.na(prof_composite_16) ~ NA_integer_,
            prof_composite_16 <= 1.4 ~ as.integer(prof_composite - prof_composite_16 >= 2),
            prof_composite_16 <= 1.9 ~ as.integer(prof_composite - prof_composite_16 >= 1.5),
            prof_composite_16 <= 2.4 ~ as.integer(prof_composite - prof_composite_16 >= 1.5),
            prof_composite_16 <= 2.9 ~ as.integer(prof_composite - prof_composite_16 >= 1.1),
            prof_composite_16 <= 3.4 ~ as.integer(prof_composite - prof_composite_16 >= 0.9),
            prof_composite_16 <= 3.9 ~ as.integer(prof_composite - prof_composite_16 >= 0.9),
            prof_composite_16 <= 4.4 ~ as.integer(prof_composite - prof_composite_16 >= 0.8),
            TRUE ~ 0L
        ),
        met_growth_standard = if_else(growth_standard_denom == 1, pmax(met_growth_standard_1yr, met_growth_standard_2yr, exit, na.rm = TRUE), NA_integer_)
    )

wida %>%
    select(-max_comp, -max_lit) %>%
    write_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_student_level.csv", na = "")

growth_standard_school <- wida %>%
    group_by(system, school) %>%
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
        system, school,
        exit_denom, n_exit,
        pct_exit = if_else(exit_denom != 0, round5(100 * n_exit/exit_denom, 1), NA_real_),
        growth_standard_denom, n_met_growth_standard,
        pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
        literacy_average, composite_average
    )

write_csv(growth_standard_school, "N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_school.csv", na = "")

growth_standard_district <- wida %>%
    group_by(system) %>%
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
        system,
        exit_denom, n_exit,
        pct_exit = if_else(exit_denom != 0, round5(100 * n_exit/exit_denom, 1), NA_real_),
        growth_standard_denom, n_met_growth_standard,
        pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
        literacy_average, composite_average
    )

write_csv(growth_standard_district, "N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_district.csv", na = "")
