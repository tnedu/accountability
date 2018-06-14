library(tidyverse)

prior <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Data/WIDA/2016-17/WIDA_student_level2017.csv") %>%
    janitor::clean_names() %>%
    select(student_id = unique_student_id,
        prof_composite_17 = composite_overall_proficiency_level,
        prof_composite_16 = prof_level_composite_lag)

wida <- haven::read_dta("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/WIDA/20180606_WIDA_AccessResults_SY2017-18_Whalen_v2.dta") %>%
    janitor::clean_names() %>%
    filter(compositeoverallproficiencylevel != "", !is.na(statestudentid) | statestudentid == "") %>%
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
    group_by(student_id) %>%
# Dedup by keeping highest composite scale score, then highest literacy scale score by student
    mutate(max_comp = max(scale_score_composite)) %>%
    filter(scale_score_composite == max_comp) %>%
    mutate(max_lit = max(scale_score_literacy)) %>%
    filter(scale_score_literacy == max_lit) %>%
    ungroup() %>%
    left_join(prior, by = "student_id") %>%
    mutate(
        denom = as.integer(!is.na(prof_composite) & !is.na(prof_composite_17)),
        exit = as.integer(prof_composite >= 5 & prof_literacy >= 5),
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
        met_growth_standard = if_else(denom == 1, pmax(met_growth_standard_1yr, met_growth_standard_2yr, exit, na.rm = TRUE), NA_integer_)
    )
    
growth_standard_school <- wida %>%
    group_by(system, school) %>%
    summarise(
        denom = sum(denom, na.rm = TRUE),
        n_met_growth_standard = sum(met_growth_standard, na.rm = TRUE),
        percent_met_growth_standard = round(100 * mean(met_growth_standard, na.rm = TRUE), 1)
    ) %>%
    ungroup()

write_csv(growth_standard_school, "N:/ORP_accountability/data/2018_final_accountability_files/wida_growth_standard_school.csv", na = "")

growth_standard_district <- wida %>%
    group_by(system) %>%
    summarise(
        denom = sum(denom, na.rm = TRUE),
        n_met_growth_standard = sum(met_growth_standard, na.rm = TRUE),
        percent_met_growth_standard = round(100 * mean(met_growth_standard, na.rm = TRUE), 1)
    ) %>%
    ungroup()

write_csv(growth_standard_district, "N:/ORP_accountability/data/2018_final_accountability_files/wida_growth_standard_district.csv", na = "")
