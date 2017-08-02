library(tidyverse)

numeric_subgroups <- c("All Students", "Black/Hispanic/Native American", "Economically Disadvantaged",
    "English Learners with T1/T2", "Students with Disabilities", "Super Subgroup")
math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")

state_base <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/state_base_2017.csv",
        col_types = c("iiccccddddddddddddddddddddddddd")) %>%
    filter(subgroup %in% numeric_subgroups, 
        subject %in% c(math_eoc, english_eoc, "Graduation Rate", "ACT Composite", "ACT Reading", "ACT Math"))

grad <- filter(state_base, subject == "Graduation Rate")
ACT <- filter(state_base, subject == "ACT Composite")

collapse <- state_base %>%
    filter(grade != "All Grades", !subject %in% c("Graduation Rate", "ACT Composite")) %>%
    rowwise() %>%
    mutate(enrolled = sum(enrolled, enrolled_part_1, enrolled_part_2, enrolled_both, na.rm = TRUE),
        tested = sum(tested, tested_part_1, tested_part_2, tested_both, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(grade = if_else(grade %in% c("3", "4", "5"), "3rd through 5th", grade),
        grade = if_else(grade %in% c("6", "7", "8"), "6th through 8th", grade),
        grade = if_else(grade %in% c("Missing Grade", "9", "10", "11", "12"), "9th through 12th", grade),
        subject = if_else(grade %in% c("3rd through 5th", "6th through 8th") & subject %in% math_eoc, "Math", subject),
        subject = if_else(grade %in% c("3rd through 5th", "6th through 8th") & subject %in% english_eoc, "ELA", subject),
        subject = if_else(subject %in% c(math_eoc, "ACT Math"), "HS Math", subject),
        subject = if_else(subject %in% c(english_eoc, "ACT Reading"), "HS English", subject)) %>%
    group_by(year, system, system_name, subject, grade, subgroup) %>%
    summarise_at(c("enrolled", "tested", "valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE)

state_numeric <- collapse %>%
    mutate(pct_approaching = if_else(valid_tests != 0, round(100 * n_approaching/valid_tests + 1e-10, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round(100 * n_on_track/valid_tests + 1e-10, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round(100 * n_mastered/valid_tests + 1e-10, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round(100 - pct_approaching - pct_on_track - pct_mastered + 1e-10, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round(100 * (n_on_track + n_mastered)/valid_tests + 1e-10, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        flag_below = as.integer(pct_below != 0 & n_below == 0),
        pct_approaching = if_else(flag_below == 1L, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(flag_below == 1L, 0, pct_below),
        flag_approaching = as.integer(pct_approaching != 0 & n_approaching == 0),
        pct_on_track = if_else(flag_approaching == 1L, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(flag_approaching == 1L, 0, pct_approaching),
    # One-Year Participation Rate
        participation_rate_1yr = if_else(enrolled != 0, round(100 * tested/enrolled + 1e-10), NA_real_))

participation_rate_2yr <- state_numeric %>%
    group_by(system, subject, grade, subgroup) %>%
    summarise_at(c("enrolled", "tested"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    transmute(year = 2017, system, subject, grade, subgroup,
        participation_rate_2yr = if_else(enrolled != 0, round(100 * tested/enrolled + 1e-10), NA_real_))

output <- state_numeric %>%
    full_join(participation_rate_2yr, by = c("year", "system", "subject", "grade", "subgroup")) %>%
    bind_rows(grad, ACT) %>%
    mutate(subgroup = if_else(subgroup == "English Learners with T1/T2", "English Learners", subgroup)) %>%
    select(year, system, system_name, subject, grade, subgroup,
        participation_rate_1yr, participation_rate_2yr, enrolled, tested, valid_tests,
        n_below, n_approaching, n_on_track, n_mastered, pct_below,
        pct_approaching, pct_on_track, pct_mastered, pct_on_mastered,
        ACT_21_and_above, ACT_18_and_below, grad_cohort, grad_count, grad_rate, dropout_count, dropout_rate)

write_csv(output, path = "K:/ORP_accountability/data/2017_final_accountability_files/state_numeric_2017.csv", na = "")
