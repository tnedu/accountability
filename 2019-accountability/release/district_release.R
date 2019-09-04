library(acct)
library(tidyverse)

district_assessment <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_assessment_file.csv") %>%
    filter(year == 2019) %>%
    filter(!(subject == "US History" & grade %in% c(7, 8)))

grade_bands <- district_assessment %>%
    filter(
        grade != "All Grades",
        subgroup != "English Learner Transitional 1-4"
    ) %>%
    mutate(
        test = if_else(grade %in% c("3", "4", "5", "6", "7", "8") & test == "EOC", "TNReady", test),
        subject = case_when(
            grade %in% c("3", "4", "5", "6", "7", "8") & subject %in% c("English I", "English II") ~ "ELA",
            grade %in% c("3", "4", "5", "6", "7", "8") & subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III") ~ "Math",
            subject %in% c("English I", "English II") ~ "HS English",
            subject %in% c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III") ~ "HS Math",
            TRUE ~ subject
        ),
        subject = if_else(grade %in% c("3", "4", "5"), paste("3-5", subject), subject),
        subject = if_else(grade %in% c("6", "7", "8"), paste("6-8", subject), subject)
    ) %>%
    group_by(year, system, system_name, test, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(
        pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        pct_approaching = if_else(pct_below != 0 & n_below == 0, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(pct_below != 0 & n_below == 0, 0, pct_below),
        pct_on_track = if_else(pct_approaching != 0 & n_approaching == 0, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(pct_approaching != 0 & n_approaching == 0, 0, pct_approaching),
    ) %>%
    select(year, system, system_name, test, subject, subgroup, 
        valid_tests, n_below, n_approaching, n_on_track, n_mastered, 
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered)

suppress <- function(file, threshold = 1) {
    
    file %>%
        rowwise() %>%
        mutate(temp = any(
            pct_below < threshold, pct_below > (100 - threshold),
            pct_approaching < threshold, pct_approaching > (100 - threshold),
            pct_on_track < threshold, pct_on_track > (100 - threshold),
            pct_mastered < threshold, pct_mastered > (100 - threshold))
        ) %>%
        ungroup() %>%
        mutate_at(
            .vars = c("n_below", "n_approaching", "n_on_track", "n_mastered",
                "pct_below", "pct_approaching", "pct_on_track", "pct_mastered"),
            .f = ~ if_else(temp, "**", as.character(.))
        ) %>%
        select(-temp) %>%
        mutate(pct_on_mastered = if_else(pct_on_mastered < 1 | pct_on_mastered > 99, "**", as.character(pct_on_mastered))) %>%
        mutate_at(
            .vars = c("n_below", "n_approaching", "n_on_track", "n_mastered",
                "pct_below", "pct_approaching", "pct_on_track", "pct_mastered", "pct_on_mastered"),
            .f = ~ if_else(valid_tests < 10, "*", as.character(.)))
    
}

combined_grades <- grade_bands %>%
    filter(grepl("3-5|6-8", subject)) %>%
    mutate(subject = str_replace(subject, "3-5|6-8", "3-8")) %>%
    group_by(year, system, system_name, test, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
    bind_rows(grade_bands) %>%
    mutate(
        pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        pct_approaching = if_else(pct_below != 0 & n_below == 0, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(pct_below != 0 & n_below == 0, 0, pct_below),
        pct_on_track = if_else(pct_approaching != 0 & n_approaching == 0, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(pct_approaching != 0 & n_approaching == 0, 0, pct_approaching)
    ) %>%
    transmute(
        year, system, system_name, test, subject, subgroup,
        valid_tests, n_below, n_approaching, n_on_track, n_mastered,
        pct_below, pct_approaching, pct_on_track, pct_mastered, pct_on_mastered
    ) %>%
    arrange(subject, subgroup)

write_csv(combined_grades, "N:/ORP_accountability/data/2019_final_accountability_files/district_release_file.csv", na = "")

combined_grades %>%
    suppress() %>%
    write_csv("N:/ORP_accountability/data/2019_final_accountability_files/district_release_file_suppressed.csv", na = "")
