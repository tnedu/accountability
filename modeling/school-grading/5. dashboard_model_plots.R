# Plots for Dashboard modeling results

library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)

school_accountability <- read_csv("data/school_accountability_file.csv")
AF_grades_metrics <- read_csv("data/AF_bottom_five_amos_metrics.csv")
AF_grades_final <- read_csv("data/AF_bottom_five_amos_final_grades.csv")

school_accountability %>%
    filter(subgroup == "All Students" & subject == "Success Rate") %>%
    full_join(AF_grades_final, by = c("system", "system_name", "school", "school_name", "designation_ineligible", "pool")) %>%
    ggplot(aes(x = final_grade, y = pct_prof_adv)) + 
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitter(width = .4, height = 0)) +
    theme_hc() +
    scale_x_discrete(limits = c("F", "D", "C", "B", "A")) +
    scale_y_continuous(limits = c(0, 100), breaks = 10 * (0:10)) +
    xlab("Final Grade") +
    ylab("All Students Success Rate")

AF_grades_metrics %>%
    filter(!is.na(grade_tvaas)) %>%
    filter(subgroup == "All Students") %>%
    inner_join(AF_grades_final, by = c("system", "system_name", "school", "school_name", "designation_ineligible", "pool")) %>%
    with(table(final_grade, grade_tvaas))

AF_grades_metrics %>%
    filter(!is.na(grade_achievement)) %>%
    filter(subgroup == "All Students") %>%
    with(table(grade_achievement))

demographics <- readxl::read_excel("K:/ORP_accountability/projects/Alex/accountability/modeling/school-grading/data/data_2015_school_profile.xlsx") %>%
    rename(system = DISTRICT, school = SCHOOL_ID, Enrollment = AVERAGE_DAILY_MEMBERSHIP, Pct_Black = AFRICAN_AMERICAN_PCT,
        Pct_Hispanic = HISPANIC_PCT, Pct_Native_American = NATIVE_AMERICAN_PCT, Pct_EL = LIMITED_ENGLISH_PROFICIENT_PCT,
        Pct_SWD = STUDENTS_WITH_DISABILITIES_PCT, Pct_ED = ECONOMICALLY_DISADVANTAGED_PCT) %>%
        filter(!(school == 0)) %>%
    select(system, school, Enrollment, Pct_Black, Pct_Hispanic, Pct_Native_American, Pct_EL, Pct_SWD, Pct_ED) %>%
    rowwise() %>%
    mutate(Pct_BHN = sum(Pct_Black, Pct_Hispanic, Pct_Native_American, na.rm = TRUE))

AF_grades_final %>%
    left_join(demographics, by = c("system", "school")) %>%
    ggplot(aes(x = final_grade, y = Pct_BHN)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitter(width = .4, height = 0)) +
    theme_hc() +
    scale_x_discrete(limits = c("F", "D", "C", "B", "A")) + 
    scale_y_continuous(limits = c(0, 100), breaks = 10 * (0:10)) +
    xlab("Final Grade") +
    ylab("Percent BHN")

AF_grades_final %>%
    left_join(demographics, by = c("system", "school")) %>%
    ggplot(aes(x = final_grade, y = Pct_ED)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitter(width = .4, height = 0)) +
    theme_hc() +
    scale_x_discrete(limits = c("F", "D", "C", "B", "A")) + 
    scale_y_continuous(limits = c(0, 100), breaks = 10 * (0:10)) +
    xlab("Final Grade") +
    ylab("Percent ED")

AF_grades_final %>%
    left_join(demographics, by = c("system", "school")) %>%
    mutate(Pct_SWD = as.numeric(Pct_SWD)) %>%
    ggplot(aes(x = final_grade, y = Pct_SWD)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitter(width = .4, height = 0)) +
    theme_hc() +
    scale_x_discrete(limits = c("F", "D", "C", "B", "A")) + 
    scale_y_continuous(limits = c(0, 40), breaks = 10 * (0:10)) +
    xlab("Final Grade") +
    ylab("Percent SWD")

AF_grades_final %>%
    left_join(demographics, by = c("system", "school")) %>%
    ggplot(aes(x = final_grade, y = Pct_EL)) +
    geom_boxplot(outlier.shape = NA) + 
    geom_jitter(position = position_jitter(width = .4, height = 0)) +
    theme_hc() +
    scale_x_discrete(limits = c("F", "D", "C", "B", "A")) + 
    scale_y_continuous(limits = c(0, 70), breaks = 10 * (0:10)) +
    xlab("Final Grade") +
    ylab("Percent EL")

readstata13::read.dta13("K:/ORP_accountability/projects/2015_school_coding/Output/reward_2015_ap.dta") %>%
    filter(reward == 1) %>%
    select(system, school, reward_performance, reward_progress) %>%
    full_join(AF_grades_final, by = c("system", "school")) %>%
    mutate(reward_performance = ifelse(is.na(reward_performance), 0, reward_performance)) %>%
    mutate(reward_progress = ifelse(is.na(reward_progress), 0, reward_progress)) %>%
    with(table(reward_progress, final_grade))

readstata13::read.dta13("K:/ORP_accountability/projects/2015_school_coding/Output/reward_2015_ap.dta") %>%
    filter(reward == 1) %>%
    select(system, school, reward_performance, reward_progress) %>%
    full_join(AF_grades_final, by = c("system", "school")) %>%
    mutate(reward_performance = ifelse(is.na(reward_performance), 0, reward_performance)) %>%
    mutate(reward_progress = ifelse(is.na(reward_progress), 0, reward_progress)) %>%
    with(table(reward_performance, final_grade))

readstata13::read.dta13("K:/ORP_accountability/projects/2015_school_coding/Output/priority_schools_not_exiting_ap.dta") %>%
    filter(priority == 1) %>%
    select(system, school, priority) %>%
    left_join(AF_grades_final, by = c("system", "school")) %>%
    mutate(priority = ifelse(is.na(priority), 0, priority)) %>%
    with(table(priority, final_grade), useNA = "ifany")
