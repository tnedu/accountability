library(acct)
library(tidyverse)

school_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv")

district_names <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/names.csv") %>%
    select(system, system_name) %>%
    distinct()

# Student ready grad file
student <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level_06182019.csv",
        col_types = "dcccciccciiciddddddddididddddddc") %>%
    rename(
        system = district_no,
        school = school_no,
        EL = elb,
        ED = econ_dis,
        SWD = swd
    ) %>%
    filter(included_in_cohort == "Y") %>%
    mutate(
        All = TRUE,
        Asian = race_ethnicity == "A",
        BHN = race_ethnicity %in% c("B", "H", "I"),
        Black = race_ethnicity == "B",
        ED = ED == "Y",
        EL = EL == "Y",
        Hispanic = race_ethnicity == "H",
        HPI = race_ethnicity == "P",
        Native = race_ethnicity == "I",
        Non_ED = !ED,
        Non_EL = !EL,
        SWD = SWD == "Y",
        Non_SWD = !SWD,
        Super = BHN | ED | EL | SWD,
        White = race_ethnicity == "W"
    ) %>%
    arrange(system, school, student_key) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(
        system, system_name, school, school_name, student_id = student_key, first_name, last_name, included_in_cohort, completion_type,
        sat_math, sat_critical_reading, sat_total, act_english, act_math, act_reading, act_science, act_composite,
        industry_cert_earned, asvab, ncrc_work_keys, participate_clg_lvl_pgm, n_cambridge, n_ap, n_ib, n_sdc, n_ldc,
        n_de, ready_graduate, All, BHN, ED, SWD, EL, Asian, Black, Hispanic, HPI, Native, White, Non_ED, Non_EL, Non_SWD, Super
    )

# Split student ready grad file
district_numbers <- sort(unique(student$system))

student %>%
    filter(not_na(system)) %>%
    mutate_at(vars(Asian, BHN, Black, ED, EL, Hispanic, HPI, Native, SWD, Super, White), as.integer) %>%
    select(-All, -Non_ED, -Non_EL, -Non_SWD, -Super) %>%
    split(., .$system) %>%
    walk2(
        .x = ., 
        .y = district_numbers, 
        .f = ~ write_csv(.x, path = paste0("N:/ORP_accountability/projects/2019_ready_graduate/Data/split/", .y, "_ReadyGraduate_Student_Level_20Jun2019.csv"), na = "")
    )

collapse <- function(s, ...) {
    s_quo <- enquo(s)

    student %>%
        filter(!!s_quo) %>%
        group_by(...) %>%
        summarise(
            grad_cohort = sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = TRUE),
            tested = sum((not_na(sat_total) & sat_total != 0 | not_na(act_composite) & act_composite != 0) & completion_type %in% c(1, 11, 12, 13), na.rm = TRUE),
            n_count = n(),
            n_ready_grad = sum(ready_graduate == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = TRUE),
            pct_ready_grad = round5(100 * mean(ready_graduate == "Y" & completion_type %in% c(1, 11, 12, 13), na.rm = TRUE), 1)
        ) %>%
        mutate(subgroup = deparse(s_quo)) %>%
        ungroup()
}

# State ready grad file
state <- map_dfr(
    .x = list(quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL), quo(Hispanic), quo(HPI), 
        quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Native), quo(Super), quo(SWD), quo(White)),
    .f = ~ collapse(!!.)
) %>%
    transmute(
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        ),
        act_participation_rate = if_else(grad_cohort != 0, round5(100 * tested/grad_cohort), NA_real_),
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(state, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_state.csv", na = "")

# District ready grad file
district <- map_dfr(
    .x = list(quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL), quo(Hispanic), quo(HPI), 
        quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Native), quo(Super), quo(SWD), quo(White)),
    .f = ~ collapse(!!., system)
) %>%
    filter(not_na(system)) %>%
    left_join(district_names, by = "system") %>%
    transmute(
        system,
        system_name,
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        ),
        act_participation_rate = if_else(grad_cohort != 0, round5(100 * tested/grad_cohort), NA_real_),
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(district, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district.csv", na = "")

# Split district ready grad file
district %>%
    split(., .$system) %>%
    walk2(
        .x = ., 
        .y = district_numbers, 
        .f = ~ write_csv(.x, path = paste0("N:/ORP_accountability/projects/2019_ready_graduate/Data/split/", .y, "_ReadyGraduate_District_Level_20Jun2019.csv"), na = "")
    )

# School ready grad file
school <- map_dfr(
    .x = list(quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL), quo(Hispanic), quo(HPI), 
        quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Native), quo(Super), quo(SWD), quo(White)),
    .f = ~ collapse(!!., system, school)
) %>%
    filter(not_na(system), not_na(school)) %>%
    left_join(school_names, by = c("system", "school")) %>%
    filter(not_na(school_name)) %>%
    transmute(
        system,
        system_name,
        school,
        school_name,
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners with Transitional 1-4",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~Super" ~ "Super Subgroup",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White"
        ),
        act_participation_rate = if_else(grad_cohort != 0, round5(100 * tested/grad_cohort), NA_real_),
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(school, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv", na = "")

# Split school ready grad file
school %>%
    split(., .$system) %>%
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/projects/2019_ready_graduate/Data/split/", .y, "_ReadyGraduate_School_Level_20Jun2019.csv"), na = ""))
