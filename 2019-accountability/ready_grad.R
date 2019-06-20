library(acct)
library(tidyverse)

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
    select(
        system, school, student_id = student_key, first_name, last_name, included_in_cohort, completion_type,
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
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/projects/2019_ready_graduate/Data/split/", .y, "ReadyGraduate_Student_Level_20Jun2019.csv"), na = ""))

collapse <- function(s, ...) {
    s_quo <- enquo(s)

    student %>%
        filter(!!s_quo) %>%
        group_by(...) %>%
        summarise(
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
            subgroup == "~EL" ~ "English Learners",
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
    transmute(
        system,
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners",
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
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(district, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district.csv", na = "")

# Split district ready grad file
district %>%
    split(., .$system) %>%
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/projects/2019_ready_graduate/Data/split/", .y, "ReadyGraduate_District_Level_20Jun2019.csv"), na = ""))

# School ready grad file
school <- map_dfr(
    .x = list(quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL), quo(Hispanic), quo(HPI), 
        quo(Non_ED), quo(Non_EL), quo(Non_SWD), quo(Native), quo(Super), quo(SWD), quo(White)),
    .f = ~ collapse(!!., system, school)
) %>%
    filter(not_na(system), not_na(school)) %>%
    transmute(
        system,
        school,
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL" ~ "English Learners",
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
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(school, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv", na = "")

# Split school ready grad file
school %>%
    split(., .$system) %>%
    walk2(., district_numbers, ~ write_csv(.x, path = paste0("N:/ORP_accountability/projects/2019_ready_graduate/Data/split/", .y, "ReadyGraduate_School_Level_20Jun2019.csv"), na = ""))
