library(acct)
library(tidyverse)

student <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level.csv",
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
    )

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
