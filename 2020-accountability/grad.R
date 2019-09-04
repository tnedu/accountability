library(acct)
library(tidyverse)

student <- read_csv("N:/ORP_accountability/projects/2019_graduation_rate/Data/studentcohortdata_20190904.csv") %>%
    filter(not_na(district_no), not_na(school_no)) %>%
    mutate(
        All = TRUE,
        BHN = race_ethnicity %in% c("B", "H", "I"),
        ED = econ_dis == "Y",
        SWD = sped == "Y",
        EL_T1234 = ell == "Y",
        Asian = race_ethnicity == "A",
        Black = race_ethnicity == "B",
        HPI = race_ethnicity == "P",
        Hispanic = race_ethnicity == "H",
        Native = race_ethnicity == "I",
        White = race_ethnicity == "W",
        Homeless = homeless == "Y",
        Migrant = migrant == "Y",
        Non_BHN = !BHN,
        Non_ED = !ED,
        Non_SWD = !SWD,
        Non_EL = !EL_T1234,
        Non_Homeless = !Homeless,
        Non_Migrant = !Migrant,
        Male = gender == "M",
        Female = gender == "F"
    )

collapse <- function(g, ...) {
    
    g_quo <- enquo(g)
    
    student %>%
        filter(!!g_quo) %>%
        group_by(...) %>%
        summarise(
            grad_cohort = sum(included_in_cohort == "Y"),
            grad_count = sum(included_in_cohort == "Y" & completion_type %in% c(1, 11, 12, 13))
        ) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo))
    
}

state <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL_T1234), quo(Female), quo(Hispanic), quo(Homeless),
        quo(HPI), quo(Male), quo(Migrant), quo(Non_BHN), quo(Non_ED), quo(Non_EL), quo(Non_Homeless), quo(Non_Migrant),
        quo(Non_SWD), quo(Native), quo(SWD), quo(White) 
    ),
    .f = ~ collapse(!!.)
) %>%
    transmute(
        year = 2019,
        system = 0,
        system_name = "State of Tennessee",
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Female" ~ "Female",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Homeless" ~ "Homeless",
            subgroup == "~Male" ~ "Male",
            subgroup == "~Migrant" ~ "Migrant",
            subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "~Non_Homeless" ~ "Non-Homeless",
            subgroup == "~Non_Migrant" ~ "Non-Migrant",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White",
            TRUE ~ subgroup
        ),
        grad_cohort,
        grad_count,
        grad_rate = if_else(grad_cohort != 0, round5(100 * grad_count/grad_cohort, 1), NA_real_)
    )

# Check Match
state_test <- read_csv("N:/ORP_accountability/data/2019_graduation_rate/state_grad_rate.csv")

setdiff(state, state_test)
setdiff(state_test, state)

district <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL_T1234), quo(Female), quo(Hispanic), quo(Homeless),
        quo(HPI), quo(Male), quo(Migrant), quo(Non_BHN), quo(Non_ED), quo(Non_EL), quo(Non_Homeless), quo(Non_Migrant),
        quo(Non_SWD), quo(Native), quo(SWD), quo(White) 
    ),
    .f = ~ collapse(!!., district_no)
) %>%
    transmute(
        year = 2019,
        system = district_no,
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Female" ~ "Female",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Homeless" ~ "Homeless",
            subgroup == "~Male" ~ "Male",
            subgroup == "~Migrant" ~ "Migrant",
            subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "~Non_Homeless" ~ "Non-Homeless",
            subgroup == "~Non_Migrant" ~ "Non-Migrant",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White",
            TRUE ~ subgroup
        ),
        grad_cohort,
        grad_count,
        grad_rate = if_else(grad_cohort != 0, round5(100 * grad_count/grad_cohort, 1), NA_real_)
    )

# Check Match
district_test <- read_csv("N:/ORP_accountability/data/2019_graduation_rate/district_grad_rate.csv") %>% 
    select(-system_name)

setdiff(district, district_test)
setdiff(district_test, district)

school <- map_dfr(
    .x = list(
        quo(All), quo(Asian), quo(BHN), quo(Black), quo(ED), quo(EL_T1234), quo(Female), quo(Hispanic),
        quo(Homeless), quo(HPI), quo(Male), quo(Migrant), quo(Non_BHN), quo(Non_ED), quo(Non_EL), quo(Non_Homeless), 
        quo(Non_Migrant), quo(Non_SWD), quo(Native), quo(SWD), quo(White) 
    ),
    .f = ~ collapse(!!., district_no, school_no)
) %>%
    transmute(
        year = 2019,
        system = district_no,
        school = school_no,
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~Native" ~ "American Indian or Alaska Native",
            subgroup == "~Asian" ~ "Asian",
            subgroup == "~Black" ~ "Black or African American",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~EL_T1234" ~ "English Learners with Transitional 1-4",
            subgroup == "~Female" ~ "Female",
            subgroup == "~Hispanic" ~ "Hispanic",
            subgroup == "~Homeless" ~ "Homeless",
            subgroup == "~Male" ~ "Male",
            subgroup == "~Migrant" ~ "Migrant",
            subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~Non_BHN" ~ "Non-Black/Hispanic/Native American",
            subgroup == "~Non_ED" ~ "Non-Economically Disadvantaged",
            subgroup == "~Non_EL" ~ "Non-English Learners/Transitional 1-4",
            subgroup == "~Non_Homeless" ~ "Non-Homeless",
            subgroup == "~Non_Migrant" ~ "Non-Migrant",
            subgroup == "~Non_SWD" ~ "Non-Students with Disabilities",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~White" ~ "White",
            TRUE ~ subgroup
        ),
        grad_cohort,
        grad_count,
        grad_rate = if_else(grad_cohort != 0, round5(100 * grad_count/grad_cohort, 1), NA_real_)
    )

# Check Match
school_test <- read_csv("N:/ORP_accountability/data/2019_graduation_rate/school_grad_rate.csv") %>%
    select(-system_name, -school_name)

setdiff(school, school_test)
setdiff(school_test, school)
