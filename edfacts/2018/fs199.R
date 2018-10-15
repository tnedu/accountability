library(tidyverse)

school_master <- readxl::read_excel("N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/2017-18  EDFacts School Master FIle_5-3-18.xls") %>%
    transmute(system = as.integer(STATE_LEAID), school = as.integer(STATE_SCHID))

school_grades <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_metrics.csv") %>%
    filter(pool == "HS", !subgroup %in% c("Black/Hispanic/Native American", "Subgroups", "Super Subgroup")) %>%
    inner_join(school_master, by = c("system", "school"))

fs199 <- school_grades %>%
    arrange(system, school, subgroup) %>%
    transmute(first = 1:nrow(school_grades),
        state_code = 47,
        state_agency_number = "01",
        system = sprintf("%05d", system),
        school = sprintf("%04d", school),
        table_name = "GRADRATESTATUS",
        racial = case_when(
            subgroup == "American Indian or Alaska Native" ~ "MAP",
            subgroup == "Asian" ~ "MA",
            subgroup == "Black or African American" ~ "MB",
            subgroup == "Hispanic" ~ "MHL",
            subgroup == "Native Hawaiian or Other Pacific Islander" ~ "MNP",
            subgroup == "White" ~ "MW",
            TRUE ~ ""
        ),
        disability = if_else(subgroup == "Students with Disabilities", "WDIS", ""),
        lep = if_else(subgroup == "English Learners with Transitional 1-4", "LEP", ""),
        eco_dis = if_else(subgroup == "Economically Disadvantaged", "ECODIS", ""),
        filler1 = "",
        filler2 = "",
        filler3 = "",
        filler4 = "",
        total_indicator = if_else(subgroup == "All Students", "Y", "N"),
        explanation = "",
        indicator_status = case_when(
            is.na(score_grad) ~ "TOOFEW",
            !is.na(score_grad) ~ "STTDEF"
        ),
        state_defined_status = score_grad
    )

write_csv(fs199, "N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/TNSCHGRADRSTATv1.csv", na = "", col_names = FALSE)

paste("SCH GRAD RATE STATUS", nrow(fs199), "TNSCHGRADRSTATv1.csv", "TNSCHGRADRSTATv1", "2017-2018", "", sep = ",")
