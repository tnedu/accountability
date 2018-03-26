library(tidyverse)

determs <- haven::read_dta("N:/ORP_accountability/projects/2017_district_accountability/final_determinations_master_JW_10232017.dta") %>%
    transmute(`District Number` = system,
        `Final Determination: Name` = final_determination,
        `Final Determination: Definition` = "",
        `All Students Determination: Name` = achievement_determination,
        `All Students Determination: Definition` = "",
        `Subgroup Determination: Name` = gap_closure_determination,
        `Subgroup Determination: Definition` = "")

write_csv(determs, "N:/ORP_accountability/data/2017_final_accountability_files/Report Card/Accountability File Layout.csv", na = "")
