library(readxl)
library(tidyverse)

school_master <- read_excel("N:/ORP_accountability/projects/Alex/accountability/edfacts/2019/2018-19 EDFacts School Master File_2019-01-25.xlsx") %>%
    transmute(system = as.integer(lea_id_state), school = as.integer(sch_id_state))

comprehensive_support <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/comprehensive_support.csv") %>%
    select(system, school, comprehensive_support, grad_less_than_67)

additional_targeted_support <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv") %>%
    select(system, school, additional_targeted_support)

fs206 <- school_master %>%
    left_join(comprehensive_support, by = c("system", "school")) %>%
    left_join(additional_targeted_support, by = c("system", "school")) %>%
    arrange(system, school) %>%
    transmute(
        first = 1:nrow(.),
        state_code = 47,
        state_agency_number = "01",
        system = sprintf("%05d", system),
        school = sprintf("%04d", school),
        status = case_when(
            comprehensive_support == 1 ~ "CSI",
            additional_targeted_support == 1 ~ "TSI",
            comprehensive_support == 0 | is.na(comprehensive_support) ~ "NOTCSITSI"
        ),
        csi = case_when(
            system == "00792" & school == "2815" ~ "CSILOWGR",
            comprehensive_support == 1 ~ "CSILOWPERF",
            TRUE ~ ""
        ),
        tsi = case_when(
            additional_targeted_support == 1 ~ "TSIOTHER",
            additional_targeted_support == 0 | is.na(additional_targeted_support) ~ ""
        ),
        filler1 = "",
        filler2 = "",
        explanation = ""
    )

write_csv(fs206, "N:/ORP_accountability/projects/Alex/accountability/edfacts/2019/TNSCHCSITSISCHv1.csv", na = "", col_names = FALSE)

paste("SCH CSI TSI", nrow(fs206), "TNSCHCSITSISCHv1.csv", "TNSCHCSITSISCHv1", "2018-2019", "", sep = ",")
