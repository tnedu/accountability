library(tidyverse)

schools <- readxl::read_excel("H:/EDEN Data/EDEN 17-18/2017-18_E EDFacts School Master FIle_1-26-18.xls") %>%
    janitor::clean_names() %>%
    mutate_at(c("state_leaid", "state_schid"), as.integer)

priority <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/priority_schools_not_exiting.csv") %>%
    transmute(state_leaid = system, state_schid = school, status = "PRIORITY")
focus <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/focus_schools_not_exiting.csv") %>%
    transmute(state_leaid = system, state_schid = school, status = "FOCUS")

output <- schools %>%
    left_join(., bind_rows(priority, focus), by = c("state_leaid", "state_schid")) %>%
    transmute(record_number, state_code, state_agency,
        state_leaid = sprintf("%05d", state_leaid),
        state_schid = sprintf("%04d", state_schid),
        status = if_else(is.na(status), "NOTPRFOC", status),
        persist = "NO",
        x = "",
        y = "",
        z = "",
        explanation = ""
    )

write_csv(output, path = "H:/EDEN Data/EDEN 17-18/Done/FS130/TNSCHSCHYRSTSTfs13018.csv", col_names = FALSE)
