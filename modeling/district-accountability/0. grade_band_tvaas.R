library(tidyverse)
library(readxl)

all_students <- read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (All Students).xlsx") %>%
    mutate(Subgroup = "All Students")

subgroups <- read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (Subgroups).xlsx")

super <- read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (Super Subgroup).xlsx")

tvaas <- bind_rows(all_students, subgroups, super) %>%
    filter(!Test == "ACT") %>%
    mutate(Index = as.numeric(Index),
        Grade = ifelse(Test == "EOC", "9-12", Grade),
        temp = 1) %>%
    group_by(`System Number`, System, Year, Subgroup, Grade) %>%
    summarise(index_avg = mean(Index, na.rm = TRUE), n_subjects = sum(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Index = index_avg * sqrt(n_subjects),
        tvaas_level = ifelse(Index < -2, "Level 1", NA),
        tvaas_level = ifelse(Index >= -2, "Level 2", tvaas_level),
        tvaas_level = ifelse(Index >= -1, "Level 3", tvaas_level),
        tvaas_level = ifelse(Index >= 1, "Level 4", tvaas_level),
        tvaas_level = ifelse(Index >= 2, "Level 5", tvaas_level)) %>%
    mutate_at("Grade", funs(recode(.,
        "4-5" = "3-5 Success Rate",
        "6-8" = "6-8 Success Rate",
        "9-12" = "HS Success Rate"))) %>%
    select(year = Year, system = `System Number`, system_name = System, subgroup = Subgroup, content_area = Grade, tvaas_level)

write_csv(tvaas, "data/grade_band_tvaas.csv", na = "")
