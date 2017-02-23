library(tidyverse)

all_students <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (All Students).xlsx") %>%
    mutate(Subgroup = "All Students")

subgroups <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (Subgroups).xlsx")
super <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (Super Subgroup).xlsx")

tvaas <- readxl::read_excel("K:/ORP_accountability/data/2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (All Students).xlsx") %>%
    mutate(Subgroup = "All Students") %>% 
    bind_rows(subgroups, super) %>% 
    filter(!Test == "ACT") %>% 
    mutate(Index = as.numeric(Index),
        Grade = ifelse(Test == "EOC", "9-12", Grade),
        temp = 1) %>% 
    group_by(`System Number`, System, Year, Subgroup, Grade) %>% 
    summarise(index = mean(Index, na.rm = TRUE), n_subjects = sum(temp, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Index = index * sqrt(n_subjects),
        tvaas_level = ifelse(index < -2, "Level 1", NA),
        tvaas_level = ifelse(index >= -2 & index < -1, "Level 2", tvaas_level),
        tvaas_level = ifelse(index >= -1 & index < 1, "Level 3", tvaas_level),
        tvaas_level = ifelse(index >= 1 & index < 2, "Level 4", tvaas_level),
        tvaas_level = ifelse(index >= 2, "Level 5", tvaas_level)) %>%
    mutate_at("Grade", funs(recode(.,
        "4-5" = "3-5 Success Rate",
        "6-8" = "6-8 Success Rate",
        "9-12" = "HS Success Rate"))) %>% 
    transmute(year = Year, system = `System Number`, system_name = System, subgroup = Subgroup, content_area = Grade, tvaas_level)

write_csv(tvaas, "data/grade_band_tvaas.csv", na = "")
