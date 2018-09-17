library(acct)
library(openxlsx)
library(tidyverse)

school_names <- readxl::read_excel("N:/ORP_accountability/data/2018_final_accountability_files/2017-18_E EDFacts School Master FIle_5-3-18.xls", sheet = 2) %>% 
    transmute(
        system = as.integer(`DG 4 LEA ID (State)`), school = as.integer(`DG 5 School ID (State)`),
        system_name = `EXTRA ITEM - LEA Name`, school_name = `DG 7 School Name`
    ) %>%
    bind_rows(
        tribble(
            ~system, ~system_name, ~school, ~school_name,
            970, "Department of Children's Services", 25, "Gateway to Independence",
            970, "Department of Children's Services", 45, "Wilder Youth Development Center",
            970, "Department of Children's Services", 65, "Mountain View Youth Development Center",
            970, "Department of Children's Services", 140, "DCS Affiliated Schools"
        )
    )

designations <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv") %>%
    select(system, school, priority, comprehensive_support, reward, additional_targeted_support) %>%
    left_join(school_names, by = c("system", "school")) %>%
    transmute(
        system, system_name, school, school_name,
        designation = case_when(
            priority == 1 & comprehensive_support == 1 ~ "Priority & Comprehensive Support",
            priority == 1 ~ "Priority",
            comprehensive_support == 1 ~ "Comprehensive Support",
            additional_targeted_support == 1 ~ "Additional Targeted Support",
            reward == 1 ~ "Reward",
            TRUE ~ ""
        )
    )

priority <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/priority.csv", col_types = "iciccididiiiiii") %>%
    filter(priority == 1) %>%
    mutate(percentile = round5(100 * rank/count, 1)) %>%
    select(-rank_eligible)
    
comprehensive_support <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/comprehensive_support.csv",
        col_types = "iciccididiiiii") %>%
    mutate(comprehensive_support = if_else(system == 985, 1L, comprehensive_support)) %>%
    filter(comprehensive_support == 1) %>%
    mutate(percentile = round5(100 * rank/count, 1)) %>%
    select(-rank_eligible)

reward <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv") %>%
    filter(reward == 1) %>%
    select(system, school, priority, reward, final_average) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, everything())

additional <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/school_grading_grades.csv") %>%
    filter(additional_targeted_support == 1) %>%
    select(system, school, additional_targeted_support, final_average, starts_with("targeted_support")) %>%
    left_join(school_names, by = c("system", "school")) %>%
    select(system, system_name, school, school_name, everything())

write.xlsx(
    list(
        "Designations" = designations,
        "Priority" = priority,
        "Comprehensive Support" = comprehensive_support,
        "Reward" = reward,
        "Additional Targeted Support" = additional
    ),
    file = "N:/ORP_accountability/data/2018_final_accountability_files/school_designations_file.xlsx"
)
