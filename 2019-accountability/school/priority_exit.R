library(acct)
library(tidyverse)

priority_csi <- read_csv("N:/ORP_accountability/projects/2018_school_accountability/priority.csv") %>%
    transmute(system, school, priority_csi = priority == 1 | comprehensive_support == 1)

tvaas <- readxl::read_excel("N:/ORP_accountability/data/2019_tvaas/School Composite Level.xlsx") %>%
    transmute(
        system = as.integer(`District Number`),
        school = as.integer(`School Number`),
        literacy = `School-Wide: Literacy`,
        numeracy = `School-Wide: Numeracy`
    )

tvaas_prior <- readxl::read_excel("N:/ORP_accountability/data/2018_tvaas/School Composite Level.xlsx") %>%
    transmute(
        system = as.integer(`District Number`),
        school = as.integer(`School Number`),
        literacy_prior = `School-Wide: Literacy`,
        numeracy_prior = `School-Wide: Numeracy`
    )

priority_exit <- read_csv("N:/ORP_accountability/data/2019_final_accountability_files/school_accountability_file.csv") %>%
    filter(indicator == "Achievement", subgroup == "All Students") %>%
    left_join(priority_csi, by = c("system", "school")) %>%
    left_join(tvaas, by = c("system", "school")) %>%
    left_join(tvaas_prior, by = c("system", "school")) %>%
    group_by(pool) %>%
    mutate(
        rank = if_else(designation_ineligible == 0 & !is.na(metric), rank(metric, ties = "min"), NA_integer_),
        rank_prior = if_else(designation_ineligible == 0 & !is.na(metric), rank(metric_prior, ties = "min"), NA_integer_),
        denom = case_when(
            pool == "HS" ~ sum(pool == "HS"),
            pool == "K8" ~ sum(pool == "K8") 
        ),
        percentile = round5(100 * rank/denom, 1),
        percentile_prior = round5(100 * rank_prior/denom, 1),
        priority_exit = as.integer(priority_csi &
            (percentile > 10 & percentile_prior > 10 |
             percentile > 15 |
             literacy %in% 4:5 & numeracy %in% 4:5 & literacy_prior %in% 4:5 & numeracy_prior %in% 4:5)
        )
    ) %>%
    ungroup() %>%
    select(
        system, system_name, school, school_name, pool, designation_ineligible, priority_csi,
        metric, rank, percentile, metric_prior, rank_prior, percentile_prior, 
        literacy, numeracy, literacy_prior, numeracy_prior, priority_exit
    )

write_csv(priority_exit, "N:/ORP_accountability/projects/2019_school_accountability/priority_exit.csv", na = "")
