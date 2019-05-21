library(tidyverse)

subgroups <- c(
    "All Students",
    "Black/Hispanic/Native American",
    "Economically Disadvantaged",
    "English Learners",
    "Students with Disabilities"
)

ready_prior <- read_csv("N:/ORP_accountability/data/2018_final_accountability_files/district_ready_grad.csv") %>%
    filter(subgroup %in% subgroups) %>%
    transmute(system, subgroup, prior = if_else(grad_cohort >= 30, pct_ready_grad, NA_real_))

ready <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district.csv") %>%
    filter(subgroup %in% subgroups) %>%
    mutate(pct_ready_grad = if_else(n_count >= 30, pct_ready_grad, NA_real_)) %>%
    left_join(ready_prior, by = c("system", "subgroup")) %>%
    group_by(subgroup) %>%
    mutate(
        value_add_metric = pct_ready_grad - prior,
        rank = if_else(!is.na(value_add_metric), rank(value_add_metric, ties.method = "max"), NA_integer_),
        denom = sum(!is.na(value_add_metric)),
        value_add_pathway = case_when(
            rank/denom >= 0.8 ~ 4L,
            rank/denom >= 0.6 ~ 3L,
            rank/denom >= 0.4 ~ 2L,
            rank/denom >= 0.2 ~ 1L,
            rank/denom < 0.2 ~ 0L
        )
    ) %>%
    ungroup() %>%
    transmute(
        system, 
        subgroup = if_else(subgroup == "English Learners", "English Learners with Transitional 1-4", subgroup),
        indicator = "Graduation Rate",
        value_add_metric,
        value_add_pathway
    )

write_csv(ready, "N:/ORP_accountability/data/2019_final_accountability_files/district_grad_va.csv", na = "")
