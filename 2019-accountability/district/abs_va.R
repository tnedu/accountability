library(acct)
library(tidyverse)

prior <- read_csv("N:/ORP_accountability/data/2018_chronic_absenteeism/student_chronic_absenteeism_primary_enrollment_only.csv") %>%
    filter(str_length(student_id) == 7) %>%
# Collapse multiple enrollments in the same district
    group_by(student_id) %>%
    summarise(
        n_absences = sum(n_absences, na.rm = TRUE),
        isp_days = sum(isp_days, na.rm = TRUE),
        instructional_calendar_days = max(instructional_calendar_days)
    ) %>%
    ungroup() %>%
    filter(isp_days/instructional_calendar_days >= 0.5) %>%
    transmute(student_id, ca_prior = n_absences/isp_days >= 0.1)

current <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/student_chronic_absenteeism_Jun17.csv") %>%
    filter(str_length(student_id) == 7) %>%
# Collapse multiple enrollments in the same district
    group_by(system, system_name, student_id) %>%
    summarise(
        n_absences = sum(n_absences, na.rm = TRUE),
        isp_days = sum(isp_days, na.rm = TRUE),
        instructional_calendar_days = max(instructional_calendar_days),
        Black = max(Black, na.rm = TRUE),
        Hispanic = max(Hispanic, na.rm = TRUE),
        Native = max(Native, na.rm = TRUE),
        ED = max(ED, na.rm = TRUE),
        SWD = max(SWD, na.rm = TRUE),
        EL = max(EL, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(isp_days/instructional_calendar_days >= 0.5) %>%
    mutate(
        All = TRUE,
        BHN = Black == 1 | Hispanic == 1 | Native == 1,
        ca = n_absences/isp_days >= 0.1
    ) %>%
    mutate_at(vars(ED, SWD, EL), as.logical) %>%
    left_join(prior, by = "student_id")

collapse <- function(g) {

    g_quo <- enquo(g)

    current %>%
        filter(!!g_quo) %>%
        group_by(system) %>%
        summarise(
            n_count = n(),
            value_add_metric = round(100 * mean(!ca & ca_prior, na.rm = TRUE), 1)
        ) %>%
        ungroup() %>%
        mutate(subgroup = deparse(g_quo))

}

abs_va <- map_dfr(
    .x = list(quo(All), quo(BHN), quo(ED), quo(SWD), quo(EL)),
    .f = ~ collapse(!!.)
) %>%
    mutate(value_add_metric = if_else(n_count < 30, NA_real_, value_add_metric)) %>%
    group_by(subgroup) %>%
    mutate(
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
        subgroup = case_when(
            subgroup == "~All" ~ "All Students",
            subgroup == "~BHN" ~ "Black/Hispanic/Native American",
            subgroup == "~ED" ~ "Economically Disadvantaged",
            subgroup == "~SWD" ~ "Students with Disabilities",
            subgroup == "~EL" ~ "English Learners with Transitional 1-4",
        ),
        value_add_metric,
        value_add_pathway
    )

write_csv(abs_va, "N:/ORP_accountability/data/2019_final_accountability_files/district_absenteeism_va.csv", na = "")
