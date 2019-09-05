library(acct)
library(tidyverse)

t1234 <- read_csv("N:/ORP_accountability/projects/2019_student_level_file/2019_student_level_file.csv") %>%
    filter(
        t1234 == 1,
        original_subject %in% c("ELA", "English I", "English II", "English III"),
        (enrolled_50_pct_district == "Y" | is.na(enrolled_50_pct_district)),
        (residential_facility == 0 | is.na(residential_facility))
    ) %>%
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, T1234 = t1234) %>%
    mutate_at(vars(BHN, ED, SWD, T1234), as.logical) %>%
    mutate(All = TRUE)

t1234_prior <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/2018_student_level_file.csv") %>%
    filter(
        el_t1234 == 1,
        original_subject %in% c("ELA", "English I", "English II", "English III"),
        enrolled_50_pct_district == "Y",
        residential_facility == 0
    ) %>%
    rename(BHN = bhn_group, ED = economically_disadvantaged, SWD = special_ed, T1234 = el_t1234) %>%
    mutate_at(vars(BHN, ED, SWD, T1234), as.logical) %>%
    mutate(All = TRUE)

collapse <- function(df, g) {

    g_quo <- enquo(g)
    
    df %>%
        filter(!!g_quo) %>%
        group_by(system) %>%
        summarise(
            former_el_ot_m = sum(performance_level %in% c("On Track", "Mastered"), na.rm = TRUE),
            valid_tests = sum(valid_test, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
            subgroup = deparse(g_quo), 
            metric = if_else(valid_tests >= 30, round5(100 * former_el_ot_m/valid_tests, 1), NA_real_)
        ) %>%
        select(-former_el_ot_m, -valid_tests)

}

current <- map_dfr(
    .x = list(quo(All), quo(BHN), quo(ED), quo(SWD), quo(T1234)),
    .f = ~ collapse(t1234, !!.)
)

collapse <- function(df, g) {

    g_quo <- enquo(g)
    
    df %>%
        filter(!!g_quo) %>%
        group_by(system) %>%
        summarise(
            former_el_ot_m = sum(performance_level %in% c("On Track", "Mastered"), na.rm = TRUE),
            valid_tests = sum(valid_test, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
            subgroup = deparse(g_quo), 
            metric_prior = if_else(valid_tests >= 30, round5(100 * former_el_ot_m/valid_tests, 1), NA_real_)
        ) %>%
        select(-former_el_ot_m, -valid_tests)
    
}

prior <- map_dfr(
    .x = list(quo(All), quo(BHN), quo(ED), quo(SWD), quo(T1234)),
    .f = ~ collapse(t1234_prior, !!.)
)

    
el_va <- left_join(current, prior, by = c("system", "subgroup")) %>%
    group_by(subgroup) %>%
    mutate(
        value_add_metric = metric - metric_prior,
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
            subgroup == "~T1234" ~ "English Learners with Transitional 1-4",
        ),
        value_add_metric,
        value_add_pathway
    )

write_csv(el_va, "N:/ORP_accountability/data/2019_final_accountability_files/district_elpa_va.csv", na = "")
