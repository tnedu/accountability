library(acct)
library(janitor)
library(tidyverse)

prior <- read_csv("N:/ORP_accountability/projects/Jessica/Data Returns/Data/WIDA/2016-17/WIDA_student_level2017.csv") %>%
    clean_names() %>%
    select(student_id = unique_student_id,
        prof_composite_17 = composite_overall_proficiency_level,
        prof_composite_16 = prof_level_composite_lag)

demographic <- read_delim("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/Demographics_SY2017_18.txt", delim = "\t") %>%
    clean_names() %>%
    mutate(
        hispanic = race == 4,
        black = race == 3,
        native = race == 1,
        hawaiian_pi = race == 5,
        asian = race == 2,
        white = race == 6
    ) %>%
    group_by(student_key) %>%
    summarise_at(c("ed", "swd", "ell", "hispanic", "black", "native"), max, na.rm = TRUE) %>%
    transmute(student_id = student_key, bhn = pmax(black, hispanic, native), ed, swd, el = ell)

wida <- haven::read_dta("N:/Assessment_Data Returns/ACCESS for ELs and ALT/2017-18/WIDA/20180706_WIDA_AccessResults_SY2017-18_Whalen_v3.dta") %>%
    clean_names() %>%
    transmute(
        student_id = as.numeric(statestudentid),
        system = as.integer(str_sub(districtnumber, 3, 5)),
        school = schoolnumber,
        scale_score_literacy = literacyscalescore,
        scale_score_composite = compositeoverallscalescore,
        prof_composite = compositeoverallproficiencylevel
    ) %>%
    mutate_at(vars(starts_with("scale_score_"), starts_with("prof_")), as.numeric) %>%
    filter(!is.na(student_id), !is.na(scale_score_composite)) %>%
    group_by(student_id) %>%
# Dedup by keeping highest composite scale score, then highest literacy scale score by student
    mutate(max_comp = max(scale_score_composite, na.rm = TRUE)) %>%
    filter(scale_score_composite == max_comp) %>%
    mutate(max_lit = max(scale_score_literacy, na.rm = TRUE)) %>%
    filter(scale_score_literacy == max_lit) %>%
    ungroup() %>%
    left_join(prior, by = "student_id") %>%
    mutate(
        growth_standard_denom = as.integer(!is.na(prof_composite) & !is.na(prof_composite_17)),
        growth_standard_1yr = case_when(
            is.na(prof_composite_17) ~ NA_real_,
            prof_composite_17 <= 1.4 ~ 1.3,
            prof_composite_17 <= 1.9 ~ 0.7,
            prof_composite_17 <= 2.4 ~ 0.8,
            prof_composite_17 <= 2.9 ~ 0.7,
            prof_composite_17 <= 3.4 ~ 0.4,
            prof_composite_17 <= 3.9 ~ 0.5,
            prof_composite_17 <= 4.4 ~ 0.4
        ),
        growth_standard_2yr = case_when(
            is.na(prof_composite_16) ~ NA_real_,
            prof_composite_16 <= 1.4 ~ round5(1.3 + prof_composite_16, 1),
            prof_composite_16 <= 1.9 ~ round5(0.7 + prof_composite_16, 1),
            prof_composite_16 <= 2.4 ~ round5(0.8 + prof_composite_16, 1),
            prof_composite_16 <= 2.9 ~ round5(0.7 + prof_composite_16, 1),
            prof_composite_16 <= 3.4 ~ round5(0.4 + prof_composite_16, 1),
            prof_composite_16 <= 3.9 ~ round5(0.5 + prof_composite_16, 1),
            prof_composite_16 <= 4.4 ~ round5(0.4 + prof_composite_16, 1)
        ),
        growth_standard_2yr = case_when(
            is.na(growth_standard_2yr) ~ NA_real_,
            growth_standard_2yr <= 1.4 ~ round5(1.3 + growth_standard_2yr, 1),
            growth_standard_2yr <= 1.9 ~ round5(0.7 + growth_standard_2yr, 1),
            growth_standard_2yr <= 2.4 ~ round5(0.8 + growth_standard_2yr, 1),
            growth_standard_2yr <= 2.9 ~ round5(0.7 + growth_standard_2yr, 1),
            growth_standard_2yr <= 3.4 ~ round5(0.4 + growth_standard_2yr, 1),
            growth_standard_2yr <= 3.9 ~ round5(0.5 + growth_standard_2yr, 1),
            growth_standard_2yr <= 4.4 ~ round5(0.4 + growth_standard_2yr, 1)
        ),
        met_growth_standard = case_when(
            growth_standard_denom == 0 ~ NA_integer_,
            round5(prof_composite - prof_composite_17, 1) >= growth_standard_1yr | prof_composite >= growth_standard_2yr ~ 1L,
            TRUE ~ 0L
        )
    ) %>%
    left_join(demographic, by = "student_id") %>%
    mutate(el = 1) %>%
    mutate_at(c("ed", "swd", "el"), ~ if_else(is.na(.), 0L, as.integer(.)))

# If not including exits
growth_standard_district <- map_dfr(
    .x = list(
        mutate(wida, subgroup = "All Students"),
        filter(wida, bhn == 1) %>% mutate(subgroup = "Black/Hispanic/Native American"),
        filter(wida, ed == 1) %>% mutate(subgroup = "Economically Disadvantaged"),
        filter(wida, el == 1) %>% mutate(subgroup = "English Learners"),
        filter(wida, swd == 1) %>% mutate(subgroup = "Students with Disabilities"),
        filter(wida, bhn == 1 | ed == 1 | el == 1 | swd == 1) %>% mutate(subgroup = "Super Subgroup")
    ),
    .f = function(x) {
        group_by(x, system, subgroup) %>%
        summarise(
            growth_standard_denom = sum(growth_standard_denom, na.rm = TRUE),
            n_met_growth_standard = sum(met_growth_standard, na.rm = TRUE)
        ) %>%
        ungroup() %>%
        transmute(
            system, subgroup,
            growth_standard_denom,
            pct_met_growth_standard = if_else(growth_standard_denom != 0, round5(100 * n_met_growth_standard/growth_standard_denom, 1), NA_real_),
            AMO_target = amo_target(growth_standard_denom, pct_met_growth_standard),
            AMO_target_double = amo_target(growth_standard_denom, pct_met_growth_standard, double = TRUE)
        )
    }
) %>%
    arrange(system, subgroup)

write_csv(growth_standard_district, path = "N:/ORP_accountability/projects/2019_amo/elpa_district.csv", na = "")

# If including exits:
district_w_exit <- read_csv("N:/ORP_accountability/data/2018_ELPA/wida_growth_standard_district.csv") %>%
    transmute(system, subgroup, growth_standard_denom, pct_met_growth_standard,
        AMO_target = amo_target(growth_standard_denom, pct_met_growth_standard),
        AMO_target_double = amo_target(growth_standard_denom, pct_met_growth_standard, double = TRUE))
    
write_csv(district_w_exit, path = "N:/ORP_accountability/projects/2019_amo/elpa_district.csv", na = "")
