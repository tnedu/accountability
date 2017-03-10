library(haven)
library(tidyverse)

# Quintiles of percent exit by subgroup
econ_dis <- read_csv("K:/ORP_accountability/projects/2016_acct_modeling/sc_ed_lw.csv") %>% 
    rename(student_id = state_id)

elpa16 <- read_dta("K:/ORP_accountability/data/2016_WIDA_Access/2016_State_Student_Data_File_ACH.dta") %>%
    mutate(timeinlepellinus = ifelse(timeinlepellinus %in% c("<1", "0.1", "0.3", "0.5", "0.7", "3m", "8M"), "0", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("1y", "1+"), "1", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("2y", "2Y", "2+"), "2", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("3y", "3+"), "3", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("4y", "4+"), "4", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus %in% c("5y", "5+"), "5", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "6y", "6", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "7y", "7", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "8y", "8", timeinlepellinus),
        timeinlepellinus = ifelse(timeinlepellinus == "  ", grade, timeinlepellinus)) %>% 
    transmute(system = as.numeric(substr(districtcode, 3, length(districtcode))), school = schoolcode, 
        student_id = statestudentid, swd = iepstatus, time_in_esl = as.numeric(timeinlepellinus),
        hispanic = ethnicityhispaniclatino, native = raceamericanindianalaskanative, black = raceblack, 
        literacy = as.numeric(literacyperformancelevel), composite = as.numeric(performancelevelcomposite)) %>%
    left_join(econ_dis, by = "student_id") %>% 
# Drop missing student ids and records with missing literacy and composite scores
    filter(!is.na(student_id)) %>%
    filter(!(is.na(literacy) & is.na(composite))) %>%
    group_by(system, school, student_id) %>%
# Dedup first by max composite score
    mutate(max = max(composite)) %>%
    ungroup() %>%
    filter(composite == max | is.na(max)) %>%
    select(-max)

# Observations by subgroup
elpa_all <- elpa16 %>%
    mutate(subgroup = "All Students")

elpa_ed <- elpa16 %>%
    filter(ed == 1) %>% 
    mutate(subgroup = "Economically Disadvantaged")

elpa_bhn <- elpa16 %>%
    filter(hispanic == "H" | native == "Y" | black == "Y") %>%
    mutate(subgroup = "Black/Hispanic/Native American")

elpa_swd <- elpa16 %>%
    filter(swd == "Y") %>% 
    mutate(subgroup = "Students with Disabilities")

elpa_el <- elpa16 %>% 
    mutate(subgroup = "English Learners")

exit <- bind_rows(elpa_all, elpa_ed, elpa_bhn, elpa_swd, elpa_el) %>% 
    mutate(valid_tests = !is.na(literacy) & !is.na(composite),
        exit_count = ifelse(valid_tests, literacy >= 5.0 & composite >= 5.0, NA),
        denom = ifelse(time_in_esl == 0, 0.2, NA),
        denom = ifelse(time_in_esl == 1, 0.4, denom),
        denom = ifelse(time_in_esl == 2, 0.6, denom),
        denom = ifelse(time_in_esl == 3, 0.8, denom),
        denom = ifelse(time_in_esl == 4, 1.0, denom),
        denom = ifelse(time_in_esl >= 5 | is.na(time_in_esl), 1.2, denom)) %>%
    group_by(system, subgroup) %>%
    summarise_each(funs(sum(., na.rm = TRUE)), valid_tests, exit_count, denom) %>%
    group_by(subgroup) %>%
    mutate(pct_exit = ifelse(valid_tests >= 10, round(100 * exit_count/denom, 1), NA),
        rank_pct_exit = ifelse(!is.na(pct_exit), rank(pct_exit, ties.method = "max"), NA),
        exit_denom = sum(!is.na(pct_exit)),
        exit_quintile = ifelse(rank_pct_exit/exit_denom < 0.2, 0, NA),
        exit_quintile = ifelse(rank_pct_exit/exit_denom >= 0.2, 1, exit_quintile),
        exit_quintile = ifelse(rank_pct_exit/exit_denom >= 0.4, 2, exit_quintile),
        exit_quintile = ifelse(rank_pct_exit/exit_denom >= 0.6, 3, exit_quintile),
        exit_quintile = ifelse(rank_pct_exit/exit_denom >= 0.8, 4, exit_quintile))

write_csv(exit, path = "data/elpa_exit.csv", na = "")

# AMOs of Percent Meeting Growth Standard
growth_standard_prior <- readxl::read_excel("K:/ORP_accountability/projects/Title III/Output/AMAO_I_2015_w_subgroups.xlsx") %>%
    select(system, subgroup, tested_prior = tested_both_years, met_growth_prior = improved) %>%
    mutate(pct_met_growth_prior = ifelse(tested_prior >= 10, round(100 * met_growth_prior/tested_prior, 1), NA),
        AMO_target = ifelse(tested_prior >= 10, round(pct_met_growth_prior + (100 + pct_met_growth_prior)/16, 1), NA),
        AMO_target_4 = ifelse(tested_prior >= 10, round(pct_met_growth_prior + (100 + pct_met_growth_prior)/8, 1), NA))

growth_standard <- read_dta("K:/ORP_accountability/projects/Title III/Output/2016_district_level_growth_standard_w_subgroups.dta") %>%
    select(system, subgroup, tested = valid_tests, met_growth) %>%
    mutate(pct_met_growth = ifelse(tested >= 10, round(met_growth/tested, 3), NA),
        upper_bound_ci = round(100 * (tested/(tested + qnorm(0.975)^2)) * (pct_met_growth + ((qnorm(0.975)^2)/(2 * tested)) +
            qnorm(0.975) * sqrt((pct_met_growth * (1 - pct_met_growth))/tested + (qnorm(0.975)^2)/(4 * tested^2))), 1),
        pct_met_growth = 100 * pct_met_growth) %>%
    left_join(growth_standard_prior, by = c("system", "subgroup")) %>%
    mutate(growth_standard_AMO = ifelse(upper_bound_ci <= pct_met_growth_prior, 0, NA),
        growth_standard_AMO = ifelse(upper_bound_ci > pct_met_growth_prior, 1, growth_standard_AMO),
        growth_standard_AMO = ifelse(upper_bound_ci >= AMO_target, 2, growth_standard_AMO),
        growth_standard_AMO = ifelse(pct_met_growth >= AMO_target, 3, growth_standard_AMO),
        growth_standard_AMO = ifelse(pct_met_growth >= AMO_target_4, 4, growth_standard_AMO))

write_csv(growth_standard, path = "data/elpa_growth_standard.csv", na = "")
