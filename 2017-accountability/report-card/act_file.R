library(haven)
library(tidyverse)

act_state <- read_dta("K:/ORP_accountability/data/2017_ACT/ACT_state2018_appeals.dta") %>%
    filter(subgroup == "All Students") %>%
    transmute(District_ID = 0, School_ID = 0, `Analysis Level` = "State", `District Name` = "",
        `ACT Dist Code` = "", `HS Name` = "", `Grad Year` = 2017, N = valid_tests,
        `Avg Eng` = english_avg, `Avg Math` = math_avg, `Avg Reading` = reading_avg, `Avg Sci` = science_avg, `Avg Comp` = act_composite_avg,
        `CRB % Eng` = pct_met_CRB_english, `CRB % Math` = pct_met_CRB_math, `CRB % Reading` = pct_met_CRB_reading,
        `CRB % Sci` = pct_met_CRB_science, `CRB % All Four` = pct_met_All4_CRB,
        n_21_or_higher = n_21_orhigher, pct_21_or_higher = pct_21_orhigher,
        n_female_21_or_higher = NA, pct_female_21_or_higher = female_pct21orhigher,
        n_male_21_or_higher = NA, pct_male_21_or_higher = male_pct21orhigher,
        n_gender_missing_21_or_higher = nmissing_21_orhigher, pct_gender_missing_21_or_higher = 0, pct_gender_missing_below_19 = "")

act_district <- read_dta("K:/ORP_accountability/data/2017_ACT/ACT_district2018_appeals.dta") %>%
    filter(subgroup == "All Students") %>%
    transmute(District_ID = as.numeric(system), School_ID = 0, `Analysis Level` = "District", `District Name` = "",
        `ACT Dist Code` = "", `HS Name` = "All Schools", `Grad Year` = 2017, N = valid_tests,
        `Avg Eng` = english_avg, `Avg Math` = math_avg, `Avg Reading` = reading_avg, `Avg Sci` = science_avg, `Avg Comp` = act_composite_avg,
        `CRB % Eng` = pct_met_CRB_english, `CRB % Math` = pct_met_CRB_math, `CRB % Reading` = pct_met_CRB_reading,
        `CRB % Sci` = pct_met_CRB_science, `CRB % All Four` = pct_met_All4_CRB,
        n_21_or_higher = n_21_orhigher, pct_21_or_higher = pct_21_orhigher,
        n_female_21_or_higher = NA, pct_female_21_or_higher = female_pct21orhigher,
        n_male_21_or_higher = NA, pct_male_21_or_higher = male_pct21orhigher,
        n_gender_missing_21_or_higher = nmissing_21_orhigher, pct_gender_missing_21_or_higher = 0, pct_gender_missing_below_19 = "")

act_school <- read_dta("K:/ORP_accountability/data/2017_ACT/ACT_school2018_appeals.dta") %>%
    filter(subgroup == "All Students") %>%
    transmute(District_ID = as.numeric(system), School_ID = as.numeric(school), `Analysis Level` = "School", `District Name` = "",
        `ACT Dist Code` = "", `HS Name` = "", `Grad Year` = 2017, N = valid_tests,
        `Avg Eng` = english_avg, `Avg Math` = math_avg, `Avg Reading` = reading_avg, `Avg Sci` = science_avg, `Avg Comp` = act_composite_avg,
        `CRB % Eng` = pct_met_CRB_english, `CRB % Math` = pct_met_CRB_math, `CRB % Reading` = pct_met_CRB_reading,
        `CRB % Sci` = pct_met_CRB_science, `CRB % All Four` = pct_met_All4_CRB,
        n_21_or_higher = n_21_orhigher, pct_21_or_higher = pct_21_orhigher,
        n_female_21_or_higher = NA, pct_female_21_or_higher = female_pct21orhigher,
        n_male_21_or_higher = NA, pct_male_21_or_higher = male_pct21orhigher,
        n_gender_missing_21_or_higher = nmissing_21_orhigher, pct_gender_missing_21_or_higher = 0, pct_gender_missing_below_19 = "")

act_all <- bind_rows(act_state, act_district, act_school) %>%
    arrange(District_ID, School_ID)

write_csv(act_all, path = "K:/ORP_accountability/data/2017_final_accountability_files/Report Card/ACT_File_2016_17.csv")
