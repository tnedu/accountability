library(acct)
library(tidyverse)

accountability_subgroups <- c("Black/Hispanic/Native American", "Economically Disadvantaged",
    "Students with Disabilities", "English Learners with T1/T2")

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II", "English III")
science_eoc <- c("Biology I", "Chemistry")

# Focus schools not exiting in 2015
focus_schools <- read_csv("K:/ORP_accountability/projects/2015_school_coding/Output/focus_schools_not_exiting_ap.csv") %>%
    select(system, school, BHN_gap_identified, ED_gap_identified, SWD_gap_identified, ELL_gap_identified,
        subgroup_path_SWD, subgroup_path_ELL)

pools_immune <- read_csv("K:/ORP_accountability/projects/2017_school_accountability/grade_pools_designation_immune.csv") %>%
    select(system, school, pool, designation_ineligible, grad_only_BHN, grad_only_ED, grad_only_SWD, grad_only_EL)

# One year success rates for Subgroup exit and Below for Gap exit
one_year_success <- read_csv("K:/ORP_accountability/data/2017_final_accountability_files/school_base_2017_for_accountability.csv",
        col_types = c("iiicccddddddddddddddddddddddddd")) %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade)) %>%
    inner_join(pools_immune, by = c("system", "school")) %>%
    filter(subject %in% c("Math", "ELA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate"),
        grade %in% as.character(3:12),
        subgroup %in% accountability_subgroups,
        !(year == 2016 & pool == "K8")) %>%
    mutate(grade = as.integer(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_on_track = if_else(subject == "Graduation Rate", grad_count, n_on_track),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "ELA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, pool, designation_ineligible, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), funs(if_else(valid_tests < 30, 0, .))) %>%
    group_by(year, system, school, pool, designation_ineligible, subgroup) %>%
    summarise_at(c("valid_tests", "n_below", "n_approaching", "n_on_track", "n_mastered"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# One year success rates
    mutate(pct_approaching = if_else(valid_tests != 0, round5(100 * n_approaching/valid_tests, 1), NA_real_),
        pct_on_track = if_else(valid_tests != 0, round5(100 * n_on_track/valid_tests, 1), NA_real_),
        pct_mastered = if_else(valid_tests != 0, round5(100 * n_mastered/valid_tests, 1), NA_real_),
        pct_below = if_else(valid_tests != 0, round5(100 - pct_approaching - pct_on_track - pct_mastered, 1), NA_real_),
        pct_on_mastered = if_else(valid_tests != 0, round5(100 * (n_on_track + n_mastered)/valid_tests, 1), NA_real_),
    # Fix % B/A/O if there are no n_B/A/O
        flag_below = pct_below != 0 & n_below == 0,
        pct_approaching = if_else(flag_below, 100 - pct_on_track - pct_mastered, pct_approaching),
        pct_below = if_else(flag_below, 0, pct_below),
        flag_approaching = pct_approaching != 0 & n_approaching == 0,
        pct_on_track = if_else(flag_approaching, 100 - pct_mastered, pct_on_track),
        pct_approaching = if_else(flag_approaching, 0, pct_approaching)) %>%
    select(year, system, school, pool, designation_ineligible, subgroup, valid_tests, pct_below, pct_on_mastered)

below_2015 <- read_csv("K:/ORP_accountability/data/2015_sas_accountability/school_base_2015_20jul2015.csv") %>%
    mutate(grade = if_else(subject == "Graduation Rate", "12", grade),
        system = as.numeric(system)) %>%
    filter(year == 2015,
        subject %in% c("Math", "RLA", "Science", math_eoc, english_eoc, science_eoc, "Graduation Rate"),
        grade %in% as.character(3:12),
        subgroup %in% c(accountability_subgroups, "English Language Learners with T1/T2")) %>%
    mutate_at(c("system", "grad_cohort", "grad_count"), as.integer) %>% 
    mutate(grade = as.integer(grade),
        valid_tests = if_else(subject == "Graduation Rate", grad_cohort, valid_tests),
        n_prof = if_else(subject == "Graduation Rate", grad_count, n_prof),
        subject = case_when(
            subject %in% math_eoc & grade %in% 3:8 ~ "Math",
            subject %in% english_eoc & grade %in% 3:8 ~ "RLA",
            subject %in% science_eoc & grade %in% 3:8 ~ "Science",
            TRUE ~ subject
        )
    ) %>%
# Aggregate by replaced subjects
    group_by(year, system, school, subject, subgroup) %>%
    summarise_at(c("valid_tests", "n_below_bsc", "n_bsc", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# Suppress below 30 at subject level
    mutate_at(c("valid_tests", "n_below_bsc", "n_bsc", "n_prof", "n_adv"), funs(if_else(valid_tests < 30, 0L, .))) %>%
    group_by(year, system, school, subgroup) %>%
    summarise_at(c("valid_tests", "n_below_bsc", "n_bsc", "n_prof", "n_adv"), sum, na.rm = TRUE) %>%
    ungroup() %>%
# One year below percentages
    mutate(pct_bsc = if_else(valid_tests != 0, round5(100 * n_bsc/valid_tests, 1), NA_real_),
        pct_prof = if_else(valid_tests != 0, round5(100 * n_prof/valid_tests, 1), NA_real_),
        pct_adv = if_else(valid_tests != 0, round5(100 * n_adv/valid_tests, 1), NA_real_),
        pct_below_bsc = if_else(valid_tests != 0, round5(100 - pct_bsc - pct_prof - pct_adv, 1), NA_real_)) %>%
    transmute(system, school,
        subgroup = if_else(subgroup == "English Language Learners with T1/T2", "English Learners with T1/T2", subgroup),
        pct_below_2015 = pct_below_bsc)

# Schools identified by subgroup path exit if subgroup earns a 20% or higher success rate
# Schools identified by subgroup path improve if subgroup earns a 15% or higher success rate
subgroup_schools <- focus_schools %>%
    filter(subgroup_path_SWD == 1 | subgroup_path_ELL == 1) %>%
    select(system, school, subgroup_path_SWD, subgroup_path_ELL) %>%
    inner_join(pools_immune, by = c("system", "school"))

subgroup_exit <- one_year_success %>%
    filter(year == 2017,
        subgroup %in% c("Students with Disabilities", "English Learners with T1/T2")) %>%
    select(system, school, designation_ineligible, subgroup, pct_on_mastered) %>%
    spread(subgroup, pct_on_mastered) %>%
    rename(pct_on_mastered_SWD = `Students with Disabilities`,
        pct_on_mastered_EL = `English Learners with T1/T2`) %>%
    inner_join(subgroup_schools, by = c("system", "school", "designation_ineligible")) %>%
    mutate(SWD_subgroup_exit = if_else(subgroup_path_SWD == 1, pct_on_mastered_SWD > 20, NA),
        SWD_subgroup_improving = if_else(subgroup_path_SWD == 1, pct_on_mastered_SWD >= 15 & pct_on_mastered_SWD <= 20, NA),
        EL_subgroup_exit = if_else(subgroup_path_ELL == 1, pct_on_mastered_EL > 20, NA),
        EL_subgroup_improving = if_else(subgroup_path_ELL == 1, pct_on_mastered_EL >= 15 & pct_on_mastered_EL <= 20, NA)) %>%
# Schools can not exit if designation ineligible or grad only
    mutate_at(c("SWD_subgroup_exit", "SWD_subgroup_improving", "EL_subgroup_exit", "EL_subgroup_improving"),
        funs(if_else(designation_ineligible == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("SWD_subgroup_exit", "SWD_subgroup_improving"),
        funs(if_else(grad_only_SWD == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("EL_subgroup_exit", "EL_subgroup_improving"),
        funs(if_else(grad_only_EL == 1, NA_integer_, as.integer(.))))

gap_exit_HS <- one_year_success %>%
    filter(pool == "HS") %>%
    select(year, system, school, pool, designation_ineligible, subgroup, pct_below) %>%
    spread(year, pct_below) %>%
# Calculate precent below reduction
    transmute(system, school, pool, designation_ineligible, subgroup,
        pct_below_reduction = round5(100 * (`2016` - `2017`)/`2016`, 1)) %>%
    spread(subgroup, pct_below_reduction) %>%
    rename(pct_below_reduction_BHN = `Black/Hispanic/Native American`,
        pct_below_reduction_ED = `Economically Disadvantaged`,
        pct_below_reduction_SWD = `Students with Disabilities`,
        pct_below_reduction_EL = `English Learners with T1/T2`) %>%
# Merge on 2015 focus schools not exiting
    inner_join(select(focus_schools, system, school, ends_with("_gap_identified")), by = c("system", "school")) %>%
# Merge on grad only
    inner_join(pools_immune, by = c("system", "school", "pool", "designation_ineligible")) %>%
# High schools identified by gap path exit if subgroup reduces % Below by 25 percentage points
# High schools identified by gap path improve if subgroup reduces % Below by 12.5 percentage points
    mutate(BHN_gap_exit = if_else(BHN_gap_identified == 1, pct_below_reduction_BHN >= 25, NA),
        BHN_gap_improving = if_else(BHN_gap_identified == 1, pct_below_reduction_BHN >= 12.5 & pct_below_reduction_BHN < 25, NA),
        ED_gap_exit = if_else(ED_gap_identified == 1, pct_below_reduction_ED >= 25, NA),
        ED_gap_improving = if_else(ED_gap_identified == 1, pct_below_reduction_ED >= 12.5 & pct_below_reduction_ED < 25, NA),
        SWD_gap_exit = if_else(SWD_gap_identified == 1, pct_below_reduction_SWD >= 25, NA),
        SWD_gap_improving = if_else(SWD_gap_identified == 1, pct_below_reduction_SWD >= 12.5 & pct_below_reduction_SWD < 25, NA),
        EL_gap_exit = if_else(ELL_gap_identified == 1, pct_below_reduction_EL >= 25, NA),
        EL_gap_improving = if_else(ELL_gap_identified == 1, pct_below_reduction_EL >= 12.5 & pct_below_reduction_EL < 25, NA)) %>%
# Schools can not exit if designation ineligible or grad only
    mutate_at(c("BHN_gap_exit", "BHN_gap_improving", "ED_gap_exit", "ED_gap_improving",
        "SWD_gap_exit", "SWD_gap_improving", "EL_gap_exit", "EL_gap_improving"),
        funs(if_else(designation_ineligible == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("BHN_gap_exit", "BHN_gap_improving"), funs(if_else(grad_only_BHN == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("ED_gap_exit", "ED_gap_improving"), funs(if_else(grad_only_ED == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("SWD_gap_exit", "SWD_gap_improving"), funs(if_else(grad_only_SWD == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("EL_gap_exit", "EL_gap_improving"), funs(if_else(grad_only_EL == 1, NA_integer_, as.integer(.))))

gap_exit_k8 <- one_year_success %>%
    filter(pool == "K8") %>%
    select(year, system, school, pool, designation_ineligible, subgroup, pct_below) %>%
    left_join(below_2015, by = c("system", "school", "subgroup")) %>%
# Denominator for pctile ranks only counts non-designation ineligible schools with % below in both 2017 and 2015
    mutate(temp = !is.na(pct_below) & !is.na(pct_below_2015)) %>%
    group_by(subgroup, designation_ineligible) %>%
    mutate(denom = sum(temp, na.rm = TRUE)) %>%
    group_by(subgroup, designation_ineligible, temp) %>%
# Rank percent below
    mutate(rank_below = if_else(!is.na(pct_below), rank(pct_below, ties.method = "max"), NA_integer_),
        rank_below_prior = if_else(!is.na(pct_below_2015), rank(pct_below_2015, ties.method = "max"), NA_integer_)) %>%
    ungroup() %>%
# Calculate below percentile change
    mutate(pctile_below = round5(100 * rank_below/denom, 1),
        pctile_below_prior = round5(100 * rank_below_prior/denom, 1),
        pctile_rank_reduction = pctile_below_prior - pctile_below) %>%
    select(system, school, pool, designation_ineligible, subgroup, pctile_rank_reduction) %>%
    spread(subgroup, pctile_rank_reduction) %>%
    rename(below_pctile_reduction_BHN = `Black/Hispanic/Native American`,
        below_pctile_reduction_ED = `Economically Disadvantaged`,
        below_pctile_reduction_SWD = `Students with Disabilities`,
        below_pctile_reduction_EL = `English Learners with T1/T2`) %>%
# Merge on 2015 focus schools not exiting
    inner_join(select(focus_schools, system, school, ends_with("_gap_identified")), by = c("system", "school")) %>%
# Merge on grad only
    inner_join(pools_immune, by = c("system", "school", "pool", "designation_ineligible")) %>%
# K8 schools identified by gap path exit if subgroup reduces Below percentile rank by 25 percentage points
# K8 schools identified by gap path improve if subgroup reduces Below percentile rank by 12.5 percentage points
    mutate(BHN_gap_exit = if_else(BHN_gap_identified == 1, below_pctile_reduction_BHN >= 25, NA),
        BHN_gap_improving = if_else(BHN_gap_identified == 1, below_pctile_reduction_BHN >= 12.5 & below_pctile_reduction_BHN < 25, NA),
        ED_gap_exit = if_else(ED_gap_identified == 1, below_pctile_reduction_ED >= 25, NA),
        ED_gap_improving = if_else(ED_gap_identified == 1, below_pctile_reduction_ED >= 12.5 & below_pctile_reduction_ED < 25, NA),
        SWD_gap_exit = if_else(SWD_gap_identified == 1, below_pctile_reduction_SWD >= 25, NA),
        SWD_gap_improving = if_else(SWD_gap_identified == 1, below_pctile_reduction_SWD >= 12.5 & below_pctile_reduction_SWD < 25, NA),
        EL_gap_exit = if_else(ELL_gap_identified == 1, below_pctile_reduction_EL >= 25, NA),
        EL_gap_improving = if_else(ELL_gap_identified == 1, below_pctile_reduction_EL >= 12.5 & below_pctile_reduction_EL < 25, NA)) %>%
# Schools can not exit if designation ineligible or grad only    
    mutate_at(c("BHN_gap_exit", "BHN_gap_improving", "ED_gap_exit", "ED_gap_improving",
        "SWD_gap_exit", "SWD_gap_improving", "EL_gap_exit", "EL_gap_improving"),
        funs(if_else(designation_ineligible == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("BHN_gap_exit", "BHN_gap_improving"), funs(if_else(grad_only_BHN == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("ED_gap_exit", "ED_gap_improving"), funs(if_else(grad_only_ED == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("SWD_gap_exit", "SWD_gap_improving"), funs(if_else(grad_only_SWD == 1, NA_integer_, as.integer(.)))) %>%
    mutate_at(c("EL_gap_exit", "EL_gap_improving"), funs(if_else(grad_only_EL == 1, NA_integer_, as.integer(.))))

focus_exit_improving <- bind_rows(gap_exit_HS, gap_exit_k8) %>%
    full_join(subgroup_exit, by = c("system", "school", "pool", "designation_ineligible",
        "grad_only_BHN", "grad_only_ED", "grad_only_SWD", "grad_only_EL")) %>%
    rowwise() %>%
# Schools exit if they meet exit criteria for all subgroups for which they are identified as focus
    mutate(gap_identified_count = sum(BHN_gap_identified, ED_gap_identified, SWD_gap_identified, ELL_gap_identified, na.rm = TRUE),
        gap_exit_count = sum(BHN_gap_exit, ED_gap_exit, SWD_gap_exit, EL_gap_exit, na.rm = TRUE),
        subgroup_identified_count = sum(subgroup_path_SWD, subgroup_path_ELL, na.rm = TRUE),
        subgroup_exit_count = sum(SWD_subgroup_exit, EL_subgroup_exit, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(gap_exit = if_else(gap_identified_count != 0, gap_identified_count == gap_exit_count, NA),
        subgroup_exit = if_else(subgroup_identified_count != 0, subgroup_identified_count == subgroup_exit_count, NA),
        pathway_identified_count = (gap_identified_count != 0) + (subgroup_identified_count != 0)) %>%
    rowwise() %>%
    mutate(focus_exit_count = sum(gap_exit, subgroup_exit, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(focus_exit = focus_exit_count == pathway_identified_count) %>%
    mutate_at(c("gap_exit", "subgroup_exit", "focus_exit"), as.integer)

output <- focus_exit_improving %>%
    select(system, school, pool, designation_ineligible, starts_with("grad_only_"),
    # Subgroup exit variables    
        starts_with("subgroup_path"), starts_with("pct_on_mastered_"),
        SWD_subgroup_exit, SWD_subgroup_improving, EL_subgroup_exit, EL_subgroup_improving,
    # Gap exit variables
        ends_with("_gap_identified"), starts_with("pct_below_reduction_"),
        starts_with("below_pctile_reduction_"), contains("_gap_"),
    # Summative variables
        subgroup_identified_count, subgroup_exit_count, subgroup_exit,
        gap_identified_count, gap_exit_count, gap_exit,
        pathway_identified_count, focus_exit_count, focus_exit)

write_csv(output, path = "K:/ORP_accountability/projects/2017_school_accountability/focus_exit_improving.csv", na = "")

# Focus not exiting for reward
focus_not_exiting <- filter(output, focus_exit == 0 | is.na(focus_exit))

write_csv(focus_not_exiting, path = "K:/ORP_accountability/projects/2017_school_accountability/focus_schools_not_exiting.csv", na = "")
