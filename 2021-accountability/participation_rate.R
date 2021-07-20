# Participation Rate
# For 80% hold harmless
# 4/29/21

library(tidyverse)

process_reg_file <- function(reg_file_path) {
  read_csv(reg_file_path,
           col_types = 'dddcccccdcccdcddccccccccccccccdddddddd') %>% 
    janitor::clean_names() %>% 
    transmute(
      system = as.numeric(district_id),
      school = as.numeric(school_id),
      unique_student_id = as.numeric(usid),
      last_name, first_name, middle_initial,
      dob, gender, grade = enrolled_grade, test_format,
      snt_subpart1, snt_subpart2, snt_subpart3,
      # diff
      test_name = case_when(
        str_detect(test_name, 'Social Studies') ~ 'Social Studies',
        str_detect(test_name, 'Science') ~ 'Science',
        str_detect(test_name, 'Gr \\d Math') ~ 'Math',
        str_detect(test_name, 'English Lang Arts') ~ 'ELA',
        str_detect(test_name, 'English I ') ~ 'English I',
        str_detect(test_name, 'English II') ~ 'English II',
        str_detect(test_name, 'U.S. History') ~ 'US History',
        str_detect(test_name, 'Braille Algebra I') ~ 'Algebra I',
        str_detect(test_name, 'Braille Algebra II') ~ 'Algebra II',
        str_detect(test_name, 'Braille Biology') ~ 'Biology',
        str_detect(test_name, 'Braille Geometry') ~ 'Geometry',
        str_detect(test_name, 'Braille Integrated Math II') ~ 'Integrated Math II',
        TRUE ~ test_name
      )
    ) %>% 
    mutate(
      # snt_reg = case_when(
      #   # !is.na(snt_subpart1) & !is.na(snt_subpart2) & !is.na(snt_subpart3) ~ snt_subpart1
      #   !is.na(snt_subpart1) ~ snt_subpart1,
      #   !is.na(snt_subpart2) ~ snt_subpart2,
      #   !is.na(snt_subpart3) ~ snt_subpart3,
      #   TRUE ~ NA_real_
      # )
      snt_reg = pmin(snt_subpart1, snt_subpart2, snt_subpart3, na.rm = TRUE)
    ) %>% 
    select(-(snt_subpart1: snt_subpart3)) %>% 
    # diff
    arrange(unique_student_id, test_name, snt_reg) %>% 
    group_by(unique_student_id, test_name) %>% 
    mutate(
      temp = first(snt_reg)
    ) %>% 
    filter(snt_reg == temp | is.na(temp)) %>% 
    ungroup() %>% 
    select(-temp) %>% 
    distinct() %>% # Distinct removing duplicates
    # filter(!is.na(system),
    #        # grade != 13 | is.na(grade),
    #        !(school %in% c(981,982) | system >= 990)#, # 981 is homeschool  residential_facility != 1 | is.na(residential_facility),
    #        # Drop medically exempt?
    # ) %>% 
    mutate(
      content_area_code = case_when(
        test_name == "ELA" ~ "EN",
        test_name == "Math" ~ "MA",
        test_name == "Science" ~ "SCI",
        test_name == "Social Studies" ~ "SS",
        test_name == "Algebra I" ~ "A1",
        test_name == "Algebra II" ~ "A2",
        test_name == "Biology" ~ "B1",
        test_name == "Chemistry" ~ "C1", # Not used
        test_name == "English I" ~ "E1",
        test_name == "English II" ~ "E2",
        test_name == "English III" ~ "E3", # Not used
        test_name == "Geometry" ~ "G1",
        test_name == "Integrated Math I" ~ "M1",
        test_name == "Integrated Math II" ~ "M2",
        test_name == "Integrated Math III" ~ "M3",
        test_name == "US History" ~ "U1",
        TRUE ~ NA_character_
      )
    )
}

# School info ===============
school_names <- read_csv("N:/ORP_accountability/data/2021_final_accountability_files/names.csv") # %>% 

alt_cte_adult <- read_csv("N:/ORP_accountability/data/2021_tdoe_provided_files/cte_alt_adult_schools.csv") %>%
  transmute(system = as.numeric(DISTRICT_NUMBER), school = as.numeric(SCHOOL_NUMBER), cte_alt_adult = 1)

# WIDA ==================
# read in WIDA ACCESS file
# diff
wida_current <- read_csv("N:/ORP_accountability/data/2021_ELPA/wida_growth_standard_student.csv")

# diff
# Demo =================
demographics <- read_csv("N:/TNReady/2020-21/spring/demographics/student_demographics_06082021.csv")

demos_filtered <- demographics%>% 
  filter(str_length(student_key) == 7) %>% 
  transmute(
    unique_student_id = student_key,
    system = district_no,
    school = school_no,
    gender, 
    hispanic = if_else(ethnicity == 'H', 'Y', 'N'),
    economically_disadvantaged = case_when(
      codeab == 1 ~ 'Y',
      codeab == 2 ~ 'N',
      TRUE ~ NA_character_
    ),
    reported_race = reportedrace,
    title_1 = title1,
    gifted = isgifted,
    functionally_delayed = isfunctionallydelayed,
    # foster = isfostercare,
    migrant = ismigrant,
    # foster = isfostercare,
    el_arrived_year_1 = elrecentlyarrivedyearone,
    el_arrived_year_2 = elrecentlyarrivedyeartwo,
    el = isel,
    t1234 = t1t2,
    special_ed = specialeducation,
    enrolled_50_pct_district = district50percent,
    enrolled_50_pct_school = school50percent
  ) %>% 
  mutate(
    native_american = if_else(reported_race == 1, 'Y', 'N'),
    asian = if_else(reported_race == 2, 'Y', 'N'),
    black = if_else(reported_race == 3, 'Y', 'N'),
    hawaiian_pi = if_else(reported_race == 5, 'Y', 'N'),
    white = if_else(reported_race == 6, 'Y', 'N'),
    reported_race = case_when(
      reported_race == 1 ~ 'American Indian/Alaska Native',
      reported_race == 2 ~ 'Asian',
      reported_race == 3 ~ 'Black or African American',
      reported_race == 4 ~ 'Hispanic/Latino',
      reported_race == 5 ~ 'Native Hawaiian/Pac. Islander',
      reported_race == 6 ~ 'White',
      TRUE ~ 'Unknown'
    ),
    bhn_group = if_else(!reported_race %in% c('American Indian/Alaska Native','Black or African American','Hispanic/Latino') | is.na(reported_race), 0, 1)
  )

# ======================================= Fall EOC ===========================================================
# Registration Fall EOC
reg_fall_eoc <- process_reg_file("N:/Assessment_Data Returns/Student Registration file/SY2020-21/EOC fall Student Registration Export 2021-06-15.csv")

# diff
# Set to blank if test code = TNSCIEBI, TNBRSCIEBI, TNSOCSUH, TNBRSOCSUH, or TNALTSCBI
fall_eoc <- read_fwf("N:/Assessment_Data Returns/TCAP_End-of-Course/2020-2021/Fall2020_EOC/2020_TN_Fall_2020_EOC_CDF_20210204.txt",
                     col_types = 'icicccciicccciiiiiciic',
                     fwf_cols(system = c(33, 37), 
                              system_name = c(38, 112), 
                              school = c(113, 116),
                              school_name = c(117, 191), 
                              last_name = c(192, 241), 
                              first_name = c(242, 291),
                              middle_initial = c(292, 292), 
                              unique_student_id = c(302, 310), 
                              grade = c(346, 347), # enrolled grade for EOC (No tested grade)
                              content_area_code = c(350, 352), 
                              attempted = c(429, 429), 
                              modified_format = c(430, 431),
                              school_type = c(546, 548),
                              teacher_of_record_tln = c(441, 460), 
                              reason_not_tested = c(543, 543), 
                              ri_status = c(544, 544),
                              raw_score = c(733, 735), 
                              scale_score= c(742, 745), 
                              performance_level = c(746, 760),
                              scale_score_lb_ci = c(764,766), 
                              scale_score_ub_ci = c(761,763), 
                              item_response_array=c(781,910)))

fall_eoc_total <- fall_eoc %>%
  full_join(
    reg_fall_eoc %>% 
      mutate(grade = as.numeric(grade)) %>% 
      select(system, school, unique_student_id, content_area_code, grade, snt_reg),
    # diff: by grade (no modified format)
    by = c("system", "school", "unique_student_id", "content_area_code", "grade")
  ) %>% 
  left_join(
    reg_fall_eoc %>% 
      select(unique_student_id, first_name, last_name, middle_initial) %>% 
      distinct() %>% 
      rename_with(~paste0(., "_reg"), first_name:middle_initial),
    by = "unique_student_id"
  ) %>% 
  mutate(
    first_name = if_else(is.na(first_name), first_name_reg, first_name),
    last_name = if_else(is.na(last_name), last_name_reg, last_name),
    middle_initial = if_else(is.na(middle_initial), middle_initial_reg, middle_initial),
    # If there is an SNT from registration, use that only if there is no score
    reason_not_tested = if_else(
      (reason_not_tested == 0 | is.na(reason_not_tested)) & !is.na(snt_reg) & is.na(raw_score),
      as.integer(snt_reg),
      reason_not_tested
    ),
    # If SNT is still missing, assign SNT of 1 (absent)
    # SNT missing at this point if they were registered with no SNT and they do not appear in the CDF
    reason_not_tested = if_else(is.na(reason_not_tested) | (reason_not_tested == 0 & is.na(raw_score)), 1L, reason_not_tested)
  ) %>% 
  left_join(demos_filtered, by = c( "system", "school", "unique_student_id")) %>%
  select(system:attempted, gender, reported_race, bhn_group, hispanic, native_american:white, 
         economically_disadvantaged, title_1, gifted, functionally_delayed, #foster,
         migrant, el, el_arrived_year_1:special_ed,  modified_format, 
         enrolled_50_pct_district, enrolled_50_pct_school,
         teacher_of_record_tln:item_response_array) %>% 
  filter(
    system <= 986,  # Private School districts
    school != 981,  # Homeschool
    grade %in% 1:12 | is.na(grade)  # Grade 13, 89 grade 0
  ) %>%
  mutate(grade = if_else(grade %in% 1:2, NA_real_, grade)) %>% 
  replace_na(list(bhn_group = 0)) %>% # race = 'Unknown', 
  select(-(hispanic:white)) %>% 
  mutate(
    test= 'EOC',
    semester = 'Fall'
  )


# =============================================== TCAP 3-8 ========================================================================
reg_3_8 <- process_reg_file("N:/Assessment_Data Returns/Student Registration file/SY2020-21/ACH Student Registration Export 2021-06-15.csv")

raw_score_3_8 <- read_csv("N:/Assessment_Data Returns/Student Registration file/SY2020-21/Raw Score/2021_TN_Spring_2021_Grades_2_8_RSF_20210607.csv")

participation_3_8 <- raw_score_3_8 %>% 
  transmute(
    system = as.numeric(DistrictNumber),
    school = as.numeric(SchoolNumber),
    unique_student_id = as.numeric(USID),
    grade = as.numeric(TestGrade),
    content_area_code = case_when(
      ContentAreaCode == "ENG" ~ "EN",
      ContentAreaCode == "MAT" ~ "MA",
      ContentAreaCode == "SOC" ~ "SS",
      TRUE ~ ContentAreaCode
    ),
    reason_not_tested = OverallSNT,
    ri_status = OverallRIStatus,
    raw_score = as.numeric(TotalRawScore)
  ) %>% 
  full_join(
    reg_3_8 %>% 
      select(system, school, unique_student_id, content_area_code, grade, snt_reg),
    by = c("system", "school", "unique_student_id", "content_area_code", "grade")
  ) %>% 
  arrange(unique_student_id, content_area_code, -raw_score) %>% 
  group_by(unique_student_id, content_area_code) %>% 
  mutate(temp = first(raw_score)) %>% 
  ungroup() %>% 
  filter(temp == raw_score | is.na(temp)) %>% 
  arrange(unique_student_id, content_area_code, reason_not_tested) %>% # -reason_not_tested
  group_by(unique_student_id, content_area_code) %>% 
  mutate(temp = first(reason_not_tested)) %>% 
  ungroup() %>% 
  filter(temp == reason_not_tested | is.na(temp)) %>% 
  left_join(
    demos_filtered,
    by = c("system", "school", "unique_student_id")
  ) %>%
  mutate(
    # performance_level = if_else(performance_level == "On track", "On Track", performance_level),
    # If there is an SNT from registration, use that only if there is no score
    reason_not_tested = if_else(
      (reason_not_tested == 0 | is.na(reason_not_tested)) &
        !is.na(snt_reg) &
        is.na(raw_score),
      as.integer(snt_reg),
      as.integer(reason_not_tested)
    ),
    # reason_not_tested = if_else(is.na(reason_not_tested) & !is.na(snt_reg), as.integer(snt_reg), reason_not_tested),
    # If SNT is still missing, assign SNT of 1 (absent)
    # SNT missing at this point if they were registered with no SNT and they do not appear in the CDF
    reason_not_tested = if_else(
      is.na(reason_not_tested) |
        (reason_not_tested == 0 & is.na(raw_score)),
      1L,
      reason_not_tested
    ),
    subject = case_when(
      content_area_code == "EN" ~ "ELA",
      content_area_code == "MA" ~ "Math",
      content_area_code == "SCI" ~ "Science",
      content_area_code == "SS" ~ "Social Studies"
    ),
    grade = as.numeric(grade),
    test = "TNReady",
    semester = "Spring"
  )

# =================================== Spring EOC ==================================================
reg_spring_eoc <- process_reg_file("N:/Assessment_Data Returns/Student Registration file/SY2020-21/EOC spring Student Registration Export 2021-06-15.csv")

raw_score_spring_eoc <- read_csv("N:/Assessment_Data Returns/Student Registration file/SY2020-21/Raw Score/2021_TN_Spring_2021_EOC_RSF_20210607.csv")

participation_spring_eoc <- raw_score_spring_eoc %>% 
  transmute(
    system = as.numeric(DistrictNumber),
    school = as.numeric(SchoolNumber),
    unique_student_id = as.numeric(USID),
    grade = as.numeric(EnrolledGrade),
    content_area_code = case_when(
      ContentAreaCode == "ENG" ~ "EN",
      ContentAreaCode == "MAT" ~ "MA",
      ContentAreaCode == "SOC" ~ "SS",
      TRUE ~ ContentAreaCode
    ),
    reason_not_tested = OverallSNT,
    ri_status = OverallRIStatus,
    raw_score = as.numeric(TotalRawScore)
  ) %>% 
  full_join(
    reg_spring_eoc %>% 
      select(system, school, unique_student_id, content_area_code, grade, snt_reg),
    by = c("system", "school", "unique_student_id", "content_area_code", "grade")
  ) %>% 
  # Dedup by raw score
  arrange(unique_student_id, content_area_code, -raw_score) %>% 
  group_by(unique_student_id, content_area_code) %>% 
  mutate(temp = first(raw_score)) %>% 
  ungroup() %>% 
  filter(temp == raw_score | is.na(temp)) %>% 
  # Dedup by reason_not_tested; handful of duplicates that this takes care of
  arrange(unique_student_id, content_area_code, -reason_not_tested) %>% 
  group_by(unique_student_id, content_area_code) %>% 
  mutate(temp = first(reason_not_tested)) %>% 
  ungroup() %>% 
  filter(temp == reason_not_tested | is.na(temp)) %>% 
  left_join(
    demos_filtered,
    by = c("system", "school", "unique_student_id")
  ) %>%
  mutate(
    grade = as.numeric(grade),
    # performance_level = if_else(performance_level == "On track", "On Track", performance_level),
    # If there is an SNT from registration, use that only if there is no score
    reason_not_tested = if_else((reason_not_tested == 0 | is.na(reason_not_tested)) & !is.na(snt_reg) & is.na(raw_score), as.integer(snt_reg), as.integer(reason_not_tested)),
    # reason_not_tested = if_else(is.na(reason_not_tested) & !is.na(snt_reg), as.integer(snt_reg), reason_not_tested),
    # If SNT is still missing, assign SNT of 1 (absent)
    # SNT missing at this point if they were registered with no SNT and they do not appear in the CDF
    reason_not_tested = if_else(is.na(reason_not_tested) | (reason_not_tested == 0 & is.na(raw_score)), 1L, reason_not_tested),
    subject = case_when(
      content_area_code == "EN" ~ "ELA",
      content_area_code == "MA" ~ "Math",
      content_area_code == "SCI" ~ "Science",
      content_area_code == "SS" ~ "Social Studies",
      content_area_code == "A1" ~ "Algebra I",
      content_area_code == "A2" ~ "Algebra II",
      content_area_code == "B1" ~ "Biology",
      content_area_code == "E1" ~ "English I",
      content_area_code == "E2" ~ "English II",
      content_area_code == "G1" ~ "Geometry",
      content_area_code == "M1" ~ "Integrated Math I",
      content_area_code == "M2" ~ "Integrated Math II",
      content_area_code == "M3" ~ "Integrated Math III",
      content_area_code == "U1" ~ "US History"
    ),
    test = "EOC",
    semester = "Spring"
    # absent = if_else(is.na(system_name), 1, absent) # Missing system name means that record came from student not in CDF, setting to absent for ease of calculation
  )
# Alt ===============
reg_alt <- process_reg_file("N:/Assessment_Data Returns/Student Registration file/SY2020-21/Alt Student Registration Export 2021-06-15.csv")

alt_science_ss <- reg_alt %>% 
  select(-gender) %>% 
  left_join(
    demos_filtered,
    by = c("system", "school", "unique_student_id")
  ) %>%
  mutate(
    test = "Alt-Science/Social Studies",
    semester = "Spring",
    # All students who take the TCAP-Alternative Assessment are considered students with disabilities (SWD)
    special_ed = 1L
  ) %>%
  mutate(
    economically_disadvantaged = if_else(economically_disadvantaged == 1, 'Y', 'N'),
    bhn_group = if_else(reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"), 1, 0)
  )
# MSAA =====================
# MSAA
msaa <- read_csv("N:/ORP_accountability/data/2021_cdf/2021_msaa_cdf.csv") %>%
  filter(!reporting_status %in% c("WDR", "NLE")) %>%
  # rename(race = reported_race) %>%
  mutate(
    test = "MSAA",
    semester = "Spring",
    # All students who take the TCAP-Alternative Assessment are considered students with disabilities (SWD)
    special_ed = 1,# = 1
    performance_level = if_else(reporting_status != "TES", NA_character_, performance_level),
    # absent = 0,
    # enrolled = 1,
    grade = as.numeric(grade),
    tested = if_else(reporting_status == "DNT", 0, 1)
  ) %>%
  # mutate_at(c("refused_to_test", "residential_facility"), function(x) x = 0) %>%
  # mutate_at(c("functionally_delayed"), function(x) x = 0) %>%
  left_join(demos_filtered %>% select(system, school, unique_student_id, bhn_group), by = c("system", "school", "unique_student_id")) %>%
  replace_na(list(bhn_group = 0))

# Total CDF =================
# Combine EOCs, 3-8
total_cdf <- bind_rows(fall_eoc_total, participation_spring_eoc, participation_3_8, alt_science_ss) %>% # , spring_eoc_total, grade_3_8_total, 
  # filter(content_area_code != 'E3') %>% 
  mutate(
    performance_level = if_else(performance_level == "On track", "On Track", performance_level),
    absent = if_else(reason_not_tested == 1, 1,0),
    not_enrolled = if_else(reason_not_tested == 2, 1,0),
    not_scheduled = if_else(reason_not_tested == 3, 1 ,0),
    medically_exempt = if_else(reason_not_tested == 4, 1,0),
    residential_facility = if_else(reason_not_tested == 5, 1,0),
    did_not_submit = if_else(reason_not_tested == 7, 1,0),
    breach_adult = if_else(ri_status == 1, 1,0),
    breach_student = if_else(ri_status == 2, 1,0),
    irregular_admin = if_else(ri_status == 3, 1,0),
    incorrect_grade_subject = if_else(ri_status == 4, 1,0),
    refused_to_test = if_else(ri_status == 5, 1,0),
    failed_attemptedness = if_else(ri_status == 6, 1,0),
    original_subject = case_when(
      content_area_code == "EN" | content_area_code == "ENG" ~ "ELA",
      content_area_code == "MA" | content_area_code == "MAT" ~ "Math",
      content_area_code == "SCI" ~ "Science",
      content_area_code == "SS" | content_area_code == "SOC" ~ "Social Studies",
      content_area_code == "A1" ~ "Algebra I",
      content_area_code == "A2" ~ "Algebra II",
      content_area_code == "B1" ~ "Biology I",
      content_area_code == "C1" ~ "Chemistry",
      content_area_code == "E1" ~ "English I",
      content_area_code == "E2" ~ "English II",
      content_area_code == "E3" ~ "English III",
      content_area_code == "G1" ~ "Geometry",
      content_area_code == "M1" ~ "Integrated Math I",
      content_area_code == "M2" ~ "Integrated Math II",
      content_area_code == "M3" ~ "Integrated Math III",
      content_area_code == "U1" ~ "US History",
      TRUE ~ NA_character_
    ),
    economically_disadvantaged = if_else(economically_disadvantaged == 'Y', 1, 0)
  ) 

math_eoc <- c("Algebra I", "Algebra II", "Geometry", "Integrated Math I", "Integrated Math II", "Integrated Math III")
english_eoc <- c("English I", "English II")
science_eoc <- c("Biology I", "Chemistry")

# Integrated Math districts for reassigning MSAA subjects
int_math_systems <- total_cdf %>%
  filter(content_area_code %in% c("A1", "M1")) %>%
  count(system, content_area_code) %>%
  group_by(system) %>%
  mutate(temp = max(n)) %>%
  filter(n == temp, content_area_code == "M1") %>%
  magrittr::extract2("system") %>%
  as.integer()


# ================================================ Student Level =====================================
student_level <- bind_rows(total_cdf, msaa) %>% # , alt_science_ss , msaa
  # total_cdf %>% 
  # Only grades 3-12
  filter(grade %in% 3:12) %>% 
  mutate(
    reason_not_tested = if_else(
      (reason_not_tested == 0 | is.na(reason_not_tested)) & !is.na(snt_reg) & is.na(raw_score),
      as.integer(snt_reg),
      as.integer(reason_not_tested)
    ),
    reason_not_tested = if_else(
      (is.na(reason_not_tested) & is.na(raw_score) & is.na(scale_score)) | (reason_not_tested == 0 & is.na(raw_score) & is.na(scale_score)),
      1L,
      reason_not_tested
    ),
    absent = if_else(reason_not_tested == 1, 1,0),
    enrolled = 1,
    tested = if_else(test != "MSAA", 1, tested), # MSAA already has a tested field 
    valid_test = NA_integer_, # initialize valid tests and assign it later
    # economically_disadvantaged = if_else(economically_disadvantaged == 'Y', 1, 0),
    el = if_else(el == 1, 1, 0),
    el_recently_arrived = if_else(el_arrived_year_1 == 1 | el_arrived_year_2 == 1, 1, 0),
    t1234 = if_else(t1234 %in% 1:4, 1, 0), # Transitional given a 0 or 1 instead of 0-4
    special_ed = if_else(special_ed == 1, 1, 0),
    functionally_delayed = if_else(functionally_delayed == 1, 1,0),
    # homebound = homebound == "Y",
    original_performance_level = performance_level,
    subject = original_subject,
    reporting_status = NA
  ) %>%
  # Drop excluded records
  filter(
    !is.na(system),
    # grade != 13 | is.na(grade),
    grade %in% 3:12,
    !(school %in% c(981,982,999) | system >= 990), # 981 is homeschool  residential_facility != 1 | is.na(residential_facility),
    # Drop CTE/Alt/Adult
    !(paste0(system, '/', school) %in% paste0(alt_cte_adult$system, '/', alt_cte_adult$school))#, # 981 is homeschool  residential_facility != 1 | is.na(residential_facility),
  ) %>%
  # Apply testing flag hierarchy
  # Absent (reason_not_tested 1) students have a missing proficiency and are not tested
  # EL Recently Arrived students with missing proficiency are not considered tested
  # EL Recently Arrived students performance level are converted to missing
  # Proficiency modified to missing if refused to test or failed attemptedness
  # Any record with an RI status of 0 or 3 (Irregular Administration) is enrolled and tested, but do not have performance levels
  # Any record with an RI status other than 0 or 3 is neither enrolled nor tested
  mutate(
    enrolled = case_when(
      reason_not_tested == 0 & (breach_adult == 1 | breach_student == 1 | irregular_admin==1 | incorrect_grade_subject == 1 | 
                                  refused_to_test == 1 | failed_attemptedness == 1) ~ 0, # RIs (Need to not have SNT)
      not_enrolled == 1 | not_scheduled == 1 ~ 0, # SNTs
      TRUE ~ 1
    ),
    tested = case_when(
      test == "MSAA" & reporting_status == "DNT" ~ 0,
      reason_not_tested == 0 & (breach_adult == 1 | breach_student ==1 | irregular_admin == 1 | incorrect_grade_subject == 1| 
                                  refused_to_test == 1 | failed_attemptedness == 1) ~ 0, # SNT overrides RI, so need to have SNT of 0 for RI to matter
      absent == 1 | not_enrolled == 1 | not_scheduled == 1 ~ 0,
      # el_recently_arrived == 1L & is.na(original_performance_level) ~ 0,
      TRUE ~ 1
    ),
    # Modify subject for MSAA tests in grades >= 9 (6.8)
    subject = case_when(
      original_subject == "Math" & test == "MSAA" & grade >= 9 & system %in% int_math_systems ~ "Integrated Math I",
      original_subject == "Math" & test == "MSAA" & grade >= 9 & !(system %in% int_math_systems) ~ "Algebra I",
      original_subject == "ELA" & test == "MSAA" & grade >= 9 ~ "English II",
      TRUE ~ subject
    ),
    # Convert subjects per accountability rules
    subject = case_when(
      grade %in% 3:8 & original_subject %in% math_eoc ~ "Math",
      grade %in% 3:8 & original_subject %in% english_eoc ~ "ELA",
      grade %in% 3:8 & original_subject == "US History" ~ "Social Studies",
      TRUE ~ subject
    )
  ) %>% 
  rename(state_student_id = unique_student_id)

# Dedup ===================

dedup <- student_level %>%
  anti_join(alt_cte_adult, by = c("system", "school")) %>%
  mutate(
    # For students with multiple records across test types, MSAA has priority, then EOC, then 3-8
    test_priority = case_when(
      test %in% c("MSAA", "Alt-Science/Social Studies") ~ 3,
      test == "EOC" ~ 2,
      test == "TNReady" ~ 1
    )
  ) %>%
  arrange(state_student_id, subject, -test_priority) %>% 
  group_by(state_student_id, subject) %>%
  # mutate(temp = max(test_priority, na.rm = TRUE)) %>%
  mutate(temp = first(test_priority)) %>% 
  filter(test_priority == temp) %>% # No missing tests so or clause not needed
  select(-test_priority, -temp) %>%
  ungroup() %>%
  # For students with multiple records within the same test, take highest proficiency level
  mutate(
    prof_priority = case_when(
      performance_level %in% c("Below", "Below Basic") ~ 1,
      performance_level %in% c("Approaching", "Basic") ~ 2,
      performance_level %in% c("On Track", "Proficient") ~ 3,
      performance_level %in% c("Mastered", "Advanced") ~ 4
    )
  ) %>%
  arrange(state_student_id, original_subject, -prof_priority) %>% 
  group_by(state_student_id, original_subject, test) %>%
  # mutate(temp = max(prof_priority, na.rm = TRUE)) %>%
  mutate(temp = first(prof_priority)) %>% 
  filter(prof_priority == temp | is.na(temp)) %>% # | (is.na(state_student_id) & test == "Alt-Social Studies")) %>%
  select(-prof_priority, -temp) %>%
  ungroup() %>%
  # For students with multiple records within the same performance level, take highest scale score
  arrange(state_student_id, original_subject, test, performance_level, -scale_score) %>% 
  group_by(state_student_id, original_subject, test, performance_level) %>%
  # mutate(temp = max(scale_score, na.rm = TRUE)) %>%
  mutate(temp = first(scale_score)) %>% 
  filter(scale_score == temp | is.na(temp)) %>%
  select(-temp) %>%
  ungroup() %>%
  # For students with multiple test records with the same proficiency across administrations, take the most recent
  mutate(
    semester_priority = case_when(
      test %in% c("MSAA", "Alt-Social Studies", "Achievement") | (test == "EOC" & semester == "Spring") ~ 2,
      test == "EOC" & semester == "Fall" ~ 1
    )
  ) %>%
  arrange(state_student_id, subject, test, -semester_priority) %>% 
  group_by(state_student_id, subject, test) %>%
  # mutate(temp = max(semester_priority, na.rm = TRUE)) %>%
  mutate(temp = first(semester_priority)) %>% 
  filter(semester_priority == temp | is.na(temp)) %>%
  select(-semester_priority, -temp) %>%
  ungroup() %>%
  # Deduplicate by missing demographic, grade
  # demographic
  mutate(
    demo_priority = case_when(
      reported_race %in% c("American Indian/Alaska Native", "Asian", "Black or African American", "Native Hawaiian/Pac. Islander",
                           "Hispanic/Latino", "White") ~ 2,
      reported_race == 'Unknown' | is.na(reported_race) ~ 1
    )
  ) %>%
  arrange(state_student_id, original_subject, test, performance_level, -demo_priority) %>% 
  group_by(state_student_id, original_subject, test, performance_level) %>% 
  # mutate(temp = max(demo_priority, na.rm = TRUE)) %>%
  mutate(temp = first(demo_priority)) %>% 
  filter(demo_priority == temp | is.na(temp)) %>%
  select(-demo_priority, -temp) %>%
  ungroup() %>% 
  # grade
  mutate(
    grade_priority = case_when(
      !is.na(grade) ~ 2,
      is.na(grade) ~ 1
    )
  ) %>%
  arrange(state_student_id, original_subject, test, performance_level, -grade_priority) %>% 
  group_by(state_student_id, original_subject, test, performance_level) %>% 
  # mutate(temp = max(grade_priority, na.rm = TRUE)) %>%
  mutate(temp = first(grade_priority)) %>% 
  filter(grade_priority == temp | is.na(temp)) %>%
  select(-grade_priority, -temp) %>%
  ungroup() %>% 
  # Valid test if there is a performance level
  mutate(valid_test = as.numeric(!is.na(performance_level)))

# Output ========================
output <- dedup %>%
  select(-system_name, -school_name) %>% 
  # filter(!(original_subject == "Science" & grade %in% c("3", "4"))) %>%
  left_join(school_names, by = c("system", "school")) %>%
  select(system, system_name, school, school_name, test, original_subject, subject, semester,
         original_performance_level, performance_level, scale_score, 
         raw_score, reason_not_tested, ri_status,
         enrolled, tested, valid_test,
         state_student_id, last_name, first_name, grade, gender, reported_race, bhn_group, teacher_of_record_tln,
         functionally_delayed, special_ed, economically_disadvantaged, gifted, migrant, el, t1234, el_recently_arrived,
         enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test, residential_facility) %>%
  mutate(
    el = if_else(state_student_id %in% wida_current$student_id, 1, el) # If student appears in WIDA file, assign el to 1
  ) %>%
  group_by(test, original_subject, grade) %>%
  # Percentiles by grade and original subject for 3-8
  mutate(
    rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(!is.na(scale_score)),
    percentile = if_else(test == "TNReady", round(100 * rank/denom + 1e-10, 1), NA_real_)
  ) %>% 
  ungroup() %>% 
  group_by(test, original_subject) %>%
  # Percentiles by original subject for EOC
  mutate(
    rank = if_else(!is.na(scale_score), rank(scale_score, ties = "max"), NA_integer_),
    denom = sum(!is.na(scale_score)),
    percentile = if_else(test == 'EOC', round(100 * rank/denom + 1e-10, 1), percentile)
  ) %>% 
  ungroup() %>% 
  select(-rank, -denom) %>% 
  arrange(system, school, state_student_id)


# Participation Calcs =======================
# district_names <- school_names %>% 
#   select(system, system_name) %>% 
#   distinct()

district_part <- output %>% 
  group_by(system, system_name) %>%
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested),
    participation_rate = round(sum(tested) / sum(enrolled) * 100 + 1e-10, 1)
  ) %>% 
  arrange(system)

state_part <- output %>% 
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested),
    participation_rate = round(sum(tested) / sum(enrolled) * 100 + 1e-10, 1)
  ) 

write_csv(district_part, "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/district_participation_rate_MSAA_TNReady_EOC_06182021.csv", na = "")


# Add ELPA into file ====================
combined_student_elpa <- bind_rows(
  output,
  wida_current %>% 
    filter(grade %in% 3:12) %>% 
    transmute(
      system, system_name, school, school_name, 
      test, # original_subject, subject, 
      semester = "Spring",
      # original_performance_level, performance_level, 
      scale_score = scale_score_composite, 
      enrolled = participation_denom, tested = participated, valid_test = exit_denom,
      state_student_id = student_id, last_name = student_last_name, 
      first_name = student_first_name, 
      grade, gender, 
      reported_race = case_when(
        Native == 1 ~ 'American Indian/Alaska Native',
        Asian == 1 ~ 'Asian',
        Black == 1 ~ 'Black or African American',
        Hispanic == 1 ~ 'Hispanic/Latino',
        HPI == 1 ~ 'Native Hawaiian/Pac. Islander',
        White == 1 ~ 'White',
        TRUE ~ 'Unknown'
      ), 
      bhn_group = BHN, # teacher_of_record_tln,
      # functionally_delayed, 
      special_ed = SWD, economically_disadvantaged = ED, gifted, migrant, foster, 
      el = EL # t1234, el_recently_arrived,
      # enrolled_50_pct_district, enrolled_50_pct_school, absent, refused_to_test, residential_facility
    ) %>% 
    mutate(
      system_name = if_else(system == 970, "Department Of Children's Services Education Division", system_name)
    )
) %>% 
  mutate(
    system_name = if_else(system == 963, "Tennessee School for Blind", system_name),
    system_name = if_else(system == 370, "Hawkins County", system_name)
  ) %>% 
  arrange(system, school, state_student_id, test)

write_csv(combined_student_elpa, "N:/ORP_accountability/projects/2021_student_level_file/student_level_raw_score_and_WIDA.csv", na = "")

district_part_elpa <- combined_student_elpa %>% 
  group_by(system, system_name) %>%
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested),
    participation_rate = round(sum(tested) / sum(enrolled) * 100 + 1e-10, 1)
  ) %>% 
  arrange(system)

write_csv(district_part_elpa, "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/district_participation_rate_MSAA_TNReady_EOC_WIDA_06242021.csv", na = "")

school_part_elpa <- combined_student_elpa %>% 
  group_by(system, system_name, school, school_name) %>%
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested),
    participation_rate = round(sum(tested) / sum(enrolled) * 100 + 1e-10, 1)
  ) %>% 
  arrange(system, school)

write_csv(school_part_elpa, "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/school_participation_rate_MSAA_TNReady_EOC_WIDA_06242021.csv", na = "")

write_csv(school_part_elpa %>% filter(system == 985), "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/asd_school_participation_rate_MSAA_TNReady_EOC_WIDA_06242021.csv", na = "")


# enrollment =================
student_enrollment <- read_csv("N:/ORP_accountability/data/2021_chronic_absenteeism/absenteeism_pull_Jun18.csv")

student_tested <- output %>% 
  filter(grade %in% 3:10) %>% 
  group_by(
    system, school, state_student_id
  ) %>% 
  summarise(
    n_tests = n()
  )

# Union County check
test_check <- student_enrollment %>% 
  filter(
    grade %in% c(paste0("0", 3:9), "10"),
    begin_date <= as.Date("2021-04-15"),
    is.na(end_date) | end_date > as.Date("2021-05-05")
  ) %>% 
  full_join(
    student_tested %>% rename(student_key = state_student_id),
    by = c("system", "school", "student_key")
  ) %>% 
  mutate(
    test_na = if_else(is.na(n_tests), 0, 1)
  ) %>% 
  group_by(student_key) %>% 
  mutate(temp = max(test_na)) %>% 
  ungroup() %>% 
  filter(test_na == temp) %>% 
  select(-test_na, - temp)

district_part_enr <- test_check %>% 
  group_by(system) %>% 
  summarise(
    n_students = n(),
    n_in_student_file = sum(!is.na(n_tests))
  ) %>% 
  ungroup() %>% 
  mutate(
    pct_in_student_file = round(n_in_student_file / n_students * 100, 1)
  )



# Participation by subgroup ====================
state_demos <- bind_rows(
  output %>% rename(subgroup = reported_race),
  output %>% filter(bhn_group > 0) %>% mutate(subgroup = "Black/Hispanic/Native American"),
  output %>% filter(bhn_group == 0 | is.na(bhn_group)) %>% mutate(subgroup = "Non-Black/Hispanic/Native American"),
  output %>% filter(economically_disadvantaged > 0) %>% mutate(subgroup = "Economically Disadvantaged"),
  output %>% filter(economically_disadvantaged == 0 | is.na(economically_disadvantaged)) %>% mutate(subgroup = "Non-Economically Disadvantaged"),
  output %>% filter(el > 0) %>% mutate(subgroup = "English Learners"),
  output %>% filter(t1234 > 0 & el == 0) %>% mutate(subgroup = "English Learner Transitional 1-4"),
  output %>% filter(t1234 > 0 | el > 0) %>% mutate(subgroup = "English Learners with Transitional 1-4"),
  output %>% filter((t1234 == 0 | is.na(t1234)) & (el == 0 | is.na(el))) %>% mutate(subgroup = "Non-English Learners/Transitional 1-4"),
  output %>% filter(gender == 'F') %>% mutate(subgroup = "Female"),
  output %>% filter(gender == 'M') %>% mutate(subgroup = "Male"),
  output %>% filter(gifted == 1) %>% mutate(subgroup = "Gifted"),
  output %>% filter(migrant == 1) %>% mutate(subgroup = "Migrant"),
  output %>% filter(special_ed > 0) %>% mutate(subgroup = "Students with Disabilities"),
  output %>% filter(special_ed == 0 | is.na(special_ed)) %>% mutate(subgroup = "Non-Students with Disabilities"),
  output %>% mutate(subgroup = "All Students"),
  # sl %>% mutate(subgroup = "All Students", grade = "All Grades"),
  output %>% filter(bhn_group > 0 | economically_disadvantaged > 0 | t1234 > 0 | el > 0 | special_ed > 0) %>% mutate(subgroup = "Super Subgroup")
)

state_participation_subgroup <- state_demos %>% 
  group_by(subgroup, test, original_subject) %>% 
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested),
    participation_rate = round(sum(tested) / sum(enrolled) * 100 + 1e-10, 1)
  ) %>% 
  filter(!is.na(subgroup)) %>%
  arrange(subgroup, test)

write_csv(state_participation_subgroup, "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/state_subgroup_test_subject_participation_rate_MSAA_TNReady_EOC_06292021.csv", na = "")




# priority/CSI participation =============
school_participation <- read_csv("N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/school_participation_rate_MSAA_TNReady_EOC_WIDA_06242021.csv")

priority_csi <- readxl::read_excel("N:/ORP_accountability/data/2020_final_accountability_files/School_Designations_List_20200901.xlsx") %>% 
  filter(designation %in% c("Priority & Comprehensive Support", "Comprehensive Support"))

priority_csi_participation <- school_participation %>% 
  # filter(paste0(system, "/", school) %in% paste0(priority_csi$system, "/", priority_csi$school))
  inner_join(
    priority_csi %>% select(system, school, designation),
    by = c("system", "school")
  )

write_csv(priority_csi_participation, "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/priority_csi_school_participation_rate_MSAA_TNReady_EOC_WIDA_07082021.csv", na = "")

# Subject Part ==============
state_subject_participation_subgroup <- state_demos %>% 
  mutate(
    original_subject = case_when(
      original_subject %in% c("Math", "ELA", "Social Studies", "Science") ~ paste("Grade", grade, original_subject),
      TRUE ~ original_subject
    )
  ) %>% 
  group_by(subgroup, test, original_subject) %>% 
  summarise(
    n_enrolled = sum(enrolled),
    n_tested = sum(tested),
    participation_rate = round(sum(tested) / sum(enrolled) * 100 + 1e-10, 1)
  ) %>% 
  filter(!is.na(subgroup), n_enrolled > 0) %>%
  arrange(subgroup, test, original_subject) %>% 
  rename(subject = original_subject)

write_csv(state_subject_participation_subgroup %>% 
            filter(subject %in% c("Grade 4 ELA", "Grade 5 ELA", "English I", "US History"),
                   test != "MSAA"), 
          "N:/ORP_accountability/projects/Andrew/Data Requests/2021/data/ELA_USHistory_EngI_state_subject_subgroup_participation_rate_07132021.csv", na = "")





