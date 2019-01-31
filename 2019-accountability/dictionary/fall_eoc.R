library(tidyverse)

# CDF is a fixed width file - create a tibble with start position, end position, and name for each variable
layout <- tribble(
    ~start, ~end, ~colnames,
    7, 11, "system",
    12, 86, "system_name",
    87, 90, "school",
    91, 165, "school_name",
    166, 200, "last_name",
    201, 235, "first_name",
    236, 236, "middle_initial",
    245, 253, "unique_student_id",
    274, 275, "grade",
    278, 280, "content_area_code",
    377, 377, "test_mode",
    378, 378, "attempted",
    379, 380, "modified_format",
    592, 593, "reason_not_tested",
    594, 595, "ri_status",
    
    702, 704, "raw_score",
    708, 711, "scale_score",
    712, 726, "performance_level",
    727, 729, "scale_score_lb_ci",
    730, 732, "scale_score_ub_ci",
    747, 876, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf(file = "N:/Assessment_Data Returns/TCAP_End-of-Course/2018-19/Fall EOC 2018/2018-2019 TN 2019 Fall EOC CDF Final Scores-20190128/2018-2019 TN 2019 Fall EOC CDF Final Scores-20190128.txt",
    col_types = "iciccccdicccciiiiciic",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    )
)

# Demographic file
demographics <- read_csv("N:/Assessment_Data Returns/TCAP_End-of-Course/2018-19/Demographic Files/fall_eoc_demographics.csv") %>%
# Student IDs should be 7 digits
    filter(str_length(as.character(STUDENT_KEY)) == 7) %>%
# File has duplicates by student across different admin windows, all other fields are the same
    select(-admin_window) %>%
    distinct() %>%
    transmute(
        unique_student_id = STUDENT_KEY,
        system = DISTRICT_ID,
        school = SCHOOL_ID,
        el = ISEL,
        el_t1234 = T1T2,
        reported_race = case_when(
            ETHNICITY == "H" ~ "Hispanic/Latino",
            ISBLACK == 1 ~ "Black or African American",
            ISAMERICANINDIAN == 1 ~ "American Indian/Alaska Native",
            ISPACIFICISLANDER == 1 ~ "Native Hawaiian/Pac. Islander",
            ISASIAN == 1 ~ "Asian",
            ISWHITE == 1 ~ "White",
            TRUE ~ "Unknown"
        ),
        el_arrived_year_1 = ELRECENTLYARRIVEDYEARONE,
        el_arrived_year_2 = ELRECENTLYARRIVEDYEARTWO,
        gender = GENDER,
        migrant = ISMIGRANT,
        title_1 = TITLE1,
        special_ed = SPECIALEDUCATION,
        functionally_delayed = ISFUNCTIONALLYDELAYED,
        economically_disadvantaged = case_when(
            CODEAB == 1 ~ 1,
            CODEAB == 2 ~ 0
        ),
        enrolled_50_pct_district = DISTRICT50PERCENT,
        enrolled_50_pct_school = SCHOOL50PERCENT
    )

cdf_with_demographics <- left_join(cdf, demographics, by = c("unique_student_id", "system", "school"))

write_csv(cdf_with_demographics, path = "N:/ORP_accountability/data/2019_cdf/2019_fall_eoc_cdf.csv")
