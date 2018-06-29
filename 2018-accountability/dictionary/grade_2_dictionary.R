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
    254, 255, "grade",
    258, 260, "content_area_code",
    331, 331, "attempted",
    332, 332, "gender",
    333, 333, "hispanic",
    334, 334, "native_american",
    335, 335, "asian",
    336, 336, "black",
    337, 337, "hawaiian_pi",
    338, 338, "white",
    339, 339, "reported_race",
    340, 342, "title_1",
    343, 343, "economically_disadvantaged",
    344, 344, "gifted",
    345, 345, "section_504",
    346, 346, "functionally_delayed",
    347, 347, "migrant",
    348, 348, "home_schooled",
    350, 350, "el_arrived_year_1",
    351, 351, "el_arrived_year_2",
    352, 352, "el",
    353, 353, "el_t1234",
    354, 354, "special_ed",
    355, 356, "modified_format",
    357, 357, "homebound",
    358, 358, "enrolled_50_pct_district",
    359, 359, "enrolled_50_pct_school",
    369, 388, "teacher_of_record_tln",
    567, 568, "reason_not_tested",
    569, 570, "ri_status",
    
    702, 704, "raw_score",
    708, 711, "scale_score",
    712, 726, "performance_level",
    727, 729, "scale_score_lb_ci",
    730, 732, "scale_score_ub_ci",
    747, 876, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf(file = "N:/Assessment_Data Returns/Grade 2_Regular and ALT/2017-18/2017-2018 TN 2018 Grade 2 CDF Final Scores - 20180627/2017-2018 TN 2018 Grade 2 CDF Final Scores - 20180627.txt",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    )
)

write_csv(cdf, path = "N:/ORP_accountability/data/2018_cdf/2018_grade_2_cdf.csv")
