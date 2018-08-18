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
    256, 257, "grade",
    258, 260, "content_area_code",
    332, 332, "attempted",
    333, 333, "gender",
    334, 334, "hispanic",
    335, 335, "native_american",
    336, 336, "asian",
    337, 337, "black",
    338, 338, "hawaiian_pi",
    339, 339, "white",
    340, 340, "reported_race",
    341, 343, "title_1",
    344, 344, "economically_disadvantaged",
    345, 345, "gifted",
    346, 346, "section_504",
    347, 347, "functionally_delayed",
    348, 348, "migrant",
    349, 349, "home_schooled",
    351, 351, "el_arrived_year_1",
    352, 352, "el_arrived_year_2",
    353, 353, "el",
    354, 354, "el_t1234",
    355, 355, "special_ed",
    356, 357, "modified_format",
    358, 358, "homebound",
    359, 359, "enrolled_50_pct_district",
    360, 360, "enrolled_50_pct_school",
    370, 389, "teacher_of_record_tln",
    568, 569, "reason_not_tested",
    570, 571, "ri_status",

    703, 705, "raw_score",
    709, 712, "scale_score",
    713, 727, "performance_level",
    728, 730, "scale_score_lb_ci",
    731, 733, "scale_score_ub_ci",
    748, 877, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf(file = "N:/Assessment_Data Returns/TCAP_Grades 3-8/2017-18/CDF/2017-2018 TN 2018 Spring 3-8 CDF Final Scores - 20180814/2017-2018 TN 2018 Spring 3-8 CDF Final Scores - 20180814 .txt",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    )
)

write_csv(cdf, path = "N:/ORP_accountability/data/2018_cdf/2018_3_8_cdf.csv")
