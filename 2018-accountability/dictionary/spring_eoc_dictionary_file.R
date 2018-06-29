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
    305, 305, "attempted",
    306, 306, "gender",
    307, 307, "hispanic",
    308, 308, "native_american",
    309, 309, "asian",
    310, 310, "black",
    311, 311, "hawaiian_pi",
    312, 312, "white",
    313, 313, "reported_race",
    314, 316, "title_1",
    317, 317, "economically_disadvantaged",
    318, 318, "gifted",
    319, 319, "section_504",
    320, 320, "functionally_delayed",
    321, 321, "migrant",
    322, 322, "home_schooled",
    324, 324, "el_arrived_year_1",
    325, 325, "el_arrived_year_2",
    326, 326, "el",
    327, 327, "el_t1234",
    328, 328, "special_ed",
    329, 330, "modified_format",
    331, 331, "homebound",
    332, 332, "enrolled_50_pct_district",
    333, 333, "enrolled_50_pct_school",
    343, 362, "teacher_of_record_tln",
    541, 542, "reason_not_tested",
    543, 544, "ri_status",
    
    676, 678, "raw_score",
    682, 685, "scale_score",
    686, 700, "performance_level",
    701, 703, "scale_score_lb_ci",
    704, 706, "scale_score_ub_ci",
    721, 845, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf(file = "N:/Assessment_Data Returns/TCAP_End-of-Course/2017-18/Spring/2017-2018 TN Spring EOC CDF Final Scores- 20180601 .txt",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    )
)

write_csv(cdf, path = "N:/ORP_accountability/data/2018_cdf/2018_spring_cdf.csv")
