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
    295, 295, "attempted",
    296, 296, "gender",
    297, 297, "hispanic",
    298, 298, "native_american",
    299, 299, "asian",
    300, 300, "black",
    301, 301, "hawaiian_pi",
    302, 302, "white",
    303, 303, "reported_race",
    304, 306, "title_1",
    307, 307, "economically_disadvantaged",
    308, 308, "gifted",
    309, 309, "section_504",
    310, 310, "functionally_delayed",
    311, 311, "migrant",
    312, 312, "home_schooled",
    314, 314, "el_arrived_year_1",
    315, 315, "el_arrived_year_2",
    316, 316, "el",
    317, 317, "el_t1234",
    318, 318, "special_ed",
    319, 320, "modified_format",
    321, 321, "homebound",
    322, 322, "enrolled_50_pct_district",
    323, 323, "enrolled_50_pct_school",
    333, 341, "teacher_of_record_tln",
    509, 510, "reason_not_tested",
    511, 512, "ri_status",

    714, 716, "raw_score",
    720, 723, "scale_score",
    724, 738, "performance_level",
    739, 741, "scale_score_lb_ci",
    742, 744, "scale_score_ub_ci",
    759, 883, "item_response_array"
)

# Read in cdf according to layout
cdf <- read_fwf(file = "N:/Assessment_Data Returns/TCAP_End-of-Course/2017-18/Fall/2017-2018 TN Fall EOC CDF - 20180117.txt",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    )
)

write_csv(cdf, path = "N:/ORP_accountability/data/2018_cdf/2018_fall_cdf.csv")
