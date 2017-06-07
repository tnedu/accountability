library(tidyverse)

# CDF is a fixed width file - create a tibble with start position, end position, and name for each variable
layout <- tribble(
    ~start, ~end, ~colnames,
    3, 11, "unique_student_id",
    21, 45, "last_name",
    46, 60, "first_name",
    61, 61, "middle_initial",
    70, 71, "grade",
    72, 72, "ethnic_origin",
    73, 73, "native_american",
    74, 74, "asian",
    75, 75, "black",
    76, 76, "hawaiian_pi",
    77, 77, "white",
    78, 78, "reported_race",
    79, 79, "gender",
    80, 80, "instructional_availability",
    82, 82, "greater_than_60_pct",
    83, 83, "title_1",
    84, 84, "economically_disadvantaged",
    87, 87, "functionally_delayed",
    88, 88, "migrant",
    90, 90, "el_excluded",
    91, 91, "el",
    92, 92, "el_t1_t2",
    93, 93, "special_ed",
    95, 95, "homebound",

    # Part 1
    98, 102, "system",
    103, 152, "system_name",
    153, 156, "school",
    157, 206, "school_name",
    328, 329, "content_area_code",
    339, 339, "modified_format",
    360, 360, "ri_status",

    # Part 2
    # 362, 366, "system",
    # 367, 416, "system_name",
    # 417, 420, "school",
    # 421, 470, "school_name",
    # 592, 593, "content_area_code",
    # 603, 603, "modified_format",
    # 624, 624, "ri_status",
    
    625, 625, "invalid_score",
    713, 713, "performance_level",
    714, 716, "scale_score",
    717, 719, "scale_score_lb_ci",
    720, 722, "scale_score_ub_ci"
)

# Read in cdf according to above layout
cdf <- read_fwf(file = "K:/ORP_accountability/data/2017_cdf/2016 EOC Fall CDF_Public20170526.txt",
    col_positions = fwf_positions(
        start = layout$start,
        end = layout$end,
        col_names = layout$colnames
    )
)

# Export cdf
write_csv(cdf, path = "K:/ORP_accountability/projects/2017_dictionary_coding/fall_eoc_cdf.csv", na = "")
