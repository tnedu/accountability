library(acct)
library(tidyverse)

student <- read_csv("N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_student_level.csv") %>%
    filter(included_in_cohort == "Y") %>%
    mutate(
        all = TRUE,
        native = race_ethnicity == "I",
        asian = race_ethnicity == "A",
        black = race_ethnicity == "B",
        bhn = race_ethnicity %in% c("B", "H", "I"),
        ed = ed == "Y",
        el = el == "Y",
        hispanic = race_ethnicity == "H",
        hpi = race_ethnicity == "P",
        swd = swd == "Y",
        super = bhn | ed | el | swd,
        white = race_ethnicity == "W"
    )

collapse <- function(s, ...) {
    s_quo <- enquo(s)
    
    student %>%
        filter(!!s_quo) %>%
        group_by(...) %>%
        summarise(
            n_count = n(),
            n_ready_grad = sum(ready_graduate == "Y", na.rm = TRUE),
            pct_ready_grad = round5(100 * mean(ready_graduate == "Y", na.rm = TRUE), 1)
        ) %>%
        mutate(subgroup = deparse(s_quo)) %>%
        ungroup()
}

district <- map_dfr(
    .x = list(quo(all), quo(native), quo(asian), quo(black), quo(bhn), quo(ed), quo(el), quo(hispanic), quo(hpi), quo(swd), quo(super), quo(white)),
    .f = ~ collapse(!!., district_no)
) %>%
    transmute(
        system = district_no,
        subgroup = case_when(
            subgroup == "~all" ~ "All Students",
            subgroup == "~asian" ~ "Asian",
            subgroup == "~bhn" ~ "Black/Hispanic/Native American",
            subgroup == "~black" ~ "Black or African American",
            subgroup == "~ed" ~ "Economically Disadvantaged",
            subgroup == "~el" ~ "English Learners",
            subgroup == "~hispanic" ~ "Hispanic",
            subgroup == "~hpi" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~native" ~ "American Indian or Alaska Native",
            subgroup == "~super" ~ "Super Subgroup",
            subgroup == "~swd" ~ "Students with Disabilities",
            subgroup == "~white" ~ "White"
        ),
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(district, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_district.csv", na = "")
    
school <- map_dfr(
    .x = list(quo(all), quo(native), quo(asian), quo(black), quo(bhn), quo(ed), quo(el), quo(hispanic), quo(hpi), quo(swd), quo(super), quo(white)),
    .f = ~ collapse(!!., district_no, school_no)
) %>%
    transmute(
        system = district_no,
        school = school_no,
        subgroup = case_when(
            subgroup == "~all" ~ "All Students",
            subgroup == "~asian" ~ "Asian",
            subgroup == "~bhn" ~ "Black/Hispanic/Native American",
            subgroup == "~black" ~ "Black or African American",
            subgroup == "~ed" ~ "Economically Disadvantaged",
            subgroup == "~el" ~ "English Learners",
            subgroup == "~hispanic" ~ "Hispanic",
            subgroup == "~hpi" ~ "Native Hawaiian or Other Pacific Islander",
            subgroup == "~native" ~ "American Indian or Alaska Native",
            subgroup == "~super" ~ "Super Subgroup",
            subgroup == "~swd" ~ "Students with Disabilities",
            subgroup == "~white" ~ "White"
        ),
        n_count,
        n_ready_grad,
        pct_ready_grad
    )

write_csv(school, "N:/ORP_accountability/projects/2019_ready_graduate/Data/ready_graduate_school.csv", na = "")
