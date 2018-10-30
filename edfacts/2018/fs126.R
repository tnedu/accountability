library(readxl)
library(tidyverse)

# EOC CDFs
fall_cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_fall_cdf.csv") %>%
    mutate(
        test = "EOC",
        semester = "Fall",
        raw_score = as.integer(raw_score)
    )

# EL for Spring EOC CDF
el_hs <- read_csv("N:/ORP_accountability/projects/2018_student_level_file/el_recently_arrived.csv") %>%
    transmute(unique_student_id = student_key,
        el = if_else(isel == 1, "Y", "N"),
        el_arrived_year_1 = if_else(ELRECENTLYARRIVEDYEARONE == 1, "Y", "N"),
        el_arrived_year_2 = if_else(ELRECENTLYARRIVEDYEARTWO == 1, "Y", "N")
    ) %>%
    distinct()

spring_cdf <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_spring_cdf.csv") %>%
    mutate(
        test = "EOC",
        semester = "Spring"
    ) %>%
    select(-el, -el_arrived_year_1, -el_arrived_year_2) %>%
    left_join(el_hs, by = "unique_student_id") %>%
    mutate_at(c("el", "el_arrived_year_1", "el_arrived_year_2"), ~ if_else(is.na(.), "N", .))

# 3-8 CDFs
el_38 <- read_csv("N:/ORP_accountability/data/2018_tdoe_provided_files/EL status and variables 2018.csv") %>%
    transmute(unique_student_id = `Student Key`,
        el = if_else(`IS EL` == 1, "Y", "N"),
        el_arrived_year_1 = if_else(`Recently Arrived Year 1` == 1, "Y", "N"),
        el_arrived_year_2 = if_else(`Recently Arrived Year 2` == 1, "Y", "N"),
        el_t1234 = `T1T2  (T1T4)`
    ) %>%
    distinct()

cdf_38 <- read_csv("N:/ORP_accountability/data/2018_cdf/2018_3_8_cdf.csv") %>%
    select(-el, -el_arrived_year_1, -el_arrived_year_2, -el_t1234) %>%
    left_join(el_38, by = "unique_student_id") %>%
    mutate_at(c("el", "el_arrived_year_1", "el_arrived_year_2"), ~ if_else(is.na(.), "N", .)) %>%
    mutate(
        el_t1234 = if_else(is.na(el_t1234), 0L, el_t1234),
        test = "TNReady",
        semester = "Spring",
        teacher_of_record_tln = as.integer(teacher_of_record_tln)
    )

cdf <- bind_rows(fall_cdf, spring_cdf, cdf_38) %>%
    mutate(
        performance_level = if_else(performance_level == "On track", "On Track", performance_level),
        original_subject = case_when(
            content_area_code == "ENG" ~ "ELA",
            content_area_code == "MAT" ~ "Math",
            content_area_code == "SCI" ~ "Science",
            content_area_code == "SOC" ~ "Social Studies",
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
            content_area_code == "U1" ~ "US History"
        )
    ) %>%
    mutate_at(c("system", "school"), as.integer) %>%
    filter(
        reason_not_tested != 1,
        el_t1234 %in% 1:4,
        content_area_code %in% c("ENG", "MAT", "SCI", "E2", "A1", "M1", "B1"),
        !is.na(scale_score)
    ) %>%
    mutate(
        count = 1,
        subject = case_when(
            content_area_code %in% c("MAT", "A1", "M1") ~ "M",
            content_area_code %in% c("ENG", "E2") ~ "RLA",
            content_area_code %in% c("SCI", "B1") ~ "S"
        ),
        proficiency = case_when(
            performance_level %in% c("Below Basic", "Below", "Basic", "Approaching") ~ "NOTPROFICIENT",
            performance_level %in% c("Proficient", "On Track", "Advanced", "Mastered") ~ "PROFICIENT"
        ),
        elb = case_when(
            el_t1234 == 1 ~ "1YEAR",
            el_t1234 == 2 ~ "2YEAR",
            el_t1234 == 3 ~ "3YEAR",
            el_t1234 == 4 ~ "4YEAR"
        )
    )

title_3_districts <- read_excel("N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/TITLEIII_DISTRICTS_1718_9-13-17.xlsx") %>%
    filter(titleiii == "Y") %>%
    select(system = leaid)

counts_system <- read_excel("N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/FS126_1718_T1T2T3T4_COUNTS_10-25-18.xlsx",
        skip = 2, sheet = "LEA") %>%
    filter(!is.na(COUNT)) %>%
    select(system = LEAID, elb = ELB, count = COUNT)

prof_system <- cdf %>%
    group_by(system, subject, proficiency, elb) %>%
    summarise_at("count", sum) %>%
    full_join(
        cross_df(list(system = unique(cdf$system), subject = unique(cdf$subject), elb = unique(cdf$elb), proficiency = unique(cdf$proficiency))),
        by = c("system", "subject", "elb", "proficiency")
    ) %>%
    mutate_at("count", function(x) if_else(is.na(x), 0, x)) %>%
    ungroup() %>%
    arrange(system, subject, elb, proficiency) %>%
    inner_join(title_3_districts, by = "system")

prof_system_swd <- cdf %>%
    filter(special_ed == "Y") %>%
    group_by(system, subject, proficiency, elb) %>%
    summarise_at("count", sum) %>%
    full_join(
        cross_df(list(system = unique(cdf$system), subject = unique(cdf$subject), elb = unique(cdf$elb), proficiency = unique(cdf$proficiency))),
        by = c("system", "subject", "elb", "proficiency")
    ) %>%
    mutate_at("count", function(x) if_else(is.na(x), 0, x)) %>%
    ungroup() %>%
    arrange(system, subject, proficiency) %>%
    inner_join(title_3_districts, by = "system") %>%
    mutate(disability_status = "WDIS")

fs126_system <- bind_rows(counts_system, prof_system, prof_system_swd) %>%
    transmute(
        first = 1:nrow(.),
        state_code = 47,
        state_agency_number = "01",
        lea_identifier = sprintf("%05d", system),
        filler1 = "",
        table_name = "LEPFORSTU",
        disability_status,
        elb,
        subject,
        proficiency,
        total_indicator = "N",
        explanation = "",
        student_count = count
    )

write_csv(fs126_system, "N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/TNLEALEPFORSTUv1.csv", na = "", col_names = FALSE)

paste("LEA LEP FORMER STUDENTS", nrow(fs126_system), "TNLEALEPFORSTUv1.csv", "TNLEALEPFORSTUv1", "2017-2018", "", sep = ",")

counts_state <- read_excel("N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/FS126_1718_T1T2T3T4_COUNTS_10-25-18.xlsx",
        skip = 2, sheet = "SEA") %>%
    filter(!is.na(Count), ELB != "TOTAL") %>%
    select(elb = ELB, count = Count)

prof_state <- cdf %>%
    group_by(subject, proficiency, elb) %>%
    summarise_at("count", sum) %>%
    ungroup()

prof_state_swd <- cdf %>%
    filter(special_ed == "Y") %>%
    group_by(subject, proficiency, elb) %>%
    summarise_at("count", sum) %>%
    ungroup() %>%
    mutate(disability_status = "WDIS")

fs126_state <- bind_rows(counts_state, prof_state, prof_state_swd) %>%
    transmute(
        first = 1:nrow(.),
        state_code = 47,
        state_agency_number = "01",
        lea_identifier = "",
        filler1 = "",
        table_name = "LEPFORSTU",
        disability_status,
        elb,
        subject,
        proficiency,
        total_indicator = "N",
        explanation = "",
        student_count = count
    )

write_csv(fs126_state, "N:/ORP_accountability/projects/Alex/accountability/edfacts/2018/TNSEALEPFORSTUv1.csv", na = "", col_names = FALSE)

paste("SEA LEP FORMER STUDENTS", nrow(fs126_state), "TNSEALEPFORSTUv1.csv", "TNSEALEPFORSTUv1", "2017-2018", "", sep = ",")
