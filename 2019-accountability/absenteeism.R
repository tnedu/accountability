options(java.parameters = "-Xmx16G")

library(acct)
library(tidyverse)
library(janitor)
library(lubridate)
library(RJDBC)

# con <- dbConnect(
#     JDBC("oracle.jdbc.OracleDriver", classPath = Sys.getenv("JAVA_PATH")),
#     Sys.getenv("CONNECTION_STRING"),
#     Sys.getenv("SCHEMA_NAME"),
#     Sys.getenv("DB_PASSWORD")
# )

con <- dbConnect(
    JDBC("oracle.jdbc.OracleDriver", classPath="C:/Users/CA20593/Downloads/ojdbc6.jar"),
    readRegistry("Environment", hive = "HCU")$EIS_MGR_CXN_STR[1],
    "EIS_MGR",
    readRegistry("Environment", hive = "HCU")$EIS_MGR_PWD[1]
)

# Pull attendance from database
attendance <- dbGetQuery(con,
    "SELECT
    TO_CHAR(ISP.SCHOOL_YEAR) || '-' || TO_CHAR(ISP.SCHOOL_YEAR+1) AS SCHOOL_YEAR,
    S.DISTRICT_NO,
    S.SCHOOL_NO,
    ISP.INSTRUCTIONAL_PROGRAM_NUM,
    ISP.ISP_ID,
    ISP.STUDENT_KEY,
    ISP.FIRST_NAME,
    ISP.MIDDLE_NAME,
    ISP.LAST_NAME,
    ISP.BEGIN_DATE,
    ISP.END_DATE,
    ISP.ENROLLMENT_REASON,
    ISP.WITHDRAWAL_REASON,
    ISP.TYPE_OF_SERVICE,
    STU.DATE_OF_BIRTH,
    STU.ETHNIC_ORIGIN,
    STU.GENDER,
    S.SCHOOL_BU_ID,
    D.DISTRICT_BU_ID,
    NVL (IG.ASSIGNMENT, ' ') AS GRADE,
    DECODE(STU.ETHNICITY,'H','Hispanic','Non Hispanic') ETHNICITY,
    STU.RACE_I,
    STU.RACE_A,
    STU.RACE_P,
    STU.RACE_B,
    STU.RACE_W,
    (CASE WHEN TRUANTS.CNT_UNEXCUSED = 0 THEN NULL ELSE TRUANTS.CNT_UNEXCUSED END) AS CNT_UNEXCUSED,
    (CASE WHEN TRUANTS.CNT_UNEXCUSED_TRANS = 0 THEN NULL ELSE TRUANTS.CNT_UNEXCUSED_TRANS END) AS CNT_UNEXCUSED_TRANS,
    (CASE WHEN TRUANTS.CNT_EXCUSED = 0 THEN NULL ELSE TRUANTS.CNT_EXCUSED END) AS CNT_EXCUSED,
    (CASE WHEN TRUANTS.CNT_EXCUSED_TRANS = 0 THEN NULL ELSE TRUANTS.CNT_EXCUSED_TRANS END) AS CNT_EXCUSED_TRANS,
    TRUANTS.CNT_TOTAL,
    (SELECT COUNT(SCAL.ID_DATE) AS ISP_DAYS_1
      FROM EIS_MGR.SCAL_ID_DAYS SCAL
      WHERE SCAL.SCHOOL_BU_ID = ISP.SCHOOL_BU_ID
        AND SCAL.SCHOOL_YEAR = ISP.SCHOOL_YEAR
        AND SCAL.INSTRUCTIONAL_PROGRAM_NUM = ISP.INSTRUCTIONAL_PROGRAM_NUM
        AND SCAL.ID_DATE >= ISP.BEGIN_DATE
        AND SCAL.ID_DATE <= NVL(ISP.END_DATE,
          (SELECT MAX(SCAL_ID_DAYS.ID_DATE)
              FROM EIS_MGR.SCAL_ID_DAYS
              WHERE SCAL_ID_DAYS.SCHOOL_BU_ID = ISP.SCHOOL_BU_ID
              AND SCAL.SCHOOL_YEAR = ISP.SCHOOL_YEAR
              AND SCAL.INSTRUCTIONAL_PROGRAM_NUM = ISP.INSTRUCTIONAL_PROGRAM_NUM)
          )
    ) AS ISP_DAYS
    FROM ISP
    JOIN EIS_MGR.STUDENT_NEW STU ON STU.STUDENT_KEY = ISP.STUDENT_KEY
    JOIN EIS_MGR.SCHOOL S ON ISP.SCHOOL_BU_ID = S.SCHOOL_BU_ID
    LEFT JOIN EIS_MGR.INSTRUCTIONAL_GRADE IG ON ISP.ISP_ID = IG.ISP_ID
    JOIN EIS_MGR.DISTRICT D ON S.DISTRICT_NO = D.DISTRICT_NO
    LEFT JOIN (
      SELECT ISP.ISP_ID,
      COUNT(CASE WHEN ATTENDANCE_TYPE = 'U' THEN 1 END) AS CNT_UNEXCUSED,
      COUNT(CASE WHEN ATTENDANCE_TYPE = 'X' THEN 1 END) AS CNT_UNEXCUSED_TRANS,
      COUNT(CASE WHEN ATTENDANCE_TYPE = 'A' THEN 1 END) AS CNT_EXCUSED,
      COUNT(CASE WHEN ATTENDANCE_TYPE = 'T' THEN 1 END) AS CNT_EXCUSED_TRANS,
      COUNT(CASE WHEN ATTENDANCE_TYPE <> 'P' THEN 1 END) AS CNT_TOTAL
      FROM EIS_MGR.INSTRUCTIONAL_SERVICE_PERIOD ISP
      JOIN EIS_MGR.STUDENT_ABSENCES SA  ON ISP.ISP_ID = SA.ISP_ID
      WHERE ISP.SCHOOL_YEAR = EXTRACT(YEAR FROM SYSDATE) - 1
      AND SA.ATTENDANCE_DATE >= ISP.BEGIN_DATE
      AND (SA.ATTENDANCE_DATE < ISP.END_DATE OR ISP.END_DATE IS NULL)
      GROUP BY ISP.ISP_ID
      ) TRUANTS ON ISP.ISP_ID = TRUANTS.ISP_ID
    WHERE ISP.SCHOOL_YEAR = EXTRACT(YEAR FROM SYSDATE) - 1
    AND IG.ASSIGNMENT NOT IN ('P3', 'P4')
    ORDER BY S.DISTRICT_NO, S.SCHOOL_NO, ISP.LAST_NAME, ISP.FIRST_NAME") %>%
    as.tbl() %>%
    clean_names() %>%
    mutate_at(
        .vars = c("instructional_program_num", "district_no", "school_no", "student_key", "isp_days", "cnt_total"),
        .f = as.numeric
    ) %>%
    select(
        instructional_program_num, system = district_no, school = school_no, grade, student_key,
        first_name, middle_name, last_name, begin_date, end_date, isp_days, cnt_total
    )

# Export pull so we don't have to run it each time
# write_csv(attendance, "N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\absenteeism_pull.csv")

# attendance <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/absenteeism_pull.csv")

# Pull instructional calendar days from database
instructional_days <- dbGetQuery(con,
    "SELECT
    SID.SCHOOL_BU_ID,
    SID.SCHOOL_YEAR AS YEAR,
    D.DISTRICT_NAME AS SYSTEM_NAME,
    D.DISTRICT_NO AS SYSTEM,
    S.SCHOOL_NAME,
    S.SCHOOL_NO AS SCHOOL,
    COUNT(DISTINCT SID.ID_DATE) AS INSTRUCTIONAL_DAYS
    FROM EIS_MGR.SCAL_ID_DAYS SID
    JOIN EIS_MGR.SCHOOL S ON SID.SCHOOL_BU_ID = S.SCHOOL_BU_ID
    JOIN EIS_MGR.DISTRICT D ON S.DISTRICT_NO = D.DISTRICT_NO
    WHERE SCHOOL_YEAR = EXTRACT(YEAR FROM SYSDATE) - 1
        AND SID.ID_DATE <= SYSDATE
    GROUP BY SID.SCHOOL_BU_ID, SID.SCHOOL_YEAR, D.DISTRICT_NAME, D.DISTRICT_NO, S.SCHOOL_NAME, S.SCHOOL_NO
    ORDER BY SID.SCHOOL_BU_ID"
) %>%
    as.tbl() %>%
    clean_names()

# write_csv(instructional_days, "N:\\ORP_accountability\\data\\2019_chronic_absenteeism\\instructional_days.csv")

# instructional_days <- read_csv("N:/ORP_accountability/data/2019_chronic_absenteeism/instructional_days.csv")

# Demographic file
demographics <- read_csv("N:/TNReady/2018-19/spring/demographics/spring_2019_assessment_demographics_combined_pull_20190610.csv") %>%
  # Student IDs should be 7 digits
  filter(str_length(student_key) == 7) %>%
  transmute(
    student_key,
    system = district_id,
    school = school_id,
    EL = isel == 1 | t1t2 %in% 1:4,
    reported_race = case_when(
      ethnicity == "H" ~ "Hispanic/Latino",
      isblack == 1 ~ "Black or African American",
      isamericanindian == 1 ~ "American Indian/Alaska Native",
      ispacificislander == 1 ~ "Native Hawaiian/Pac. Islander",
      isasian == 1 ~ "Asian",
      iswhite == 1 ~ "White",
      TRUE ~ "Unknown"
    ),
    BHN = reported_race %in% c("Black or African American", "Hispanic/Latino", "American Indian/Alaska Native"),
    Black = reported_race == "Black or African American",
    Hispanic = reported_race == "Hispanic/Latino",
    Native = reported_race == "American Indian/Alaska Native",
    HPI = reported_race == "Native Hawaiian/Pac. Islander",
    Asian = reported_race == "Asian",
    White = reported_race == "White",
    SWD = specialeducation == 1,
    ED = codeab == 1
  ) %>%
  select(-reported_race)

# Absenteeism
absenteeism <- attendance %>%
    filter(grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")) %>%
    mutate(count_total = if_else(is.na(cnt_total), 0, cnt_total)) %>%
# For students with same system, school, student ID, enrollment dates,
# take maximum instructional program days
    group_by(system, school, student_key, grade, begin_date, end_date) %>%
    mutate(count = n(), temp = max(isp_days)) %>%
    filter(count == 1 | isp_days == temp) %>%
# For students with same system, school, student ID, enrollment dates, instructional program days,
# take maximum number of absences
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days) %>%
    mutate(count = n(), temp = max(count_total)) %>%
    filter(count == 1 | count_total == temp) %>%
# For students with same system, school, student ID, enrollment dates, instructional program days, absences,
# take maximum instructional program number
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total) %>%
    mutate(count = n(), temp = max(instructional_program_num)) %>%
    filter(count == 1 | instructional_program_num == temp) %>%
# Drop duplicates on system, school, student ID, enrollment dates, instructional program days, absences, instructional program
    group_by(system, school, student_key, grade, begin_date, end_date, isp_days, count_total, instructional_program_num) %>%
    mutate(count = 1, temp = cumsum(count)) %>%
    filter(temp == 1) %>%
# Collapse multiple enrollments at the same school
    rename(n_absences = count_total) %>%
    group_by(system, school, grade, student_key) %>%
    summarise(
        first_name = first(first_name),
        middle_name = first(middle_name),
        last_name = first(last_name),
        n_absences = sum(n_absences, na.rm = TRUE), 
        isp_days = sum(isp_days, na.rm = TRUE)
    ) %>%
    ungroup() %>%
# Merge on instructional calendar file
    inner_join(instructional_days, by = c("system", "school")) %>%
    mutate(
        grade = case_when(
            grade %in% c("K", "01", "02", "03", "04", "05", "06", "07", "08") ~ "K through 8th",
            grade %in% c("09", "10", "11", "12") ~ "9th through 12th"
        ),
        chronic_absence = as.integer(n_absences/isp_days >= 0.1),
        All = TRUE
    ) %>%
  left_join(demographics, by = c("system", "school", "student_key"))

# Function to calculate chronic absenteeism by student group and school/district/state level
groups <- list(quo(All), quo(BHN), quo(ED), quo(SWD), quo(EL), quo(Black), quo(Hispanic), quo(Native), quo(HPI), quo(Asian), quo(White))

collapse <- function(df, g, ...) {
  
  g_quo <- enquo(g)
  
  all_grades <- df %>%
    filter(!!g_quo) %>%
    group_by(...) %>%
    summarise(
      n_students = n(),
      n_chronically_absent = sum(chronic_absence, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(subgroup = deparse(g_quo), grade = "All Grades")
  
  by_grade <- df %>%
    filter(!!g_quo) %>%
    group_by(grade, ...) %>%
    summarise(
      n_students = n(),
      n_chronically_absent = sum(chronic_absence, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(subgroup = deparse(g_quo), grade = as.character(grade))
  
  bind_rows(all_grades, by_grade)
  
}

# State
students_state <- absenteeism %>%
# Add up absences and ISP days across every enrollment
  group_by(student_key, grade) %>%
  summarise(
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    instructional_days = max(instructional_days),
    All = any(All),
    BHN = any(BHN, na.rm = TRUE),
    ED = any(ED, na.rm = TRUE),
    SWD = any(SWD, na.rm = TRUE),
    EL = any(EL, na.rm = TRUE),
    Black = any(Black, na.rm = TRUE),
    Hispanic = any(Hispanic, na.rm = TRUE),
    Native = any(Black, na.rm = TRUE),
    HPI = any(HPI, na.rm = TRUE),
    Asian = any(Asian, na.rm = TRUE),
    White = any(White, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(isp_days >= 45) %>%
  mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1))

state <- groups %>%
  map_dfr(~ collapse(students_state, !!.)) %>%
  transmute(
    system = 0,
    system_name = "State of Tennessee",
    subgroup = case_when(
      subgroup == "~All" ~ "All Students",
      subgroup == "~BHN" ~ "Black/Hispanic/Native American",
      subgroup == "~ED" ~ "Economically Disadvantaged",
      subgroup == "~SWD" ~ "Students with Disabilities",
      subgroup == "~EL" ~ "English Learners with Transitional 1-4",
      subgroup == "~Black" ~ "Black or African American",
      subgroup == "~Hispanic" ~ "Hispanic",
      subgroup == "~Native" ~ "American Indian or Alaska Native",
      subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
      subgroup == "~Asian" ~ "Asian",
      subgroup == "~White" ~ "White"
    ),
    grade_band = grade,
    n_students,
    n_chronically_absent,
    pct_chronically_absent = round5(100 * n_chronically_absent/n_students, 1)
  ) %>%
  arrange(subgroup, grade_band)

# District
students_district <- absenteeism %>%
# Add up absences and ISP days by student and district
  group_by(student_key, system, system_name, grade) %>%
  summarise(
    n_absences = sum(n_absences, na.rm = TRUE),
    isp_days = sum(isp_days, na.rm = TRUE),
    instructional_days = max(instructional_days),
    All = any(All),
    BHN = any(BHN, na.rm = TRUE),
    ED = any(ED, na.rm = TRUE),
    SWD = any(SWD, na.rm = TRUE),
    EL = any(EL, na.rm = TRUE),
    Black = any(Black, na.rm = TRUE),
    Hispanic = any(Hispanic, na.rm = TRUE),
    Native = any(Black, na.rm = TRUE),
    HPI = any(HPI, na.rm = TRUE),
    Asian = any(Asian, na.rm = TRUE),
    White = any(White, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(isp_days/instructional_days >= 0.5) %>%
  mutate(chronic_absence = as.integer(n_absences/isp_days >= 0.1))

district <- groups %>%
  map_dfr(~ collapse(students_district, !!., system, system_name)) %>%
  transmute(
    system,
    system_name,
    subgroup = case_when(
      subgroup == "~All" ~ "All Students",
      subgroup == "~BHN" ~ "Black/Hispanic/Native American",
      subgroup == "~ED" ~ "Economically Disadvantaged",
      subgroup == "~SWD" ~ "Students with Disabilities",
      subgroup == "~EL" ~ "English Learners with Transitional 1-4",
      subgroup == "~Black" ~ "Black or African American",
      subgroup == "~Hispanic" ~ "Hispanic",
      subgroup == "~Native" ~ "American Indian or Alaska Native",
      subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
      subgroup == "~Asian" ~ "Asian",
      subgroup == "~White" ~ "White"
    ),
    grade_band = grade,
    n_students,
    n_chronically_absent,
    pct_chronically_absent = round5(100 * n_chronically_absent/n_students, 1)
  ) %>%
  arrange(system, subgroup, grade_band)

# School
students_school <- absenteeism %>% filter(isp_days/instructional_days >= 0.5)

school <- groups %>%
  map_dfr(~ collapse(students_school, !!., system, system_name, school, school_name)) %>%
  filter(grade == "All Grades") %>%
  transmute(
    system,
    system_name,
    school,
    school_name,
    subgroup = case_when(
      subgroup == "~All" ~ "All Students",
      subgroup == "~BHN" ~ "Black/Hispanic/Native American",
      subgroup == "~ED" ~ "Economically Disadvantaged",
      subgroup == "~SWD" ~ "Students with Disabilities",
      subgroup == "~EL" ~ "English Learners with Transitional 1-4",
      subgroup == "~Black" ~ "Black or African American",
      subgroup == "~Hispanic" ~ "Hispanic",
      subgroup == "~Native" ~ "American Indian or Alaska Native",
      subgroup == "~HPI" ~ "Native Hawaiian or Other Pacific Islander",
      subgroup == "~Asian" ~ "Asian",
      subgroup == "~White" ~ "White"
    ),
    grade_band = grade,
    n_students,
    n_chronically_absent,
    pct_chronically_absent = round5(100 * n_chronically_absent/n_students, 1)
  ) %>%
  arrange(system, school, subgroup, grade_band)

# Student
student <- absenteeism %>%
  transmute(
    system, system_name, school, school_name, 
    student_id = student_key, first_name, middle_name, last_name,
    n_absences, isp_days, instructional_calendar_days = instructional_days,
    absentee_rate = round5(100 * n_absences/isp_days, 1),
    Black, Hispanic, Native, HPI, Asian, White, ED, SWD, EL
  ) %>%
  mutate_at(
    .vars = c("Black", "Hispanic", "Native", "HPI", "Asian", "White", "ED", "SWD", "EL"),
    .f = ~ if_else(is.na(.), 0L, as.integer(.))
  )

setwd(str_c("N:/ORP_accountability/data/", year(today()), "_chronic_absenteeism"))

# State, district, school, and student output
write_csv(student, str_c("student_chronic_absenteeism_", month(today(), label = TRUE), day(today()), ".csv"), na = "")
write_csv(state, str_c("state_chronic_absenteeism_", month(today(), label = TRUE), day(today()), ".csv"), na = "")
write_csv(district, str_c("district_chronic_absenteeism_", month(today(), label = TRUE), day(today()), ".csv"), na = "")
write_csv(school, str_c("school_chronic_absenteeism_", month(today(), label = TRUE), day(today()), ".csv"), na = "")

# Split Files
district_numbers <- sort(unique(student$system))

student %>%
  split(., .$system) %>%
  walk2(
    .x = ., 
    .y = district_numbers, 
    .f = ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", .y, "_ChronicAbsenteeismStudentFile_17Jun2019.csv"), na = "")
  )

district %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers, 
    .f = ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", .y, "_ChronicAbsenteeismDistrictFile_17Jun2019.csv"), na = "")
  )

school %>%
  split(., .$system) %>%
  walk2(
    .x = .,
    .y = district_numbers, 
    .f = ~ write_csv(.x, path = paste0("N:/ORP_accountability/data/2019_chronic_absenteeism/split/", .y, "_ChronicAbsenteeismSchoolFile_17Jun2019.csv"), na = "")
  )
