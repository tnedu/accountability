## accountability

Code for reporting state, district, and school-level assessment results, both with (numeric/accountability files) 
and without (base/assessment files) accountability rules applied, as well as AMO targets, and non-assessment indicators 
used in accountability such as absenteeism, graduation rate, ACT, and ELPA. Files suppressed under FERPA rules are  
available for public download on the [Tennnessee Department of Education](https://www.tn.gov/education/data/data-downloads.html) 
website.

This repository also contains code for producing district accountability designations. 
These designations are [publicly available](https://www.tn.gov/education/data/accountability.html).

**Requirements**

See `installs.R`.

**Procedures**

Each year, accountability determinations are produced by running calculations in approximately the following order:

1) `dictionary` files read in assessment Comprehensive Data Files (CDFs). The Department receives a CDF for each of 
Fall EOC, Spring EOC, 3-8, grade 2, MSAA, and Alt-Science/Social Studies as fixed width files. Each CDF has a
corresponding file layout that provides the column specifications.
2) `cdf_to_student_level` converts the 3-8, EOC, and Alternative assessment files into a student level assessment file.
Business rules relating to RI statuses, reasons not tested, and duplication of student records are applied at this stage.
3) `*_assessment_file` converts the student level file to assessment level files, where `*` is each of state, district, 
and school. Assessment files are counts of students enrolled and tested, and counts and percentages of students and at 
each performance level, by assessment type, grade, subject, student group. Assessment files also include two years of 
historical data.
4) `act_substitution` identifies high school juniors who do not have a math EOC record in the student level file but met 
the ACT college readiness benchmark in math.
5) Non-assessment data:       
    a) chronic `absenteeism`: students missing 10% or more of the school year.  
    b) `graduation` rate for accountability purposes use data lagged one year.  
    c) `ready_graduate` identifies students who meet a 21+ ACT composite score or alternative postsecondary readiness criteria. 
    Also lagged one year.  
    d) `elpa`: English learner students exiting EL status or meeting the growth standard on WIDA access.   
6) `*_accountability_file` converts the student level file and non-assessment data to district and school accountability 
files. Success Rates, Graduation Rate, Ready Graduates, and Absenteeism indicators include AMO targets set using prior 
year data.
7) `district_accountability` identifies district accountability designations using the district_accountability file.
8) `grade_pools_designation_immune` identifies schools in the HS and K8 pool and schools not eligible for a grade. 
9) `priority`/`priority_exit` identifies the lowest performing schools (every three years) or schools exiting
priority status. 
10) `final_scores` calculates school grades, targeted support, and additional targeted support schools.
11) `school_designations` summarizes school statuses.
