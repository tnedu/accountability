#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  CDF to Student Level

Edited last by:  Alexander Poon

Date edited last:  10/24/2016
***************************************************************/

* MSAA;
import delim using "K:\ORP_accountability\data\2016_cdf\MSAA/2016_TN_StateStudentResults.csv", clear;

drop if grade < 9;

* EL, ED status all missing in this file;
foreach s in mat ela {;

	preserve;

	gen reported_race = 4 if hispanicorlatinaethnicity == "Yes";
	replace reported_race = 3 if reported_race == . & blackorafricanamerican == "Yes";
	replace reported_race = 1 if reported_race == . & americanindianoralaskanative == "Yes";
	replace reported_race = 2 if reported_race == . & asian == "Yes";
	replace reported_race = 5 if reported_race == . & nativehawaiianothpacificislander == "Yes";
	replace reported_race = 6 if reported_race == . & white == "YES";

	gen el = lepstatus == 1;
	gen migrant = migrantstatus == 1;

	keep districtid schoolid state_student_id grade lastorsurname firstname reported_race `s'scaledscore `s'perflevel;

	rename `s'scaledscore scale_score;
	rename `s'perflevel performance_level;

	gen test = "MSAA";
	gen original_subject = "`s'";

	tempfile msaa_`s';
	save `msaa_`s'', replace;

	restore;

};

use `msaa_mat', clear;
append using `msaa_ela';

replace original_subject = "Math" if original_subject == "mat";
replace original_subject = "ELA" if original_subject == "ela";

rename (districtid schoolid state_student_id lastorsurname firstname) (system school unique_student_id last_name first_name);

tempfile msaa;
save `msaa', replace;

* EOCs;
use "K:\ORP_accountability\projects\2016_dictionary_coding/spring_math_english_cdf.dta", clear;
append using "K:\ORP_accountability\projects\2016_dictionary_coding/fall_math_english_cdf.dta";

* Code A/B - A (1) is ED, B (2) is Non-ED;
replace economically_disadvantaged = . if economically_disadvantaged == 0;
replace economically_disadvantaged = 0 if economically_disadvantaged == 2;
replace special_ed = special_ed == 1 | special_ed == 2 | special_ed == 3;

preserve;

use "K:\ORP_accountability\projects\2016_dictionary_coding/2016_State_Student_Fall_File_science.dta", clear;
gen test_admin_final = "FB";

append using "K:\ORP_accountability\projects\2016_dictionary_coding/2016_State_Student_Spring_File_science.dta";
replace test_admin_final = "TR" if test_admin_final == "";

keep system_name system_number school_name school_number grade content_area_code unique_student_id ethnic_origin 
	Native_American Asian Black_or_AfricanAmerican Hawaiian_PacificIslander White race_reported gender economically_disadvantaged ell ell_t1_t2
	special_ed functionally_delayed ri_status perf_level_total_sci scale_score_total_sci district_enroll test_admin_final;

tostring ethnic_origin, replace;
replace ethnic_origin = "H" if ethnic_origin == "0";
replace ethnic_origin = "N" if ethnic_origin == "1";
replace ethnic_origin = "U" if ethnic_origin == "2";

replace economically_disadvantaged = 1 - economically_disadvantaged;

destring special_ed, replace;
replace special_ed = special_ed == 0 | special_ed == 1 | special_ed == 2;

rename (system_name system_number school_name school_number Native_American Asian Black_or_AfricanAmerican Hawaiian_PacificIslander 
	White ell ell_t1_t2 race_reported perf_level_total_sci scale_score_total_sci content_area_code ri_status district_enroll)
	(system_name_final system_final school_name_final school_final native_american asian black hawaiian_pi white el el_t1_t2 reported_race 
	performance_level scale_score content_area_code_final ri_status_part_2 greater_than_60_pct);

tempfile science;
save `science', replace;

restore;

append using `science';

gen residential_facility_part_1 = ri_status_part_1 == 1;
gen residential_facility_part_2 = ri_status_part_2 == 1;
gen nullify_part_1 = ri_status_part_1 == 2;
gen nullify_part_2 = ri_status_part_2 == 2;
gen did_not_attempt_part_1 = ri_status_part_1 == 4;
gen did_not_attempt_part_2 = ri_status_part_2 == 4;
gen absent_part_1 = ri_status_part_1 == 5;
gen absent_part_2 = ri_status_part_2 == 5;

* Drop tests flagged as void and medically exempt;
drop if ri_status_part_1 == 3 | ri_status_part_2 == 3 | ri_status_part_1 == 9 | ri_status_part_2 == 9;

gen test = "EOC";

gen semester = "Spring" if test_admin_final == "SB" | test_admin_final == "TR";
replace semester = "Fall" if test_admin_final == "FB";

rename (system_final system_name_final school_final school_name_final content_area_code_final) (system system_name school school_name original_subject);

replace original_subject = "Algebra I" if original_subject == "A1";
replace original_subject = "Algebra II" if original_subject == "A2";
replace original_subject = "Biology I" if original_subject == "B1";
replace original_subject = "Chemistry" if original_subject == "C1";
replace original_subject = "English I" if original_subject == "E1";
replace original_subject = "English II" if original_subject == "E2";
replace original_subject = "English III" if original_subject == "E3";
replace original_subject = "Geometry" if original_subject == "G1";
replace original_subject = "Integrated Math I" if original_subject == "M1";
replace original_subject = "Integrated Math II" if original_subject == "M2";
replace original_subject = "Integrated Math III" if original_subject == "M3";
replace original_subject = "US History" if original_subject == "U1";

append using `msaa';

* Create student level file variables;
gen enrolled = 1 if test == "MSAA" | original_subject == "Biology I" | original_subject == "Chemistry";
gen enrolled_part_1_only = 1 if system_part_1 != . & system_part_2 == . & test != "MSAA" & (original_subject != "Biology I" & original_subject != "Chemistry");
gen enrolled_part_2_only = 1 if system_part_2 != . & system_part_1 == . & test != "MSAA" & (original_subject != "Biology I" & original_subject != "Chemistry");
gen enrolled_both = 1 if system_part_1 != . & system_part_2 != . & test != "MSAA" & (original_subject != "Biology I" & original_subject != "Chemistry");

gen tested = 1 if test == "MSAA" | original_subject == "Biology I" | original_subject == "Chemistry";
gen tested_part_1_only = 1 if system_part_1 != . & system_part_2 == . & test != "MSAA" & original_subject != "Biology I" & original_subject != "Chemistry";
gen tested_part_2_only = 1 if system_part_1 == . & system_part_2 != . & test != "MSAA" & original_subject != "Biology I" & original_subject != "Chemistry";
gen tested_both = 1 if system_part_1 != . & system_part_2 != . & test != "MSAA" & original_subject != "Biology I" & original_subject != "Chemistry";

gen valid_test = .;

gen original_proficiency_level = "1. Below" if performance_level == 1;
replace original_proficiency_level = "2. Approaching" if performance_level == 2;
replace original_proficiency_level = "3. On Track" if performance_level == 3;
replace original_proficiency_level = "4. Mastered" if performance_level == 4;

gen race = "Hispanic/Latino" if reported_race == 4;
replace race = "Black or African American" if reported_race == 3;
replace race = "American Indian/Alaska Native" if reported_race == 1;
replace race = "Native Hawaiian/Pac. Islander" if reported_race == 5;
replace race = "Asian" if reported_race == 2;
replace race = "White" if reported_race == 6;
replace race = "Unknown" if reported_race == 0 | reported_race == .;

gen bhn_group = (race == "Black or African American" | race == "Hispanic/Latino" | race == "American Indian/Alaska Native");

* Modifications and dropping excluded records;
gen proficiency_level = original_proficiency_level;
gen subject = original_subject;

* Home schools and private schools;
drop if system == 981 | system > 1000;

drop if grade == 13;

* Apply testing flag hierarchy (5.2.1);
* Absent students have a missing proficiency and tested value;
replace tested_part_1_only = 0 if absent_part_1 == 1 & (subject != "Biology I" & subject != "Chemistry");
replace tested_part_2_only = 0 if absent_part_2 == 1 & (subject != "Biology I" & subject != "Chemistry");
replace tested_both = 0 if absent_part_1 == 1 & (subject != "Biology I" & subject != "Chemistry");
replace tested_both = 0 if absent_part_2 == 1 & (subject != "Biology I" & subject != "Chemistry");

replace tested = 0 if absent_part_2 == 1 & (subject == "Biology I" | subject == "Chemistry");

replace proficiency_level = "" if absent_part_1 == 1 | absent_part_2 == 1;

* EL Excluded students are considered tested unless they do not have a performance level, but are not considered valid tests;
* EL Excluded students with a proficiency level are tested, but proficiency modified to missing (5.2.1);
replace valid_test = 0 if el_excluded == 1;
replace tested_part_1_only = 0 if el_excluded == 1 & original_proficiency_level == "" & 
	(original_subject == "Math" | original_subject == "Algebra I" | original_subject == "Algebra II" | 
	original_subject == "Geometry" | regexm(original_subject, "Integrated Math"));
replace tested_part_2_only = 0 if el_excluded == 1 & original_proficiency_level == "" & 
	(original_subject == "Math" | original_subject == "Algebra I" | original_subject == "Algebra II" | 
	original_subject == "Geometry" | regexm(original_subject, "Integrated Math"));
replace tested_both = 0 if el_excluded == 1 & original_proficiency_level == "" &
	(original_subject == "Math" | original_subject == "Algebra I" | original_subject == "Algebra II" | 
	original_subject == "Geometry" | regexm(original_subject, "Integrated Math"));
replace tested = 0 if el_excluded == 1 & original_proficiency_level == "" & (original_subject == "Biology I" | original_subject == "Chemistry");
replace proficiency_level = "" if el_excluded == 1;

* Proficiency modified to missing if nullify or did not attempt or part 1 or part 2 only (5.2.1);
replace proficiency_level = "" if (nullify_part_1 == 1 | nullify_part_2 == 1 | did_not_attempt_part_1 == 1 | did_not_attempt_part_2 == 1 | 
	part_1_or_2_only == 1 | part_1_or_2_only == 2);
replace proficiency_level = "" if invalid_score != .;
	
* Students taking MSAA are considered special education (5.5);
replace special_ed = 1 if test == "MSAA";

* Modify subject for MSAA tests in grade >= 9 (6.8);
replace subject = "Algebra I" if original_subject == "Math" & test == "MSAA" & (grade >= 9 & grade != .) &
	(system != 30 & system != 60 & system != 80 & system != 100 & system != 110 & system != 140 & system != 150 &
	system != 190 & system != 440 & system != 580 & system != 590 & system != 710 & system != 800 & system != 821 &
	system != 850 & system != 890 & system != 930);

replace subject = "Integrated Math I" if original_subject == "Math" & test == "MSAA" & (grade >= 9 & grade != .) &
	(system == 30 | system == 60 | system == 80 | system == 100 | system == 110 | system == 140 | system == 150 | 
	system == 190 | system == 440 | system == 580 | system == 590 | system == 710 | system == 800 | system == 821 | 
	system == 850 | system == 890 | system == 930);

replace subject = "English II" if original_subject == "ELA" & test == "MSAA" & (grade >= 9 & grade != .);

* Drop tests from CTE, adult HS, and alternative schools (6.9);
preserve;

import excel using "K:\ORP_accountability\data\2016_tdoe_provided_files/5 Other School Type - CTE AHS and ALT.xls", firstrow clear;

rename (DISTRICTNUMBER SCHOOLNUMBER) (system school);
keep system school;

destring system school, replace;

tempfile alt_hs_cte;
save `alt_hs_cte', replace;

restore;

mmerge system school using `alt_hs_cte', type(n:1);
keep if _merge == 1;
drop _merge;

* Convert subjects for students in grade < 9 taking EOCs;
replace subject = "Math" if (original_subject == "Algebra I" | original_subject == "Algebra II" | original_subject == "Geometry" |
	original_subject == "Integrated Math I" | original_subject == "Integrated Math II" | original_subject == "Integrated Math III") & grade < 9;
replace test = "Achievement" if (original_subject == "Algebra I" | original_subject == "Algebra II" | original_subject == "Geometry" |
	original_subject == "Integrated Math I" | original_subject == "Integrated Math II" | original_subject == "Integrated Math III") & grade < 9;

replace subject = "Reading/Language" if (original_subject == "English I" | original_subject == "English II" | original_subject == "English III") & grade < 9;
replace test = "Achievement" if (original_subject == "English I" | original_subject == "English II" | original_subject == "English III") & grade < 9;

replace subject = "Science" if (original_subject == "Biology I" | original_subject == "Chemistry") & grade < 9;
replace test = "Achievement" if (original_subject == "Biology I" | original_subject == "Chemistry") & grade < 9;

replace subject = "Social Studies" if original_subject == "US History" & grade < 9;
replace test = "Achievement" if original_subject == "US History" & grade < 9;

keep system_part_1 system_name_part_1 school_part_1 school_name_part_1 system system_name school school_name test
	original_subject subject original_proficiency_level proficiency_level scale_score enrolled* tested* valid_test 
	unique_student_id last_name first_name grade race bhn_group functionally_delayed special_ed economically_disadvantaged 
	el el_t1_t2 el_exclude greater_than_60_pct part_1_or_2_only migrant homebound absent_part_* did_not_attempt_part_* 
	nullify_part_* residential_facility_part_* semester;

/* Handling multiple test records */

* For students with multiple records across test types, MSSA has priority, then TCAP/EOC (6.10);
gen test_priority = 2 if test == "MSAA";
replace test_priority = 1 if test_priority == .;

bysort unique_student_id subject grade: egen temp = max(test_priority);
drop if test_priority != temp;

drop test_priority temp;

* For students with multiple records within the same test, take highest proficiency level (6.10);
gen proficiency_priority = 4 if proficiency_level == "4. Mastered";
replace proficiency_priority = 3 if proficiency_level == "3. On Track";
replace proficiency_priority = 2 if proficiency_level == "2. Approaching";
replace proficiency_priority = 1 if proficiency_level == "1. Below";

bysort unique_student_id subject grade test: egen temp = max(proficiency_priority);
drop if proficiency_priority != temp;

drop proficiency_priority temp;

* For students with multiple records with the same proficiency across different administrations, take the most recent (6.10);
gen semester_priority = 3 if test == "MSAA" | test == "Achievement" | (test == "EOC" & semester == "Spring");
replace semester_priority = 2 if test == "EOC" & semester == "Fall";
replace semester_priority = 1 if test == "EOC" & semester == "Summer";

bysort unique_student_id subject grade test: egen temp = max(semester_priority);
drop if semester_priority != temp;

drop semester_priority temp semester;

* Valid test if there is a proficiency level;
replace valid_test = proficiency_level != "" if valid_test == .;

preserve;

* Fill in missing ED data;
import delim using "K:\ORP_accountability\data\2016_tdoe_provided_files/Code AB for SAS.csv", clear;

keep districtid schoolid student_key codeab;
rename (districtid schoolid) (system school);

duplicates drop;

tempfile econ_dis;
save `econ_dis', replace;

restore;

mmerge system school unique_student_id using `econ_dis', type(n:1) umatch(system school student_key);
replace economically_disadvantaged = 1 if economically_disadvantaged == . & _merge == 3 & codeab == 1;
replace economically_disadvantaged = 0 if economically_disadvantaged == . & _merge == 3 & codeab == 2;

drop if _merge == 2;
drop codeab _merge;

* Clean and output file;
drop if unique_student_id == .;

replace economically_disadvantaged = 0 if economically_disadvantaged == .;
replace special_ed = 0 if special_ed == .;
replace el = 0 if el == .;
replace el_t1_t2 = 0 if el_t1_t2 == .;
replace el_t1_t2 = 1 if el_t1_t2 == 1 | el_t1_t2 == 2;

order system_part_1 system_name_part_1 school_part_1 school_name_part_1 system system_name school school_name test 
	original_subject subject original_proficiency_level proficiency_level scale_score enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both
	tested tested_part_1 tested_part_2 tested_both valid_test unique_student_id last_name first_name grade race bhn_group functionally_delayed 
	special_ed economically_disadvantaged el el_t1_t2 el_exclude greater_than_60_pct part_1_or_2_only migrant homebound absent_part_1 
	absent_part_2 did_not_attempt_part_1 did_not_attempt_part_2 nullify_part_1 nullify_part_2 residential_facility_part_1 residential_facility_part_2;

compress;

save "K:\ORP_accountability\projects\2016_student_level_file/state_student_level_2016.dta", replace;

duplicates tag system school subject grade test unique_student_id, gen(dup);
drop if dup > 0;
drop dup;

save "K:\ORP_accountability\projects\2016_student_level_file/state_student_level_2016_for_match.dta", replace;
