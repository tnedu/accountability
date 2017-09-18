#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Student Level to School Numeric

Edited last by:  Alexander Poon

Date edited last:  8/1/2017
***************************************************************/

use "K:\ORP_accountability\projects\2016_student_level_file/state_student_level_2016.dta", clear;

* Unaka High School Correction;
drop if subject == "Integrated Math I" & (unique_student_id == 3288069 | unique_student_id == 3288078 |
	unique_student_id == 3288082 | unique_student_id == 3292339 | unique_student_id == 3288107 |
	unique_student_id == 3170748 | unique_student_id == 3288120 | unique_student_id == 3258979 |
	unique_student_id == 3288132 | unique_student_id == 3288148 | unique_student_id == 3288162 |
	unique_student_id == 3288188 | unique_student_id == 3288191 | unique_student_id == 3288202 |
	unique_student_id == 3288215 | unique_student_id == 4328806 | unique_student_id == 3170837 |
	unique_student_id == 3288228 | unique_student_id == 3288239 | unique_student_id == 3292127 |
	unique_student_id == 4444607 | unique_student_id == 3288279 | unique_student_id == 3170752 |
	unique_student_id == 3288307 | unique_student_id == 3288314 | unique_student_id == 3288336 |
	unique_student_id == 3288337 | unique_student_id == 3291564 | unique_student_id == 3288355 |
	unique_student_id == 3288368 | unique_student_id == 4130583 | unique_student_id == 3258975 |
	unique_student_id == 3288397 | unique_student_id == 3805610 | unique_student_id == 3288443 |
	unique_student_id == 3288505 | unique_student_id == 3288535 | unique_student_id == 3749220 |
	unique_student_id == 3288556 | unique_student_id == 3291903 | unique_student_id == 3917581 |
	unique_student_id == 3247449 | unique_student_id == 3288594 | unique_student_id == 3120833 |
	unique_student_id == 3288612 | unique_student_id == 3288616);

gen year = 2016;

* Omit < 60% Enrollment;
drop if greater_than_60_pct == "N";

* MSAA tests above grade 9 are reassigned to EOCs;
replace original_subject = "Algebra I" if original_subject == "Math" & test == "MSAA" & grade >= 9 &
	(system != 30 & system != 60 & system != 80 & system != 100 & system != 110 & system != 140 & system != 150 &
	system != 190 & system != 440 & system != 580 & system != 590 & system != 710 & system != 800 & system != 821 &
	system != 850 & system != 890 & system != 930);

replace original_subject = "Integrated Math I" if original_subject == "Math" & test == "MSAA" & grade >= 9 &
	(system == 30 | system == 60 | system == 80 | system == 100 | system == 110 | system == 140 | system == 150 | 
	system == 190 | system == 440 | system == 580 | system == 590 | system == 710 | system == 800 | system == 821 | 
	system == 850 | system == 890 | system == 930);

replace original_subject = "English II" if test == "MSAA" & original_subject == "ELA";

* Residential Facility and Homebound students are dropped from school level;
drop if residential_facility_part_1 == 1 | residential_facility_part_2 == 1;
drop if homebound == 1;

* Proficiency levels;
gen n_below = 1 if proficiency_level == "1. Below" | proficiency_level == "1. Below Basic";
gen n_approaching = 1 if proficiency_level == "2. Approaching" | proficiency_level == "2. Basic";
gen n_on_track = 1 if proficiency_level == "3. On Track" | proficiency_level == "3. Proficient";
gen n_mastered = 1 if proficiency_level == "4. Mastered" | proficiency_level == "4. Advanced";

* Create subgroup variables for collapse;
gen All = 1;
rename (bhn_group economically_disadvantaged special_ed el el_t1_t2) (BHN ED SWD EL EL_T1_T2);
replace EL_T1_T2 = 1 if EL == 1;
gen Super = (BHN == 1 | ED == 1 | SWD == 1 | EL_T1_T2 == 1);

* Collapse test proficiency by subject and subgroup;
quietly foreach s in All BHN ED SWD EL_T1_T2 Super {;

	* Individual Grades;
	
	preserve;

	keep if `s' == 1;
	gen subgroup = "`s'";

	collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1 tested_part_2 tested_both 
		valid_test n_below n_approaching n_on_track n_mastered, by(year system school original_subject grade subgroup);

	tempfile `s'_ind_grades;
	save ``s'_ind_grades', replace;

	restore;

};

clear;

foreach s in All BHN ED SWD EL_T1_T2 Super {;

	append using ``s'_ind_grades';

};

rename (original_subject valid_test) (subject valid_tests);

* ACT Substitution;
preserve;

use "K:\ORP_accountability\data\2016_ACT/school_act_substitution_2016.dta", clear;

destring grade, replace;

replace subgroup = "All";
rename (n_not_met_benchmark n_met_benchmark) (n_approaching n_on_track);

drop pct_not_met_benchmark pct_met_benchmark;

tempfile act_sub;
save `act_sub', replace;

restore;

append using `act_sub';

gen grade_band = "6th through 8th" if grade == 6 | grade == 7 | grade == 8;
replace grade_band = "9th through 12th" if grade == 9 | grade == 10 | grade == 11 | grade == 12;

replace subject = "Math" if grade_band == "6th through 8th" & (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math")); 
replace subject = "ELA" if grade_band == "6th through 8th" & (subject == "English I" | subject == "English II" | subject == "English III"); 

replace subject = "HS Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") | subject == "ACT Math";
replace subject = "HS English" if subject == "English I" | subject == "English II" | subject == "English III" | subject == "ACT Reading";

drop if subject == "Biology I" | subject == "Chemistry" | subject == "US History";

collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both
	valid_tests n_below n_approaching n_on_track n_mastered, by(year system school subject subgroup grade_band);

rename grade_band grade;

* Generate %B, A, O, M, O+M;
foreach p in approaching on_track mastered {;

	gen pct_`p' = round(1000 * n_`p'/valid_tests)/10;

};

gen pct_below = round(100 - pct_approaching - pct_on_track - pct_mastered, 0.1);
order pct_below, before(pct_approaching);

gen pct_on_mastered = round(100 * (n_on_track + n_mastered)/valid_tests, 0.1);

* Fix % BB/B/P if there are no n_BB, n_B, n_P;
gen flag_below = 1 if pct_below != 0 & pct_below != . & n_below == 0;
replace pct_approaching = (100 - pct_on_track - pct_mastered) if flag_below == 1;
replace pct_below = 0 if flag_below == 1;

gen flag_b_a = 1 if (pct_below != 0 & pct_below != . & n_below == 0) | (pct_approaching != 0 & pct_approaching != . & n_approaching == 0);
replace pct_on_track = (100 - pct_mastered) if flag_b_a == 1;
replace pct_approaching = 0 if flag_b_a == 1;

drop flag_below flag_b_a;

* Create New Entries for missing subgroups (with 0 enrolled, valid tests, etc.);
reshape wide enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both valid_tests 
	n_below n_approaching n_on_track n_mastered pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered,
	i(year system school subject grade) j(subgroup) string;

foreach v in enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both valid_tests 
	n_below n_approaching n_on_track n_mastered pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered {;

	foreach s in All BHN ED SWD EL EL_T1_T2 Super {;

		replace `v'`s' = 0 if `v'`s' ==.;

	};
};

reshape long;

replace subgroup = "All Students" if subgroup == "All";
replace subgroup = "Black/Hispanic/Native American" if subgroup == "BHN";
replace subgroup = "Economically Disadvantaged" if subgroup == "ED";
replace subgroup = "English Learners" if subgroup == "EL_T1_T2";
replace subgroup = "Students with Disabilities" if subgroup == "SWD";
replace subgroup = "Super Subgroup" if subgroup == "Super";

* Participation Rate;
gen participation_rate_1yr = round(100 * (tested + tested_part_1_only + tested_part_2_only + tested_both)/(enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both));

* Output numeric file;
gsort system subject subgroup;

order year system school subject grade subgroup participation_rate_1yr enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both 
	tested tested_part_1_only tested_part_2_only tested_both valid_tests n_below n_approaching n_on_track n_mastered 
	pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered;

compress;

export delim using "K:\ORP_accountability\data\2016_accountability/school_numeric_with_unaka_correction_2016.csv", delim(",") replace;
