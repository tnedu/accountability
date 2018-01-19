#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Student Level to School Base

Edited last by:  Alexander Poon

Date edited last:  1/19/2018
***************************************************************/

use "K:\ORP_accountability\projects\2016_student_level_file/state_student_level_2016.dta", clear;

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

* Drop greater than 60 percent for 2018 school grading preview;
drop if greater_than_60_pct == "N";

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
gen Asian = (race == "Asian");
gen Black = (race == "Black or African American");
gen Hispanic = (race == "Hispanic/Latino");
gen Hawaiian = (race == "Native Hawaiian/Pac. Islander");
gen Native = (race == "American Indian/Alaska Native");
gen White = (race == "White");

rename (bhn_group economically_disadvantaged special_ed el el_t1_t2) (BHN ED SWD EL EL_T1_T2);
replace EL_T1_T2 = 1 if EL == 1;
gen Non_ED = (ED == 0);
gen Non_SWD = (SWD == 0);
gen Non_EL = (EL == 0);
gen Non_EL_T1_T2 = (EL == 0 & EL_T1_T2 == 0);
gen Super = (BHN == 1 | ED == 1 | SWD == 1 | EL == 1 | EL_T1_T2 == 1);

* Collapse test proficiency by subject and subgroup;
quietly foreach s in All Asian Black Hispanic Hawaiian Native White BHN ED SWD EL EL_T1_T2 Non_ED Non_SWD Non_EL Non_EL_T1_T2 Super {;

	* All Grades;

	preserve;

	keep if `s' == 1;
	gen subgroup = "`s'";

	collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1 tested_part_2 tested_both 
		valid_test n_below n_approaching n_on_track n_mastered, by(year system school original_subject subgroup);

	gen grade = "All Grades";

	tempfile `s'_all_grades;
	save ``s'_all_grades', replace;

	restore;
	
	* Individual Grades;
	
	preserve;

	keep if `s' == 1;
	gen subgroup = "`s'";

	collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1 tested_part_2 tested_both 
		valid_test n_below n_approaching n_on_track n_mastered, by(year system school original_subject grade subgroup);
	
	tostring grade, replace;

	tempfile `s'_ind_grades;
	save ``s'_ind_grades', replace;

	restore;

};

clear;

foreach s in All Asian Black Hispanic Hawaiian Native White BHN ED SWD EL EL_T1_T2 Non_ED Non_SWD Non_EL Non_EL_T1_T2 Super {;

	append using ``s'_all_grades';
	append using ``s'_ind_grades';

};

* File Cleaning/Formatting;
rename (original_subject valid_test) (subject valid_tests);

* Generate %BB, B, P, A, BB+B, P+A;
foreach p in approaching on_track mastered {;

	gen pct_`p' = round(100 * n_`p'/valid_tests, 0.1);

};

gen pct_below = round(100 - pct_approaching - pct_on_track - pct_mastered, 0.1);
gen pct_on_mastered = round(100 * (n_on_track + n_mastered)/valid_tests, 0.1);

* Fix % BB/B/P if there are no n_BB, n_B, n_P;
gen flag_below = 1 if pct_below != 0 & pct_below != . & n_below == 0;
replace pct_approaching = (100 - pct_on_track - pct_mastered) if flag_below == 1;
replace pct_below = 0 if flag_below == 1;

gen flag_b_a = 1 if (pct_below != 0 & pct_below != . & n_below == 0) | (pct_approaching != 0 & pct_approaching != . & n_approaching == 0);
replace pct_on_track = (100 - pct_mastered) if flag_b_a == 1;
replace pct_approaching = 0 if flag_b_a == 1;

drop flag_below flag_b_a;

* Check everything sums to 100;
egen pct_total = rowtotal(pct_below pct_approaching pct_on_track pct_mastered);
replace pct_total = round(pct_total, 0.1);
tab pct_total;
* Should all be 0 (if no valid tests) or 100;

drop pct_total;

* Create New Entries for missing subgroups (with 0 enrolled, valid tests, etc.);
reshape wide enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both 
	valid_tests n_below n_approaching n_on_track n_mastered pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered,
	i(year system school subject grade) j(subgroup) string;

foreach v in enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both 
	valid_tests n_below n_approaching n_on_track n_mastered pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered {;

	foreach s in All Asian Black Hispanic Hawaiian Native White BHN ED SWD EL EL_T1_T2 Non_ED Non_SWD Non_EL Non_EL_T1_T2 Super {;

		replace `v'`s' = 0 if `v'`s' ==.;

	};
};

reshape long;

replace subgroup = "All Students" if subgroup == "All";
replace subgroup = "Black/Hispanic/Native American" if subgroup == "BHN";
replace subgroup = "Economically Disadvantaged" if subgroup == "ED";
replace subgroup = "English Learners" if subgroup == "EL";
replace subgroup = "English Learners with T1/T2" if subgroup == "EL_T1_T2";
replace subgroup = "Students with Disabilities" if subgroup == "SWD";
replace subgroup = "Non-Economically Disadvantaged" if subgroup == "Non_ED";
replace subgroup = "Non-English Learners" if subgroup == "Non_EL";
replace subgroup = "Non-English Learners/T1 or T2" if subgroup == "Non_EL_T1_T2";
replace subgroup = "Non-Students with Disabilities" if subgroup == "Non_SWD";
replace subgroup = "Super Subgroup" if subgroup == "Super";
replace subgroup = "Black or African American" if subgroup == "Black";
replace subgroup = "Native Hawaiian or Other Pacific Islander" if subgroup == "Hawaiian";
replace subgroup = "American Indian or Alaska Native" if subgroup == "Native";

* Clean and output base file;
gsort system school subject grade subgroup;

order year system school subject grade subgroup enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both
	tested tested_part_1_only tested_part_2_only tested_both valid_tests n_below n_approaching n_on_track n_mastered 
	pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered;

compress;

export delim "K:/ORP_accountability/data/2016_accountability/school_base_2016_for_accountability.csv", delim(",") replace;
