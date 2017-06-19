#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  System Numeric for Report Card

Edited last by:  Alexander Poon

Date edited last:  11/18/2016
***************************************************************/

use "K:\ORP_accountability\projects\2016_student_level_file/state_student_level_2016.dta", clear;

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

* Residential Facility students are dropped from system level;
drop if residential_facility_part_1 == 1 | residential_facility_part_2 == 1;

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
quietly foreach s in All BHN ED SWD EL_T1_T2 Super {;

	* Individual Grades;
	
	preserve;

	keep if `s' == 1;
	gen subgroup = "`s'";

	collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1 tested_part_2 tested_both 
		valid_test n_below n_approaching n_on_track n_mastered, by(year system original_subject grade subgroup);

	tempfile `s'_ind_grades;
	save ``s'_ind_grades', replace;

	restore;

};

clear;

foreach s in All BHN ED SWD EL_T1_T2 Super {;

	append using ``s'_ind_grades';

};

rename (original_subject valid_test) (subject valid_tests);

* Numeric will only include high school subjects, so drop grade we would normally reassign;
drop if grade == 6 | grade == 7 | grade == 8;

gen grade_band = "9th through 12th";

replace subject = "HS Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math");
replace subject = "HS English" if subject == "English I" | subject == "English II" | subject == "English III";
replace subject = "HS Science" if subject == "Biology I" | subject == "Chemistry";

collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both
	valid_tests n_below n_approaching n_on_track n_mastered, by(year system subject subgroup grade_band);

rename grade_band grade;

* Generate %B, A, O, M, O+M;
foreach p in approaching on_track mastered {;

	gen pct_`p' = round(100 * n_`p'/valid_tests, 0.1);

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

* Check everything sums to 100;
egen pct_total = rowtotal(pct_below pct_approaching pct_on_track pct_mastered);
replace pct_total = round(pct_total, 0.1);
tab pct_total;
* Should all be 0 (if no valid tests) or 100;

drop pct_total;

* Create New Entries for missing subgroups (with 0 enrolled, valid tests, etc.);
reshape wide enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both valid_tests 
	n_below n_approaching n_on_track n_mastered pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered,
	i(year system subject grade) j(subgroup) string;

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
replace subgroup = "English Learners with T1/T2" if subgroup == "EL_T1_T2";
replace subgroup = "Students with Disabilities" if subgroup == "SWD";
replace subgroup = "Super Subgroup" if subgroup == "Super";

* Participation Rate;
gen participation_rate = round(100 * (tested + tested_part_1_only + tested_part_2_only + tested_both)/(enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both));

* Percentile rank;
gen eligible = valid_tests >= 30 & valid_tests != .;
gen pctile_rank_OM = .;

count;
local count = r(N);

* Looping over observations, generate a new variable equal to the number of eligible districts with the 
* same or lower percent O/M within the same year, subject, grade band, and subgroup;
quietly forval k = 1(1)`count' {;

	if pct_on_mastered[`k'] != . {;

		count if pct_on_mastered <= pct_on_mastered[`k'] & subject == subject[`k'] & subgroup == subgroup[`k'] & 
			grade == grade[`k'] & year == year[`k'] & pct_on_mastered != . & eligible == 1;

		replace pctile_rank_OM = r(N) if [_n] == `k' & eligible == 1;

	};
};

bysort subject grade year subgroup: egen eligible_count = sum(eligible);

gsort -year subject grade subgroup -eligible -pct_on_mastered;
gen OM_percentile = round(100 * pctile_rank_OM/eligible_count, 0.1) if eligible == 1;

drop eligible pctile_rank_* eligible_count;

* Merge on prior percentile rank;
preserve;

use "K:\ORP_accountability\projects\2016_pre_coding\Output/system_numeric_with_super_subgroup_2016.dta", clear;

keep if year == 2015;
keep if subject == "HS Math" | subject == "HS English";

replace subgroup = "English Learners with T1/T2" if subgroup == "English Language Learners";

keep system system_name subject subgroup PA_percentile;
rename PA_percentile PA_percentile_prior;

tempfile pctile_prior;
save `pctile_prior', replace;

keep system system_name;
duplicates drop;

tempfile names;
save `names', replace;

restore;

mmerge system subject subgroup using `pctile_prior', type(1:1);
drop _merge;

mmerge system using `names', type(n:1) update;
drop _merge;

* Output numeric file;
gsort system subject subgroup;

order year system system_name subject grade subgroup participation_rate enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both 
	tested tested_part_1_only tested_part_2_only tested_both valid_tests n_below n_approaching n_on_track n_mastered 
	pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered OM_percentile PA_percentile_prior;

compress;

export excel using "K:\ORP_accountability\projects\2016_state_results\For Report Card/system_numeric_2016_for_report_card.xlsx", firstrow(var) replace;
