#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  School Base to Numeric

Edited last by:  Alexander Poon

Date edited last:  11/4/2016
***************************************************************/

use "K:\ORP_accountability\projects\2016_state_results/school_base_with_super_subgroup_2016.dta", clear;

* Keep only subjects, subgroups in accountability;
keep if subgroup == "All Students" | subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" |
	subgroup == "Students with Disabilities" | subgroup == "English Learners with T1/T2";

* Drop all grades, have to collapse to manually create grade combinations;
drop if grade == "All Grades";

gen grade_band = "9th through 12th";

replace subject = "HS Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math");
replace subject = "HS English" if subject == "English I" | subject == "English II" | subject == "English III";
replace subject = "HS Science" if subject == "Biology I" | subject == "Chemistry";

collapse (sum) enrolled enrolled_part_1 enrolled_part_2 tested tested_part_1_only tested_part_2_only tested_both
	valid_tests n_below n_approaching n_on_track n_mastered, by(year system school subject subgroup grade_band);

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

* Output numeric file;
gsort system school subject subgroup;

order year system school subject grade subgroup enrolled enrolled_part_1 enrolled_part_2 tested tested_part_1_only tested_part_2_only 
	tested_both valid_tests n_below n_approaching n_on_track n_mastered pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered;

compress;

save "K:\ORP_accountability\projects\2016_state_results/school_numeric_with_super_subgroup_2016.dta", replace;
