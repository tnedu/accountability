#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  System Base to Numeric

Edited last by:  Alexander Poon

Date edited last:  11/4/2016
***************************************************************/

use "K:\ORP_accountability\projects\2016_state_results/system_base_with_super_subgroup_2016.dta", clear;

* Keep only subjects, subgroups in accountability;
keep if subgroup == "All Students" | subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" |
	subgroup == "Students with Disabilities" | subgroup == "English Learners with T1/T2" | subgroup == "Super Subgroup";

* Drop all grades, have to collapse to manually create grade combinations;
drop if grade == "All Grades";

gen grade_band = "9th through 12th";

replace subject = "HS Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math");
replace subject = "HS English" if subject == "English I" | subject == "English II" | subject == "English III";

drop if subject == "Biology I" | subject == "Chemistry" | subject == "US History";

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

restore;

mmerge system subject subgroup using `pctile_prior', type(1:1);
drop _merge;

* Output numeric file;
gsort system subject subgroup;

order year system system_name subject grade subgroup participation_rate enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both 
	tested tested_part_1_only tested_part_2_only tested_both valid_tests n_below n_approaching n_on_track n_mastered 
	pct_below pct_approaching pct_on_track pct_mastered pct_on_mastered OM_percentile PA_percentile_prior;

compress;

save "K:\ORP_accountability\projects\2016_state_results/system_numeric_with_super_subgroup_2016.dta", replace;
