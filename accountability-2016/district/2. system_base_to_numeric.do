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

Date edited last:  03/08/2016

Outstanding issues:  Subgroup ACT TVAAS Data is incorrect
***************************************************************/

global base "K:\ORP_accountability\projects\2016_pre_coding\Output\system_base_with_super_subgroup_2016.dta";
global tvaas "K:\ORP_accountability\data\2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (All Students).xlsx";
global tvaas_subgroup "K:\ORP_accountability\data\2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (Subgroups).xlsx";
global tvaas_super "K:\ORP_accountability\data\2015_tvaas/District-Level Intra-Year NCE MRM and URM Results (Super Subgroup).xlsx";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;
local lag_yr = `current_yr' - 1;

use $base, clear;

keep if year == `current_yr' | year == `lag_yr';

* Graduation rate;
preserve;

keep if subject == "Graduation Rate";

keep if subgroup == "All Students" | subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" |
	subgroup == "Students with Disabilities" | subgroup == "English Language Learners with T1/T2" | subgroup == "Super Subgroup";

replace subgroup = "English Language Learners" if subgroup == "English Language Learners with T1/T2";
	
tempfile grad;
save `grad', replace;

restore;

* Keep only subjects, subgroups in district accountability;
keep if subgroup == "All Students" | subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" |
	subgroup == "Students with Disabilities" | subgroup == "English Language Learners" | subgroup == "English Language Learners with T1/T2" |
	subgroup == "Super Subgroup";

keep if regexm(subject, "ACT") | subject == "Math" | subject == "ELA" | subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" |
	regexm(subject, "Integrated Math") | subject == "English I" | subject == "English II" | subject == "English III";

* Reassign ACT variables to BB, PA;
replace n_below_bsc = n_below_19 if subject == "ACT Composite";
replace n_prof = n_21_and_above if subject == "ACT Composite";

* Drop all grades, have to collapse to manually create grade combinations;
drop if grade == "All Grades";

* Reassign subjects for EOCs with grades less than 9;
replace subject = "Math" if (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math")) & 
	(grade == "3" | grade == "4" | grade == "5" | grade == "6" | grade == "7" | grade == "8");

replace subject = "ELA" if (subject == "English I" | subject == "English II" | subject == "English III") & 
	(grade == "3" | grade == "4" | grade == "5" | grade == "6" | grade == "7" | grade == "8");

* Collapse test counts across replaced subjects;
collapse (sum) enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv, by(year system system_name subject grade subgroup);

* Create accountability subject combinations, 3-5, 6-8, and 9-12 grade bands;
gen grade_band = "3rd through 5th" if (subject == "Math" | subject == "ELA") & 
	(grade == "3" | grade == "4" | grade == "5");
replace grade_band = "6th through 8th" if (subject == "Math" | subject == "ELA") & 
	(grade == "6" | grade == "7" | grade == "8");

replace grade_band = "9th through 12th" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") |
	subject == "English I" | subject == "English II" | subject == "English III" | regexm(subject, "ACT");

replace subject = "HS Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") | subject == "ACT Math";
replace subject = "HS English" if subject == "English I" | subject == "English II" | subject == "English III" | subject == "ACT English";

collapse (sum) enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv,
	by(year system system_name subject subgroup grade_band);

rename grade_band grade;

* If ELL valid test count >= 30, use T1/T2. Otherwise, keep ELL;
gen ell_30 = (valid_tests >= 30 & valid_tests != .) if subgroup == "English Language Learners";
gen comparison = "ELL" if subgroup == "English Language Learners" | subgroup == "English Language Learners with T1/T2";

bysort year system subject grade comparison: egen temp = max(ell_30);

drop if temp == 0 & subgroup == "English Language Learners with T1/T2";
drop if temp == 1 & subgroup == "English Language Learners";
replace subgroup = "English Language Learners" if subgroup == "English Language Learners with T1/T2";

duplicates tag year system subject grade comparison, gen(dup);
tab dup if comparison == "ELL";
* Should be 0 for all;

drop dup ell_30 comparison temp;

* Generate %BB, B, P, A, BB+B, P+A;
foreach p in bsc prof adv {;

	gen pct_`p' = round(100 * n_`p'/valid_tests, 0.1);

};

gen pct_below_bsc = round(100 - pct_bsc - pct_prof - pct_adv, 0.1);
order pct_below_bsc, before(pct_bsc);

gen pct_prof_adv = round(100 * (n_prof + n_adv)/valid_tests, 0.1);

* Fix % BB/B if there are no n_BB, n_B;
gen flag_bb = 1 if pct_below_bsc != 0 & pct_below_bsc != . & n_below_bsc == 0;
replace pct_bsc = (100 - pct_prof - pct_adv) if flag_bb == 1;
replace pct_below_bsc = 0 if flag_bb == 1;

gen flag_bb_b = 1 if (pct_below_bsc != 0 & pct_below_bsc != .) & (pct_bsc != 0 & pct_bsc != .)
	& n_below_bsc == 0 & n_bsc == 0;
replace pct_prof = (100 - pct_adv) if flag_bb_b == 1;
replace pct_bsc = 0 if flag_bb_b == 1;
	
drop flag_bb flag_bb_b;

* Check everything sums to 100;
egen pct_total = rowtotal(pct_below_bsc pct_bsc pct_prof pct_adv);
replace pct_total = round(pct_total, 0.1);
tab pct_total;
* Should all be 0 or 100;

drop pct_total;

* Participation rates;
gsort system system_name subject grade subgroup -year;

foreach y in 1 2 {;

	gen tested_lag_`y'yr = tested[_n+`y']
		if system == system[_n+`y'] & subject == subject[_n+`y'] & grade == grade[_n+`y'] & subgroup == subgroup[_n+`y'];
	gen enrolled_lag_`y'yr = enrolled[_n+`y']
		if system == system[_n+`y'] & subject == subject[_n+`y'] & grade == grade[_n+`y'] & subgroup == subgroup[_n+`y'];

};

egen tested_2yr = rowtotal(tested tested_lag_1yr);
egen enrolled_2yr = rowtotal(enrolled enrolled_lag_1yr);

egen tested_3yr = rowtotal(tested tested_lag_1yr tested_lag_2yr);
egen enrolled_3yr = rowtotal(enrolled enrolled_lag_1yr enrolled_lag_2yr);

gen participation_rate_1yr = round(100 * (tested/enrolled), 1) if year == `current_yr';
gen participation_rate_2yr = round(100 * (tested_2yr/enrolled_2yr), 1) if year == `current_yr';
gen participation_rate_3yr = round(100 * (tested_3yr/enrolled_3yr), 1) if year == `current_yr';

drop enrolled_* tested_*;

order participation_rate_1yr participation_rate_2yr participation_rate_3yr, before(enrolled);

* 95% CI for proficiency;
scalar qnorm95 = invnormal(0.975);

replace pct_prof_adv = pct_prof_adv/100;
replace pct_below_bsc = pct_below_bsc/100;

gen temp = sqrt((pct_prof_adv * (1 - pct_prof_adv)/valid_tests) + (qnorm95 ^ 2)/(4 * valid_tests ^ 2));
gen temp2 = pct_prof_adv + (qnorm95 ^ 2/(2 * valid_tests)) + qnorm95 * temp;
gen ub_ci_prof_adv = round(100 * (valid_tests/(valid_tests + qnorm95 ^ 2)) * temp2 , 0.1);
drop temp*;

gen temp = sqrt((pct_below_bsc * (1 - pct_below_bsc)/valid_tests) + (qnorm95 ^ 2)/(4 * valid_tests ^ 2));
gen temp2 = pct_below_bsc + (qnorm95 ^ 2/(2 * valid_tests)) - qnorm95 * temp;
gen lb_ci_below_bsc = round(100 * (valid_tests/(valid_tests + qnorm95 ^ 2)) * temp2 , 0.1);
drop temp*;

replace pct_prof_adv = 100 * pct_prof_adv;
replace pct_below_bsc = 100 * pct_below_bsc;

tempfile numeric;
save `numeric', replace;

* TVAAS;
preserve;

import excel using "$tvaas", firstrow clear;

tolower;

destring systemnumber year index, replace;

* ACT data is lagged one year for accountability;
replace year = year + 1 if test == "ACT";
replace subject = "ACT Composite" if test == "ACT" & subject == "Composite";

replace subject = "ELA" if subject == "Reading/Language";
replace grade = "9th through 12th" if subject == "HS Math" | subject == "HS English";
replace grade = "9th through 12th" if test == "ACT";

replace grade = "3rd through 5th" if grade == "4-5";
replace grade = "6th through 8th" if grade == "6-8";

drop system;
rename (systemnumber index level) (system tvaas_index tvaas_level);

drop if test == "ACT" & subject != "ACT Composite";

keep year system subject grade tvaas_index tvaas_level;

gen subgroup = "All Students";

tempfile tvaas_all;
save `tvaas_all', replace;

import excel using "$tvaas_subgroup", firstrow clear;

tolower;

destring systemnumber year index, replace;

replace subgroup = "Students with Disabilities" if subgroup == "Students With Disabilities";

* ACT data is lagged one year for accountability;
replace year = year + 1 if test == "ACT";
replace subject = "ACT Composite" if test == "ACT" & subject == "Composite";

replace subject = "ELA" if subject == "Reading/Language";
replace grade = "9th through 12th" if subject == "HS Math" | subject == "HS English";
replace grade = "9th through 12th" if test == "ACT";

replace grade = "3rd through 5th" if grade == "4-5";
replace grade = "6th through 8th" if grade == "6-8";

drop system;
rename (systemnumber index level) (system tvaas_index tvaas_level);

drop if test == "ACT" & subject != "ACT Composite";

keep year system subject subgroup grade tvaas_index tvaas_level;

tempfile tvaas_subgroup;
save `tvaas_subgroup', replace;

import excel using "$tvaas_super", firstrow clear;

tolower;

destring systemnumber year index, replace;

* ACT data is lagged one year for accountability;
replace year = year + 1 if test == "ACT";
replace subject = "ACT Composite" if test == "ACT" & subject == "Composite";

replace subject = "ELA" if subject == "Reading/Language";
replace grade = "9th through 12th" if subject == "HS Math" | subject == "HS English";
replace grade = "9th through 12th" if test == "ACT";

replace grade = "3rd through 5th" if grade == "4-5";
replace grade = "6th through 8th" if grade == "6-8";

rename (system systemnumber index level) (sysetm_name system tvaas_index tvaas_level);

drop if test == "ACT" & subject != "ACT Composite";

keep year system subject subgroup grade tvaas_index tvaas_level;

tempfile tvaas_super;
save `tvaas_super', replace;

restore;

mmerge year system subject grade subgroup using `tvaas_all', type(1:1);
drop if _merge == 2;
drop _merge;

mmerge year system subject grade subgroup using `tvaas_subgroup', type(1:1) update;
drop if _merge == 2;
drop _merge;

mmerge year system subject grade subgroup using `tvaas_super', type(1:1) update;
drop if _merge == 2;
drop _merge;

* Append grad data;
append using `grad';

* Creating percentile ranks for %BB and %PA;
replace valid_tests = grad_cohort if subject == "Graduation Rate";
replace pct_below_bsc = dropout_rate if subject == "Graduation Rate";
replace pct_prof_adv = grad_rate if subject == "Graduation Rate";

* Districts are only counted for percentiles if they meet the test count in both years;
gen test_count = valid_tests >= 30 & valid_tests != 0 & valid_tests !=.;
bysort system system_name subject subgroup grade: egen temp = sum(test_count);

gen eligible = (temp == 2);
drop test_count temp;

* Generate new variables pctile_rank_PA and pctile_rank_BB which count the number of eligible districts in the same
* year, subject, grade band, and subgroup with a lower or equal %PA (%BB) - egen track doesn't do this correctly;
gen pctile_rank_PA = .;
gen pctile_rank_BB = .;

count;
local count = r(N);

* Looping over observations, generate a new variable equal to the number of eligible districts with the 
* same or lower percent P/A within the same year, subject, grade band, and subgroup;
quietly forval k = 1(1)`count' {;

	if pct_prof_adv[`k'] != . {;

		count if pct_prof_adv <= pct_prof_adv[`k'] & subject == subject[`k'] & subgroup == subgroup[`k'] & 
			grade == grade[`k'] & year == year[`k'] & pct_prof_adv != . & eligible == 1;

		replace pctile_rank_PA = r(N) if [_n] == `k' & eligible == 1;

	};
};

* Repeat for percent BB;
quietly forval k = 1(1)`count' {;

	if pct_below_bsc[`k'] != . {;

		count if pct_below_bsc <= pct_below_bsc[`k'] & subject == subject[`k'] & subgroup == subgroup[`k'] &
			grade == grade[`k'] & year == year[`k'] & pct_below_bsc != . & eligible == 1;

		replace pctile_rank_BB = r(N) if [_n] == `k' & eligible == 1;

	};
};

bysort subject grade year subgroup: egen eligible_count = sum(eligible);

gsort -year subject grade subgroup -eligible -pct_below_bsc;
gen BB_percentile = round(100 * pctile_rank_BB/eligible_count, 0.1) if eligible == 1;

gsort -year subject grade subgroup -eligible -pct_prof_adv;
gen PA_percentile = round(100 * pctile_rank_PA/eligible_count, 0.1) if eligible == 1;

drop eligible pctile_rank_* eligible_count;

replace valid_tests = . if subject == "Graduation Rate";
replace pct_below_bsc = . if subject == "Graduation Rate";
replace pct_prof_adv = . if subject == "Graduation Rate";

* Clean and output numeric file;
gsort system system_name grade subject subgroup -year;

replace enrolled = . if subject == "ACT Composite" & subgroup != "All Students";
replace tested = . if subject == "ACT Composite" & subgroup != "All Students";

compress;

save "$output/system_numeric_with_super_subgroup_2016.dta", replace;
export delim using "$output/system_numeric_with_super_subgroup_2016.csv", delim(",") replace;
