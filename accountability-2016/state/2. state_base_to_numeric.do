#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  State Base to Numeric

Edited last by:  Alexander Poon

Date edited last:  03/08/2016

Outstanding issues:  
***************************************************************/

global base "K:\ORP_accountability\projects\2016_pre_coding\Output\state_base_with_super_subgroup_2016.dta";

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

keep year subject subgroup grade grad_cohort grad_count grad_rate dropout_count dropout_rate;

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
collapse (sum) enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv, 
	by(year subject grade subgroup);

* Create accountability subject combinations, 3-5, 6-8, and 9-12 grade bands;
gen grade_band = "3rd through 5th" if (subject == "Math" | subject == "ELA") & 
	(grade == "3" | grade == "4" | grade == "5");
replace grade_band = "6th through 8th" if (subject == "Math" | subject == "ELA") & 
	(grade == "6" | grade == "7" | grade == "8");

replace grade_band = "9th through 12th" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") |
	subject == "English I" | subject == "English II" | subject == "English III" | regexm(subject, "ACT");

replace subject = "HS Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") | subject == "ACT Math";
replace subject = "HS English" if subject == "English I" | subject == "English II" | subject == "English III" | subject == "ACT English";

collapse (sum) enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv, by(year subject subgroup grade_band);

rename grade_band grade;

* If ELL valid test count >= 30, use T1/T2. Otherwise, keep ELL;
gen ell_30 = (valid_tests >= 30 & valid_tests !=.) if subgroup == "English Language Learners";
gen comparison = "ELL" if subgroup == "English Language Learners" | subgroup == "English Language Learners with T1/T2";

bysort year grade comparison: egen temp = max(ell_30);

drop if temp == 0 & subgroup == "English Language Learners with T1/T2";
drop if temp == 1 & subgroup == "English Language Learners";
replace subgroup = "English Language Learners" if subgroup == "English Language Learners with T1/T2";

duplicates tag year subject grade comparison, gen(dup);
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
gen flag_bb = 1 if pct_below_bsc != 0 & pct_below_bsc !=. & n_below_bsc == 0;
replace pct_bsc = (100 - pct_prof - pct_adv) if flag_bb == 1;
replace pct_below_bsc = 0 if flag_bb == 1;

gen flag_bb_b = 1 if (pct_below_bsc != 0 & pct_below_bsc !=.) & (pct_bsc != 0 & pct_bsc !=.)
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
gsort subject grade subgroup -year;

foreach y in 1 2 {;

	gen tested_lag_`y'yr = tested[_n+`y']
		if subject == subject[_n+`y'] & grade == grade[_n+`y'] & subgroup == subgroup[_n+`y'];
	gen enrolled_lag_`y'yr = enrolled[_n+`y']
		if subject == subject[_n+`y'] & grade == grade[_n+`y'] & subgroup == subgroup[_n+`y'];

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

* Append grad data;
append using `grad';

* Output numeric file;
gsort grade subject subgroup -year;

gen system = .;
gen system_name = "";
order system system_name, after(year);

compress;

save "$output/state_numeric_with_super_subgroup_2016.dta", replace;
export delim using "$output/state_numeric_with_super_subgroup_2016.csv", delim(",") replace;
