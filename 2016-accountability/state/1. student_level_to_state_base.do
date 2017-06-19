#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Student Level to State Base

Edited last by:  Alexander Poon

Date edited last:  03/08/2016

Outstanding issues:
***************************************************************/

global student_level "K:\ORP_accountability\data\2015_sas_accountability/state-student-level_2015_19jul2015.dta";
global grad "K:\ORP_accountability\data\2015_graduation_rate";
global act "K:\ORP_accountability\data\2015_ACT";
global act_substitution "K:\ORP_accountability\projects\2016_pre_coding\Output/state_act_substitution.dta";
global SAS_base "K:\ORP_accountability\data\2015_sas_accountability/state_base_2015_13jun2015.csv";
global fall_preview "K:\ORP_accountability\projects\2015_fall_district_preview\Alex\Output/state_base_with_super_subgroup_apr5.csv";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;
local lag_yr = `current_yr' - 1;

use $student_level, clear;

gen year = `current_yr';

* Portfolio tests above grade 9 are reassigned to EOCs;
replace original_subject = "Algebra I" if test == "Portfolio" & grade > 8 & original_subject == "Math";
replace original_subject = "English II" if test == "Portfolio" & grade > 8 & original_subject == "Reading/Language";
replace original_subject = "Biology I" if test == "Portfolio" & grade > 8 & original_subject == "Science";
replace original_subject = "U.S. History" if test == "Portfolio" & grade > 8 & original_subject == "Social Studies";

* Enrolled, proficiency levels;
gen enrolled = 1;
gen n_below_bsc = 1 if proficiency_level == "1. Below Basic" & subject != "U.S. History";

gen n_bsc = 1 if proficiency_level == "1. Below Proficient" & subject == "U.S. History";
replace n_bsc = 1 if proficiency_level == "2. Basic" & subject != "U.S. History";

gen n_prof = 1 if proficiency_level == "2. Proficient" & subject == "U.S. History";
replace n_prof = 1 if proficiency_level == "3. Proficient" & subject != "U.S. History";

gen n_adv = 1 if proficiency_level == "3. Advanced" & subject == "U.S. History";
replace n_adv = 1 if proficiency_level == "4. Advanced" & subject != "U.S. History";

* Create subgroup variables for collapse;
gen All = 1;
gen Asian = (race == "Asian");
gen Black = (race == "Black/African American");
gen Hispanic = (race == "Hispanic/Latino");
gen Hawaiian = (race == "Native Hawaiian/Pac. Islander");
gen Native = (race == "American Indian/Alaskan Native");
gen White = (race == "White");

rename (bhn_group economically_disadvantaged special_ed ell ell_t1_t2) (BHN ED SWD ELL ELL_T1_T2);
replace ELL_T1_T2 = 1 if ELL == 1;
gen Non_ED = (ED == 0);
gen Non_SWD = (SWD == 0);
gen Non_ELL = (ELL == 0);
gen Non_ELL_T1_T2 = (ELL == 0 & ELL_T1_T2 == 0);
gen Super = (BHN == 1 | ED == 1 | SWD == 1 | ELL == 1 | ELL_T1_T2 == 1);

* Collapse test proficiency by subject and subgroup;
quietly foreach s in All Asian Black Hispanic Hawaiian Native White BHN ED SWD ELL ELL_T1_T2 Non_ED Non_SWD Non_ELL Non_ELL_T1_T2 Super {;

	* All Grades;

	preserve;

	keep if `s' == 1;
	gen subgroup = "`s'";

	collapse (sum) enrolled tested valid_test n_below_bsc n_bsc n_prof n_adv, by(year original_subject subgroup);

	gen grade = "All Grades";

	tempfile `s'_all_grades;
	save ``s'_all_grades', replace;

	restore;
	
	* Individual Grades;
	
	preserve;

	keep if `s' == 1;
	gen subgroup = "`s'";

	collapse (sum) enrolled tested valid_test n_below_bsc n_bsc n_prof n_adv, by(year original_subject grade subgroup);
	
	tostring grade, replace;

	tempfile `s'_ind_grades;
	save ``s'_ind_grades', replace;

	restore;

};

clear;

foreach s in All Asian Black Hispanic Hawaiian Native White BHN ED SWD ELL ELL_T1_T2 Non_ED Non_SWD Non_ELL Non_ELL_T1_T2 Super {;
	
	append using ``s'_all_grades';
	append using ``s'_ind_grades';
		
};


* File Cleaning/Formatting;
rename (original_subject valid_test) (subject valid_tests);

order year subject grade subgroup, first;
replace grade = "Missing Grade" if grade == ".";
replace subject = "ELA" if subject == "Reading/Language";
replace subject = "US History" if subject == "U.S. History";

replace subgroup = "All Students" if subgroup == "All";
replace subgroup = "Black/Hispanic/Native American" if subgroup == "BHN";
replace subgroup = "Economically Disadvantaged" if subgroup == "ED";
replace subgroup = "English Language Learners" if subgroup == "ELL";
replace subgroup = "English Language Learners with T1/T2" if subgroup == "ELL_T1_T2";
replace subgroup = "Students with Disabilities" if subgroup == "SWD";
replace subgroup = "Non-Economically Disadvantaged" if subgroup == "Non_ED";
replace subgroup = "Non-English Language Learners" if subgroup == "Non_ELL";
replace subgroup = "Non-English Language Learners/T1 or T2" if subgroup == "Non_ELL_T1_T2";
replace subgroup = "Non-Students with Disabilities" if subgroup == "Non_SWD";
replace subgroup = "Super Subgroup" if subgroup == "Super";
replace subgroup = "Black or African American" if subgroup == "Black";
replace subgroup = "Native Hawaiian or Other Pacific Islander" if subgroup == "Hawaiian";
replace subgroup = "American Indian or Alaska Native" if subgroup == "Native";

* Generate %BB, B, P, A, BB+B, P+A;
foreach p in bsc prof adv {;

	gen pct_`p' = round(100 * n_`p'/valid_tests, 0.1);

};

gen pct_below_bsc = round(100 - pct_bsc - pct_prof - pct_adv, 0.1);
order pct_below_bsc, before(pct_bsc);

gen pct_prof_adv = round(100 * (n_prof + n_adv)/valid_tests, 0.1);

* Fix % BB/B/P if there are no n_BB, n_B, n_P;
gen flag_bb = 1 if pct_below_bsc != 0 & pct_below_bsc != . & n_below_bsc == 0;
replace pct_bsc = (100 - pct_prof - pct_adv) if flag_bb == 1;
replace pct_below_bsc = 0 if flag_bb == 1;

gen flag_bb_b = 1 if (pct_below_bsc != 0 & pct_below_bsc != .) & (pct_bsc != 0 & pct_bsc != .)
	& n_below_bsc == 0 & n_bsc == 0;
replace pct_prof = (100 - pct_adv) if flag_bb_b == 1;
replace pct_bsc = 0 if flag_bb_b == 1;

gen flag_bb_b_p = 1 if (pct_below_bsc != 0 & pct_below_bsc != .) & (pct_bsc != 0 & pct_bsc != .) & (pct_prof != 0 & pct_prof != .) & 
	n_below_bsc == 0 & n_bsc == 0 & n_prof == 0;
replace pct_adv = 100 if flag_bb_b_p == 1;
replace pct_prof = 0 if flag_bb_b_p == 1;

drop flag_bb flag_bb_b flag_bb_b_p;

* Check everything sums to 100;
egen pct_total = rowtotal(pct_below_bsc pct_bsc pct_prof pct_adv);
replace pct_total = round(pct_total, 0.1);
tab pct_total;
* Should all be 0 (if no valid tests) or 100;

drop pct_total;

tempfile base;
save `base', replace;

* ACT and Grad Rate;
foreach y in 2015 2016 {;

	use $act/ACT_state`y'.dta, clear;

	rename (n_21_orhigher pct_21_orhigher n_below19 pct_below19) (n_21_and_above pct_21_and_above n_below_19 pct_below_19);
	
	replace subject = "ACT Composite";
	
	keep year subject grade subgroup enrolled tested valid_tests n_21_and_above pct_21_and_above n_below_19 pct_below_19;
	
	tempfile act_`y';
	save `act_`y'', replace;
	
};

foreach y in 2015 2016 {;

	use $grad/state_grad_rate`y'.dta, clear;

	rename (drop_count drop_rate) (dropout_count dropout_rate);

	replace subgroup = "Black or African American" if subgroup == "Black";
	replace subgroup = "American Indian or Alaska Native" if subgroup == "Native American";
	replace subgroup = "Non-English Language Learners/T1 or T2" if subgroup == "Non-English Language Learners with T1/T2";
	replace subgroup = "Native Hawaiian or Other Pacific Islander" if subgroup == "Hawaiian or Pacific Islander";
	
	keep year subject grade subgroup grad_cohort grad_count grad_rate dropout_count dropout_rate;
	
	tempfile grad_`y';
	save `grad_`y'', replace;
	
};

* Append ACT and Grad to Base;
use `base', clear;

foreach y in 2015 2016 {;

	append using `act_`y'';

};

foreach y in 2015 2016 {;

	append using `grad_`y'';
	
};

* Create New Entries for missing subgroups (with 0 enrolled, valid tests, etc.);
replace subgroup = "All" if subgroup == "All Students";
replace subgroup = "Native" if subgroup == "American Indian or Alaska Native";
replace subgroup = "Black" if subgroup == "Black or African American";
replace subgroup = "BHN" if subgroup == "Black/Hispanic/Native American";
replace subgroup = "ED" if subgroup == "Economically Disadvantaged";
replace subgroup = "ELL" if subgroup == "English Language Learners";
replace subgroup = "ELL_T1_T2" if subgroup == "English Language Learners with T1/T2";
replace subgroup = "Hawaiian" if subgroup == "Native Hawaiian or Other Pacific Islander";
replace subgroup = "Non_ED" if subgroup == "Non-Economically Disadvantaged";
replace subgroup = "Non_ELL" if subgroup == "Non-English Language Learners";
replace subgroup = "Non_ELL_T1_T2" if subgroup == "Non-English Language Learners/T1 or T2";
replace subgroup = "Non_SWD" if subgroup == "Non-Students with Disabilities";
replace subgroup = "SWD" if subgroup == "Students with Disabilities";
replace subgroup = "Super" if subgroup == "Super Subgroup";

reshape wide enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv 
	pct_below_bsc pct_bsc pct_prof pct_adv pct_prof_adv
	n_21_and_above pct_21_and_above n_below_19 pct_below_19 
	grad_count grad_cohort grad_rate dropout_count dropout_rate, 
	i(year subject grade) j(subgroup) string;

foreach v in enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv
	pct_below_bsc pct_bsc pct_prof pct_adv pct_prof_adv 
	n_21_and_above pct_21_and_above n_below_19 pct_below_19 
	grad_count grad_cohort grad_rate dropout_count dropout_rate {;

	foreach s in All Asian Black Hispanic Hawaiian Native White BHN ED SWD ELL ELL_T1_T2 Non_ED Non_SWD Non_ELL Non_ELL_T1_T2 Super {;

		replace `v'`s' = 0 if `v'`s' ==.;

	};
};

reshape long;

replace subgroup = "All Students" if subgroup == "All";
replace subgroup = "American Indian or Alaska Native" if subgroup == "Native";
replace subgroup = "Black or African American" if subgroup == "Black";
replace subgroup = "Black/Hispanic/Native American" if subgroup == "BHN";
replace subgroup = "Economically Disadvantaged" if subgroup == "ED";
replace subgroup = "English Language Learners" if subgroup == "ELL";
replace subgroup = "English Language Learners with T1/T2" if subgroup == "ELL_T1_T2";
replace subgroup = "Native Hawaiian or Other Pacific Islander" if subgroup == "Hawaiian";
replace subgroup = "Non-Economically Disadvantaged" if subgroup == "Non_ED";
replace subgroup = "Non-English Language Learners" if subgroup == "Non_ELL";
replace subgroup = "Non-English Language Learners/T1 or T2" if subgroup == "Non_ELL_T1_T2";
replace subgroup = "Non-Students with Disabilities" if subgroup == "Non_SWD";
replace subgroup = "Students with Disabilities" if subgroup == "SWD";
replace subgroup = "Super Subgroup" if subgroup == "Super";

* No ACT data for non-accountability subgroups;
drop if subject == "ACT Composite" & (regexm(subgroup, "Non-") | subgroup == "American Indian or Alaska Native" | subgroup == "Asian" |
	subgroup == "Black or African American" | subgroup == "Hispanic" | subgroup == "Native Hawaiian or Other Pacific Islander" | subgroup == "White");

drop if subject == "Graduation Rate" & (subgroup == "English Language Learners" | subgroup == "Non-English Language Learners");

* Append ACT substitution;
append using $act_substitution;

order n_met_benchmark pct_met_benchmark, after(pct_below_19);

* Append 2014 data from SAS Base;
preserve;

import delim using "$SAS_base", clear;

keep if year == 2014;
drop if subject == "Graduation Rate";

replace subject = "ELA" if subject == "RLA";
replace subgroup = "Native Hawaiian or Other Pacific Islander" if subgroup == "Hawaiian or Pacific Islander";
replace subgroup = "American Indian or Alaska Native" if subgroup == "Native American";
replace subgroup = "Black or African American" if subgroup == "Black";

foreach v in enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv pct_below_bsc pct_bsc pct_prof pct_adv pct_prof_adv {;

	replace `v' = 0 if `v' == .;

};

drop system system_name school school_name pct_bsc_and_below;

tempfile 2014;
save `2014', replace;

* Append 2014 super subgroup from fall district preview;
import delim using $fall_preview, clear;

keep if year == 2014 & subgroup == "Super Subgroup";
drop if subject == "Graduation Rate";

replace subject = "ELA" if subject == "RLA";

keep year subject grade subgroup enrolled tested valid_tests 
	n_below_bsc n_bsc n_prof n_adv pct_below_bsc pct_bsc pct_prof pct_adv pct_prof_adv;

tempfile super_2014;
save `super_2014', replace;

restore;

append using `2014';
append using `super_2014';

* Clean and output base file;
gsort subject grade subgroup -year;

replace enrolled = . if subject == "ACT Composite" & subgroup != "All Students";
replace tested = . if subject == "ACT Composite" & subgroup != "All Students";

foreach v in n_below_bsc n_bsc n_prof n_adv pct_below_bsc pct_bsc pct_prof pct_adv pct_prof_adv {;

	replace `v' = . if subject == "ACT Composite";

};

foreach v in n_21_and_above pct_21_and_above n_below_19 pct_below_19 {;

	replace `v' = . if subject != "ACT Composite";

};

foreach v in enrolled tested valid_tests n_below_bsc n_bsc n_prof n_adv pct_below_bsc pct_bsc pct_prof pct_adv pct_prof_adv {;
	
	replace `v' = . if subject == "Graduation Rate";

};

foreach v in grad_count grad_cohort grad_rate dropout_count dropout_rate {;

	replace `v' = . if subject != "Graduation Rate";

};

gen system = 0;
gen system_name = "State of Tennessee";
order system system_name, after(year);

compress;

save "$output/state_base_with_super_subgroup_2016.dta", replace;
export delim using "$output/state_base_with_super_subgroup_2016.csv", delim(",") replace;
