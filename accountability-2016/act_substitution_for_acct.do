#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  ACT Substitution Data for District Accountability

Edited last by:  Alexander Poon

Date edited last:  2/16/2015
***************************************************************/

global act "K:\ORP_accountability\projects\2016_acct_modeling\ACT\TN_790Select_2015.dta";
global school_crosswalk "K:\ORP_accountability\projects\2015_fall_district_preview/ACT-SchCrosswalk2015rev.csv";

* ACT HS Crosswalk;
import delim using $school_crosswalk, clear;

keep acthscode sysno schno;

tempfile crosswalk;
save `crosswalk', replace;

* ACT data;
use $act, clear;

mmerge acthscode using `crosswalk', type(n:1);
drop if _merge == 2;
drop _merge;

rename (sysno schno) (system school);

preserve;

bysort state_stud_id: egen temp = max(act_eng);
drop if temp != act_eng;
drop temp;

tempfile act_student_level_eng;
save `act_student_level_eng', replace;

restore;

bysort state_stud_id: egen temp = max(act_math);
drop if temp != act_math;

duplicates tag state_stud_id, gen(dup);
bysort state_stud_id: egen temp2 = max(act_eng) if dup != 0;
drop if temp2 != act_eng & dup != 0;

drop temp;

tempfile act_student_level_math;
save `act_student_level_math', replace;

* Merging ACT onto Student Level file;
use "K:\ORP_accountability\data\2015_sas_accountability/state-student-level_2015_19jul2015.dta", clear;

destring state_student_id, replace force;
drop if state_student_id == .;

preserve;

keep if subject == "Algebra I" | subject == "Algebra II";

mmerge state_student_id using `act_student_level_math', type(n:1) umatch(state_stud_id);
keep if _merge == 2 & grade == 11 & act_math != .;

keep state_student_id last_name first_name middle_i grade system school yr_hs_grad act_math;

rename (yr_hs_grad act_math) (year act_subscore);

gen subject = "ACT Math";
gen n_met_benchmark = (act_subscore >= 22);
gen valid_tests = 1;

tempfile math;
save `math', replace;

restore;

keep if subject == "English I" | subject == "English II" | subject == "English III";

mmerge state_student_id using `act_student_level_eng', type(n:1) umatch(state_stud_id);
keep if _merge == 2 & grade == 11 & act_eng != .;

keep state_student_id last_name first_name middle_i grade system school yr_hs_grad act_eng;

rename (yr_hs_grad act_eng) (year act_subscore);

gen subject = "ACT English";
gen n_met_benchmark = (act_subscore >= 18);
gen valid_tests = 1;

append using `math';

collapse (sum) n_met_benchmark valid_tests, by(year system subject);

gen subgroup = "All Students";
gen grade = "11";

gen pct_met_benchmark = round(100 * n_met_benchmark/valid_tests, 0.1);

replace subject = "English" if subject == "ACT English";
replace subject = "Math" if subject == "ACT Math";

reshape wide valid_tests n_met_benchmark pct_met_benchmark, i(year system subgroup grade) j(subject) string;

foreach v in valid_testsEnglish n_met_benchmarkEnglish pct_met_benchmarkEnglish valid_testsMath n_met_benchmarkMath pct_met_benchmarkMath {;

	replace `v' = 0 if `v' == .;

};

reshape long;

replace subject = "ACT English" if subject == "English";
replace subject = "ACT Math" if subject == "Math";

order year system subject subgroup grade valid_tests n_met_benchmark pct_met_benchmark;

save "K:\ORP_accountability\projects\2016_pre_coding\Output/system_act_substitution.dta", replace;
export delim using "K:\ORP_accountability\projects\2016_pre_coding\Output/system_act_substitution.csv", delim(",") replace;


* State;
collapse (sum) valid_tests n_met_benchmark, by(year subject subgroup grade);

gen pct_met_benchmark = round(100 * n_met_benchmark/valid_tests, 0.1);

save "K:\ORP_accountability\projects\2016_pre_coding\Output/state_act_substitution.dta", replace;
export delim using "K:\ORP_accountability\projects\2016_pre_coding\Output/state_act_substitution.csv", delim(",") replace;
