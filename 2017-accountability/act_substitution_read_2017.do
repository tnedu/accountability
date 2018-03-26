#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  2017 ACT Substitution for District Accountability

Edited last by:  Alexander Poon

Date edited last:  7/31/2017
***************************************************************/

global act "N:\Assessment_Data Returns\ACT\2016-17\Junior Day File/20170713_ACT_JuniorDayResults_SY2016-17_Whalen_v1.dta";
global school_crosswalk "N:\ORP_accountability\projects\2015_fall_district_preview/ACT-SchCrosswalk2015rev.csv";

* ACT HS Crosswalk;
import delim using $school_crosswalk, clear;

keep acthscode sysno schno;

tempfile crosswalk;
save `crosswalk', replace;

* ACT data;
use "$act", clear;

drop if state_stud_id == .;
drop if test_location == "M";

mmerge acthscode using `crosswalk', type(n:1);
keep if _merge == 3;
drop _merge;

rename (sysno schno) (system school);

preserve;

bysort state_stud_id: egen temp = max(act_read);
drop if temp != act_read;
drop temp;

duplicates drop state_stud_id, force;

tempfile act_student_read;
save `act_student_read', replace;

restore;

bysort state_stud_id: egen temp = max(act_math);
drop if temp != act_math;

duplicates tag state_stud_id, gen(dup);
bysort state_stud_id: egen temp2 = max(act_math) if dup != 0;
drop if temp2 != act_math & dup != 0;
drop temp;

duplicates drop state_stud_id, force;

tempfile act_student_math;
save `act_student_math', replace;

* Merging ACT onto Student Level file;
use "N:\ORP_accountability\projects\2017_student_level_file\state_student_level_2017_JP_final_09142017.dta", clear;

preserve;

keep if subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" |
	subject == "Integrated Math I" | subject == "Integrated Math II" | subject == "Integrated Math III";

mmerge id using `act_student_math', type(n:1) umatch(state_stud_id);
keep if _merge == 2 & grade == 11 & act_math != .;

keep id last_name first_name grade system school act_math;

rename act_math act_subscore;

gen subject = "ACT Math";
gen n_met_benchmark = (act_subscore >= 22);
gen n_not_met_benchmark = (act_subscore < 22);
gen valid_tests = 1;

tempfile math;
save `math', replace;

restore;

keep if subject == "English I" | subject == "English II" | subject == "English III";

mmerge id using `act_student_read', type(n:1) umatch(state_stud_id);
keep if _merge == 2 & grade == 11 & act_read != .;

keep id last_name first_name grade system school act_read;

rename act_read act_subscore;

gen subject = "ACT Reading";
gen n_met_benchmark = (act_subscore >= 22);
gen n_not_met_benchmark = (act_subscore < 22);
gen valid_tests = 1;

append using `math';

* Save student level ACT substitution file;
gsort system school id subject;
export delim "N:\ORP_accountability\data\2017_final_accountability_files/student_level_act_substitution.csv", delim(",") replace;

* Collapse for school, system, and state ACT substitution files;
collapse (sum) n_met_benchmark n_not_met_benchmark valid_tests, by(system school subject);

gen subgroup = "All Students";
gen grade = "11";

gen pct_met_benchmark = round(1000 * n_met_benchmark/valid_tests)/10;
gen pct_not_met_benchmark = 100 - pct_met_benchmark;

replace subject = "Reading" if subject == "ACT Reading";
replace subject = "Math" if subject == "ACT Math";

reshape wide valid_tests n_not_met_benchmark n_met_benchmark pct_not_met_benchmark pct_met_benchmark, i(system school subgroup grade) j(subject) string;

foreach v in valid_testsReading n_met_benchmarkReading n_not_met_benchmarkReading pct_not_met_benchmarkReading pct_met_benchmarkReading
	valid_testsMath n_met_benchmarkMath n_not_met_benchmarkMath pct_not_met_benchmarkMath pct_met_benchmarkMath {;

	replace `v' = 0 if `v' == .;

};

reshape long;

replace subject = "ACT Reading" if subject == "Reading";
replace subject = "ACT Math" if subject == "Math";

gen year = 2017;

order year system school subject subgroup grade valid_tests n_not_met_benchmark n_met_benchmark pct_not_met_benchmark pct_met_benchmark;

save "N:\ORP_accountability\data\2017_ACT\school_act_substitution_2017.dta", replace;
export delim using "N:\ORP_accountability\data\2017_ACT\school_act_substitution_2017.csv", delim(",") replace;

* System;

collapse (sum) valid_tests n_not_met_benchmark n_met_benchmark, by(year system subject subgroup grade);

gen pct_met_benchmark = round(1000 * n_met_benchmark/valid_tests)/10;
gen pct_not_met_benchmark = 100 - pct_met_benchmark;

save "N:\ORP_accountability\data\2017_ACT\system_act_substitution_2017.dta", replace;
export delim using "N:\ORP_accountability\data\2017_ACT\system_act_substitution_2017.csv", delim(",") replace;

* State;
collapse (sum) valid_tests n_not_met_benchmark n_met_benchmark, by(year subject subgroup grade);

gen pct_met_benchmark = round(1000 * n_met_benchmark/valid_tests)/10;
gen pct_not_met_benchmark = 100 - pct_met_benchmark;

save "N:\ORP_accountability\data\2017_ACT/state_act_substitution_2017.dta", replace;
export delim using "N:\ORP_accountability\data\2017_ACT/state_act_substitution_2017.csv", delim(",") replace;
