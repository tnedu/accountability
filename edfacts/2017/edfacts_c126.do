#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Edfacts C126

Edited last by:  Alexander Poon

Date edited last: 11/7/2017
***************************************************************/

* Fall EOC;
use "K:/ORP_accountability/data/2017_cdf/fall_eoc_cdf_081517.dta", clear;
keep if content_area_code == "A1" | content_area_code == "B1" | content_area_code == "E2";
keep if el_t1_t2 == 1 | el_t1_t2 == 2;

drop if ri_status_part_1 == 5 | ri_status_part_2 == 5;
drop if scale_score == .;
drop if system == .;

keep system unique_student_id content_area_code el_t1_t2 performance_level;

tempfile fall;
save `fall', replace;

* Spring EOC;
use "K:/ORP_accountability/data/2017_cdf/Spring_EOC_CDF_101117.dta", clear;
keep if content_area_code == "A1" | content_area_code == "B1" | content_area_code == "E2";
keep if el_t1_t2 == 1 | el_t1_t2 == 2;

drop if ri_status_final == 5;
drop if scale_score == .;
drop if system == .;

keep system unique_student_id content_area_code el_t1_t2 performance_level;

tempfile spring;
save `spring', replace;

* 3-8;
use "K:/ORP_accountability/data/2017_cdf/38_cdf_091417.dta", clear;

keep if content_area_code == "ENG" | content_area_code == "MAT" | content_area_code == "SCI";
keep if el_t1_t2 == 1 | el_t1_t2 == 2;

drop if ri_status_final == 5;
drop if scale_score == .;
drop if system == .;

keep system unique_student_id content_area_code el_t1_t2 performance_level;

append using `fall';
append using `spring';

preserve;

* Proficiency counts by system and state;
gen subject = "M" if content_area_code == "A1" | content_area_code == "MAT";
replace subject = "RLA" if content_area_code == "E2" | content_area_code == "ENG";
replace subject = "S" if content_area_code == "B1" | content_area_code == "SCI";

gen proficiency = "PROFICIENT" if performance_level == 3 | performance_level == 4;
replace proficiency = "NOTPROFICIENT" if performance_level == 1 | performance_level == 2;

gen count = 1;

collapse (sum) count, by(system subject proficiency);

reshape wide count, i(system subject) j(proficiency) string;

replace countPROFICIENT = 0 if countPROFICIENT == .;
replace countNOTPROFICIENT = 0 if countNOTPROFICIENT == .;

reshape long;

reshape wide count, i(system proficiency) j(subject) string;

replace countM = 0 if countM == .;
replace countRLA = 0 if countRLA == .;
replace countS = 0 if countS == .;

reshape long;

tempfile prof_system;
save `prof_system', replace;

collapse (sum) count, by(proficiency subject);

tempfile prof_state;
save `prof_state', replace;

restore;

gen countT1 = el_t1_t2 == 1;
gen countT2 = el_t1_t2 == 2;

duplicates drop unique_student_id, force;

collapse (sum) countT1 countT2, by(system);

reshape long count, i(system) j(el_t1_t2) string;

tempfile counts_system;
save `counts_system', replace;

reshape wide;

collapse (sum) countT1 countT2;

gen system = 0;

reshape long count, i(system) j(el_t1_t2) string;

tempfile counts_state;
save `counts_state', replace;

* For district file, omit districts without a Title III LEP program;
preserve;

import excel "H:\EDEN Data\EDEN 15-16\Done\C126\T1T2 Counts/C126_1516_T1T2_COUNTS_11-22-16.xlsx", firstrow sheet(TITLEIII_DISTRICTS_1516) clear;

import excel "K:\ORP_accountability\projects\Jessica\Data Returns\Data\WIDA\EdFacts\title III districts\16-17/TITLEIII_DISTRICTS_2016-17_FOR_CPM_9-11-17.xlsx", firstrow clear;

drop if leaid == .;
drop if titleiii == "N";

keep leaid;

tempfile title_III;
save `title_III', replace;

restore;

* Put together district file;
use `counts_system', clear;
append using `prof_system';

replace el_t1_t2 = "1YEAR" if el_t1_t2 == "T1";
replace el_t1_t2 = "2YEAR" if el_t1_t2 == "T2";

mmerge system using `title_III', type(n:1) umatch(leaid);
keep if _merge == 3;
drop _merge;

gsort system -subject el_t1_t2 -proficiency;

gen state = 47;
egen first = seq();
gen agency = "01";
gen n = "N";
gen LEPFORSTU = "LEPFORSTU";
gen x = .;
gen y = .;
gen z = .;

order first state agency system x LEPFORSTU y el_t1_t2 subject proficiency n z count;

tostring system, replace;
replace system = "000" + system if real(system) < 100;
replace system = "00" + system if real(system) >= 100 & real(system) < 1000;

export delimited "H:\EDEN Data\EDEN 16-17\Done\C126/TNLEALEPFORSTU2017-01.csv", delim(",") novarnames replace;


* State (includes districts without Title III LEP program as per file specifications);
use `counts_state', clear;
append using `prof_state';

replace el_t1_t2 = "1YEAR" if el_t1_t2 == "T1";
replace el_t1_t2 = "2YEAR" if el_t1_t2 == "T2";

drop system;

gen state = 47;
egen first = seq();
gen agency = "01";
gen n = "N";
gen LEPFORSTU = "LEPFORSTU";
gen w = .;
gen x = .;
gen y = .;
gen z = .;

order first state agency w x LEPFORSTU y el_t1_t2 subject proficiency n z count;

export delimited "H:\EDEN Data\EDEN 16-17\Done\C126/TNSEALEPFORSTU2017-01.csv", delim(",") novarnames replace;
