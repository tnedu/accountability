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

Date edited last: 11/30/2016
***************************************************************/

global input "K:\ORP_accountability\projects\2016_dictionary_coding";

use $input/fall_math_english_cdf.dta, clear;
append using $input/spring_math_english_cdf.dta;

keep if el_t1_t2 == 1 | el_t1_t2 == 2;
drop if performance_level == .;

keep if content_area_code_final == "A1" | content_area_code_final == "E2";

rename (system_final content_area_code_final) (system subject);
keep system subject el_t1_t2 performance_level;

tempfile math_english;
save `math_english', replace;


use $input/2016_State_Student_Fall_File_science.dta, clear;
append using $input/2016_State_Student_Spring_file_science.dta;

keep if ell_t1_t2 == 1 | ell_t1_t2 == 2;
drop if perf_level_total_sci == .;

keep if content_area_code == "B1";

rename (system_number ell_t1_t2 content_area_code perf_level_total_sci) (system el_t1_t2 subject performance_level); 
keep system subject el_t1_t2 performance_level; 

append using `math_english';

* System proficiency counts;
replace subject = "M" if subject == "A1";
replace subject = "RLA" if subject == "E2";
replace subject = "S" if subject == "B1";

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

* Test taker counts by district from Trish's file;
preserve;

import excel "H:\EDEN Data\EDEN 15-16\Done\C126\T1T2 Counts/C126_1516_T1T2_COUNTS_11-22-16.xlsx", firstrow sheet(LEA) clear;

rename leaid system;
drop year;
drop if system == .;

reshape wide count, i(system) j(elb) string;

replace count1YEAR = 0 if count1YEAR == .;
replace count2YEAR = 0 if count2YEAR == .;

reshape long;

tempfile takers_system;
save `takers_system', replace;

restore;

append using `takers_system';

tempfile predrop;
save `predrop', replace;

* For district file, omit districts without a Title III LEP program;
preserve;

import excel "H:\EDEN Data\EDEN 15-16\Done\C126\T1T2 Counts/C126_1516_T1T2_COUNTS_11-22-16.xlsx", firstrow sheet(TITLEIII_DISTRICTS_1516) clear;

drop if LEAID == .;
drop if TitleIII == "N";

keep LEAID;

tempfile title_III;
save `title_III', replace;

restore;

mmerge system using `title_III', type(n:1) umatch(LEAID);
keep if _merge == 3;
drop _merge;

gsort system -subject elb -proficiency;

gen state = 47;
egen first = seq();
gen agency = "01";
gen n = "N";
gen LEPFORSTU = "LEPFORSTU";
gen x = .;
gen y = .;
gen z = .;

order first state agency system x LEPFORSTU y elb subject proficiency n z count;

tostring system, replace;
replace system = "0" + system if real(system) < 100;

* export delimited "H:\EDEN Data\EDEN 15-16\Done\C126/TNLEALEPFORSTU2016-01.csv", delim(",") novarnames replace;

* State (includes districts without Title III LEP program as per file specifications);
use `predrop', clear;

drop if elb != "";

preserve;

import excel "H:\EDEN Data\EDEN 15-16\Done\C126\T1T2 Counts/C126_1516_T1T2_COUNTS_11-22-16.xlsx", firstrow sheet(SEA) cellrange(B1:C3) clear;

tempfile takers_state;
save `takers_state', replace;

restore;

collapse (sum) count, by(subject proficiency);

append using `takers_state';

gen state = 47;
egen first = seq();
gen agency = "01";
gen n = "N";
gen LEPFORSTU = "LEPFORSTU";
gen w = .;
gen x = .;
gen y = .;
gen z = .;

order first state agency w x LEPFORSTU y elb subject proficiency n z count;

* export delimited "H:\EDEN Data\EDEN 15-16\Done\C126/TNSEALEPFORSTU2016-01.csv", delim(",") novarnames replace;
