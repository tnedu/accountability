#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Edfacts C173

Edited last by:  Alexander Poon

Date edited last: 12/2/2016
***************************************************************/

* Prior Grad Rate to create targets;
use "K:\ORP_accountability\data\2015_graduation_rate/district_grad_rate2015.dta", clear;

keep if subgroup == "Black/Hispanic/Native American";

gen grad_target = round(grad_rate + (100 - grad_rate)/16, 0.1) if grad_cohort >= 30;

keep system grad_target;

tempfile targets;
save `targets', replace;

* System;
use "K:\ORP_accountability\data\2015_graduation_rate/district_grad_rate2016.dta", clear;

keep if subgroup == "Black/Hispanic/Native American";
replace subgroup = "HDS";

mmerge system using `targets', type(1:1);
keep if _merge == 3;
drop _merge;

gen status = "NOSTUDENTS" if grad_cohort == 0;
replace status = "TOOFEW" if status == "" & grad_cohort < 30;
replace status = "METGOAL" if status == "" & grad_rate >= 95;
replace status = "METTARGET" if status == "" & grad_rate >= grad_target & grad_rate != .;
replace status = "NOTMETADDIND" if status == "" & grad_rate < 95;

drop if system == 970;

keep system subgroup status;

egen first = seq();
gen state = 47;
gen agency = "01";
gen school = "";
gen FLEXHSGRDRTIND = "FLEXHSGRDRTIND";
gen a = "";
gen b = "";
gen explanation = "";

order first state agency system school FLEXHSGRDRTIND subgroup a b explanation status;

tostring system, replace;
replace system = "0" + system if real(system) < 100;

* export delimited "H:\EDEN Data\EDEN 15-16\Done\C173/TNLEAFLXSTATUSc173y16.csv", delim(",") novarnames replace;

* Prior Grad Rate to create targets;
use "K:\ORP_accountability\data\2015_graduation_rate/school_grad_rate2015.dta", clear;

* School grad;
keep if subgroup == "Black/Hispanic/Native American";

gen grad_target = round(grad_rate + (100 - grad_rate)/16, 0.1) if grad_cohort >= 30;

keep system school grad_target;

tempfile targets_school;
save `targets_school', replace;

use "K:\ORP_accountability\data\2015_graduation_rate/school_grad_rate2016.dta", clear;

keep if subgroup == "Black/Hispanic/Native American";
replace subgroup = "HDS";

mmerge system school using `targets_school', type(1:1);
keep if _merge == 3;
drop _merge;

gen status = "NOSTUDENTS" if grad_cohort == 0;
replace status = "TOOFEW" if status == "" & grad_cohort < 30;
replace status = "METGOAL" if status == "" & grad_rate >= 95;
replace status = "METTARGET" if status == "" & grad_rate >= grad_target & grad_rate != .;
replace status = "NOTMETADDIND" if status == "" & grad_rate < 95;

replace subject = "FLEXHSGRDRTIND";

keep system school subject subgroup status;

tempfile school_grad;
save `school_grad', replace;

* School participation rate;
use "K:\ORP_accountability\data\2016_accountability/school_base_with_super_subgroup_2016.dta", clear;

keep if subgroup == "Black/Hispanic/Native American";
replace subgroup = "HDS";

keep if subject == "Algebra I" | subject == "English II";
replace subject = "FLEXMATPARST" if subject == "Algebra I";
replace subject = "FLEXRLAPARST" if subject == "English II";

drop if grade == "All Grades";

collapse (sum) enrolled enrolled_part_1 enrolled_part_2 enrolled_both tested tested_part_1_only tested_part_2_only tested_both, by(system school subject subgroup);

* Participation Rate;
gen participation_rate = round(100 * (tested + tested_part_1_only + tested_part_2_only + tested_both)/(enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both));

gen status = "NOSTUDENTS" if enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both == 0;
replace status = "TOOFEW" if status == "" & enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both < 30;
replace status = "NOTMET" if status == "" & participation_rate < 95;
replace status = "MET" if status == "" & participation_rate >= 95;

drop enrolled* tested* participation_rate;

append using `school_grad';

drop if system == 970;

preserve;

import excel using "H:\EDEN Data\EDEN 15-16\Done\C173/2015-16 EDFacts School Master FIle_11-18-16.xlsx", firstrow clear;

keep STATE_LEAID STATE_SCHID;

destring STATE_LEAID STATE_SCHID, replace;

tempfile school_pool;
save `school_pool', replace;

restore;

mmerge system school using `school_pool', type(n:1) umatch(STATE_LEAID STATE_SCHID);
keep if _merge == 3;
drop _merge;

egen first = seq();
gen state = 47;
gen agency = "01";
gen a = "";
gen b = "";
gen explanation = "";

order first state agency system school subject subgroup a b explanation status;

compress;

tostring system school, replace;
replace system = "0" + system if real(system) < 100;
replace school = "000" + school if real(school) < 10;
replace school = "00" + school if real(school) >= 10 & real(school) < 100;
replace school = "0" + school if real(school) >= 100 & real(school) < 1000;

* export delimited "H:\EDEN Data\EDEN 15-16\Done\C173/TNSCHFLXSTATUSc173y16.csv", delim(",") novarnames replace;
