#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Edfacts C108

Edited last by:  Alexander Poon

Date edited last: 12/1/2016
***************************************************************/

* System;
use "K:\ORP_accountability\projects\2016_state_results/system_base_with_super_subgroup_2016.dta", clear;

* Drop all grades, collapse to manually create grade combinations;
drop if grade == "All Grades";

keep if subject == "Algebra I";

collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both, by(year system subject subgroup);

* Participation Rate;
gen participation_rate = round(100 * (tested + tested_part_1_only + tested_part_2_only + tested_both)/(enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both));

replace subgroup = "" if subgroup == "All Students";
replace subgroup = "MA" if subgroup == "Asian";
replace subgroup = "MAN" if subgroup == "American Indian or Alaska Native";
replace subgroup = "MB" if subgroup == "Black or African American";
replace subgroup = "MHL" if subgroup == "Hispanic";
replace subgroup = "MNP" if subgroup == "Native Hawaiian or Other Pacific Islander";
replace subgroup = "MW" if subgroup == "White";
replace subgroup = "WDIS" if subgroup == "Students with Disabilities";
replace subgroup = "LEP" if subgroup == "English Learners with T1/T2";
replace subgroup = "ECODIS" if subgroup == "Economically Disadvantaged";

drop if subgroup == "Black/Hispanic/Native American" | subgroup == "English Learners" | subgroup == "Non-Economically Disadvantaged" | 
	subgroup == "Non-English Learners" | subgroup == "Non-English Learners/T1 or T2" | subgroup == "Non-Students with Disabilities" | subgroup == "Super Subgroup";

gen status = "NOSTUDENTS" if enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both == 0;
replace status = "TOOFEW" if status == "" & enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both < 30;
replace status = "NOTMET" if status == "" & participation_rate < 95;
replace status = "MET" if status == "" & participation_rate >= 95;

drop year subject enrolled* tested* participation_rate;

gen All = "All" if subgroup == "";
gen WDIS = "WDIS" if subgroup == "WDIS";
gen LEP = "LEP" if subgroup == "LEP";
gen ECODIS = "ECODIS" if subgroup == "ECODIS";

replace subgroup = "" if subgroup == "LEP" | subgroup == "WDIS" | subgroup == "ECODIS";

gsort ECODIS LEP WDIS subgroup system;

drop if system == 970;

egen first = seq();
gen state = 47;
gen agency = "01";
gen school = "";
gen MTHPRTSTS = "MTHPRTSTS";
gen y = "";
gen z = "";
gen a = "";
gen b = "";
gen explanation = "";

order first state agency system school MTHPRTSTS All subgroup y WDIS LEP z ECODIS a b explanation status;

replace All = "";

compress;

tostring system, replace;
replace system = "0" + system if real(system) < 100;

* export delimited "H:\EDEN Data\EDEN 15-16\Done\C108/TNLEAMTHPRTSTc108y16.csv", delim(",") novarnames replace;


* School;
use "K:\ORP_accountability\projects\2016_state_results/school_base_with_super_subgroup_2016.dta", clear;

* Drop all grades, collapse to manually create grade combinations;
drop if grade == "All Grades";

keep if subject == "Algebra I";

collapse (sum) enrolled enrolled_part_1_only enrolled_part_2_only enrolled_both tested tested_part_1_only tested_part_2_only tested_both, by(year system school subject subgroup);

* Participation Rate;
gen participation_rate = round(100 * (tested + tested_part_1_only + tested_part_2_only + tested_both)/(enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both));

replace subgroup = "" if subgroup == "All Students";
replace subgroup = "MA" if subgroup == "Asian";
replace subgroup = "MAN" if subgroup == "American Indian or Alaska Native";
replace subgroup = "MB" if subgroup == "Black or African American";
replace subgroup = "MHL" if subgroup == "Hispanic";
replace subgroup = "MNP" if subgroup == "Native Hawaiian or Other Pacific Islander";
replace subgroup = "MW" if subgroup == "White";
replace subgroup = "WDIS" if subgroup == "Students with Disabilities";
replace subgroup = "LEP" if subgroup == "English Learners with T1/T2";
replace subgroup = "ECODIS" if subgroup == "Economically Disadvantaged";

drop if subgroup == "Black/Hispanic/Native American" | subgroup == "English Learners" | subgroup == "Non-Economically Disadvantaged" | 
	subgroup == "Non-English Learners" | subgroup == "Non-English Learners/T1 or T2" | subgroup == "Non-Students with Disabilities" | subgroup == "Super Subgroup";

gen status = "NOSTUDENTS" if enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both == 0;
replace status = "TOOFEW" if status == "" & enrolled + enrolled_part_1_only + enrolled_part_2_only + enrolled_both < 30;
replace status = "NOTMET" if status == "" & participation_rate < 95;
replace status = "MET" if status == "" & participation_rate >= 95;

drop year subject enrolled* tested* participation_rate;

gen All = "All" if subgroup == "";
gen WDIS = "WDIS" if subgroup == "WDIS";
gen LEP = "LEP" if subgroup == "LEP";
gen ECODIS = "ECODIS" if subgroup == "ECODIS";

replace subgroup = "" if subgroup == "LEP" | subgroup == "WDIS" | subgroup == "ECODIS";

gsort ECODIS LEP WDIS subgroup system school;

drop if system == 970;

gen state = 47;
egen first = seq();
gen agency = "01";
gen MTHPRTSTS = "MTHPRTSTS";
gen y = .;
gen z = .;
gen a = .;
gen b = .;
gen explanation = "";

order first state agency system school MTHPRTSTS All subgroup y WDIS LEP z ECODIS a b explanation status;

replace All = "";

compress;

tostring system school, replace;
replace system = "0" + system if real(system) < 100;
replace school = "000" + school if real(school) < 10;
replace school = "00" + school if real(school) >= 10 & real(school) < 100;
replace school = "0" + school if real(school) >= 100 & real(school) < 1000;

* export delimited "H:\EDEN Data\EDEN 15-16\Done\C108/TNSCHMTHPRTSTc108y16.csv", delim(",") novarnames replace;
