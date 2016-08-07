#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  System Numeric to Master Heat Maps

Edited last by:  Alexander Poon

Date edited last:  04/25/2016

Outstanding issues:  
***************************************************************/

global numeric "K:\ORP_accountability\projects\2016_pre_coding\Output/system_numeric_with_super_subgroup_2016.dta";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;
local lag_yr = `current_yr' - 1;

use $numeric, clear;

keep if year == `current_yr';

* Participation rate;
gen participation_test_eligible = (enrolled >= 30) if enrolled != . & subject != "Graduation Rate";

gen participation_test = (participation_rate_1yr >= 95 & participation_rate_1yr != .) |
	(participation_rate_2yr >= 95 & participation_rate_2yr != .) |
	(participation_rate_3yr >= 95 & participation_rate_3yr != .)
	if participation_test_eligible == 1 & subject != "ACT Composite" & subject != "Graduation Rate";

replace participation_test = (participation_rate_1yr >= 80 & participation_rate_1yr != .) 
	if participation_test_eligible == 1 & subject == "ACT Composite";

keep system system_name subject subgroup grade participation_test_eligible participation_test;

bysort system subgroup: egen subjects_eligible = sum(participation_test_eligible);
bysort system subgroup: egen subjects_passed = sum(participation_test);

collapse subjects_eligible subjects_passed, by(system system_name subgroup);

gen participation_test = (subjects_passed == subjects_eligible) if subjects_eligible != 0;
drop subjects_eligible subjects_passed;

replace subgroup = "All" if subgroup == "All Students";
replace subgroup = "BHN" if subgroup == "Black/Hispanic/Native American";
replace subgroup = "ED" if subgroup == "Economically Disadvantaged";
replace subgroup = "SWD" if subgroup == "Students with Disabilities";
replace subgroup = "ELL" if subgroup == "English Language Learners";
replace subgroup = "Super" if subgroup == "Super Subgroup";

reshape wide participation_test, i(system system_name) j(subgroup) string;

* Export participation master file;
save $output/participation_test_master.dta, replace;


* Minimum Performance Goal - Achievement Key;
use $numeric, clear;

keep if subgroup == "All Students";
keep year system system_name subgroup subject grade valid_tests PA_percentile;

reshape wide valid_tests PA_percentile, i(system system_name subgroup subject grade) j(year);

gen eligible_achievement = (valid_tests`current_yr' >= 30 & valid_tests`current_yr' != . & valid_tests`lag_yr' >= 30 & valid_tests`lag_yr' != .);
gen achievement_key = (PA_percentile`current_yr' - PA_percentile`lag_yr' >= -2) if eligible == 1;

keep system system_name subject grade eligible_achievement achievement_key;

tempfile gate_ach;
save `gate_ach', replace;

* Minimum Performance Goal - Gap Closure Key;
use $numeric, clear;

keep if subgroup == "Super Subgroup";
keep year system system_name subgroup subject grade valid_tests tvaas_level BB_percentile;

reshape wide valid_tests tvaas_level BB_percentile, i(system system_name subgroup subject grade) j(year);

gen eligible_gap_closure = (valid_tests`current_yr' >= 30 & valid_tests`current_yr' != . & valid_tests`lag_yr' >= 30 & valid_tests`lag_yr' != .);

gen gap_BB_reduction_key = (BB_percentile`current_yr' - BB_percentile`lag_yr' <= 2) if eligible_gap_closure == 1;
gen gap_tvaas_key = (tvaas_level`current_yr' == "Level 3" | tvaas_level`current_yr' == "Level 4" | tvaas_level`current_yr' == "Level 5") 
	if eligible_gap_closure == 1 & subject != "Graduation Rate";

gen gap_closure_key = (gap_BB_reduction_key == 1 | gap_tvaas_key == 1) if eligible_gap_closure == 1;

keep system system_name subject grade eligible_gap_closure gap_BB_reduction_key gap_tvaas_key gap_closure_key;

tempfile gate_gap;
save `gate_gap', replace;

* Minimum Performance Goal - TVAAS Key;
use $numeric, clear;

keep if subgroup == "All Students";
keep if year == `current_yr' | year == `lag_yr';

gsort system system_name subject grade subgroup -year;

gen valid_tests_lag = valid_tests[_n+1] if year == `current_yr' & 
	system == system[_n+1] & subject == subject[_n+1] & grade == grade[_n+1] & subgroup == subgroup[_n+1];

drop if year == `lag_yr';

gen eligible_tvaas = (valid_tests >= 30 & valid_tests != . & valid_tests_lag >= 30 & valid_tests_lag != .) if subject != "Graduation Rate";
gen tvaas_key = (tvaas_level == "Level 3" | tvaas_level == "Level 4" | tvaas_level == "Level 5") if eligible_tvaas == 1 & subject != "Graduation Rate";

keep system system_name subject grade eligible_tvaas tvaas_key;

tempfile gate_tvaas;
save `gate_tvaas', replace;

* Merge three gate keys together;
use `gate_ach', clear;

mmerge system system_name subject grade using `gate_tvaas', type(1:1);
drop _merge;
mmerge system system_name subject grade using `gate_gap', type(1:1);
drop _merge;

* Create new entries for those that don't currently exist;
replace subject = "35Math" if subject == "Math" & grade == "3rd through 5th";
replace subject = "68Math" if subject == "Math" & grade == "6th through 8th";
replace subject = "35ELA" if subject == "ELA" & grade == "3rd through 5th";
replace subject = "68ELA" if subject == "ELA" & grade == "6th through 8th";
replace subject = "HSEnglish" if subject == "HS English";
replace subject = "HSMath" if subject == "HS Math";
replace subject = "ACT" if subject == "ACT Composite";
replace subject = "Grad" if subject == "Graduation Rate";

drop grade;
reshape wide eligible_* *_key, i(system system_name) j(subject) string;

ds eligible* achievement_key* gap_*_key* tvaas_key*;
local variables = r(varlist);

foreach v in `variables' {;

	replace `v' = 0 if `v' == .;
};

reshape long;

replace gap_tvaas_key = . if subject == "Grad";
replace eligible_tvaas = . if subject == "Grad";
replace tvaas_key = . if subject == "Grad";

foreach k in achievement gap_closure tvaas {;

	replace `k'_key = . if eligible_`k' == 0;

};

gen grade = "3rd through 5th" if subject == "35Math" | subject == "35ELA";
replace grade = "6th through 8th" if subject == "68Math" | subject == "68ELA";
replace grade = "9th through 12th" if subject == "HSMath" | subject == "HSEnglish";
replace grade = "All Grades" if subject == "ACT" | subject == "Grad";

gsort system system_name grade subject;
drop grade;

replace subject = "3-5 Math" if subject == "35Math";
replace subject = "6-8 Math" if subject == "68Math";
replace subject = "3-5 ELA" if subject == "35ELA";
replace subject = "6-8 ELA" if subject == "68ELA";
replace subject = "High School Math" if subject == "HSMath";
replace subject = "High School English" if subject == "HSEnglish";
replace subject = "ACT Composite" if subject == "ACT";
replace subject = "Graduation Rate" if subject == "Grad";

tostring achievement_key tvaas_key gap_BB_reduction_key gap_tvaas_key gap_closure_key, replace;

foreach v in achievement_key tvaas_key gap_BB_reduction_key gap_tvaas_key gap_closure_key {;

	replace `v' = "Yes" if `v' == "1";
	replace `v' = "No" if `v' == "0";

};

* Export minimum performance gate master file;
save $output/performance_gate_master.dta, replace;


* Achievement Heat Maps;
use $numeric, clear;

keep if subgroup == "All Students";
keep if year == `current_yr' | year == `lag_yr';

gsort system system_name subject grade subgroup -year;

gen eligible = valid_tests >= 30 & valid_tests != . & valid_tests[_n+1] >= 30 & valid_tests[_n+1] != . if year == `current_yr'
	& system == system[_n+1] & subject == subject[_n+1] & grade == grade[_n+1] & subgroup == subgroup[_n+1];
gen PA_percentile_diff = (PA_percentile - PA_percentile[_n+1]) if eligible == 1 & year == `current_yr' &
	system == system[_n+1] & subject == subject[_n+1] & grade == grade[_n+1] & subgroup == subgroup[_n+1];

gen achievement_goal = 4 if eligible == 1 & PA_percentile_diff >= 10 & PA_percentile_diff != .;
replace achievement_goal = 3 if achievement_goal == . & eligible == 1 & PA_percentile_diff > 0 & PA_percentile_diff < 10;
replace achievement_goal = 3 if achievement_goal == . & eligible == 1 & 
	PA_percentile >= 95 & PA_percentile != . & PA_percentile[_n+1] >= 95 & PA_percentile[_n+1] != .; /* Safe Harbor for scoring above 95th %ile */
replace achievement_goal = 2 if achievement_goal == . & eligible == 1 & PA_percentile_diff >= -2 & PA_percentile_diff <= 0;
replace achievement_goal = 1 if achievement_goal == . & eligible == 1 & PA_percentile_diff < -2 & PA_percentile_diff >= -10;
replace achievement_goal = 0 if achievement_goal == . & eligible == 1 & PA_percentile_diff < -10;

drop if year == `lag_yr';

gen tvaas_goal = 4 if eligible == 1 & tvaas_level == "Level 5";
replace tvaas_goal = 3 if eligible == 1 & tvaas_level == "Level 4";
replace tvaas_goal = 2 if eligible == 1 & tvaas_level == "Level 3";
replace tvaas_goal = 1 if eligible == 1 & tvaas_level == "Level 2";
replace tvaas_goal = 0 if eligible == 1 & tvaas_level == "Level 1";

keep system system_name subject subgroup grade eligible achievement_goal tvaas_goal;

egen best_score = rowmax(achievement_goal tvaas_goal) if eligible == 1;

* Export achievement master file;
save $output/achievement_master.dta, replace;


* Gap Heat Maps;
use $numeric, clear;

keep if subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" | subgroup == "Students with Disabilities" | subgroup == "English Language Learners";
keep if year == `current_yr' | year == `lag_yr';

gsort system system_name subgroup grade subject -year;

gen eligible = valid_tests >= 30 & valid_tests != . & valid_tests[_n+1] >= 30 & valid_tests[_n+1] != . if year == `current_yr'
	& system == system[_n+1] & subject == subject[_n+1] & grade == grade[_n+1] & subgroup == subgroup[_n+1];
gen PA_percentile_diff = (PA_percentile - PA_percentile[_n+1]) if eligible == 1 & year == `current_yr' &
	system == system[_n+1] & subject == subject[_n+1] & grade == grade[_n+1] & subgroup == subgroup[_n+1];

gen subgroup_achievement_goal = 4 if eligible == 1 & PA_percentile_diff >= 10 & PA_percentile_diff != .;
replace subgroup_achievement_goal = 3 if subgroup_achievement_goal == . & eligible == 1 & PA_percentile_diff > 0 & PA_percentile_diff < 10;
replace subgroup_achievement_goal = 3 if subgroup_achievement_goal == . & eligible == 1 & 
	PA_percentile >= 95 & PA_percentile != . & PA_percentile[_n+1] >= 95 & PA_percentile[_n+1] != .; /* Safe Harbor for scoring above 95th %ile */
replace subgroup_achievement_goal = 2 if subgroup_achievement_goal == . & eligible == 1 & PA_percentile_diff >= -2 & PA_percentile_diff <= 0;
replace subgroup_achievement_goal = 1 if subgroup_achievement_goal == . & eligible == 1 & PA_percentile_diff < -2 & PA_percentile_diff >= -10;
replace subgroup_achievement_goal = 0 if subgroup_achievement_goal == . & eligible == 1 & PA_percentile_diff < -10;

drop if year == `lag_yr';

gen subgroup_tvaas_goal = 4 if eligible == 1 & tvaas_level == "Level 5";
replace subgroup_tvaas_goal = 3 if eligible == 1 & tvaas_level == "Level 4";
replace subgroup_tvaas_goal = 2 if eligible == 1 & tvaas_level == "Level 3";
replace subgroup_tvaas_goal = 1 if eligible == 1 & tvaas_level == "Level 2";
replace subgroup_tvaas_goal = 0 if eligible == 1 & tvaas_level == "Level 1";

keep system system_name subject subgroup grade eligible subgroup_achievement_goal subgroup_tvaas_goal;

egen best_score = rowmax(subgroup_achievement_goal subgroup_tvaas_goal) if eligible == 1;

* Export gap closure master file;
save $output/gap_closure_master.dta, replace;


