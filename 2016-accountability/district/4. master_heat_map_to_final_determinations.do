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

Date edited last:  03/23/2016

Outstanding issues:  
***************************************************************/

global participation "K:\ORP_accountability\projects\2016_pre_coding\Output/participation_test_master.dta";
global performance_gate "K:\ORP_accountability\projects\2016_pre_coding\Output/performance_gate_master.dta";
global achievement "K:\ORP_accountability\projects\2016_pre_coding\Output/achievement_master.dta";
global gap "K:\ORP_accountability\projects\2016_pre_coding\Output/gap_closure_master.dta";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

* Participation Test;
use $participation, clear;

egen participation_test = rowmean(participation_testAll participation_testBHN participation_testED participation_testELL participation_testSWD participation_testSuper);
replace participation_test = 0 if participation_test != 1 & participation_test != .;

keep system system_name participation_test;

gen final_determination = "In Need of Improvement" if participation_test == 0;

tempfile final;
save `final', replace;

preserve;

* Minimum Performance Gate;
use $performance_gate, clear;

foreach v in achievement_key tvaas_key gap_BB_reduction_key gap_tvaas_key gap_closure_key {;

	replace `v' = "1" if `v' == "Yes";
	replace `v' = "0" if `v' == "No";

};

destring achievement_key tvaas_key gap_BB_reduction_key gap_tvaas_key gap_closure_key, replace;

bysort system system_name: egen achievement_eligibles = sum(eligible_achievement);
bysort system system_name: egen achievement_passed = sum(achievement_key);

gen ach_gate = (achievement_passed/achievement_eligibles) >= 0.25 if achievement_eligibles != 0;

bysort system system_name: egen tvaas_eligibles = sum(eligible_tvaas);
bysort system system_name: egen tvaas_passed = sum(tvaas_key);

gen tvaas_gate = (tvaas_passed/tvaas_eligibles) >= 0.25 if tvaas_eligibles != 0;

bysort system system_name: egen gap_eligibles = sum(eligible_gap_closure);
bysort system system_name: egen gap_passed = sum(gap_closure_key);

gen gap_closure_gate = (gap_passed/gap_eligibles) >= 0.25 if gap_eligibles != 0;

collapse ach_gate tvaas_gate gap_closure_gate, by(system system_name);

tempfile perf_gate;
save `perf_gate', replace;

restore;

mmerge system system_name using `perf_gate', type(1:1);
drop _merge;

replace final_determination = "In Need of Improvement" if ach_gate == 0 | tvaas_gate == 0 | gap_closure_gate == 0;
order final_determination, last;

preserve;

* Achievement;
use $achievement, clear;

collapse best_score, by(system system_name);
replace best_score = round(best_score, 0.01);

rename best_score achievement_score;

tempfile ach;
save `ach', replace;

restore;

mmerge system system_name using `ach', type(1:1);
drop _merge;

gen achievement_determination = "Progressing" if final_determination == "" & achievement_score < 2;
replace achievement_determination = "Achieving" if final_determination == "" & achievement_score >= 2 & achievement_score < 3;
replace achievement_determination = "Exemplary" if final_determination == "" & achievement_score >= 3 & achievement_score != .;

order final_determination, last;

preserve;

* Gap;
use $gap, clear;

collapse best_score, by(system system_name subgroup);
replace best_score = round(best_score, 0.01);

rename best_score gap_score;

replace subgroup = "BHN" if subgroup == "Black/Hispanic/Native American";
replace subgroup = "ED" if subgroup == "Economically Disadvantaged";
replace subgroup = "SWD" if subgroup == "Students with Disabilities";
replace subgroup = "ELL" if subgroup == "English Language Learners";

reshape wide gap_score, i(system system_name) j(subgroup) string;

egen gap_score = rowmean(gap_scoreBHN gap_scoreED gap_scoreELL gap_scoreSWD);
replace gap_score = round(gap_score, 0.01);

tempfile gap;
save `gap', replace;

restore;

mmerge system system_name using `gap', type(1:1);
drop _merge;

gen gap_determination = "Progressing" if final_determination == "" & gap_score < 2;
replace gap_determination = "Achieving" if final_determination == "" & gap_score >= 2 & gap_score < 3;
replace gap_determination = "Exemplary" if final_determination == "" & gap_score >= 3 & gap_score != .;

egen overall_average = rowmean(achievement_score gap_score);
replace overall_average = round(overall_average, 0.01);

order final_determination, last;

replace final_determination = "Progressing" if final_determination == "" & overall_average < 2;
replace final_determination = "Achieving" if final_determination == "" & overall_average >= 2 & overall_average < 3;
replace final_determination = "Exemplary" if final_determination == "" & overall_average >= 3 & overall_average != .;

save "$output/final_determinations_master.dta", replace;
export delim using "$output/final_determinations_master.csv", delim(",") replace;
