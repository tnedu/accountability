#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Student Match for Subgroup Growth Measure

Edited last by:  Alexander Poon

Date edited last:  11/29/2016
***************************************************************/

use "K:\ORP_accountability\data\2014_sas_accountability/state-student-level_2014_09dec2014_withchem.dta", clear;

duplicates tag state_student_id subject, gen(dup);
drop if dup != 0;
drop dup;

drop if real(state_student_id) == .;
destring state_student_id, replace;

gen content_area = "Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Math";
replace content_area = "ELA" if subject == "English I" | subject == "English II" | subject == "English III" | subject == "Reading/Language";
replace content_area = "Science" if subject == "Biology I" | subject == "Chemistry" | subject == "Science";
drop if subject == "Social Studies" | subject == "U.S. History";

drop if proficiency_level == "";
rename (proficiency_level subject) (proficiency_level_prior subject_prior);

* For student with multiple tests in a content area, take lower scale score, then force drop if scale score are the same;
bysort state_student_id content_area: egen temp = min(scale_score);
drop if scale_score != temp;

duplicates drop state_student_id content_area, force;

keep state_student_id subject_prior proficiency_level_prior content_area;

tempfile prior;
save `prior', replace;

* Current;
use "K:\ORP_accountability\data\2015_sas_accountability/state-student-level_2015_19jul2015.dta", clear;

duplicates tag state_student_id subject, gen(dup);
drop if dup != 0;
drop dup;

drop if real(state_student_id) == .;
destring state_student_id, replace;

replace ell_t1_t2 = 1 if ell == 1;

gen asian = race == "Asian";
gen black = race == "Black/African American";
gen hpi = race == "Native Hawaiian/Pac. Islander";
gen hispanic = race == "Hispanic/Latino";
gen native = race == "American Indian/Alaskan Native";
gen white = race == "White";

keep system school state_student_id subject proficiency_level bhn_group economically_disadvantaged special_ed ell_t1_t2 asian black hispanic hpi white native;

gen super = bhn_group == 1 | economically_disadvantaged == 1 | special_ed == 1 | ell_t1_t2 == 1;

gen content_area = "Math" if subject == "Algebra I" | subject == "Algebra II" | subject == "Math";
replace content_area = "ELA" if subject == "English I" | subject == "English II" | subject == "English III" | subject == "Reading/Language";
replace content_area = "Science" if subject == "Biology I" | subject == "Chemistry" | subject == "Science";

drop if proficiency_level == "";

mmerge state_student_id content_area using `prior', type(n:1);
keep if _merge == 3;
drop _merge;

gen improved = 1 if proficiency_level_prior == "1. Below Basic" & (proficiency_level == "2. Basic" | proficiency_level == "3. Proficient" | proficiency_level  == "4. Advanced");
replace improved = 1 if proficiency_level_prior == "2. Basic" & (proficiency_level == "3. Proficient" | proficiency_level  == "4. Advanced");
replace improved = 1 if proficiency_level_prior == "3. Proficient" & proficiency_level  == "4. Advanced";
replace improved = 1 if proficiency_level_prior == "4. Advanced" & proficiency_level  == "4. Advanced";

replace improved = 0 if improved == .;
gen students = 1;

foreach s in bhn_group economically_disadvantaged special_ed ell_t1_t2 super asian black hispanic hpi white native {;

	preserve;

	keep if `s' == 1;

	gen subgroup = "`s'";

	collapse (sum) improved students, by(system school subgroup);
	gen pct_improved = round(1000 * improved/students)/10;

	tempfile `s';
	save ``s'', replace;

	restore;

};

clear;

foreach s in bhn_group economically_disadvantaged special_ed ell_t1_t2 super asian black hispanic hpi white native {;

	append using ``s'';

};

replace subgroup = "Asian" if subgroup == "asian";
replace subgroup = "Black" if subgroup == "black";
replace subgroup = "Hispanic" if subgroup == "hispanic";
replace subgroup = "Hawaiian/Pacific Islander" if subgroup == "hpi";
replace subgroup = "White" if subgroup == "white";
replace subgroup = "Native American" if subgroup == "native";
replace subgroup = "Black/Hispanic/Native American" if subgroup == "bhn_group";
replace subgroup = "Economically Disadvantaged" if subgroup == "economically_disadvantaged";
replace subgroup = "English Learners" if subgroup == "ell_t1_t2";
replace subgroup = "Students with Disabilities" if subgroup == "special_ed";
replace subgroup = "Super Subgroup" if subgroup == "super";

gen eligible = students >= 30 & students != .;

bysort eligible subgroup: egen rank = rank(pct_improved);
bysort eligible subgroup: egen pool = sum(eligible);

gen percentile_rank = round(1000 * rank/pool)/10;

gen grade_growth = "F" if eligible == 1 & percentile_rank < 20;
replace grade_growth = "D" if eligible == 1 & percentile_rank >= 20;
replace grade_growth = "C" if eligible == 1 & percentile_rank >= 40;
replace grade_growth = "B" if eligible == 1 & percentile_rank >= 60;
replace grade_growth = "A" if eligible == 1 & percentile_rank >= 80 & percentile_rank <= 100;

export delim using "K:\ORP_accountability\projects\Alex\accountability\modeling\school-grading\data/student_match_ranks.csv", delim(",") replace;
