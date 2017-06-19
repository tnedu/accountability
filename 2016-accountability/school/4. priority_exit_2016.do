#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Priority Exit 2016

Edited last by:  Alexander Poon

Date edited last:  03/23/2016

Outstanding Issues:  Update TVAAS for 2016
***************************************************************/

global base "K:\ORP_accountability\projects\2016_pre_coding\Output/school_base_with_super_subgroup_2016.dta";
global numeric "K:\ORP_accountability\projects\2016_pre_coding\Output/school_numeric_with_super_subgroup_2016.dta";
global school_pools "K:\ORP_accountability\projects\2016_pre_coding\Output/grade_pools_designation_immune_2016.dta";
global priority_2015 "K:\ORP_accountability\projects\2016_pre_coding\Output/priority_list_for_2016_school_acct.dta";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;

* School pools;
use $school_pools, clear;

keep system school pool designation_ineligible_priority;

tempfile pools;
save `pools', replace;

* One-year success rates for priority exit;
use $base, clear;

mmerge system school using `pools', type(n:1);
keep if _merge == 3;
drop _merge;

keep if year == `current_yr';
keep if subgroup == "All Students";

replace valid_tests = grad_cohort if subject == "Graduation Rate";
replace n_prof = grad_count if subject == "Graduation Rate";
replace grade = "12" if subject == "Graduation Rate";

drop if grade == "All Grades" | grade == "Missing Grade";
drop if subject == "ACT Composite";
destring grade, replace;

drop if (subject == "US History" | subject == "Graduation Rate") & pool == "K8";
drop if subject == "US History" & pool == "HS";
* Change EOC subjects for grade <= 8 students;
replace subject = "Math" if (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math")) & grade <= 8;
replace subject = "ELA" if (subject == "English I" | subject == "English II" | subject == "English III") & grade <= 8;
replace subject = "Science" if (subject == "Biology I" | subject == "Chemistry") & grade <= 8;

drop if (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") |
	subject == "English I" | subject == "English II" | subject == "English III" |
	subject == "Biology I" | subject == "Chemistry") & pool == "K8";

egen n_PA = rowtotal(n_prof n_adv);

* Collapse test counts across grades and replaced subjects;
collapse (sum) n_PA valid_tests, by(system system_name school school_name pool subject designation_ineligible);

* Suppress test counts below 30;
gen eligible = (valid_tests >= 30 & valid_tests !=.);

replace valid_tests = 0 if eligible == 0;
replace n_PA = 0 if eligible == 0;

drop eligible;

* Collapse test count across subjects;
collapse (sum) n_PA valid_tests, by(system system_name school school_name pool designation_ineligible);

* One-year success rates;
gen one_year_success = round(100 * n_PA/valid_tests, 0.1);

tempfile success_rates;
save `success_rates', replace;

use $priority_2015, clear;

drop priority_exit;
rename priority_improving priority_improving2015;

mmerge system system_name school school_name using `success_rates', type(1:1);
drop _merge;

* 15th, 10th percentile of success rate for priority exit/improving;
* Designation ineligible schools are included in the pool (denominator), but not in the percentile calculation;
count if pool == "HS";
local hs_15_perc = ceil(0.15 * r(N));
local hs_10_perc = ceil(0.10 * r(N));

count if pool == "K8";
local k8_15_perc = ceil(0.15 * r(N));
local k8_10_perc = ceil(0.10 * r(N));

preserve;

keep if pool == "HS";

gsort designation_ineligible one_year_success;

gen percentile_15 = ([_n] <= `hs_15_perc');
gen priority_exit = priority == 1 & percentile_15 == 0 & designation_ineligible == 0;

gen percentile_10 = ([_n] <= `hs_10_perc');
replace priority_exit = 1 if priority == 1 & percentile_10 == 0 & designation_ineligible == 0 & priority_improving2015 == 1;
gen priority_improving = priority == 1 & percentile_10 == 0 & designation_ineligible == 0 & priority_exit == 0;

tempfile hs;
save `hs', replace;

restore;

keep if pool == "K8";

gsort designation_ineligible one_year_success;

gen percentile_15 = ([_n] <= `k8_15_perc');
gen priority_exit = priority == 1 & percentile_15 == 0 & designation_ineligible == 0;

gen percentile_10 = ([_n] <= `k8_10_perc');
replace priority_exit = 1 if priority == 1 & percentile_10 == 0 & designation_ineligible == 0 & priority_improving2015 == 1;

gen priority_improving = priority == 1 & percentile_10 == 0 & designation_ineligible == 0 & priority_exit == 0;

append using `hs';

drop percentile_15 percentile_10;

* Alternate exit pathway;
* School earns level 4 or 5 TVAAS in all eligible accountability subject/grade combinations two years in a row;
preserve;

use $numeric, clear;

mmerge system school using `pools', type(n:1);
keep if _merge == 3;
drop _merge;

gen eligible = valid_tests >= 30 if valid_tests != .;

gen tvaas_4_or_5 = (tvaas_level == "Level 4" | tvaas_level == "Level 5") if eligible == 1;

collapse (sum) eligible tvaas_4_or_5, by(year system system_name school school_name pool);

* Number of TVAAS 4 and 5s should equal number of eligible subjects each year;
gen all_4_or_5 = (tvaas_4_or_5 == eligible) if eligible != 0;

* Check that condition is met both years;
collapse (sum) all_4_or_5, by(system system_name school school_name pool);

tempfile tvaas_exit;
save `tvaas_exit', replace;

restore;

mmerge system system_name school school_name pool using `tvaas_exit', type(1:1);
drop _merge;

replace priority_exit = 1 if all_4_or_5 == 2;
drop all_4_or_5;

* ASD Partial Takeover schools cannot be named priority exit, but can be priority improving;
replace priority_improving = 1 if system == 985 & (school == 8090 | school == 8020) & priority_exit == 1;
replace priority_exit = 0 if system == 985 & (school == 8090 | school == 8020);

* Output priority exit file;
compress;

save "$output/priority_exit_2016.dta", replace;
export delim using "$output/priority_exit_2016.csv", delim(",") replace;

preserve;

* Output priority non-exiting for reward;
keep if priority == 1 & priority_exit == 0;

save "$output/priority_schools_not_exiting_2016.dta", replace;

restore;
