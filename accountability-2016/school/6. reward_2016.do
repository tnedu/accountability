#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Reward Schools 2016

Edited last by:  Alexander Poon

Date edited last:  04/20/2016
***************************************************************/

global base "K:\ORP_accountability\projects\2016_pre_coding\Output/school_base_with_super_subgroup_2016.dta";
global focus "K:\ORP_accountability\projects\2016_pre_coding\Output/focus_schools_not_exiting_2016.dta";
global priority "K:\ORP_accountability\projects\2016_pre_coding\Output/priority_schools_not_exiting_2016.dta";
global pool_immune "K:\ORP_accountability\projects\2016_pre_coding\Output/grade_pools_designation_immune_2016.dta";
global tvaas "K:\Research and Policy\data\data_tvaas\raw\2014-15/URM School Value-Added and Composites.xlsx";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;
local lag_1yr = `current_yr' - 1;
local lag_2yr = `current_yr' - 2;

* Focus and priority schools are not eligible for reward;
use $focus, clear;

keep system school focus_any_pathway;

tempfile focus;
save `focus', replace;

use $priority, clear;

keep system school priority;

append using `focus';

tempfile focus_priority;
save `focus_priority', replace;

* Grade pools, designation immune;
use $pool_immune, clear;

preserve;

keep system school pool one_yr_or_lessAll grad_onlyAll one_yr_or_lessBHN grad_onlyBHN 
	one_yr_or_lessED grad_onlyED one_yr_or_lessNon_ED grad_onlyNon_ED 
	one_yr_or_lessELL grad_onlyELL one_yr_or_lessNon_ELL grad_onlyNon_ELL 
	one_yr_or_lessNon_SWD grad_onlyNon_SWD one_yr_or_lessSWD grad_onlySWD;

reshape long one_yr_or_less grad_only, i(system school pool) j(subgroup) string;

gen comparison = "BHN vs. All" if subgroup == "BHN" | subgroup == "All";
replace comparison = "ED vs. Non-ED" if subgroup == "ED" | subgroup == "Non_ED";
replace comparison = "SWD vs. Non-SWD" if subgroup == "SWD" | subgroup == "Non_SWD";
replace comparison = "ELL vs. Non-ELL" if subgroup == "ELL" | subgroup == "Non_ELL";

replace subgroup = "Target" if subgroup == "BHN" | subgroup == "ED" | subgroup == "ELL" | subgroup == "SWD";
replace subgroup = "Comparison" if subgroup == "All" | subgroup == "Non_ED" | subgroup == "Non_ELL" | subgroup == "Non_SWD";

reshape wide one_yr_or_less grad_only, i(system school pool comparison) j(subgroup) string;

tempfile one_year_data_grad_only;
save `one_year_data_grad_only', replace;

restore;

preserve;

keep system school designation_ineligible;

tempfile designation_ineligible;
save `designation_ineligible', replace;

restore;

keep system school pool;

tempfile pools;
save `pools', replace;

* 1 and 3-year success rates, gaps for reward exemption;
use $base, clear;

replace valid_tests = grad_cohort if subject == "Graduation Rate";
replace n_prof = grad_count if subject == "Graduation Rate";
replace grade = "12" if subject == "Graduation Rate";

mmerge system school using `pools', type(n:1);
* Schools without pools don't get school designations;
keep if _merge == 3;
drop _merge;

keep if subgroup == "All Students" | subgroup == "Black/Hispanic/Native American" |
	subgroup == "Economically Disadvantaged" | subgroup == "Non-Economically Disadvantaged" |
	subgroup == "Students with Disabilities" | subgroup == "Non-Students with Disabilities" |
	subgroup == "English Language Learners" | subgroup == "Non-English Language Learners" |
	subgroup == "English Language Learners with T1/T2" | subgroup == "Non-English Language Learners/T1 or T2";

drop if grade == "All Grades" | grade == "Missing Grade";
drop if subject == "ACT Composite";
destring grade, replace;

drop if (subject == "US History" | subject == "Graduation Rate") & pool == "K8";
drop if subject == "US History" & pool == "HS";
* Change EOC subjects for grade <= 8 students;
replace subject = "Math" if (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry") & grade <= 8;
replace subject = "Math" if (subject == "Integrated Math I" | subject == "Integrated Math II" | subject == "Integrated Math III") & grade <= 8;
replace subject = "RLA" if (subject == "English I" | subject == "English II" | subject == "English III") & grade <= 8;
replace subject = "Science" if (subject == "Biology I" | subject == "Chemistry") & grade <= 8;

drop if (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" |
	subject == "Integrated Math I" | subject == "Integrated Math II" | subject == "Integrated Math III" |
	subject == "English I" | subject == "English II" | subject == "English III" |
	subject == "Biology I" | subject == "Chemistry") & pool == "K8";

egen n_PA = rowtotal(n_prof n_adv);

* Collapse test counts across reassigned subjects and grades;
collapse (sum) n_PA valid_tests, by(year system system_name school school_name pool subgroup subject);

* Suppress test counts below 30;
gen eligible = (valid_tests >= 30 & valid_tests !=.);

replace valid_tests = 0 if eligible == 0;
replace n_PA = 0 if eligible == 0;

drop eligible;

* Keep ELL T1/T2 if greater than 30 ELL tests, otherwise, keep ELL;
gen ell_30 = (valid_tests >= 30 & valid_tests !=.) if subgroup == "English Language Learners";

gen comparison = "ELL vs. Non-ELL" if subgroup == "English Language Learners" | subgroup == "Non-English Language Learners" |
	subgroup == "English Language Learners with T1/T2" | subgroup == "Non-English Language Learners/T1 or T2";

bysort year system school subject comparison: egen temp = max(ell_30);

drop if temp == 0 & (subgroup == "English Language Learners with T1/T2" | subgroup == "Non-English Language Learners") & subject != "Graduation Rate";
drop if temp == 1 & (subgroup == "English Language Learners" | subgroup == "Non-English Language Learners") & subject != "Graduation Rate";
replace subgroup = "English Language Learners" if subgroup == "English Language Learners with T1/T2";
replace subgroup = "Non-English Language Learners" if subgroup == "Non-English Language Learners/T1 or T2";

drop ell_30 comparison temp;

* 3-Year success rates;
preserve;

* Collapsing test counts across years and subjects;
collapse (sum) valid_tests n_PA, by(system system_name school school_name pool subgroup);

* Three-year success rate;
gen pct_PA_3yr = round((100 * n_PA/valid_tests), 0.1);

tempfile three_year_success;
save `three_year_success', replace;

* 3-year gaps;
gen comparison = "BHN vs. All" if subgroup == "Black/Hispanic/Native American" | subgroup == "All Students";
replace comparison = "ED vs. Non-ED" if subgroup == "Economically Disadvantaged" | subgroup == "Non-Economically Disadvantaged";
replace comparison = "SWD vs. Non-SWD" if subgroup == "Students with Disabilities" | subgroup == "Non-Students with Disabilities";
replace comparison = "ELL vs. Non-ELL" if subgroup == "English Language Learners" | subgroup == "Non-English Language Learners";

replace subgroup = "Target" if subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" | 
	subgroup == "English Language Learners" | subgroup == "Students with Disabilities";
replace subgroup = "Comparison" if subgroup == "All Students" | subgroup == "Non-Economically Disadvantaged" | 
	subgroup == "Non-English Language Learners" | subgroup == "Non-Students with Disabilities";

reshape wide valid_tests n_PA pct_PA_3yr, i(system system_name school school_name comparison pool) j(subgroup) string;

drop n_*;

gen gap_3yr = round((pct_PA_3yrComparison - pct_PA_3yrTarget), 0.1);

mmerge system school pool comparison using `one_year_data_grad_only', type(1:1);
drop _merge;

replace gap_3yr = . if one_yr_or_lessComparison == 1 | grad_onlyComparison == 1 | one_yr_or_lessTarget == 1 | grad_onlyTarget == 1;

bysort pool comparison: egen pool_median_gap = median(gap_3yr);

gen gap_larger_than_median = gap_3yr > pool_median_gap if gap_3yr !=. & pool_median_gap !=.; 

keep system system_name school school_name pool comparison gap_3yr pool_median_gap gap_larger_than_median;

tempfile three_year_gaps;
save `three_year_gaps', replace;

restore;

* 1-Year success rates;
keep if year == `current_yr' | year == `lag_1yr';

* Collapsing test counts across subjects;
collapse (sum) valid_tests n_PA, by(year system system_name school school_name pool subgroup);

* One-year success rates;
gen pct_PA = round((100 * n_PA/valid_tests), 0.1);

preserve;

keep if year == `current_yr';
keep if subgroup == "All Students";

tempfile reward_success_rate;
save `reward_success_rate', replace;

restore;

* Check for narrowing of 1-year gaps;
gen comparison = "BHN vs. All" if subgroup == "Black/Hispanic/Native American" | subgroup == "All Students";
replace comparison = "ED vs. Non-ED" if subgroup == "Economically Disadvantaged" | subgroup == "Non-Economically Disadvantaged";
replace comparison = "SWD vs. Non-SWD" if subgroup == "Students with Disabilities" | subgroup == "Non-Students with Disabilities";
replace comparison = "ELL vs. Non-ELL" if subgroup == "English Language Learners" | subgroup == "Non-English Language Learners";

replace subgroup = "Target" if subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" | 
	subgroup == "English Language Learners" | subgroup == "Students with Disabilities";
replace subgroup = "Comparison" if subgroup == "All Students" | subgroup == "Non-Economically Disadvantaged" | 
	subgroup == "Non-English Language Learners" | subgroup == "Non-Students with Disabilities";

reshape wide valid_tests n_PA pct_PA, i(year system system_name school school_name pool comparison) j(subgroup) string;

gen gap_1yr = round(pct_PAComparison - pct_PATarget, 0.1);

drop valid_tests* n_* pct_PA*;

reshape wide gap_1yr, i(system system_name school school_name pool comparison) j(year);

gen gap_narrowed = gap_1yr`current_yr' < gap_1yr`lag_1yr' if gap_1yr`current_yr' !=. & gap_1yr`lag_1yr' !=.;

keep system system_name school school_name pool comparison gap_1yr* gap_narrowed;

mmerge system system_name school school_name pool comparison using `three_year_gaps', type(1:1);
drop _merge;

* Schools are exempt from reward designation if gap is larger than pool median AND gap has not narrowed;
gen subgroup_exempt = gap_larger_than_median == 1 & gap_narrowed == 0;

bysort system system_name school school_name: egen reward_exemption = max(subgroup_exempt);

collapse reward_exemption, by(system system_name school school_name pool);

tempfile reward_exemption;
save `reward_exemption', replace;

* Reward school identification;
use `reward_success_rate', clear;

mmerge system system_name school school_name pool using `reward_exemption', type(1:1);
drop _merge;

mmerge system school using `designation_ineligible', type(1:1);
drop _merge;

mmerge system school using `focus_priority', type(1:1);
gen focus_priority = focus_any_pathway == 1 | priority == 1;
drop _merge focus_any_pathway priority;

count if pool == "HS";
local hs_count = r(N);
local hs_5_perc = ceil(0.05 * `hs_count');

count if pool == "K8";
local k8_count = r(N);
local k8_5_perc = ceil(0.05 * `k8_count');

* Reward performance;
preserve;

keep if pool == "HS";

gsort reward_exemption designation_ineligible focus_priority -pct_PA;
gen percentile_5 = ([_n] <= `hs_5_perc');

gen reward_performance = focus_priority == 0 & percentile_5 == 1 & designation_ineligible == 0 & reward_exemption == 0;

tempfile hs_reward_perf;
save `hs_reward_perf', replace;

restore;

keep if pool == "K8";

gsort reward_exemption designation_ineligible focus_priority -pct_PA;
gen percentile_5 = ([_n] <= `k8_5_perc');

gen reward_performance = focus_priority == 0 & percentile_5 == 1 & designation_ineligible == 0 & reward_exemption == 0;

append using `hs_reward_perf';
drop percentile_5;

* TVAAS for reward progress;
preserve;

import excel using "$tvaas", firstrow clear;

rename (DistrictNumber School_Code) (system school);
destring system school, replace;

keep if Subject == "Overall";
keep if Test == "TCAP/EOC";

keep system school Index;

tempfile tvaas;
save `tvaas', replace;

restore;

mmerge system school using `tvaas', type(1:1);
drop if _merge == 2;
drop _merge;

* Reward progress;
gen reward_progress = 0;
gen reward = reward_performance;

count if pool == "HS";
local hs_count = r(N);
local hs_5_perc = ceil(0.05 * `hs_count');

count if pool == "K8";
local k8_count = r(N);
local k8_5_perc = ceil(0.05 * `k8_count');

preserve;

keep if pool == "HS";

count if reward_progress == 1 & reward_performance == 0;
local reward_progress_unique = r(N);
while `reward_progress_unique' < `hs_5_perc' {;

	gsort reward_exemption designation_ineligible focus_priority reward_progress -Index;

	quietly replace reward_progress = 1 if [_n] == 1;
	quietly replace reward = 1 if [_n] == 1;

	count if reward_progress == 1 & reward_performance == 0;
	local reward_progress_unique = r(N);

};

* Schools tied at the margin should be considered reward;
egen temp = min(Index) if reward_progress == 1;
egen cutoff = max(temp);
replace reward_progress = 1 if Index == cutoff & reward_progress == 0 & reward_exemption == 0 & focus_priority == 0 & designation_ineligible == 0;
replace reward = 1 if Index == cutoff & reward == 0 & reward_exemption == 0 & focus_priority == 0 & designation_ineligible == 0;

drop temp;

tempfile reward_hs;
save `reward_hs', replace;

restore;

keep if pool == "K8";

count if reward_progress == 1 & reward_performance == 0;
local reward_progress_unique = r(N);
while `reward_progress_unique' < `k8_5_perc' {;

	gsort reward_exemption designation_ineligible focus_priority reward_progress -Index;

	quietly replace reward_progress = 1 if [_n] == 1;
	quietly replace reward = 1 if [_n] == 1;

	count if reward_progress == 1 & reward_performance == 0;
	local reward_progress_unique = r(N);

};

* Schools tied at the margin should be considered reward;
egen temp = min(Index) if reward_progress == 1;
egen cutoff = max(temp);
replace reward_progress = 1 if Index == cutoff & reward_progress == 0 & reward_exemption == 0 & focus_priority == 0 & designation_ineligible == 0;
replace reward = 1 if Index == cutoff & reward == 0 & reward_exemption == 0 & focus_priority == 0 & designation_ineligible == 0;

drop temp;

append using `reward_hs';

* Output reward file;
compress;

save "$output/reward_2016.dta", replace;
export delim using "$output/reward_2016.csv", delim(",") replace;
