#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Focus Exit 2016

Edited last by:  Alexander Poon

Date edited last:  03/29/2016

Outstanding issues:  Ensure ACT Composite is incorporated correctly for ELL
***************************************************************/

global base "K:\ORP_accountability\projects\2016_pre_coding\Output/school_base_with_super_subgroup_2016.dta";
global school_pools "K:\ORP_accountability\projects\2016_pre_coding\Output/grade_pools_designation_immune_2016.dta";
global focus_2015 "K:\ORP_accountability\projects\2016_pre_coding\Output/focus_list_for_2016_school_acct.dta";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;
local lag_yr = `current_yr' - 1;

* School pools;
use $school_pools, clear;

preserve;

keep system school pool designation_ineligible;

tempfile pools;
save `pools', replace;

restore;

* Subgroup Ineligibles;
keep system school grad_onlyBHN grad_only`current_yr'BHN grad_only`lag_yr'BHN grad_onlyED grad_only`current_yr'ED grad_only`lag_yr'ED
	grad_onlySWD grad_only`current_yr'SWD grad_only`lag_yr'SWD grad_onlyELL grad_only`current_yr'ELL grad_only`lag_yr'ELL;

reshape long grad_only grad_only`current_yr' grad_only`lag_yr', i(system school) j(subgroup) string;

replace subgroup = "Black/Hispanic/Native American" if subgroup == "BHN";
replace subgroup = "Economically Disadvantaged" if subgroup == "ED";
replace subgroup = "Students with Disabilities" if subgroup == "SWD";
replace subgroup = "English Language Learners" if subgroup == "ELL";

tempfile subgroup_grad_only;
save `subgroup_grad_only', replace;

* Applying school accountability rules to data, creating percent BB/PA;
use $base, clear;

mmerge system school using `pools', type(n:1);
keep if _merge == 3;
drop _merge;

replace valid_tests = grad_cohort if subject == "Graduation Rate";
replace n_below_bsc = dropout_count if subject == "Graduation Rate";
replace n_prof = grad_count if subject == "Graduation Rate";
replace grade = "12" if subject == "Graduation Rate";

keep if subgroup == "Black/Hispanic/Native American" | subgroup == "Economically Disadvantaged" | subgroup == "Students with Disabilities" | 
	subgroup == "English Language Learners" | subgroup == "English Language Learners with T1/T2";

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

* Collapse test counts across reassigned subjects and grades;
collapse (sum) n_below_bsc n_PA valid_tests, by(year system system_name school school_name pool subgroup subject designation_ineligible);

* If ELL valid test count >= 30, use T1/T2. Otherwise, keep ELL;
replace valid_tests = 0 if valid_tests == . & subgroup == "English Language Learners";
gen ell_30 = valid_tests >= 30 if subgroup == "English Language Learners";

gen comparison = "ELL vs. Non-ELL" if subgroup == "English Language Learners" | subgroup == "English Language Learners with T1/T2";

bysort year system school subject comparison: egen temp = max(ell_30);

drop if temp == 0 & subgroup == "English Language Learners with T1/T2";
drop if temp == 1 & subgroup == "English Language Learners";
replace subgroup = "English Language Learners" if subgroup == "English Language Learners with T1/T2";

drop ell_30 comparison temp;

* Suppress test counts below 30;
gen eligible = (valid_tests >= 30 & valid_tests != .);

replace valid_tests = 0 if eligible == 0;
replace n_PA = 0 if eligible == 0;
replace n_below_bsc = 0 if eligible == 0;

drop eligible;

* Collapse across subject;
collapse (sum) valid_tests n_below_bsc n_PA, by(year system system_name school school_name pool subgroup designation_ineligible);

gen pct_below_bsc = round(100 * n_below_bsc/valid_tests, 0.1);
gen one_year_success = round(100 * n_PA/valid_tests, 0.1);
drop n_PA n_below_bsc;

* Percentile Ranks for %BB in current and prior year;
preserve;

* Schools are only counted for percentiles if they meet 30 test count in both years;
keep if year == `current_yr' | year == `lag_yr';

gen test_count = (valid_tests >= 30 & valid_tests != .);

mmerge system school subgroup using `subgroup_grad_only', type(n:1);
drop _merge;

replace test_count = 0 if grad_only == 1;
replace test_count = 0 if year == `current_yr' & grad_only`current_yr' == 1;
replace test_count = 0 if year == `lag_yr' & grad_only`lag_yr' == 1;

bysort system system_name school school_name subgroup pool: egen temp = sum(test_count);

gen eligible = (temp == 2);
drop test_count grad_only* temp;

* Generate new variable which counts the number of eligible schools in the same year/subgroup with a lower or equal %BB;
gen pctile_rank_BB = .;

count;
local count = r(N);

* Looping over observations, generate a new variable which represents the number of eligible
* schools with the same or lower percent BB within the same subgroup and year;
quietly forval k = 1(1)`count' {;

	if pct_below_bsc[`k'] != . {;

		count if pct_below_bsc <= pct_below_bsc[`k'] & pool == pool[`k'] & subgroup == subgroup[`k'] & year == year[`k'] & eligible == 1;
		replace pctile_rank_BB = r(N) if [_n] == `k' & eligible == 1;

	};
};

bysort year subgroup pool: egen eligible_count = sum(eligible);

gsort -year subgroup pool -eligible -pct_below_bsc;
gen BB_percentile = round(100 * pctile_rank_BB/eligible_count, 0.1) if eligible == 1;

drop eligible valid_tests pct_below_bsc one_year_success pctile_rank_* eligible_count;

reshape wide BB_percentile, i(system system_name school school_name subgroup pool) j(year);

gen BB_percentile_diff = BB_percentile`lag_yr' - BB_percentile`current_yr';

keep system system_name school school_name subgroup pool BB_percentile_diff;

replace subgroup = "BHN" if subgroup == "Black/Hispanic/Native American";
replace subgroup = "ED" if subgroup == "Economically Disadvantaged";
replace subgroup = "SWD" if subgroup == "Students with Disabilities";
replace subgroup = "ELL" if subgroup == "English Language Learners";

reshape wide BB_percentile_diff, i(system system_name school school_name pool) j(subgroup) string;

tempfile bb_percentiles;
save `bb_percentiles', replace;

restore;

* Success rates for subgroup exit;
keep if year == `current_yr';
keep system system_name school school_name subgroup pool designation_ineligible one_year_success;

replace subgroup = "BHN" if subgroup == "Black/Hispanic/Native American";
replace subgroup = "ED" if subgroup == "Economically Disadvantaged";
replace subgroup = "SWD" if subgroup == "Students with Disabilities";
replace subgroup = "ELL" if subgroup == "English Language Learners";

reshape wide one_year_success, i(system system_name school school_name pool designation_ineligible) j(subgroup) string;

* Merge below basic percentiles for gap exit;
mmerge system system_name school school_name pool using `bb_percentiles', type(1:1);
drop _merge;

* Merge focus list onto data;
mmerge system school using $focus_2015, type(1:1);
drop if _merge == 2;
drop _merge;

* Subgroup exit/improving;
foreach s in ED SWD ELL {;

	gen `s'_subgroup_exit = subgroup_path_`s' == 1 & one_year_success`s' > 20 & one_year_success`s' != .;
	gen `s'_subgroup_improving = subgroup_path_`s' == 1 & one_year_success`s' > 15 & one_year_success`s' != . if `s'_subgroup_exit != 1;

};

egen subgroup_identified_count = rowtotal(subgroup_path_SWD subgroup_path_ELL subgroup_path_ED);
egen subgroup_exit_count = rowtotal(ED_subgroup_exit SWD_subgroup_exit ELL_subgroup_exit);
egen subgroup_improving_count = rowtotal(ED_subgroup_improving SWD_subgroup_improving ELL_subgroup_improving);

gen subgroup_exit = (subgroup_exit_count == subgroup_identified_count) if focus_any_pathway == 1 & subgroup_identified_count != 0;
gen subgroup_improving = (subgroup_improving_count == subgroup_identified_count) if focus_any_pathway == 1 & subgroup_identified_count != 0;

* Gap exit/improving;
foreach s in ED BHN SWD ELL {;

	gen `s'_gap_exit = `s'_gap_identified == 1 & BB_percentile_diff`s' >= 25 & BB_percentile_diff`s' != .;
	gen `s'_gap_improving = `s'_gap_identified == 1 & BB_percentile_diff`s' >= 12.5 & BB_percentile_diff`s' != . if `s'_gap_exit == 0;

};

* Schools identified as gap improving two years in a row for the same subgroup exit for that subgroup;
foreach s in ED BHN SWD ELL {;

	replace `s'_gap_exit = 1 if `s'_gap_identified == 1 & `s'_gap_improving == 1 & gap_improving_`s'2015 == 1;
	replace `s'_gap_improving = 0 if `s'_gap_identified == 1 & `s'_gap_improving == 1 & gap_improving_`s'2015 == 1;

};

egen gap_identified_count = rowtotal(BHN_gap_identified ED_gap_identified SWD_gap_identified ELL_gap_identified);
egen gap_exit_count = rowtotal(ED_gap_exit BHN_gap_exit SWD_gap_exit ELL_gap_exit);
egen gap_improving_count = rowtotal(ED_gap_improving BHN_gap_improving SWD_gap_improving ELL_gap_improving);

gen gap_exit = (gap_exit_count == gap_identified_count) if gap_identified_count != 0 & designation_ineligible == 0;
gen gap_improving = (gap_exit_count + gap_improving_count == gap_identified_count) if gap_exit == 0 & gap_identified_count != 0 & designation_ineligible == 0;

egen focus_pathway_count = rowtotal(gap_pathway subgroup_pathway);
egen focus_exit_count = rowtotal(subgroup_exit gap_exit);
egen focus_improving_count = rowtotal(subgroup_improving gap_improving);

gen focus_exit = 0 if focus_any_pathway == 1 & designation_ineligible == 0;
replace focus_exit = 1 if focus_any_pathway == 1 & (focus_exit_count == focus_pathway_count) & designation_ineligible == 0;

* Output focus exit file;
compress;

save "$output/focus_exit_2016.dta", replace;
export delim using "$output/focus_exit_2016.csv", delim(",") replace;

preserve;

* Output focus non-exiting for reward;
keep if focus_any_pathway == 1 & focus_exit == 0;

save "$output/focus_schools_not_exiting_2016.dta", replace;

restore;
