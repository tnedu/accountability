#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  School Pool Determination

Edited last by:  Alexander Poon

Date edited last:  03/23/2016
***************************************************************/

global base "K:\ORP_accountability\projects\2016_pre_coding\Output/school_base_with_super_subgroup_2016.dta";
global title_1 "K:\ORP_accountability\data\2015_tdoe_provided_files\directory/2014-15 Title I Schools List.xlsx";
global sped "K:\ORP_accountability\data\2015_tdoe_provided_files\directory/SpecialEducationSchools07092015.xls";
global cte_alt_adult "K:\ORP_accountability\data\2015_tdoe_provided_files\directory/SchoolTypes689_2015.xlsx";
global closed "K:\ORP_accountability\data\2015_tdoe_provided_files\directory/ClosedSchools07092015_use for school accountability.xls";

global output "K:\ORP_accountability\projects\2016_pre_coding\Output";

local current_yr = 2015;
local lag_yr = `current_yr' - 1;
local lag_2yr = `current_yr' - 2;

use $base, clear;

drop if subject == "ACT Composite";

replace valid_tests = grad_cohort if subject == "Graduation Rate";
replace grade = "12" if subject == "Graduation Rate";

* Drop systems immune from designations;
drop if system == 970 | system == 964 | system == 960 | system == 963 | system == 972;

drop if grade == "All Grades" | grade == "Missing Grade";
destring grade, replace;

* Change EOC subjects for grade <= 8 students;
replace subject = "Math" if (subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math")) & grade <= 8;
replace subject = "ELA" if (subject == "English I" | subject == "English II" | subject == "English III") & grade <= 8;
replace subject = "Science" if (subject == "Biology I" | subject == "Chemistry") & grade <= 8;

* Collapse test counts across replaced subjects;
collapse (sum) valid_tests, by(year system system_name school school_name subgroup subject grade);

preserve;

* School Pool Identification;
keep if year == `current_yr';
keep if subgroup == "All Students";

* Collapse test counts across grades, using only most recent year;
collapse (sum) valid_tests, by(year system system_name school school_name subgroup subject);

gen tests_30 = valid_tests >= 30 & valid_tests !=.;

gen hs_subjects = (tests_30 == 1) & 
	(subject == "Algebra I" | subject == "Algebra II" | subject == "Geometry" | regexm(subject, "Integrated Math") |
	subject == "English I" | subject == "English II" | subject == "English III" |
	subject == "Biology I" | subject == "Chemistry" | subject == "Graduation Rate");
gen elem_subjects = (tests_30 == 1) & (subject == "Math" | subject == "ELA" | subject == "Science" | subject == "Social Studies");

bysort system system_name school school_name: egen dummy_38 = max(elem_subjects);
bysort system system_name school school_name: egen dummy_hs = max(hs_subjects);

collapse dummy_38 dummy_hs, by(system system_name school school_name);

* Generate pool variable;
gen pool = "HS" if dummy_hs == 1;
replace pool = "K8" if (dummy_hs == 0 & dummy_38 == 1);

gen K8_pool = pool == "K8";
gen HS_pool = pool == "HS";

tab pool, missing;
* 14 schools missing a grade pool;

keep system system_name school school_name pool;

tempfile pools;
save `pools', replace;

restore;

* Designation immunities for schools with one year or less, grad only;
* If ELL valid test count >= 30, keep T1/T2. Otherwise, keep ELL;
gen ell_30 = (valid_tests >= 30 & valid_tests != .) if subgroup == "English Language Learners";

gen comparison = "ELL vs. Non-ELL" if subgroup == "English Language Learners" | subgroup == "English Language Learners with T1/T2";

bysort year system school subject grade comparison: egen temp = max(ell_30);

drop if temp == 0 & subgroup == "English Language Learners with T1/T2" & subject != "Graduation Rate";
drop if temp == 1 & subgroup == "English Language Learners" & subject != "Graduation Rate";
replace subgroup = "English Language Learners" if subgroup == "English Language Learners with T1/T2";

drop if subgroup == "Non-English Language Learners";

* Collapse test counts across grade;
collapse (sum) valid_tests, by(year system system_name school school_name subgroup subject);

gen tests_30 = (valid_tests >= 30 & valid_tests != .);
gen tests_30_nograd = (valid_tests >= 30 & valid_tests != .) if subject != "Graduation Rate";

bysort system system_name school school_name subgroup: egen year_`current_yr' = max(tests_30_nograd) 
	if year == `current_yr' & valid_tests >= 30 & valid_tests != .;
bysort system system_name school school_name subgroup: egen year_`lag_yr' = max(tests_30_nograd) 
	if year == `lag_yr' & valid_tests >= 30 & valid_tests != .;
bysort system system_name school school_name subgroup: egen year_`lag_2yr' = max(tests_30_nograd) 
	if year == `lag_2yr' & valid_tests >= 30 & valid_tests != .;

egen temp = tag(year system system_name school school_name subgroup year) 
	if valid_tests >= 30 & valid_tests != . & subject != "Graduation Rate";
bysort system system_name school school_name subgroup: egen num_years = sum(temp);

drop temp tests_30_nograd;

gen one_yr_or_less = (num_years == 1 | num_years == 0);

* Grad only;
gen temp = tests_30;
replace temp = 0 if subject == "Graduation Rate";

bysort system system_name school school_name subgroup: egen elig_nograd = max(temp);
bysort system system_name school school_name subgroup: egen elig_wgrad = max(tests_30);
gen grad_only = (elig_wgrad == 1 & elig_nograd == 0);

drop elig_nograd elig_wgrad;

bysort year system system_name school school_name subgroup: egen elig_nograd = max(temp);
bysort year system system_name school school_name subgroup: egen elig_wgrad = max(tests_30);
gen grad_only`current_yr' = (elig_wgrad == 1 & elig_nograd == 0) if year == `current_yr';
gen grad_only`lag_yr' = (elig_wgrad == 1 & elig_nograd == 0) if year == `lag_yr';

drop elig_nograd elig_wgrad tests_30 temp;

collapse one_yr_or_less num_years grad_only grad_only`current_yr' grad_only`lag_yr', by(system system_name school school_name subgroup);

keep if subgroup == "All Students" | subgroup == "Black/Hispanic/Native American" | 
	subgroup == "Economically Disadvantaged" | subgroup == "Non-Economically Disadvantaged" |
	subgroup == "Students with Disabilities" | subgroup == "Non-Students with Disabilities" |
	subgroup == "English Language Learners" | subgroup == "Non-English Language Learners/T1 or T2";

replace subgroup = "All" if subgroup == "All Students";
replace subgroup = "BHN" if subgroup == "Black/Hispanic/Native American";
replace subgroup = "ED" if subgroup == "Economically Disadvantaged";
replace subgroup = "SWD" if subgroup == "Students with Disabilities";
replace subgroup = "ELL" if subgroup == "English Language Learners";
replace subgroup = "Non_ED" if subgroup == "Non-Economically Disadvantaged";
replace subgroup = "Non_SWD" if subgroup == "Non-Students with Disabilities";
replace subgroup = "Non_ELL" if subgroup == "Non-English Language Learners/T1 or T2";

reshape wide one_yr_or_less num_years grad_only grad_only`current_yr' grad_only`lag_yr', i(system system_name school school_name) j(subgroup) string;

* Merge on pools;
mmerge system system_name school school_name using `pools', type(1:1);
drop _merge;

order pool, after(school_name);

tempfile pools_immune;
save `pools_immune', replace;

* Merge on Title 1 status (Using 2015 data);
preserve;

import excel using "$title_1", sheet("K-12 Title I Schools") firstrow clear;

drop if A ==.;
gen system = regexs(0) if regexm(SchoolID, "^[0-9][0-9][0-9]");
gen school = regexs(0) if regexm(SchoolID, "[0-9][0-9][0-9][0-9]$");

drop SchoolID;
rename (LEAName SchoolName Service) (system_name school_name Title_1);

replace Title_1 = "1";
destring Title_1 system school, replace;

drop if system == 970 | system == 964 | system == 960 | system == 963 | system == 972;

keep system school Title_1;

tempfile title_1;
save `title_1', replace;

restore;

mmerge system school using `title_1', type(1:1);
drop if _merge == 2;
drop _merge;

replace Title_1 = 0 if Title_1 ==.;
order Title_1, after(pool);

* Closed schools (Using 2015 data);
preserve;

import excel using "$closed", firstrow clear;

rename (DISTRICT_NUMBER SCHOOL_NUMBER) (system school);
keep system school;

destring system school, replace;

gen closed = 1;

tempfile closed;
save `closed', replace;

restore;

mmerge system school using `closed', type(1:1);
drop if _merge == 2;
drop _merge;

replace closed = 0 if closed ==.;

* CTE/Alternative/Adult schools (Using 2015 data);
preserve;

import excel using $cte_alt_adult, firstrow clear;

rename (DISTRICT_NUMBER SCHOOL_NUMBER) (system school);
destring system school, replace;

drop if system  == . & school == .;

gen cte_alt_adult = 1;
keep system school cte_alt_adult;

tempfile cte_alt_adult;
save `cte_alt_adult', replace;

restore;

mmerge system school using `cte_alt_adult', type(1:1);
drop if _merge == 2;
drop _merge;

replace cte_alt_adult = 0 if cte_alt_adult ==.;

* Sped schools (Using 2015 data);
preserve;

import excel using $sped, firstrow clear;

rename (DISTRICT_NUMBER SCHOOL_NUMBER) (system school);
destring system school, replace;

keep system school;
duplicates drop;

gen sped_school = 1;

tempfile sped;
save `sped', replace;

restore;

mmerge system school using `sped', type(1:1);
drop if _merge == 2;
drop _merge;

replace sped_school = 0 if sped_school ==.;

* Generate designation_ineligible;
gen designation_ineligible = (one_yr_or_lessAll == 1 | grad_onlyAll == 1 | grad_only`current_yr'All == 1 | closed == 1 | sped_school == 1 | cte_alt_adult == 1);
gen designation_ineligible_priority = (grad_onlyAll == 1 | grad_only`current_yr'All == 1 | closed == 1 | sped_school == 1 | cte_alt_adult == 1);

* Output file;
drop if pool == "";

compress;

save "$output/grade_pools_designation_immune_2016.dta", replace;
