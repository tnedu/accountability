#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  CDF to Student Level

Edited last by:  Alexander Poon

Date edited last:  04/04/2016

Outstanding issues:  
***************************************************************/

global summer_eoc "K:\ORP_accountability\projects\2015_dictionary_coding/Summer_2014_State_Student_Data_File_EOC.dta";
global fall_eoc "K:\ORP_accountability\projects\2015_dictionary_coding/FALL_2014_State_Student_Data_File_EOC.dta";
global spring_eoc "K:\ORP_accountability\projects\2015_dictionary_coding/SPRING_2015_State_Student_Data_File_EOC.dta";
global achievement "K:\ORP_accountability\projects\2015_dictionary_coding/2015_State_Student_Data_File_ACH_with_ell_exclude_fix.dta";
global alternative "K:\ORP_accountability\projects\2015_dictionary_coding/portfolio_cdf_2015.dta";

global alt_hs_cte "K:\ORP_accountability\data\2015_tdoe_provided_files\directory/SchoolTypes689_2015.xlsx";

/* Read in CDFs and append to create student level file */

* Achievement;
use $achievement, clear;

destring grade, replace;

rename (performance_level_ma testing_status_for_ma) (performance_level_math testing_status_for_math);

quietly foreach s in rla math sc {;

	preserve;

	gen elsa_taken = (modified_format_test_type == 2);
	gen absent = (testing_status_for_`s' == 4);
	gen did_not_attempt = (testing_status_for_`s' == 3);
	gen residential_facility = (testing_status_for_`s' == 0);
	gen nullify_flag = (testing_status_for_`s' == 1);

	* Drop tests flagged as ineligible and medically exempt;
	drop if (testing_status_for_`s' == 8 | testing_status_for_`s' == 2);

	keep system_number system_name school_number school_name grade unique_student_id last_name first_name middle_initial 
		race_reported code_a_b_in elsa_taken homebound ell_excluded esl_services_ell esl_services_t1_t2 functionally_delayed 
		special_education performance_level_`s' scale_score_`s' absent did_not_attempt residential_facility nullify_flag;

	rename (performance_level_`s' scale_score_`s') (performance_level scale_score);

	replace special_education = (special_education != .);

	gen original_subject = "`s'";
	gen test = "Achievement";

	tempfile tcap_`s';
	save `tcap_`s'', replace;

	restore;

};

* EOCs;
quietly foreach s in summer fall spring {;

	use $`s'_eoc, clear;

	drop absent;

	gen elsa_taken = (mod_format_test_type == 2);
	gen absent = (ri_status == 4);
	gen did_not_attempt = (ri_status == 3);
	gen residential_facility = (ri_status == 0);
	gen nullify_flag = (ri_status == 1);

	* Drop tests flagged as void and medically exempt;
	drop if (ri_status == 5 | ri_status == 2);

	keep system_number system_name school_number school_name grade unique_student_id last_name first_name race_reported 
		code_a_b_in elsa_taken ell_excluded esl_services_ell esl_services_t1_t2 functionally_delayed special_education 
		content_area_code performance_level scale_score absent did_not_attempt residential_facility nullify_flag;

	rename content_area_code original_subject;

	replace special_education = (special_education != .);

	replace original_subject = "Algebra I" if original_subject == "A1";
	replace original_subject = "Algebra II" if original_subject == "A2";
	replace original_subject = "Biology I" if original_subject == "B1";
	replace original_subject = "Chemistry" if original_subject == "C1";
	replace original_subject = "English I" if original_subject == "E1";
	replace original_subject = "English II" if original_subject == "E2";
	replace original_subject = "English III" if original_subject == "E3";

	drop if original_subject == "U1";

	gen test = "EOC/AYP";
	gen semester = "`s'";

	tempfile eoc_`s';
	save `eoc_`s'', replace;

};

* Portfolio;
use $alternative, clear;

destring grade unique_student_id, replace force;

foreach s in reading math science {;

	preserve;

	gen race_reported = 3 if ethnic_hispanic == 1;
	replace race_reported = 2 if race_black == 1;
	replace race_reported = 0 if race_amer_ind_alaska_ntv == 1;
	replace race_reported = 4 if race_ntv_hwn_opacisl == 1;
	replace race_reported = 1 if race_asian == 1;
	replace race_reported = 5 if race_white == 1;
	
	gen homebound = (rubric == 3);
	gen esl_services_t1_t2 = (t1 == 1) | (t2 == 1);
	gen nullify_flag = (nullified == 1);
	drop if (`s'cuts == . & nullified == 0);
	drop if void == 1 | med_exempt == 1;

	keep system_number system_name school_number school_name grade unique_student_id last_name first_name race_reported code_a_b 
		ell_excluded homebound esl_services_ell esl_services_t1_t2 functionally_delayed special_education `s'total `s'cuts nullify_flag;
	
	destring `s'total, replace force;
	rename (`s'total `s'cuts) (scale_score performance_level);

	replace special_education = (special_education != .);

	gen original_subject = "`s'";
	gen test = "Portfolio";

	replace code_a_b = "0" if code_a_b == "A";
	replace code_a_b = "1" if code_a_b == "B";

	rename code_a_b code_a_b_in;
	destring code_a_b_in, replace;

	tempfile portfolio_`s';
	save `portfolio_`s'', replace;

	restore;

};

* Put everything together;
clear;

foreach t in tcap_rla tcap_math tcap_sc eoc_summer eoc_fall eoc_spring portfolio_reading portfolio_math portfolio_science {;

	append using ``t'';

};

* Create student level file variables;
gen tested = 1;
gen valid_test = .;

replace original_subject = "Math" if original_subject == "math";
replace original_subject = "Reading/Language" if original_subject == "reading" | original_subject == "rla";
replace original_subject = "Science" if original_subject == "science" | original_subject == "sc";

rename unique_student_id state_student_id;

gen original_proficiency_level = "1. Below Basic" if performance_level == 1;
replace original_proficiency_level = "2. Basic" if performance_level == 2;
replace original_proficiency_level = "3. Proficient" if performance_level == 3;
replace original_proficiency_level = "4. Advanced" if performance_level == 4;

order scale_score tested valid_test state_student_id last_name first_name middle_initial grade, last;

gen race = "Hispanic/Latino" if race_reported == 3;
replace race = "Black or African American" if race_reported == 2;
replace race = "American Indian/Alaska Native" if race_reported == 0;
replace race = "Native Hawaiian/Pac. Islander" if race_reported == 4;
replace race = "Asian" if race_reported == 1;
replace race = "White" if race_reported == 5;
replace race = "Unknown" if race_reported == 6 | race_reported == .;

gen bhn_group = (race == "Black or African American" | race == "Hispanic/Latino" | race == "American Indian/Alaska Native");
order functionally_delayed, last;

rename special_education special_ed;
order special_ed, last;

gen economically_disadvantaged = (code_a_b_in == 0);

rename (esl_services_ell esl_services_t1_t2) (ell ell_t1_t2);
order ell ell_t1_t2 ell_exclude elsa_taken, last;

gen non_ell_elsa_flag = (elsa_taken == 1) & (ell == . & ell_t1_t2 == . & ell_exclude == .);

order homebound absent did_not_attempt nullify_flag residential_facility, last;

rename (system_number school_number) (system school);

keep system system_name school school_name test original_subject original_proficiency_level scale_score tested valid_test 
	state_student_id last_name first_name middle_initial grade race bhn_group functionally_delayed special_ed economically_disadvantaged
	ell ell_t1_t2 ell_exclude elsa_taken non_ell_elsa_flag homebound absent did_not_attempt nullify_flag residential_facility semester;

order system system_name school school_name test original_subject original_proficiency_level scale_score tested valid_test 
	state_student_id last_name first_name middle_initial grade race bhn_group functionally_delayed special_ed economically_disadvantaged
	ell ell_t1_t2 ell_exclude elsa_taken non_ell_elsa_flag homebound absent did_not_attempt nullify_flag residential_facility;

replace system_name = proper(system_name);
replace school_name = proper(school_name);


* Modifications and dropping excluded records;
gen proficiency_level = original_proficiency_level;
order proficiency_level, after(original_proficiency_level);

gen subject = original_subject;
order subject, after(original_subject);

* Home schools and private schools;
drop if system == 981 | system > 1000;

drop if grade == 13;

* Apply testing flag hierarchy (5.2.1);
* Absent students have a missing proficiency and tested value;
replace tested = 0 if absent == 1;
replace proficiency_level = "" if absent == 1;

* ELL Excluded students with missing proficiency are not considered tested (Non-ELA and Social Studies subjects);
* ELL Excluded students with a proficiency level are tested, but proficiency modified to missing (5.2.1);
replace tested = 0 if ell_excluded == 1 & (original_subject != "Reading/Language" | original_subject == "Social Studies");
replace proficiency_level = "" if ell_excluded == 1;

* Proficiency modified to missing if nullify or did not attempt (5.2.1);
replace proficiency_level = "" if (nullify_flag == 1 | did_not_attempt == 1);

* Students taking portfolio are considered special education (5.5);
replace special_ed = 1 if test == "Portfolio";

* Proficiency modified to missing if Non-ELL, T1, T2 and ELL Exclude student took ELSA (6.5);
* Tested modified to missing if Non-ELL, T1, T2 and ELL Exclude student took ELSA (6.6)
replace proficiency_level = "" if non_ell_elsa_flag == 1;
replace tested = 0 if non_ell_elsa_flag == 1;

* Modify subject for portfolio tests in grade >= 9 (6.8);
replace subject = "Algebra I" if original_subject == "Math" & test == "Portfolio" & (grade >= 9 & grade != .);
replace subject = "English II" if original_subject == "Reading/Language" & test == "Portfolio" & (grade >= 9 & grade != .);
replace subject = "Biology I" if original_subject == "Science" & test == "Portfolio" & (grade >= 9 & grade != .);

* Drop tests from CTE, adult HS, and alternative schools (6.9);
preserve;

import excel using $alt_hs_cte, firstrow clear;

rename (DISTRICT_NUMBER SCHOOL_NUMBER) (system school);
keep system school;

destring system school, replace;

drop if system == .;

tempfile alt_hs_cte;
save `alt_hs_cte', replace;

restore;

mmerge system school using `alt_hs_cte', type(n:1);
keep if _merge == 1;
drop _merge;

* Convert subjects for students in grade < 9 taking EOCs;
replace subject = "Math" if (original_subject == "Algebra I" | original_subject == "Algebra II" | original_subject == "Geometry") & grade < 9;
replace test = "Achievement" if (original_subject == "Algebra I" | original_subject == "Algebra II" | original_subject == "Geometry") & grade < 9;

replace subject = "Reading/Language" if (original_subject == "English I" | original_subject == "English II" | original_subject == "English III") & grade < 9;
replace test = "Achievement" if (original_subject == "English I" | original_subject == "English II" | original_subject == "English III") & grade < 9;

replace subject = "Science" if (original_subject == "Biology I" | original_subject == "Chemistry") & grade < 9;
replace test = "Achievement" if (original_subject == "Biology I" | original_subject == "Chemistry") & grade < 9;


/* Handling multiple test records */

* For students with multiple records across test types, ELSA has highest priority, then Portfolio, then TCAP/EOC (6.10);
gen test_priority = 3 if elsa_taken == 1;
replace test_priority = 2 if test == "Portfolio";
replace test_priority = 1 if test_priority == .;

bysort state_student_id subject grade: egen temp = max(test_priority);
drop if test_priority != temp;

drop test_priority temp;

* For students with EOC and Achievement records in the same content area, keep the EOC (6.9);
gen content_area = "Math" if subject == "Math" & | subject == "Algebra I" | subject == "Algebra II";
replace content_area = "English" if subject == "Reading/Language" | subject == "English I" | subject == "English II" | subject == "English III";
replace content_area = "Science" if subject == "Science" | subject == "Biology I" | subject == "Chemistry";

gen content_priority = 2 if original_subject == "Algebra I" | original_subject == "Algebra II" | original_subject == "English I" |
	original_subject == "English II" | original_subject == "English III" | original_subject == "Biology I" | original_subject == "Chemistry";
replace content_priority = 1 if original_subject == "Math" | original_subject == "Reading/Language" | original_subject == "Science";

bysort state_student_id content_area grade: egen temp = max(content_priority);
drop if content_priority != temp;

drop content_priority temp content_area;

* For students with multiple records within the same test, take highest proficiency level (6.10);
gen proficiency_priority = 4 if proficiency_level == "4. Advanced";
replace proficiency_priority = 3 if proficiency_level == "3. Proficient";
replace proficiency_priority = 2 if proficiency_level == "2. Basic";
replace proficiency_priority = 1 if proficiency_level == "1. Below Basic";

bysort state_student_id subject grade test: egen temp = max(proficiency_priority);
drop if proficiency_priority != temp;

drop proficiency_priority temp;

* For students with multiple records with the same proficiency across different administrations, take the most recent (6.10);
gen semester_priority = 3 if test == "Portfolio" | test == "Achievement" | (test == "EOC/AYP" & semester == "spring");
replace semester_priority = 2 if test == "EOC/AYP" & semester == "fall";
replace semester_priority = 1 if test == "EOC/AYP" & semester == "summer";

bysort state_student_id subject grade test: egen temp = max(semester_priority);
drop if semester_priority != temp;

drop semester_priority temp semester;

* Students with Achievement records across grades are tagged as absent for some records, dropping these records;
duplicates tag system school test subject state_student_id, gen(dup);
drop if dup != 0 & test == "Achievement" & absent == 1;

drop dup;

* Valid test if there is a proficiency level;
replace valid_test = (proficiency_level != "");

* Clean and Export file;
replace school = 8055 if system == 190 & school == 8053;

foreach v in functionally_delayed ell ell_t1_t2 ell_excluded elsa_taken homebound absent did_not_attempt residential_facility {;
	
	replace `v' = 0 if `v' == .;

};

sort state_student_id system school subject test grade;
quietly by state_student_id system school subject test grade: gen dup = cond(_N==1, 0, _n);

compress;

save "K:\ORP_accountability\projects\2016_pre_coding\Output/state_student_level_2016.dta", replace;
