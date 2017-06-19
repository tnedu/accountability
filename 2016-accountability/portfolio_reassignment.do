#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Portfolio Reassignment Modeling

Edited last by:  Alexander Poon

Date edited last:  10/8/2015
***************************************************************/

global student_level "K:\ORP_accountability\data\2015_sas_accountability/state-student-level_2015_19jul2015.dta";

use $student_level, clear;

gen portfolio_PA = original_proficiency_level == "3. Proficient" | original_proficiency_level == "4. Advanced" 
	if valid_test == 1 & test == "Portfolio";
	
gen enrolled = 1;

replace subject = "SS" if subject == "Social Studies";
replace subject = "RLA" if subject == "Reading/Language";
replace subject = "AlgI" if subject == "Algebra I";
replace subject = "AlgII" if subject == "Algebra II";
replace subject = "BioI" if subject == "Biology I";
replace subject = "EngI" if subject == "English I";
replace subject = "EngII" if subject == "English II";
replace subject = "EngIII" if subject == "English III";
replace subject = "USHistory" if subject == "U.S. History";

* By subject, calculate percent of PA (out of enrolled) that come from Portfolio;
levelsof subject, local(subjects);
foreach s in `subjects' {;

	disp "`s'";
	quietly count if subject == "`s'" & portfolio_PA == 1;
	local `s'_PA = r(N);
	
	* Count individual records for enrolled;
	quietly count if subject == "`s'" & enrolled == 1;
	local `s'_enrolled = r(N);
	
	local `s'_portfolio_PA = 100 * ``s'_PA'/``s'_enrolled';
	disp "``s'_portfolio_PA'%";

};

egen subgroup_count = rowtotal(bhn_group special_ed economically_disadvantaged ell ell_t1_t2);
gen portfolio_reassigned_ap = 0;
gen student_random = runiform();

foreach s in `subjects' {;

	preserve;

	disp "`s'";
	keep if subject == "`s'";

	quietly while ``s'_portfolio_PA' > 1 {;
		
		bysort system system_name subject: egen district_portfolio_PA_count = sum(portfolio_PA);
		bysort system system_name subject: egen district_enrolled_count = sum(enrolled);
		gen district_PA_pct = district_portfolio_PA_count/district_enrolled_count;

		bysort system system_name school school_name subject: egen school_portfolio_PA_count = sum(portfolio_PA);
		bysort system system_name school school_name subject: egen school_enrolled_count = sum(enrolled);
		gen school_PA_pct = school_portfolio_PA_count/school_enrolled_count;

		* Districts with the highest percentage of portfolio students are reassigned first;
		* Schools with the highest percentage of portfolio students are reassigned first within districts;
		* Schools with more enrolled are reassigned first;
		* Students in more subgroups are reassigned last, counting only the 4 main subgroups;
		gsort -district_PA_pct -school_PA_pct -enrolled -portfolio_PA subgroup_count portfolio_reassigned_ap -student_random;

		replace portfolio_reassigned_ap = 1 if test == "Portfolio" & [_n] == 1;
		replace portfolio_PA = 0 if test == "Portfolio" & [_n] == 1;

		drop district_PA_pct district_portfolio_PA_count district_enrolled_count school_PA_pct school_portfolio_PA_count school_enrolled_count;

		quietly count if subject == "`s'" & portfolio_PA == 1;
		local `s'_PA = r(N);

		quietly count if subject == "`s'" & enrolled == 1;
		local `s'_enrolled = r(N);

		local `s'_portfolio_PA = 100 * ``s'_PA'/``s'_enrolled';
		noisily disp "`s' Percent P/A from Portfolio = ``s'_portfolio_PA'";

	};

	tempfile `s'_portfolio_reassigned;
	save ``s'_portfolio_reassigned', replace;

	restore;

};

clear;
foreach s in `subjects' {;

	append using ``s'_portfolio_reassigned';
	
};
