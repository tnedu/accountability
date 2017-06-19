#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  District/School Accountability 2016

Edited last by:  Alexander Poon

Date edited last:  04/05/2016
***************************************************************/

* ACT Substitution for Accountability;
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/act_substitution_for_acct.do";

* CDF to Student Level;
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/cdf_to_student_level.do";

* District Accountability;
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/District/1. student_level_to_system_base.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/District/2. system_base_to_numeric.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/District/3. system_numeric_to_master_heat_map.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/District/4. master_heat_map_to_final_determinations.do";

* School Accountability;
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/School/1. student_level_to_school_base.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/School/2. school_base_to_numeric.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/School/3. grade_pools_designation_immune.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/School/4. priority_exit_2016.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/School/5. focus_exit_2016.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/School/6. reward_2016.do";

* State Base and Numeric;
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/State/1. student_level_to_state_base.do";
do "K:\ORP_accountability\projects\2016_pre_coding\Alex\Stata/State/2. state_base_to_numeric.do";
