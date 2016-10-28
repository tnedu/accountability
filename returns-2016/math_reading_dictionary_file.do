#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Math + Reading Dictionary File

Edited last by:  Alexander Poon

Date edited last:  10/24/2016
***************************************************************/

infix

	unique_student_id 3-11
	str last_name 12-36
	str first_name 37-51
	grade 61-62
	str ethnic_origin 63-63
	native_american 64-64
	asian 65-65
	black 66-66
	hawaiian_pi 67-67
	white 68-68
	reported_race 69-69
	str gender 70-70
	instructional_avail 71-71
	str greater_than_60_pct 73-73
	title_1 74-74
	economically_disadvantaged 75-75
	functionally_delayed 78-78
	migrant 79-79
	el_excluded 81-81
	el 82-82
	el_t1_t2 83-83
	special_ed 84-84
	homebound 86-86

	system_part_1 89-93
	str system_name_part_1 94-143
	school_part_1 144-147
	str school_name_part_1 148-197
	tln_part_1 274-282
	str content_area_code_part_1 341-342
	str test_admin_part_1 343-344
	modified_format_part_1 352-352
	ri_status_part_1 372-372

	system_part_2 415-419
	str system_name_part_2 420-469
	school_part_2 470-473
	str school_name_part_2 474-523
	tln_part_2 600-608
	str content_area_code_part_2 667-668
	str test_admin_part_2 669-670
	modified_format_part_2 678-678
	ri_status_part_2 698-698

	system_final 740-744
	str system_name_final 745-794
	school_final 795-798
	str school_name_final 799-848
	str content_area_code_final 889-890
	str test_admin_final 891-892
	part_1_or_2_only 895-895
	invalid_score 896-896
	performance_level 903-903
	scale_score 904-906
	scale_score_lb_ci 907-909
	scale_score_ub_ci 910-912

using "K:\ORP_accountability\data\2016_cdf\TNREADY_EOC/00000_0000_State_Student_Datafile_EOC_100percent.txt", clear;

save "K:\ORP_accountability\projects\2016_student_level_file/spring_math_english_cdf.dta", replace;

infix

	unique_student_id 3-11
	str last_name 12-36
	str first_name 37-51
	grade 61-62
	str ethnic_origin 63-63
	native_american 64-64
	asian 65-65
	black 66-66
	hawaiian_pi 67-67
	white 68-68
	reported_race 69-69
	str gender 70-70
	instructional_avail 71-71
	str greater_than_60_pct 73-73
	title_1 74-74
	economically_disadvantaged 75-75
	functionally_delayed 78-78
	migrant 79-79
	el_excluded 81-81
	el 82-82
	el_t1_t2 83-83
	special_ed 84-84
	homebound 86-86

	system_part_1 89-93
	str system_name_part_1 94-143
	school_part_1 144-147
	str school_name_part_1 148-197
	tln_part_1 274-282
	str content_area_code_part_1 341-342
	str test_admin_part_1 343-344
	modified_format_part_1 352-352
	ri_status_part_1 372-372

	system_part_2 415-419
	str system_name_part_2 420-469
	school_part_2 470-473
	str school_name_part_2 474-523
	tln_part_2 600-608
	str content_area_code_part_2 667-668
	str test_admin_part_2 669-670
	modified_format_part_2 678-678
	ri_status_part_2 698-698

	system_final 740-744
	str system_name_final 745-794
	school_final 795-798
	str school_name_final 799-848
	str content_area_code_final 889-890
	str test_admin_final 891-892
	part_1_or_2_only 895-895
	invalid_score 896-896
	performance_level 903-903
	scale_score 904-906
	scale_score_lb_ci 907-909
	scale_score_ub_ci 910-912

using "K:\ORP_accountability\data\2016_cdf\TNREADY_EOC/00000_0000_State_Student_Datafile_EOC_fall.txt", clear;

save "K:\ORP_accountability\projects\2016_student_level_file/fall_math_english_cdf.dta", replace;
