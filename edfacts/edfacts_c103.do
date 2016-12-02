#delimit;
clear all;
set more off, perm;
set type double, perm;
capture log close;
macro drop _all;
program drop _all;
estimates drop _all;

/***************************************************************
Do File description:  Edfacts C103

Edited last by:  Alexander Poon

Date edited last: 12/2/2016
***************************************************************/

* State;
set obs 1;

egen first = seq();
gen state = 47;
gen agency = "01";
gen a = "";
gen b = "";
gen c = "";
gen d = "";
gen e = "";
gen ayp_status = "NOTREQ";
gen f = "";
gen g = "";
gen explanation = "";

* export delim "H:\EDEN Data\EDEN 15-16\Done\C103/TNSEASTATAYPc103y16.csv", delim(",") novarnames replace;

* System;
import delim "K:\ORP_accountability\data\2015_sas_accountability/system_final-determination_2015_20jul2015.csv", clear;

keep system;
egen first = seq();
gen state = 47;
gen agency = "01";
gen a = "";
gen alternate_approach = "NA";
gen b = "";
gen c = "";
gen ayp_status = "NOTREQ";
gen d = "";
gen e = "";
gen explanation = "";

order system, after(agency);
tostring system, replace;
replace system = "0" + system if real(system) < 100;

* export delim "H:\EDEN Data\EDEN 15-16\Done\C103/TNLEASTATAYPc103y16.csv", delim(",") novarnames replace;

* School;
import excel "H:\EDEN Data\EDEN 15-16\Done\C103/2015-16 EDFacts School Master FIle_11-18-16.xlsx", firstrow clear;

drop F G H I J;

gen alternate_approach = "NA";
gen a = "";
gen b = "";
gen ayp_status = "NOTREQ";
gen c = "";
gen explanation = "";

* export delim "H:\EDEN Data\EDEN 15-16\Done\C103/TNSCHSTATAYPc103y16.csv", delim(",") replace;
