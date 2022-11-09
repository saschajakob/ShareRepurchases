version 14.2
set rmsg on
set more off

********************************************************************************
*
*	JOB:			Prepare data for stock return decomposition
*	PROJECT:		SBB
*	INPUT:			Compustat and CRSP
*	OUTPUT:			Main data set for Return Decomposition
*
*	DESCRIPTION: 	This job puts together the sample for the decomposition.
*
********************************************************************************


* Define paths

global temp		"C:\Users\\`=c(username)'\Desktop\temp"
global data2	"/Users/saschajakob/Desktop/Paper 1"

clear
clear matrix
set matsize 800
set scrollbufsize 500000
capture program drop _all
global F9 "browse;"

use "$data2/decomp_data_single_gamma_uval.dta"
g error_cluster =1

append using "$data2/decomp_data_single_gamma_oval.dta"
replace error_cluster =5  if error_cluster ==.
duplicates tag dealnumber, gen(tag)
drop if tag==1

append using "$data2/decomp_data_single_gamma_fundamental.dta"
replace error_cluster =3 if error_cluster ==.
duplicates drop dealnumber, force

append using "$data2/decomp_data_single_gamma_uval_fundamental.dta"
replace error_cluster =2 if error_cluster ==.
duplicates drop dealnumber, force

append using "$data2/decomp_data_single_gamma_oval_fundamental.dta"
replace error_cluster =4 if error_cluster ==.
duplicates drop dealnumber, force

save "$data2/decomp_data_single_gamma_mispricing2.dta", replace
