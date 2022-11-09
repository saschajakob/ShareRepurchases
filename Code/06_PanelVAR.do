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

global temp		"/Users/saschajakob/Desktop/Paper 1/Temp"
*global data2	"A:\Sascha\Buyback Anomalies"
global data2 	"/Users/saschajakob/Desktop/Paper 1"

clear
clear matrix
set matsize 800
set scrollbufsize 500000
capture program drop _all
global F9 "browse;"

log using temp


*** portfolio approach -----------------------------------------------------

use "$data2/VAR_data.dta", clear
drop if fyear <1990

merge 1:1 permno month using "$data2/RKRV_misvaluation_monthly.dta", keepusing(RKRV_firm_error)
drop if _merge==2
drop _merge

tsfill
by permno: replace ccm_yq = l.ccm_yq + 1 if ccm_yq ==.
sort permno ccm_yq q_ret
duplicates drop permno ccm_yq, force

*preserve

* keep data within 20 quaretrs of a repurchase announcement
g rep_tag=.
replace rep_tag = 1 if rep_q ==1

forvalues i = 1/20 {
   bys permno: replace rep_tag=1 if rep_q[_n+`i']==1 & dealnumber[_n+`i']!=.
}

forvalues i = 1/20 {
   bys permno: replace rep_tag=1 if rep_q[_n-`i']==1 & dealnumber[_n-`i']!=.
}

keep if rep_tag==1
drop rep_tag


* filter for mispricing if necessary
*egen mispricing_group = xtile(RKRV_firm_error), nq(3)
*keep if mispricing_group==3

replace q_ret = 0.9*q_ret + 0.1*q_rf
replace roe_q = 0.9*roe_q + 0.1*q_rf
replace btm_q = 0.9*btm_q + 0.1*1

*market-adjusted excess log returns
gen lq_ret = ln(1+q_ret) - lq_rf
bysort ccm_yq: center(lq_ret)

*market-adjusted exces log ROE
gen lroe_q =ln(1+roe_q)-lq_rf
by ccm_yq: center(lroe_q)

* log Book-to-Market ratio
gen lbtm_q = ln(btm_q)
by ccm_yq: center(lbtm_q)

winsor2 btm_q roe_q lev_q lq_ret c_lq_ret lroe_q c_lroe_q lbtm_q c_lbtm_q q_ret btm_q roe_q, replace cuts(1 99)


*Discount factor regression

xtset permno ccm_yq
g dep_var = (c_lroe_q - c_lq_ret) + l.c_lbtm_q
reg dep_var c_lbtm_q

keep permno dealnumber rep_q ccm_yq lq_ret lroe_q lbtm_q c_lq_ret c_lroe_q c_lbtm_q q_ret btm_q roe_q
sort permno ccm_yq


*** VAR --------------------------------------------------------------------------------------------

xtset permno ccm_yq

* Panel VAR
pvar c_lq_ret c_lbtm_q c_lroe_q, lags(1)
log close

matrix coeff = e(b)
    matrix list coeff
    putexcel set "$data2\var_coeff_mat", replace
    putexcel A1 = matrix(coeff)
    putexcel close

matrix sigma_coeff = e(V)
    matrix list sigma_coeff
    putexcel set "$data2\var_sigma_coeff_mat", replace
    putexcel A1 = matrix(sigma_coeff)
    putexcel close

matrix sigma_resid = e(Sigma)
    matrix list sigma_resid
    putexcel set "$data2\var_sigma_resid_mat", replace
    putexcel A1 = matrix(sigma_resid)
    putexcel close

restore
***********************************************************************

*** generate data set for mispricing analysis (separate VARs)
egen mispricing_tag = xtile(RKRV_firm_error) if rep_q ==1, nq(5)


forvalues i = 1/20 {
   bys permno: replace mispricing_tag=1 if mispricing_tag[_n+`i']==4 & dealnumber[_n+`i']!=.
}

forvalues i = 1/20 {
   bys permno: replace mispricing_tag=1 if mispricing_tag[_n-`i']==4 & dealnumber[_n-`i']!=.
}


keep if mispricing_tag ==4

replace q_ret = 0.9*q_ret + 0.1*q_rf
replace roe_q = 0.9*roe_q + 0.1*q_rf
replace btm_q = 0.9*btm_q + 0.1*1


*market-adjusted excess log returns
gen lq_ret = ln(1+q_ret) - lq_rf
bysort ccm_yq: center(lq_ret)

*market-adjusted exces log ROE
gen lroe_q =ln(1+roe_q)-lq_rf
by ccm_yq: center(lroe_q)

* log Book-to-Market ratio
gen lbtm_q = ln(btm_q)
by ccm_yq: center(lbtm_q)

winsor2 btm_q roe_q lev_q lq_ret c_lq_ret lroe_q c_lroe_q lbtm_q c_lbtm_q q_ret btm_q roe_q, replace cuts(1 99)


*Discount factor regression

xtset permno ccm_yq
g dep_var = (c_lroe_q - c_lq_ret) + l.c_lbtm_q
*reg dep_var c_lbtm_q if mispricing_tag ==1

keep permno dealnumber rep_q ccm_yq lq_ret lroe_q lbtm_q c_lq_ret c_lroe_q c_lbtm_q q_ret btm_q roe_q mispricing_tag
sort permno ccm_yq


save "$data2\return_decomp_data_1980_2019_oval_fundamental.dta", replace
