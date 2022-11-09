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


* Format Data

*market value from CRSP
use "$data2/CRSP_monthly_1970_2019.dta", clear
g mktcap = abs(prc)*shrout/1000
keep permno date mktcap
rename date datadate
duplicates drop permno datadate, force
save "$temp/marketcap.dta", replace


*Load Quaterly Data
use gvkey linkprim liid linktype lpermno lpermco linkdt linkenddt datadate fyear fyr fqtr apdedateq indfmt consol popsrc datafmt cusip datafqtr datacqtr txditcq  ///
cheq atq ltq lltq actq cshoq cshopq prccq seqq ceqq pstkq pstkrq dlcq dlttq lctq txtq niq dvpspq dvy mkvaltq epsfiq epspiq cusip ///
using "$data2/CCM_NA_Quarterly_1980_2019.dta", clear

keep if (indfmt=="INDL") & (datafmt=="STD") & (popsrc=="D") & (consol=="C")
drop indfmt datafmt popsrc consol

keep if linktype == "LU" | linktype == "LC"
keep if linkprim == "P" | linkprim == "C"

rename lpermno permno
sort permno datadate
duplicates drop permno datadate, force
g quarter = qofd(datadate)
g month = mofd(datadate)
format quarter %tq
format month %tm
duplicates drop permno quarter, force


merge 1:1 permno datadate using "$temp/marketcap.dta"
drop if _merge == 2
drop _merge

sort permno quarter
xtset permno quarter

* market equity
replace mktcap = mkvaltq if mktcap ==.
replace mktcap = prccq*cshoq if mktcap == .
replace mktcap = l.mktcap*(prccq/l.prccq) if mktcap ==.
drop mkvaltq
rename mktcap mkvaltq

*** book equity Michaely (2020): ----------------------------------------------------------------------------------------

    * Depending on availability, we use stockholders' equity (item SEQQ),
    * or common equity (item CEQQ) plus the carrying value of preferred stock (item PSTKQ),
        replace seqq = ceqq + pstkq if seqq == .
    * or total assets (item ATQ) minus total liabilities (item LTQ)
        replace seqq = atq - ltq if seqq == .
    * in that order as shareholders' equity.

    * Book equity is defined as in Weber (2018) and Chinco et al. (2019) and equals
    * shareholders' equity plus balance-sheet deferred taxes and investment tax credit (item TXDITCQ)
    * if available,
        replace txditcq = 0 if txditcq == .
    * minus the book value of preferred stock.
    * We use redemption value (item PSTKRQ) if available,
    * or carrying value for the book value of preferred stock.

g b_equity_q = seqq + txditcq - pstkrq
replace b_equity_q = seqq + txditcq - pstkq if b_equity_q == .

    * If book equity is unavailable, we proxy it by the last period's book equity plus
    * earnings, less dividends.

    g q_temp = substr(datafqtr,6,1)
    destring q_temp, replace
    g divq =.
    replace divq = dvy if q_temp ==1
    replace divq =dvy - l.dvy if inrange(q_temp,2,4)
    // g divq = dvpspq*cshoq
    drop q_temp

    replace b_equity_q = l.b_equity_q + niq - divq if b_equity_q ==.

    * If neither earnings nor book equity are available, we assume the  book-to-market ratio has not changed,
    * and compute the book-equity proxy from the last period's book-to-market ratio and this period's market equity.
    replace b_equity_q =. if b_equity_q <=0

    g btm_q = b_equity_q/mkvaltq

    replace b_equity_q = mkvaltq*l.btm_q if b_equity_q ==.
    replace b_equity_q =. if b_equity_q <=0
    replace btm_q = b_equity_q/mkvaltq if btm_q ==.
*** ----------------------------------------------------------------------------------------------------------------------




*** ROE Michaely (2020): -------------------------------------------------------------------------------------------------

    * We use earnings available for common equity, in the ROE formula.
        g roe_q = niq/l.b_equity_q
    * When earnings are missing, we use the clean-surplus formula to compute a proxy for earnings.
        replace roe_q = ((b_equity - l.b_equity_q) + divq)/l.b_equity_q if roe_q ==.
    * In either case, we do not allow the firm to los emore than its book equity. Hence, the minimum GAAP ROE is truncated to -100%.
        // replace roe_q = -1 if roe_q < -1
        drop if roe_q <-1
*** --------------------------------------------------------------------------------------------------------------------------




*** Leverage Michaely (2020) -------------------------------------------------------------------------------------------------

    * Book debt is the sum of debt in current liabilities, total long-term debt, and preferred stock.
        g b_debt_q = dlcq + dlttq + pstkrq
        replace b_debt_q = dlcq + dlttq + pstkq if b_debt_q ==.
    * We calculate leverage as book debt over the sum of book equity and book debt
        g lev_q = b_debt_q/(b_debt_q + b_equity_q)

        g y = yofd(datadate)
        drop if y <1980
        g m = month(datadate)
        replace y = y-1 if m==1

        g ccm_q =.
        replace ccm_q =1 if m==2 | m==3 | m==4
        replace ccm_q =2 if m==5 | m==6 | m==7
        replace ccm_q =3 if m==8 | m==9 | m==10
        replace ccm_q =4 if m==11 | m==12 | m==1

        g ccm_yq = yq(y, ccm_q)
        format ccm_yq %tq

        g y_temp = substr(datacqtr,1,4)
        g q_temp = substr(datacqtr,6,1)
        destring y_temp, replace
        destring q_temp, replace
        gen yc_temp = yq(y_temp, q_temp)
        format yc_temp %tq
        g check = ccm_yq - yc_temp

        duplicates tag permno ccm_yq, gen(tag)
        g tag2 =.
        replace tag2 =1 if tag ==1 & btm_q==.
        drop if tag2 ==1
        duplicates drop permno ccm_yq, force

drop if l.b_equity_q ==. | l2.b_equity_q ==. | l3.b_equity_q ==. | l.niq ==. | l2.niq ==. | l.dlttq ==. | l2.dlttq ==. | l.mkvaltq ==. | l2.mkvaltq ==. | l3.mkvaltq ==. | l.mkvaltq <10 | l.btm_q < 0.01 | l.btm_q > 100


keep gvkey permno datadate fyearq fqtr fyr cusip datacqtr datafqtr quarter month btm_q roe_q lev_q ccm_yq mkvaltq cusip b_equity_q niq dlttq mkvaltq
replace cusip = substr(cusip,1,8)

save "$temp/CCM_decomp_clean.dta", replace


use "$data2/sdc_repurchase_clean", clear
gen rep_date = date(daterepurchaseauthorizedbyboard, "DMY")
format rep_date %td
g y = yofd(rep_date)
drop if y <1980
g m = month(rep_date)
replace y = y-1 if m==1

g ccm_q =.
replace ccm_q =1 if m==2 | m==3 | m==4
replace ccm_q =2 if m==5 | m==6 | m==7
replace ccm_q =3 if m==8 | m==9 | m==10
replace ccm_q =4 if m==11 | m==12 | m==1

g ccm_yq = yq(y, ccm_q)
format ccm_yq %tq
g rep_q =1

keep cusip rep_date rep_q ccm_yq dealnumber
duplicates drop ccm_yq cusip, force

merge 1:1 cusip ccm_yq using "$temp/CCM_decomp_clean.dta"
drop if _merge==1
drop _merge
sort permno quarter

/*
drop if rep_q == 1 & l.b_equity_q ==.
drop if rep_q == 1 & l2.b_equity_q ==.
drop if rep_q == 1 & l3.b_equity_q ==.
drop if rep_q == 1 & l.niq ==.
drop if rep_q == 1 & l2.niq ==.
drop if rep_q == 1 & l.dlttq ==.
drop if rep_q == 1 & l2.dlttq ==.
drop if rep_q == 1 & l.mkvaltq ==.
drop if rep_q == 1 & l2.mkvaltq ==.
drop if rep_q == 1 & l3.mkvaltq ==.
drop if rep_q == 1 & l.mkvaltq <10
drop if rep_q == 1 & l.btm_q < 0.01
drop if rep_q == 1 & l.btm_q > 100
drop if l.mkvaltq <10 | l.btm_q < 0.01 | l.btm_q > 100*/

save "$temp/CCM_decomp_clean.dta", replace


*** Risk free rate from FF --------------------
use "$data2/ff5f.dta", clear
drop if y <1980
replace y = y-1 if m==1
replace rf = rf/100

g ccm_q =.
replace ccm_q =1 if m==2 | m==3 | m==4
replace ccm_q =2 if m==5 | m==6 | m==7
replace ccm_q =3 if m==8 | m==9 | m==10
replace ccm_q =4 if m==11 | m==12 | m==1

egen ccm_yq = group(y ccm_q)
gen ln_rf =ln(1+(rf))
bysort ccm_yq: egen q_rf= total(ln_rf)
replace q_rf = exp(q_rf)-1


replace ccm_yq = yq(y, ccm_q)
format ccm_yq %tq
keep ccm_yq q_rf
duplicates drop ccm_yq q_rf, force
gen lq_rf = ln(1+q_rf)

save "$temp/rf_decomp.dta", replace


*** CRSP --------------------------------------------------------------------------------------------------

use "$data2/CRSP_monthly_1970_2019.dta", clear
g y = yofd(date)
drop if y <1980
g m = month(date)
replace y = y-1 if m==1
duplicates drop permno date, force
replace dlret =-0.3 if dlret ==. & dlstcd == 500 | inrange(dlstcd,520,584)
replace ret = dlret if ret ==. & dlpdt !=.

g ccm_q =.
replace ccm_q =1 if m==2 | m==3 | m==4
replace ccm_q =2 if m==5 | m==6 | m==7
replace ccm_q =3 if m==8 | m==9 | m==10
replace ccm_q =4 if m==11 | m==12 | m==1

egen ccm_yq = group(permno y ccm_q)

gen ln_ret =ln(1+ret)
bysort ccm_yq: egen q_ret = total(ln_ret)
replace q_ret = exp(q_ret)-1

replace ccm_yq = yq(y, ccm_q)
format ccm_yq %tq
g calendar_yq = qofd(date)
format calendar_yq %tq

keep permno ccm_yq q_ret
sort permno ccm_yq
duplicates drop permno ccm_yq, force

merge m:1 ccm_yq using "$temp/rf_decomp.dta"
keep if _merge == 3
drop _merge

*** merging -----------------------------------------------------------------

merge 1:1 permno ccm_yq using "$temp/CCM_decomp_clean.dta"
keep if _merge ==3
drop _merge

drop if fyear <1980

save "$temp/return_decomp_data_1980_2019.dta", replace
save "$data2/VAR_data.dta", replace

*** portfolio approach -----------------------------------------------------
//
// use "$temp/return_decomp_data_1980_2019.dta", clear
// drop if fyear <1990
//
// merge 1:1 permno month using "$data2/RKRV_misvaluation_monthly.dta", keepusing(RKRV_firm_error)
// drop if _merge==2
// drop _merge
//
// tsfill
// by permno: replace ccm_yq = l.ccm_yq + 1 if ccm_yq ==.
// sort permno ccm_yq q_ret
// duplicates drop permno ccm_yq, force
//
// *preserve
//
// g rep_tag=.
// replace rep_tag = 1 if rep_q ==1
//
// forvalues i = 1/20 {
//    bys permno: replace rep_tag=1 if rep_q[_n+`i']==1 & dealnumber[_n+`i']!=.
// }
//
// forvalues i = 1/20 {
//    bys permno: replace rep_tag=1 if rep_q[_n-`i']==1 & dealnumber[_n-`i']!=.
// }
//
// keep if rep_tag==1
// drop rep_tag
//
// *egen mispricing_group = xtile(RKRV_firm_error), nq(3)
//
//
// *keep if RKRV_firm_error <-0.33
// *keep if mispricing_group==3
//
// replace q_ret = 0.9*q_ret + 0.1*q_rf
// replace roe_q = 0.9*roe_q + 0.1*q_rf
// replace btm_q = 0.9*btm_q + 0.1*1
//
// *market-adjusted excess log returns
// gen lq_ret = ln(1+q_ret) - lq_rf
// bysort ccm_yq: center(lq_ret)
//
// *market-adjusted exces log ROE
// gen lroe_q =ln(1+roe_q)-lq_rf
// by ccm_yq: center(lroe_q)
//
// * log Book-to-Market ratio
// gen lbtm_q = ln(btm_q)
// by ccm_yq: center(lbtm_q)
//
// winsor2 btm_q roe_q lev_q lq_ret c_lq_ret lroe_q c_lroe_q lbtm_q c_lbtm_q q_ret btm_q roe_q, replace cuts(1 99)
//
//
// *Discount factor regression
//
// xtset permno ccm_yq
// g dep_var = (c_lroe_q - c_lq_ret) + l.c_lbtm_q
// reg dep_var c_lbtm_q
//
// keep permno dealnumber rep_q ccm_yq lq_ret lroe_q lbtm_q c_lq_ret c_lroe_q c_lbtm_q q_ret btm_q roe_q
// sort permno ccm_yq
//
//
// *** VAR --------------------------------------------------------------------------------------------
//
// xtset permno ccm_yq
//
// * Panel VAR
// pvar c_lq_ret c_lbtm_q c_lroe_q, lags(1)
//
//
// matrix coeff = e(b)
//     matrix list coeff
//     putexcel set "$data2\var_coeff_mat", replace
//     putexcel A1 = matrix(coeff)
//     putexcel close
//
// matrix sigma_coeff = e(V)
//     matrix list sigma_coeff
//     putexcel set "$data2\var_sigma_coeff_mat", replace
//     putexcel A1 = matrix(sigma_coeff)
//     putexcel close
//
// matrix sigma_resid = e(Sigma)
//     matrix list sigma_resid
//     putexcel set "$data2\var_sigma_resid_mat", replace
//     putexcel A1 = matrix(sigma_resid)
//     putexcel close
//
// restore
//
// ***********************************************************************
// egen mispricing_tag = xtile(RKRV_firm_error) if rep_q ==1, nq(5)
//
//
// forvalues i = 1/20 {
//    bys permno: replace mispricing_tag=1 if mispricing_tag[_n+`i']==4 & dealnumber[_n+`i']!=.
// }
//
// forvalues i = 1/20 {
//    bys permno: replace mispricing_tag=1 if mispricing_tag[_n-`i']==4 & dealnumber[_n-`i']!=.
// }
//
//
// keep if mispricing_tag ==4
//
// replace q_ret = 0.9*q_ret + 0.1*q_rf
// replace roe_q = 0.9*roe_q + 0.1*q_rf
// replace btm_q = 0.9*btm_q + 0.1*1
//
//
// *market-adjusted excess log returns
// gen lq_ret = ln(1+q_ret) - lq_rf
// bysort ccm_yq: center(lq_ret)
//
// *market-adjusted exces log ROE
// gen lroe_q =ln(1+roe_q)-lq_rf
// by ccm_yq: center(lroe_q)
//
// * log Book-to-Market ratio
// gen lbtm_q = ln(btm_q)
// by ccm_yq: center(lbtm_q)
//
// winsor2 btm_q roe_q lev_q lq_ret c_lq_ret lroe_q c_lroe_q lbtm_q c_lbtm_q q_ret btm_q roe_q, replace cuts(1 99)
//
//
// *Discount factor regression
//
// xtset permno ccm_yq
// g dep_var = (c_lroe_q - c_lq_ret) + l.c_lbtm_q
// *reg dep_var c_lbtm_q if mispricing_tag ==1
//
// keep permno dealnumber rep_q ccm_yq lq_ret lroe_q lbtm_q c_lq_ret c_lroe_q c_lbtm_q q_ret btm_q roe_q mispricing_tag
// sort permno ccm_yq
//
//
// save "$data2\return_decomp_data_1980_2019_oval_fundamental.dta", replace
