version 14.2
set rmsg on
set more off

********************************************************************************
*
*	JOB:			Format and merge SDC repruchase data, Compustat, and CRSP
*	PROJECT:		SBB
*	INPUT:			SDC Repurchase data, CRSP, Compustat, ownership data (13F),
*					short interest data, amihud measure
*	OUTPUT:			Main data set for Share repurchase analysis
*
*	DESCRIPTION: 	This job puts together the final share repurchase sample.
*
********************************************************************************

* Define paths

global temp		"C:\Users\\`=c(username)'\Desktop\temp"
global data		"A:\Sascha\Buyback Anomalies"
*global data		"A:\Shares\Atlas\Sascha\Buyback Anomalies"

clear
clear matrix
set matsize 800
set scrollbufsize 500000
capture program drop _all
global F9 "browse;"

*******************************************************
capture program drop winsor
  program define winsor
    quiet sum `1', detail
    replace `1' = r(p1)  if `1' ~= . & (`1' < r(p1))  & `2' == 1
    replace `1' = r(p99) if `1' ~= . & (`1' > r(p99)) & `2' == 1
    replace `1' = r(p5)  if `1' ~= . & (`1' < r(p5))  & `2' == 5
    replace `1' = r(p95) if `1' ~= . & (`1' > r(p95)) & `2' == 5
  end
**********************************************************

********************************************************************************
************************** SDC Repurchase Data *********************************
********************************************************************************

use "$data\sdc_repurchase_clean.dta", clear
distinct dealnumber																// 19380
cross using "$data\time.dta"
rename date deal_date
gen date = deal_date + time
format date %d
sort cusip date
save "$temp\sdc_repurchase_clean.dta", replace
clear

********************************************************************************
************************** CRSP Daily Data *************************************
********************************************************************************

use "$data\CRSP_daily_1985_2018.dta", clear
duplicates drop cusip date, force
sort cusip date
joinby cusip date using "$temp\sdc_repurchase_clean.dta"
distinct dealnumber

merge m:1 permno month using "$data\buyback_entropy.dta", keep(3) nogen
sort permno dealnumber date
replace entropy_H =. if time!=0
replace entropy_I =. if time!=0
rename month dealmonth
g month =mofd(date)
format month %tm
save "$data\sdc_repurchase_daily.dta", replace

use "$data\ff_daily.dta", clear
merge 1:m date using "$data\sdc_repurchase_daily.dta", keep(3) nogen
save "$data\sdc_repurchase_daily.dta", replace

********************************************************************************
*********************************** CARs ***************************************
********************************************************************************
use "$data\sdc_repurchase_daily.dta", clear

			drop if ret == .

			sort time
			egen group = group(dealnumber cusip)
			duplicates drop group date, force

			bysort dealnumber cusip: egen tmp = max(time)										// drop stocks whose time series finishes before the deal date

			drop if tmp < 0
			drop tmp

			bysort dealnumber: egen tmp = min(time) if time >= 0

			tsset dealnumber date
			gsort dealnumber -date
			bysort dealnumber: carryforward tmp, replace

			bysort dealnumber: gen dist = 0 if tmp == time
			sort dealnumber time
			by dealnumber: replace dist = dist[_n-1] + 1 if time > tmp
			by dealnumber: egen tmp2 = count(dealnumber)
			gsort dealnumber -date
			by dealnumber: replace dist = dist[_n-1] - 1 if time < tmp

			sort dealnumber cusip dist
			drop tmp tmp2

			bysort group: egen rank = rank(-time) if time <= 0									// ranks rival prices/returns for each deal from lowest (day of the deal, rank 1) to highest (380 obs away from the deal, rank 381)
			keep if rank <= 271 | missing(rank)

			bysort group: gen diff_time = time[_n+1] - time[_n]									// drops observations before the last return break to the deal date
			gen tmp = 1 if (diff_time > 10 & !missing(diff_time))
			gsort group -time
			by group: carryforward tmp, replace
			drop if tmp == 1
			drop tmp

			gen ar_m      = .
			gen ar_ff	  = .

			gen contemp_r2 	  = .
			gen contemp_r2a   = .
			gen lag_r2 	  = .
			gen lag_r2a   = .
			gen b0 = .
			gen se0 = .
			gen b1 = .
			gen se1 = .
			gen b2 = .
			gen se2 = .
			gen b3 = .
			gen se3 = .
			gen b4 = .
			gen se4 = .
			gen b5 = .
			gen se5 = .

            gen contemp_r2_12m 	  = .
            gen contemp_r2a_12m   = .
            gen lag_r2_12m 	  = .
            gen lag_r2a_12m   = .
            gen b0_12m = .
            gen se0_12m = .
            gen b1_12m = .
            gen se1_12m = .
            gen b2_12m = .
            gen se2_12m = .
            gen b3_12m = .
            gen se3_12m = .
            gen b4_12m = .
            gen se4_12m = .
            gen b5_12m = .
            gen se5_12m = .

            gen contemp_r2_24m 	  = .
            gen contemp_r2a_24m   = .
            gen lag_r2_24m 	  = .
            gen lag_r2a_24m   = .
            gen b0_24m = .
            gen se0_24m = .
            gen b1_24m = .
            gen se1_24m = .
            gen b2_24m = .
            gen se2_24m = .
            gen b3_24m = .
            gen se3_24m = .
            gen b4_24m = .
            gen se4_24m = .
            gen b5_24m = .
            gen se5_24m = .

            gen contemp_r2_36m 	  = .
            gen contemp_r2a_36m   = .
            gen lag_r2_36m 	  = .
            gen lag_r2a_36m   = .
            gen b0_36m = .
            gen se0_36m = .
            gen b1_36m = .
            gen se1_36m = .
            gen b2_36m = .
            gen se2_36m = .
            gen b3_36m = .
            gen se3_36m = .
            gen b4_36m = .
            gen se4_36m = .
            gen b5_36m = .
            gen se5_36m = .

            gen contemp_r2_48m 	  = .
            gen contemp_r2a_48m   = .
            gen lag_r2_48m 	  = .
            gen lag_r2a_48m   = .
            gen b0_48m = .
            gen se0_48m = .
            gen b1_48m = .
            gen se1_48m = .
            gen b2_48m = .
            gen se2_48m = .
            gen b3_48m = .
            gen se3_48m = .
            gen b4_48m = .
            gen se4_48m = .
            gen b5_48m = .
            gen se5_48m = .

			g retrf = ret - rf

			drop group
			egen group = group(dealnumber cusip)
			tsset group dist

			qui su group
			qui forvalues i = 1(1)`r(max)'{														// loop over all groups
				qui su ret if group == `i' & dist < -21											// determine the number of available returns

				local nobs = `r(N)'
				if `nobs' >= 100 {																// number of obs is larger or equal to 100

					reg ret vwretd if group == `i' & dist < -21 								// regress return on market return for each ID if return occurs at least 21 obs before the deal, estimation period (-250 to -21)
					predict e, resid															// calculates least squares residuals
					replace ar_m = e if group ==`i'
					drop e

					reg ret vwretd smb hml umd if group ==`i' & dist < -21						// regress target return on value-weighted market return for each ID if return occurs at least 21 obs before the deal, estimation period (-250 to -21)
					predict e, resid															// calculates least squares residuals
					replace ar_ff = e if group ==`i'
					drop e

					*myopic
					capture reg ret vwretd if group == `i' & inrange(dist,-126,-1)
                    if _rc != 0{
                    continue
                    }
					replace contemp_r2 = e(r2) if group ==`i' & dist == 0
					replace contemp_r2a = e(r2_a) if group ==`i' & dist == 0
					capture reg ret vwretd l1.vwretd l2.vwretd l3.vwretd l4.vwretd l5.vwretd if group == `i' & inrange(dist,-126,-1)
                    if _rc != 0{
                    continue
                    }
					replace lag_r2 = e(r2) if group ==`i' & dist == 0
					replace lag_r2a = e(r2_a) if group ==`i' & dist == 0
					replace b0 = _b[vwretd] if group ==`i' & dist == 0
					replace se0 = _se[vwretd] if group ==`i' & dist == 0
					replace b1 = _b[L1.vwretd] if group ==`i' & dist == 0
					replace se1 = _se[L1.vwretd] if group ==`i' & dist == 0
					replace b2 = _b[L2.vwretd] if group ==`i' & dist == 0
					replace se2 = _se[L2.vwretd] if group ==`i' & dist == 0
					replace b3 = _b[L3.vwretd] if group ==`i' & dist == 0
					replace se3 = _se[L3.vwretd] if group ==`i' & dist == 0
					replace b4 = _b[L4.vwretd] if group ==`i' & dist == 0
					replace se4 = _se[L4.vwretd] if group ==`i' & dist == 0
					replace b5 = _b[L5.vwretd] if group ==`i' & dist == 0
					replace se5 = _se[L5.vwretd] if group ==`i' & dist == 0
				}

                qui su ret if group == `i' & inrange(dist,127,252)
                if `nobs' >= 60 {

                    *12 months
                    capture reg ret vwretd if group == `i' & inrange(dist,127,252)
                    if _rc != 0{
                    continue
                    }
					replace contemp_r2_12m = e(r2) if group ==`i' & dist == 0
					replace contemp_r2a_12m = e(r2_a) if group ==`i' & dist == 0
					capture reg ret vwretd l1.vwretd l2.vwretd l3.vwretd l4.vwretd l5.vwretd if group == `i' & inrange(dist,127,252)
                    if _rc != 0{
                    continue
                    }
					replace lag_r2_12m = e(r2) if group ==`i' & dist == 0
					replace lag_r2a_12m = e(r2_a) if group ==`i' & dist == 0
					replace b0_12m = _b[vwretd] if group ==`i' & dist == 0
					replace se0_12m = _se[vwretd] if group ==`i' & dist == 0
					replace b1_12m = _b[L1.vwretd] if group ==`i' & dist == 0
					replace se1_12m = _se[L1.vwretd] if group ==`i' & dist == 0
					replace b2_12m = _b[L2.vwretd] if group ==`i' & dist == 0
					replace se2_12m = _se[L2.vwretd] if group ==`i' & dist == 0
					replace b3_12m = _b[L3.vwretd] if group ==`i' & dist == 0
					replace se3_12m = _se[L3.vwretd] if group ==`i' & dist == 0
					replace b4_12m = _b[L4.vwretd] if group ==`i' & dist == 0
					replace se4_12m = _se[L4.vwretd] if group ==`i' & dist == 0
					replace b5_12m = _b[L5.vwretd] if group ==`i' & dist == 0
					replace se5_12m = _se[L5.vwretd] if group ==`i' & dist == 0
                }

                qui su ret if group == `i' & inrange(dist,378,504)
                if `nobs' >= 60 {

                    *24 months
                    capture reg ret vwretd if group == `i' & inrange(dist,378,504)
                    if _rc != 0{
                    continue
                    }
					replace contemp_r2_24m = e(r2) if group ==`i' & dist == 0
					replace contemp_r2a_24m = e(r2_a) if group ==`i' & dist == 0
					capture reg ret vwretd l1.vwretd l2.vwretd l3.vwretd l4.vwretd l5.vwretd if group == `i' & inrange(dist,378,504)
                    if _rc != 0{
                    continue
                    }
					replace lag_r2_24m = e(r2) if group ==`i' & dist == 0
					replace lag_r2a_24m = e(r2_a) if group ==`i' & dist == 0
					replace b0_24m = _b[vwretd] if group ==`i' & dist == 0
					replace se0_24m = _se[vwretd] if group ==`i' & dist == 0
					replace b1_24m = _b[L1.vwretd] if group ==`i' & dist == 0
					replace se1_24m = _se[L1.vwretd] if group ==`i' & dist == 0
					replace b2_24m = _b[L2.vwretd] if group ==`i' & dist == 0
					replace se2_24m = _se[L2.vwretd] if group ==`i' & dist == 0
					replace b3_24m = _b[L3.vwretd] if group ==`i' & dist == 0
					replace se3_24m = _se[L3.vwretd] if group ==`i' & dist == 0
					replace b4_24m = _b[L4.vwretd] if group ==`i' & dist == 0
					replace se4_24m = _se[L4.vwretd] if group ==`i' & dist == 0
					replace b5_24m = _b[L5.vwretd] if group ==`i' & dist == 0
					replace se5_24m = _se[L5.vwretd] if group ==`i' & dist == 0
                }

                qui su ret if group == `i' & inrange(dist,630,756)
                if `nobs' >= 60 {

                    *36 months
                    capture reg ret vwretd if group == `i' & inrange(dist,630,756)
                    if _rc != 0{
                    continue
                    }
					replace contemp_r2_36m = e(r2) if group ==`i' & dist == 0
					replace contemp_r2a_36m = e(r2_a) if group ==`i' & dist == 0
					reg ret vwretd l1.vwretd l2.vwretd l3.vwretd l4.vwretd l5.vwretd if group == `i' & inrange(dist,630,756)
					replace lag_r2_36m = e(r2) if group ==`i' & dist == 0
					replace lag_r2a_36m = e(r2_a) if group ==`i' & dist == 0
					replace b0_36m = _b[vwretd] if group ==`i' & dist == 0
					replace se0_36m = _se[vwretd] if group ==`i' & dist == 0
					replace b1_36m = _b[L1.vwretd] if group ==`i' & dist == 0
					replace se1_36m = _se[L1.vwretd] if group ==`i' & dist == 0
					replace b2_36m = _b[L2.vwretd] if group ==`i' & dist == 0
					replace se2_36m = _se[L2.vwretd] if group ==`i' & dist == 0
					replace b3_36m = _b[L3.vwretd] if group ==`i' & dist == 0
					replace se3_36m = _se[L3.vwretd] if group ==`i' & dist == 0
					replace b4_36m = _b[L4.vwretd] if group ==`i' & dist == 0
					replace se4_36m = _se[L4.vwretd] if group ==`i' & dist == 0
					replace b5_36m = _b[L5.vwretd] if group ==`i' & dist == 0
					replace se5_36m = _se[L5.vwretd] if group ==`i' & dist == 0
                }

                qui su ret if group == `i' & inrange(dist,882,1008)
                if `nobs' >= 60 {

                    *48 months
                    capture reg ret vwretd if group == `i' & inrange(dist,882,1008)
                    if _rc != 0{
                    continue
                    }
					replace contemp_r2_48m = e(r2) if group ==`i' & dist == 0
					replace contemp_r2a_48m = e(r2_a) if group ==`i' & dist == 0
					capture reg ret vwretd l1.vwretd l2.vwretd l3.vwretd l4.vwretd l5.vwretd if group == `i' & inrange(dist,882,1008)
                    if _rc != 0{
                    continue
                    }
					replace lag_r2_48m = e(r2) if group ==`i' & dist == 0
					replace lag_r2a_48m = e(r2_a) if group ==`i' & dist == 0
					replace b0_48m = _b[vwretd] if group ==`i' & dist == 0
					replace se0_48m = _se[vwretd] if group ==`i' & dist == 0
					replace b1_48m = _b[L1.vwretd] if group ==`i' & dist == 0
					replace se1_48m = _se[L1.vwretd] if group ==`i' & dist == 0
					replace b2_48m = _b[L2.vwretd] if group ==`i' & dist == 0
					replace se2_48m = _se[L2.vwretd] if group ==`i' & dist == 0
					replace b3_48m = _b[L3.vwretd] if group ==`i' & dist == 0
					replace se3_48m = _se[L3.vwretd] if group ==`i' & dist == 0
					replace b4_48m = _b[L4.vwretd] if group ==`i' & dist == 0
					replace se4_48m = _se[L4.vwretd] if group ==`i' & dist == 0
					replace b5_48m = _b[L5.vwretd] if group ==`i' & dist == 0
					replace se5_48m = _se[L5.vwretd] if group ==`i' & dist == 0
                }

			}


			replace entropy_H_binomial =. if dist !=0
			save "$data/repurchase_AR_entropy.dta", replace										// file with abnormal returns for a given year for US targets



	use "$data/repurchase_AR_entropy.dta", clear

	*** delay measure ***
	sort dealnumber dist
	g D1 = 1-(contemp_r2/lag_r2)
	g D2 = ((abs(b1)/se1)+2*(abs(b2)/se2)+3*(abs(b3)/se3)+4*(abs(b4)/se4)+5*(abs(b5)/se5))/((abs(b0)/se0)+(abs(b1)/se1)+2*(abs(b2)/se2)+3*(abs(b3)/se3)+4*(abs(b4)/se4)+5*(abs(b5)/se5))

	g D1_12m = 1-(contemp_r2_12m/lag_r2_12m)
	g D2_12m = ((abs(b1_12m)/se1_12m)+2*(abs(b2_12m)/se2_12m)+3*(abs(b3_12m)/se3_12m)+4*(abs(b4_12m)/se4_12m)+5*(abs(b5_12m)/se5_12m))/((abs(b0_12m)/se0_12m)+(abs(b1_12m)/se1_12m)+2*(abs(b2_12m)/se2_12m)+3*(abs(b3_12m)/se3_12m)+4*(abs(b4_12m)/se4_12m)+5*(abs(b5_12m)/se5_12m))

	g D1_24m = 1-(contemp_r2_24m/lag_r2_24m)
	g D2_24m = ((abs(b1_24m)/se1_24m)+2*(abs(b2_24m)/se2_24m)+3*(abs(b3_24m)/se3_24m)+4*(abs(b4_24m)/se4_24m)+5*(abs(b5_24m)/se5_24m))/((abs(b0_24m)/se0_24m)+(abs(b1_24m)/se1_24m)+2*(abs(b2_24m)/se2_24m)+3*(abs(b3_24m)/se3_24m)+4*(abs(b4_24m)/se4_24m)+5*(abs(b5_24m)/se5_24m))

	g D1_36m = 1-(contemp_r2_36m/lag_r2_36m)
	g D2_36m = ((abs(b1_36m)/se1_36m)+2*(abs(b2_36m)/se2_36m)+3*(abs(b3_36m)/se3_36m)+4*(abs(b4_36m)/se4_36m)+5*(abs(b5_36m)/se5_36m))/((abs(b0_36m)/se0_36m)+(abs(b1_36m)/se1_36m)+2*(abs(b2_36m)/se2_36m)+3*(abs(b3_36m)/se3_36m)+4*(abs(b4_36m)/se4_36m)+5*(abs(b5_36m)/se5_36m))

	g D1_48m = 1-(contemp_r2_48m/lag_r2_48m)
	g D2_48m = ((abs(b1_48m)/se1_48m)+2*(abs(b2_48m)/se2_48m)+3*(abs(b3_48m)/se3_48m)+4*(abs(b4_48m)/se4_48m)+5*(abs(b5_48m)/se5_48m))/((abs(b0_48m)/se0_48m)+(abs(b1_48m)/se1_48m)+2*(abs(b2_48m)/se2_48m)+3*(abs(b3_48m)/se3_48m)+4*(abs(b4_48m)/se4_48m)+5*(abs(b5_48m)/se5_48m))

	**** Compute CARs

	*standard errors*
	gen se_m 	=.
	gen se_ff	=.
	drop group
	egen group = group(dealnumber cusip)
	keep if dist >= -271 & !missing(ar_m)
	drop group
	egen group = group(dealnumber cusip)

			qui su group

			qui forvalues i = 1(1)`r(max)'{
				qui su ar_m if group == `i' & inrange(dist,-271,-22)
				replace se_m = `r(sd)' if group == `i' & dist ==0
				qui su ar_ff if group == `i' & inrange(dist,-271,-22)
				replace se_ff = `r(sd)' if group == `i' & dist ==0
			}

	gen mkvalt = prc*shrout
	save "$data/repurchase_AR_entropy.dta", replace
	* +1/-1 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)
	keep if dist >= -1 & dist <= 1
	bysort dealnumber: egen car_11_m 	= total(ar_m)
	bysort dealnumber: egen car_11_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_11_m car_11_ff
	save "$temp\car_11.dta", replace

	* +3/-3 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)
	keep if dist >= -3 & dist <= 3
	bysort dealnumber: egen car_33_m 	= total(ar_m)
	bysort dealnumber: egen car_33_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_33_m car_33_ff
	save "$temp\car_33.dta", replace

	* +3/-1 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)

	keep if dist >= -1 & dist <= 3
	bysort dealnumber: egen car_13_m 	= total(ar_m)
	bysort dealnumber: egen car_13_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_13_m car_13_ff
	save "$temp\car_13.dta", replace

	* +5/-5 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)

	keep if dist >= -5 & dist <= 5
	bysort dealnumber: egen car_55_m 	= total(ar_m)
	bysort dealnumber: egen car_55_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_55_m car_55_ff
	save "$temp\car_55.dta", replace

	* +5/-1 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)

	keep if dist >= -1 & dist <= 5
	bysort dealnumber: egen car_15_m 	= total(ar_m)
	bysort dealnumber: egen car_15_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_15_m car_15_ff
	save "$temp\car_15.dta", replace

	* +10/-10 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)

	keep if dist >= -10 & dist <= 10
	bysort dealnumber: egen car_1010_m 	= total(ar_m)
	bysort dealnumber: egen car_1010_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_1010_m car_1010_ff
	save "$temp\car_1010.dta", replace

	* +10/-1 CARs
	use "$data/repurchase_AR_entropy.dta", clear
	gen deal_year = yofd(deal_date)

	keep if dist >= -1 & dist <= 10
	bysort dealnumber: egen car_110_m 	= total(ar_m)
	bysort dealnumber: egen car_110_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber totalpercentofsharesauth cusip deal_date deal_year car_110_m car_110_ff entropy_H entropy_H_binomial entropy_I p_SBB se_m se_ff mkvalt D1 D1_12m D1_24m D1_36m D1_48m D2 D2_12m D2_24m D2_36m D2_48m


	merge 1:1 dealnumber using "$temp\car_11.dta"
	keep if _merge == 3
	drop _merge

	merge 1:1 dealnumber using "$temp\car_13.dta"
	keep if _merge == 3
	drop _merge

	merge 1:1 dealnumber using "$temp\car_33.dta"
	keep if _merge == 3
	drop _merge

	merge 1:1 dealnumber using "$temp\car_15.dta"
	keep if _merge == 3
	drop _merge

	merge 1:1 dealnumber using "$temp\car_55.dta"
	keep if _merge == 3
	drop _merge

	merge 1:1 dealnumber using "$temp\car_11.dta"
	keep if _merge == 3
	drop _merge

	merge 1:1 dealnumber using "$temp\car_1010.dta"
	keep if _merge == 3
	drop _merge

	rename deal_year year
	sort cusip year

	sort year dealnumber

	*** merge with stock entropy
	use "$data/repurchase_AR_entropy_clean.dta", clear
    merge 1:1 dealnumber using "$data/stock_entropy.dta", nogen keep(3)

    save "$data/repurchase_AR_entropy_clean.dta", replace


********************************************************************************
******************************  Analysis  **************************************
********************************************************************************

	use "$data/repurchase_AR_entropy_clean.dta", clear
	winsor2 entropy_H entropy_H_binomial entropy_I D1 D2, replace cuts(0.5 99.5)

	hist D1, saving(D1, replace) bin(50) xtitle(Delay) graphregion(color(white)) bgcolor(white) color(ltblue%50)
	graph export "C:\Users\sajakob\Dropbox\Buybacks\Text\D1_dist.png", replace
	hist D2, saving(D2, replace) bin(50) xtitle(Coefficent-based Delay) graphregion(color(white)) bgcolor(white) color(ltblue%50)
	graph export "C:\Users\sajakob\Dropbox\Buybacks\Text\D2_dist.png", replace


	*keep if mkvalt >2000000
	egen entropy_cluster = xtile(entropy_H), nq(5) by(year)
	egen binomial_entropy_cluster = xtile(entropy_H_binomial), nq(5) by(year)
	egen information_cluster = xtile(entropy_I), nq(5) by(year)
	egen D1_cluster = xtile(D1), nq(5) by(year)
	egen D2_cluster = xtile(D2), nq(5) by(year)
	egen size_cluster = xtile(mkvalt), nq(5) by(year)
	egen probability_cluster = xtile(p_SBB), nq(5) by(year)
    egen ML_entropy_cluster = xtile(ML_entropy), nq(5) by(year)
    egen KT_entropy_cluster = xtile(KT_Entropy), nq(5) by(year)
	la var mkvalt "Size"
	la var p_SBB "Probability"
	la var entropy_H "Entropy"
	la var entropy_H_binomial "Binomial"
	la var entropy_I "Information"
    la var ML_entropy "ML Stock Entropy"
    la var KT_Entropy "KT Stock Entropy"

	est clear
	eststo clear
	estpost correlate mkvalt p_SBB entropy_H entropy_H_binomial entropy_I D1 D2 ML_entropy KT_Entropy, matrix listwise
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\correl.tex", replace unstack not noobs compress nonumber label title(Correlation table)

	g var_m = se_m^2
	g var_ff = se_ff^2

	bysort binomial_entropy_cluster: egen temp = total(var_m)
	bysort binomial_entropy_cluster: egen temp2 = count(var_m)

	g se_aar_m = sqrt(temp/temp2^2)
	drop temp temp2

	replace car_11_m = car_11_m*100
	replace car_13_m = car_13_m*100
	replace car_15_m = car_15_m*100
	replace car_110_m = car_110_m*100

	la var car_11_m "CAR[-1/+1]"
	la var car_13_m "CAR[-1/+3]"
	la var car_15_m "CAR[-1/+5]"
	la var car_110_m "CAR[-1/+10]"

	est clear
	eststo clear
	sort binomial_entropy_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m, by(binomial_entropy_cluster) statistics(mean) columns(variables) nototal
	sort information_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m, by(information_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\AR_entropy.tex", replace ///
	nomtitle nonumber cells("car_11_m(fmt(%9.2f) label([-1/+1])) car_13_m(fmt(%9.2f) label([-1/+3])) car_15_m(fmt(%9.2f) label([-1/+5])) car_110_m(fmt(%9.2f) label([-1/+10]))") ///
	noobs label nonumber title(Cumulative Average Abnormal Return 1996 - 2018) ///
	substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{ccccc@{\hskip 0.5in}cccc}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{Binomial Entropy}&\multicolumn{4}{c}{Information Content}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
	"\caption{Cumulative Average Abnormal Return 1996 - 2018}" "\caption{Cumulative Average Abnormal Return 1996 - 2018} \label{table:CAR}")

	est clear
	eststo clear
	sort binomial_entropy_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m if year>=2009, by(binomial_entropy_cluster) statistics(mean) columns(variables) nototal
	sort information_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m if year>=2009, by(information_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\AR_entropy2009.tex", replace ///
	nomtitle nonumber cells("car_11_m(fmt(%9.2f) label([-1/+1])) car_13_m(fmt(%9.2f) label([-1/+3])) car_15_m(fmt(%9.2f) label([-1/+5])) car_110_m(fmt(%9.2f) label([-1/+10]))") ///
	noobs label nonumber title(Cumulative Average Abnormal Return 2009 - 2018) ///
	substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{ccccc@{\hskip 0.5in}cccc}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{Binomial Entropy}&\multicolumn{4}{c}{Information Content}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
	"\caption{Cumulative Average Abnormal Return 2009 - 2018}" "\caption{Cumulative Average Abnormal Return 2009 - 2018} \label{table:CAR2009}")

	est clear
	eststo clear
	sort binomial_entropy_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m, by(probability_entropy_cluster) statistics(mean) columns(variables) nototal
	sort information_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m, by(information_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\AR_entropy.tex", replace ///
	nomtitle nonumber cells("car_11_m(fmt(%9.2f) label([-1/+1])) car_13_m(fmt(%9.2f) label([-1/+3])) car_15_m(fmt(%9.2f) label([-1/+5])) car_110_m(fmt(%9.2f) label([-1/+10]))") ///
	noobs label nonumber title(Cumulative Average Abnormal Return 1996 - 2018) ///
	substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{ccccc@{\hskip 0.5in}cccc}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{Binomial Entropy}&\multicolumn{4}{c}{Information Content}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
	"\caption{Cumulative Average Abnormal Return 1996 - 2018}" "\caption{Cumulative Average Abnormal Return 1996 - 2018} \label{table:CAR}")

	est clear
	eststo clear
	sort size_cluster
	eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m D1 D2 ML_entropy KT_Entropy entropy_H entropy_I, by(size_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\size_CAR.tex", replace ///
	nomtitle nonumber cells("car_11_m(fmt(%9.2f) label([-1/+1])) car_13_m(fmt(%9.2f) label([-1/+3])) car_15_m(fmt(%9.2f) label([-1/+5])) car_110_m(fmt(%9.2f) label([-1/+10])) D1(fmt(%9.2f) label(D1)) D2(fmt(%9.2f) label(D2)) ML_entropy(fmt(%9.2f) label(ML)) KT_Entropy(fmt(%9.2f) label(KT)) entropy_H(fmt(%9.2f) label(\emph{H})) entropy_I(fmt(%9.2f) label(\emph{I}))") ///
	noobs label nonumber title(Cumulative Average Abnormal Return by Firm Size 1996 - 2018) ///
	substitute("\begin{tabular}{l*{1}{cccccccccc}}" "\begin{tabular}{c*{1}{cccc@{\hskip 0.5in}cc@{\hskip 0.25in}cc@{\hskip 0.5in}cc}}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{\hspace{-1cm}Cumulative Abnormal Returns}&\multicolumn{4}{c}{\hspace{-1cm}Price Efficiency}&\multicolumn{2}{c}{Entropy}\\ \cmidrule{2-5} \cmidrule{6-9} \cmidrule{10-11}" ///
	"\caption{Cumulative Average Abnormal Return by Firm Size 1996 - 2018}" "\caption{Cumulative Average Abnormal Return by Firm Size 1996 - 2018} \label{table:CARSize}")

    est clear
    eststo clear
    sort D2_cluster
    eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m, by(D2_cluster) statistics(mean) columns(variables) nototal
    sort KT_entropy_cluster
    eststo: estpost tabstat car_11_m car_13_m car_15_m car_110_m, by(KT_entropy_cluster) statistics(mean) columns(variables) nototal
    esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Stock_entropy_AR.tex", replace ///
    nomtitle nonumber cells("car_11_m(fmt(%9.2f) label([-1/+1])) car_13_m(fmt(%9.2f) label([-1/+3])) car_15_m(fmt(%9.2f) label([-1/+5])) car_110_m(fmt(%9.2f) label([-1/+10]))") ///
    noobs label nonumber title(Price Efficiency and Cumulative Average Abnormal Return 1996 - 2018) ///
    substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{ccccc@{\hskip 0.5in}cccc}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{Maximum Likelihood Entropy}&\multicolumn{4}{c}{Kontoyiannis Entropy}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
    "\caption{Price Efficiency and Cumulative Average Abnormal Return 1996 - 2018}" "\caption{Price Efficiency and Cumulative Average Abnormal Return 1996 - 2018} \label{table:EfficiencyCAR}")


	*** Double sorts ***
