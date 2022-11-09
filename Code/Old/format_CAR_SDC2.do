version 14.2
set rmsg on
set more off

************************************************************************
*
*	JOB:		Format SDC and compute CARs.do
*	PROJECT:	SBB
*	INPUT:		SDC Repurchase data and CRSP
*	OUTPUT:		File with abnormal returns around share repurchases
*
*	DESCRIPTION: This job computes the AR of US share repurchasers. 
*
*************************************************************************


* Define paths

global data		"C:\Users\valta\Dropbox\work\SBB\Data"
global temp		"C:\Users\valta\Desktop\temp"														

clear
clear matrix

set matsize 800
set scrollbufsize 500000		
capture program drop _all

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
  
**** Format the sdc data (country names)

	use "$data/SDC_repurchase_1985_2015.dta", clear												// 22,389 lines in the raw file
	
	drop if cusip8 == ""
	drop var7
	
	tab corporateaction
	keep if corporateaction == ""																// Drop corporate actions other than share repurchases
	
	keep daterepurchaseauthorizedbyboard totalsharesauthorized name dealnumber numberofsecuritiesoutstanding ///
		  cusip cusip8 pctsharesauthorizedforrepurchase totalpercentofsharesauth 
	
	destring pctsharesauthorizedforrepurchase, replace force
	
	replace pctsharesauthorizedforrepurchase = 100 if pctsharesauthorizedforrepurchase > 100 & pctsharesauthorizedforrepurchase != .
	replace totalpercentofsharesauth = 100 if totalpercentofsharesauth > 100 & totalpercentofsharesauth != .	
	
	gen date = date(daterepurchaseauthorizedbyboard, "DMY")
	format date %d
	
	sort cusip8 date
	
	duplicates drop cusip8 date, force
	
	sort cusip8 date
	by cusip8: g last_rep = date[_n-1]
	by cusip8: g next_rep = date[_n+1]
	format last_rep %d
	format next_rep %d
	
	g rep_differencel = date - last_rep									
	g rep_differencef = next_rep - date									
	
	drop if rep_differencel < 30 & rep_differencel != .
	drop if rep_differencef < 30 & rep_differencef != .	
	
	drop last_rep next_rep rep_differencel rep_differencef
	
	g ayear = year(date)
	g amonth = month(date)
	g aday   = day(date)	
	
	drop if ayear < 1990
	
	rename cusip cusip6
	rename cusip8 cusip
	
	sort cusip date
	count
	
	save "$data/sdc_repurchase_clean.dta", replace															// _N = 16,674
	
	
	
  **** Format and clean CRSP data
  
	use "$data/crsp_daily.dta", clear												
  
	* Merge the file with the data containing other factors
	sort date
	merge m:1 date using "$data\FF_4factors.dta"
	keep if _merge == 3
	drop _merge
	
	sort cusip date
	count																									// _N = 56,378,087
	
	save "$data/crsp_daily.dta", replace														

	
																
	
		use "$data/sdc_repurchase_clean.dta", clear															
			
		keep cusip date dealnumber pctsharesauthorizedforrepurchase
		cross using "$data/time.dta" 															// time is a file with one variable (time) from -380 to +20, each previous observation is repeated 401 times

		rename date deal_date
		gen date = deal_date + time
		format date %d
		joinby cusip date using "$data/crsp_daily.dta"											// get stock return information


	*	if `nobs' > 0 {

			drop if ret_us == .
	
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
			keep if dist <= 10
			drop tmp tmp2

			bysort group: egen rank = rank(-time) if time <= 0									// ranks rival prices/returns for each deal from lowest (day of the deal, rank 1) to highest (380 obs away from the deal, rank 381)									
			keep if rank <= 251 | missing(rank)
			
			bysort group: gen diff_time = time[_n+1] - time[_n]									// drops observations before the last return break to the deal date
			gen tmp = 1 if (diff_time > 10 & !missing(diff_time))
			gsort group -time
			by group: carryforward tmp, replace
			drop if tmp == 1
			drop tmp
			
*			bysort group: gen cusip8_deal_date = cusip if dist == 0							// this drops observations with changing cusip8 before and after the deal
*			gsort group -cusip8_deal_date -dist
*			by group: carryforward cusip8_deal_date, replace
*			drop if cusip8_deal_date != cusip8
*			drop cusip8_deal_date
*			sort group dist
	
			save "$temp/sdc_crsp.dta", replace											// file with stock information and between -250 and 10 obs around the deal

			use "$temp/sdc_crsp.dta", clear
			gen ar_m      = .
			gen ar_ff	  = .
			
			g retrf = ret_us - rf
			
			drop group
			egen group = group(dealnumber cusip)	
			qui su group

*			qui forvalues i = 1(1)100{	
			qui forvalues i = 1(1)`r(max)'{														// loop over all groups
				qui su ret_us if group == `i' & dist < -21										// determine the number of available returns	

				local nobs = `r(N)' 
				if `nobs' >= 100 {																// number of obs is larger or equal to 100

					reg ret_us vwretd if group == `i' & dist < -21 								// regress return on market return for each ID if return occurs at least 21 obs before the deal, estimation period (-250 to -21)
					predict e, resid															// calculates least squares residuals
					replace ar_m = e if group ==`i'
					drop e

					reg ret_us vwretd smb hml umd if group ==`i' & dist < -21					// regress target return on value-weighted market return for each ID if return occurs at least 21 obs before the deal, estimation period (-250 to -21)
					predict e, resid															// calculates least squares residuals
					replace ar_ff = e if group ==`i'
					drop e
				}
			}
				
			keep if dist >= -20 & !missing(ar_m)												// keep only observations within a distance of 20 obs with respect to the deal date and with abnormal returns
			
			save "$data/repurchase_AR.dta", replace												// file with abnormal returns for a given year for US targets


			
	**** Compute CARs
	
	* +1/-1 CARs
	use "$data/repurchase_AR.dta", clear		
	gen deal_year = yofd(deal_date)	
	
	keep if dist >= -1 & dist <= 1 
	bysort dealnumber: egen car_11_m 	= total(ar_m)
	bysort dealnumber: egen car_11_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber pctsharesauthorizedforrepurchase cusip deal_date deal_year car_11_m car_11_ff
*	winsor car_11_m 	  1
*	winsor car_11_ff 	  1
	save "$temp\car_11.dta", replace						
	
	* +3/-3 CARs	
	use "$data/repurchase_AR.dta", clear		
	gen deal_year = yofd(deal_date)	
	
	keep if dist >= -3 & dist <= 3 
	bysort dealnumber: egen car_33_m 	= total(ar_m)
	bysort dealnumber: egen car_33_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber pctsharesauthorizedforrepurchase cusip deal_date deal_year car_33_m car_33_ff
*	winsor car_33_m 	  1
*	winsor car_33_ff 	  1
	save "$temp\car_33.dta", replace		
	
	
	* +5/-5 CARs	
	use "$data/repurchase_AR.dta", clear		
	gen deal_year = yofd(deal_date)	
	
	keep if dist >= -5 & dist <= 5 
	bysort dealnumber: egen car_55_m 	= total(ar_m)
	bysort dealnumber: egen car_55_ff 	= total(ar_ff)
	keep if dist == 0
	keep dealnumber pctsharesauthorizedforrepurchase cusip deal_date deal_year car_55_m car_55_ff
*	winsor car_55_m 	  1
*	winsor car_55_ff 	  1
	save "$temp\car_55.dta", replace	

	merge 1:1 dealnumber using "$temp\car_33.dta"
	keep if _merge == 3
	drop _merge
	
	merge 1:1 dealnumber using "$temp\car_11.dta"
	keep if _merge == 3
	drop _merge
	
	rename deal_year year
	sort cusip year
	
	* Note that there are firms that have multiple repurchase announcements in a given year
	* But there are no duplicates in terms of dealnumber and deal_date
	duplicates list cusip year
	duplicates list cusip deal_date	
	
	replace car_11_m = car_11_m*100
	replace car_33_m = car_33_m*100
	replace car_55_m = car_55_m*100
	replace car_11_ff = car_11_ff*100
	replace car_33_ff = car_33_ff*100
	replace car_55_ff = car_55_ff*100	
	
	save "$data\repurchase_cars.dta", replace		
	
	* Prior 6-month raw returns
	
	clear
	use "V:\CRSP\Securities\Monthly\1926-2016.dta"
	keep date cusip ret
	duplicates drop cusip date, force
	egen firm_id = group(cusip)
	gen month = mofd(date)
	format %tm month
	tsset firm_id month, monthly
	
	
	
	gen lag_1 = l.ret
	gen lag_2 = l2.ret
	gen lag_3 = l3.ret
	gen lag_4 = l4.ret
	gen lag_5 = l5.ret
	gen lag_6 = l6.ret
	
	save "$temp\returns.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\repurchase_cars.dta", clear
	gen month = mofd(deal_date)
	format %tm month
	
	merge 1:1 cusip month using "$temp\returns.dta"
	keep if _merge == 3
	drop _merge date
	egen six_m_sum_ret= rowtotal(lag_1 lag_2 lag_3 lag_4 lag_5 lag_6)															// sum of past six monthly returns prior to announcement date
	gen six_m_cum_ret = (1+lag_1)*(1+lag_2)*(1+lag_3)*(1+lag_4)*(1+lag_5)*(1+lag_6)-1											// six month cumulative return prior to announcement date
	
	tabstat six_m_sum_ret, by(year)																								
	tabstat six_m_cum_ret, by(year)																								
	
	collapse (mean) car_11_m car_11_ff car_33_m car_33_ff car_55_m car_55_ff pctsharesauthorizedforrepurchase six_m_sum_ret six_m_cum_ret , by(year)	
	
	*save "raw_returns.dta"
	
	use "V:\Fama French & Liqudity Factors\Factory Mothly\01Jul1926 - 31Jul2017", clear
	gen year = yofd(dateff)
	drop if year <= 1989
	drop year
	gen month = mofd(dateff)
	format %tm month
	
	save "$temp\ff4f.dta", replace
	
	use "V:\CRSP\Securities\Monthly\1926-2016.dta", clear
	keep date cusip ret
	gen year = yofd(date)
	drop if year <= 1989
	drop year
	gen month = mofd(date)
	format %tm month
	
	merge m:1 month using "$temp\ff4f.dta"
	keep if _merge==3
	drop _merge
	gen retrf = ret - rf
	duplicates drop cusip month, force
	save "$temp\monthly_data.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\repurchase_ar.dta", clear
	keep cusip deal_date 
	gen month = mofd(deal_date)
	format %tm month
	duplicates drop cusip deal_date month, force
	
	merge 1:1 cusip month using "$temp\monthly_data.dta"
	
	
	sort cusip month
	egen firm_id = group(cusip) 

	sort firm_id
	by firm_id: carryforward deal_date, gen(announcement_date)
	drop if announcement_date ==.
	gen deal_month = mofd(announcement_date)
	format %tm deal_month
	drop announcement_date
	drop _merge
	gen dist = month - deal_month
	
	*save "long_run_ar.dta"
	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1
	
	gen year = yofd(date)
	drop if year < 1991
	*drop if year > 2001
	drop year
	*drop if n_obs <48
	
	gen alpha_cs = .
	
	
			
	forvalues i = 1(1)48{														
												

					reg retrf mktrf smb hml if dist == `i'  																							
					replace alpha_cs = _b[_cons] if dist ==`i'
					
				}
						
	sort group dist
	by group1: gen alpha_cs_sum = sum(alpha_cs)

	
	
	
	
	use "$data\Compustat_sample.dta", replace
	
	sort cusip year
	merge 1:n cusip year using "$data\repurchase_cars.dta"
	keep if _merge == 3
	drop _merge
	
	tabstat car_11_m, by(year) stats(n sd med mean)
	tabstat car_11_ff, by(year) stats(n sd med mean)
	tabstat car_33_m, by(year) stats(n sd med mean)
	tabstat car_33_ff, by(year) stats(n sd med mean)
	

	
	reg car_11_m, vce(cl sic)	
	reg car_11_m, vce(cl year)	
	reg car_11_ff, vce(cl sic)	
	reg car_11_ff, vce(cl year)	
	
	
	reg car_11_m l_a mb cash_at div_d sr_index, cluster(gvkey)
	
	reg car_11_m l_a mb cash_at div_d cgov_str_numl1, cluster(gvkey) 
	
	g sr_pos = 1 if sr_index >=0
	replace sr_pos = 0 if sr_index <0
	replace sr_pos = . if sr_index == .
	
	g sr_neg = 1 if sr_index <0
	replace sr_neg = 0 if sr_index >=0
	replace sr_neg = . if sr_index == .		
	
	reg car_11_m l_a mb cash_at div_d sr_pos, cluster(gvkey)
	
	reg car_11_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic)
	reg car_33_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic)
	reg car_55_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic)
	
	areg car_11_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic) a(sic)
	areg car_33_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic) a(sic)
	areg car_55_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic) a(sic)
	
	areg car_11_ff l_al1 mbl1 cash_atl1 div_d i.cgov_str_numl1 i.year, cluster(sic) a(sic)
	areg car_33_ff l_al1 mbl1 cash_atl1 div_d i.cgov_str_numl1 i.year, cluster(sic) a(sic)
	areg car_55_ff l_al1 mbl1 cash_atl1 div_d i.cgov_str_numl1 i.year, cluster(sic) a(sic)	
	
	
	