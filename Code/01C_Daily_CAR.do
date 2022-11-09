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


global temp		"C:\Users\\`=c(username)'\Desktop\temp"
global data		"A:\Sascha\Buyback Anomalies"														

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
  
													// _N = 16,674
	
  **** Format and clean CRSP data
  
	use "$data/CRSP_daily_1985_2019.dta", clear												
  
	* Merge the file with the data containing other factors
	sort date
	merge m:1 date using "$data\ff6f.dta"
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
	
	use "V:\CRSP\Securities\Monthly\1926-2016.dta", clear
	keep if shrcd == 10 | shrcd == 11
	keep date permno cusip ret
	duplicates drop permno date, force
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

	preserve
	collapse (mean) car_11_m car_11_ff car_33_m car_33_ff car_55_m car_55_ff pctsharesauthorizedforrepurchase six_m_sum_ret six_m_cum_ret , by(year)	
	save "raw_returns.dta", replace
	restore 
	
	preserve
	keep dealnumber cusip deal_date six_m_sum_ret six_m_cum_ret 
	save "six_m_ret.dta", replace
	restore
	
	
	
	use "V:\Fama French & Liqudity Factors\Factory Mothly\01Jul1926 - 31Jul2017", clear
	gen year = yofd(dateff)
	drop if year <= 1989
	drop year
	gen month = mofd(dateff)
	format %tm month
	save "$temp\ff4f.dta", replace
	
	import delimited "J:\Sascha\Buyback Anomalies\FF5F.csv", clear
	gen date2 = date(date,"DMY")
	format date2 %td
	gen month = mofd(date2)
	drop date2
	format month %tm
	drop date rf hml smb mktrf
	replace rmw = rmw/100
	replace cma = cma/100
	merge 1:1 month using "$temp\ff4f.dta"
	keep if _merge ==3
	drop _merge
	save "$temp\ff6f.dta", replace
	
	
	use "V:\CRSP\Securities\Monthly\1926-2016.dta", clear
	keep date cusip ret
	gen year = yofd(date)
	drop if year <= 1989
	drop year
	gen month = mofd(date)
	format %tm month
	
	merge m:1 month using "$temp\ff6f.dta"
	keep if _merge==3
	drop _merge
	gen retrf = ret - rf
	duplicates drop cusip month, force
	save "$temp\monthly_data.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\repurchase_ar.dta", clear
	keep cusip deal_date dealnumber siccode
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
	by firm_id: carryforward dealnumber, replace
	by firm_id: carryforward siccode, replace
	
	merge m:1 dealnumber cusip deal_date using "J:\Sascha\Buyback Anomalies\six_m_ret.dta"
	drop _merge
	
	save "long_run_ar.dta", replace
	
	
	*** long run CARs
	
	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	
	sort firm_id dealnumber dist
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1
	
	
	gen year = yofd(deal_date)
	sort group1 dist
	by group1: carryforward year, replace
	drop if year < 1991																	//Define time period, lower bound
	drop if year > 2007																	//Define time period, upper bound
	drop year
	*drop if n_obs <48

	
	
	**** cross-sectional regression for all firms *
	
		
	gen alpha_cs = .
	gen beta_cs_mktrf = .
	gen beta_cs_smb = .
	gen beta_cs_hml = .
	gen beta_cs_umd = .
	
	forvalues i = 1(1)48{														
												

					reg retrf mktrf smb hml umd if dist == `i'  																							
					replace alpha_cs = _b[_cons] if dist ==`i'
					replace beta_cs_mktrf = _b[mktrf] if dist ==`i'
					replace beta_cs_smb = _b[smb] if dist ==`i'
					replace beta_cs_hml = _b[hml] if dist ==`i'
					replace beta_cs_umd = _b[umd] if dist ==`i'
					
					}
						
	sort group1 dist
	
	by group1: gen alpha_cs_sum = sum(alpha_cs)

	egen alpha_cs_12m = total(alpha_cs) if dist <=12 & n_obs >= 12, by(group1)
	egen alpha_cs_24m = total(alpha_cs) if dist <=24 & n_obs >= 24, by(group1)
	egen alpha_cs_36m = total(alpha_cs) if dist <=36 & n_obs >= 36, by(group1)	
	egen alpha_cs_48m = total(alpha_cs) if dist <=48 & n_obs >= 48, by(group1)	
	
	*keep if dist ==0
	
	
	*** Coss-sectional Quintile regressions ***
	
	by group1: carryforward six_m_sum_ret, replace
	by group1: carryforward six_m_cum_ret, replace
	
	gen winner_sum = 0
	replace winner_sum = 1 if six_m_sum_ret > 0										// Dummy variable for winner stocks subject to the sum of prior six month returns, positive sign denotes a winner
	
	gen winner_cum = 0
	replace winner_cum = 1 if six_m_cum_ret > 0										// Dummy variable for winner stocks subject to the prior six month cumulative return, positive sign denotes a winner
	
	xtile quint_sum = six_m_sum_ret if dist==0, nq(5)								// Quintiles subject to the sum of prior six months returns, top quintile denoted with 5
	xtile quint_cum = six_m_cum_ret if dist==0, nq(5)								// Quintiles subject to the prior six months cummualtive returns, top quintile denoted with 5
	
	by group1: carryforward quint_sum, replace
	by group1: carryforward quint_cum, replace
	
	sort group1 dist
	tsset group1 dist
	
	forvalues i = 1(1)5{																// Number of Quintiles
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd if dist <=12 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_q`i'_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_q`i'_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_q`i'_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_q`i'_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_q`i'_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_q`i'_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_q`i'_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_q`i'_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_q`i'_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_q`i'_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_q`i'_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_q`i'_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd if dist <=24 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		*gen se_alpha_q1_24m = 
		
		gen beta_mktrf_q`i'_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_q`i'_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_q`i'_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_q`i'_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_q`i'_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_q`i'_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_q`i'_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_q`i'_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_q`i'_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_q`i'_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_q`i'_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_q`i'_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd if dist <=36 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		*gen se_alpha_q1_36m = 
		
		gen beta_mktrf_q`i'_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_q`i'_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_q`i'_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_q`i'_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_q`i'_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_q`i'_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_q`i'_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_q`i'_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_q`i'_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_q`i'_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_q`i'_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_q`i'_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		*gen se_alpha_q1_48m = 
		
		gen beta_mktrf_q`i'_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_q`i'_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_q`i'_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_q`i'_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_q`i'_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_q`i'_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_q`i'_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_q`i'_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_q`i'_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_q`i'_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_q`i'_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_q`i'_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48
		
		
		}
	
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
	gen se_alpha =.
	gen se_alpha_sq = .
	
	forvalues i = 1(1)5{
	
		forvalues j = 1(1)48{														
												

				reg retrf mktrf smb hml umd if dist == `j' & quint_cum == `i' 																							
				replace se_alpha = _se[_cons] if dist ==`j'
				
						}
				 
				replace se_alpha_sq = se_alpha^2
				egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
				egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
				egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
				egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
				gen t_alpha_q`i'_12m = alpha_q`i'_12m/sqrt(sum_se_alpha_sq_12m)
				gen t_alpha_q`i'_24m = alpha_q`i'_24m/sqrt(sum_se_alpha_sq_24m)
				gen t_alpha_q`i'_36m = alpha_q`i'_36m/sqrt(sum_se_alpha_sq_36m)
				gen t_alpha_q`i'_48m = alpha_q`i'_48m/sqrt(sum_se_alpha_sq_48m)
				
				drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m	
			}
		
		
	*** 
	
	*keep if dist == 0
	
	
	
	
	
	*** KLD performance indicators ***
	
	*use "J:\Sascha\Buyback Anomalies\KLD.dta", clear
	*drop if cusip == ""
	*duplicates drop year cusip, force
	*save "$temp\KLD_raw.dta", replace
	
	*use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	
	*sort firm_id dealnumber dist
	
	*egen group1 = group(firm_id deal_month)
	*egen n_obs = count(dist), by(group1)
	*replace n_obs = n_obs-1
	
	
	*gen year = yofd(deal_date)
	*sort group1 dist
	*by group1: carryforward year, replace
	*drop if year < 1991																	//Define time period, lower bound
	*drop if year > 2007																	//Define time period, upper bound
	*drop year
	*drop if n_obs <48
	
	*gen year = yofd(deal_date)
	*merge m:1 cusip year using "$temp\KLD_raw.dta"
	*sort cusip month
	*drop if dealnumber ==.
	*gen indicator =.
	*replace indicator = 1 if dist==0 & _merge ==3
	*sort group1 indicator
	*by group1: carryforward indicator, replace
	*drop if indicator ==.
	*drop _merge companyid ticker companyname indicator
	
	* Governance score
	
	*egen pos_gov_score = rowtotal(cgov_str_a cgov_str_c cgov_str_d cgov_str_e ///
									*cgov_str_f cgov_str_x cgov_str_g cgov_str_h) ///
									*if dist==0
									
	*egen neg_gov_score = rowtotal(cgov_con_b cgov_con_f cgov_con_g cgov_con_h ///
									*cgov_con_i cgov_con_j cgov_con_m cgov_con_x ///
									*cgov_con_k cgov_con_l) if dist==0
									
	*gen net_gov_score =  pos_gov_score - neg_gov_score if dist==0
	*sort group1 net_gov_score
	*by group1: carryforward net_gov_score, replace
	
	*sort group1 dist
	*tsset group1 dist
	
	***** single sector scores
	
	use "J:\Sascha\Buyback Anomalies\KLD_clean.dta", clear
	duplicates drop year cusip, force
	egen firm_id = group(cusip)
	
		gen net_gov_score = cgov_str_num - cgov_con_num
		gen net_env_score = env_str_num - env_con_num
		gen net_hum_score = hum_str_num - hum_con_num
		gen net_emp_score = emp_str_num - emp_con_num
	
	sort firm_id year
	tsset firm_id year
	
		gen delta_net_gov_score = d.net_env_score
		gen lead_delta_net_gov_score = f.delta_net_gov_score
		gen two_lead_delta_net_gov_score = f2.delta_net_gov_score
		gen three_lead_delta_net_gov_score = f3.delta_net_gov_score
		gen four_lead_delta_net_gov_score = f4.delta_net_gov_score
		gen five_lead_delta_net_gov_score = f5.delta_net_gov_score
		
			gen y0_y4_delta_net_gov_score = lead_delta_net_gov_score + two_lead_delta_net_gov_score ///
			+ three_lead_delta_net_gov_score + four_lead_delta_net_gov_score + five_lead_delta_net_gov_score
			gen y0_y2_delta_net_gov_score = lead_delta_net_gov_score + two_lead_delta_net_gov_score + three_lead_delta_net_gov_score
			gen y3_y4_delta_net_gov_score = four_lead_delta_net_gov_score + five_lead_delta_net_gov_score
			
			drop lead_delta_net_gov_score two_lead_delta_net_gov_score ///
			three_lead_delta_net_gov_score four_lead_delta_net_gov_score ///
			five_lead_delta_net_gov_score
			
			
		gen delta_net_env_score = d.net_env_score
		gen lead_delta_net_env_score = f.delta_net_env_score
		gen two_lead_delta_net_env_score = f2.delta_net_env_score
		gen three_lead_delta_net_env_score = f3.delta_net_env_score
		gen four_lead_delta_net_env_score = f4.delta_net_env_score
		gen five_lead_delta_net_env_score = f5.delta_net_env_score
		
			gen y0_y4_delta_net_env_score = lead_delta_net_env_score + two_lead_delta_net_env_score ///
			+ three_lead_delta_net_env_score + four_lead_delta_net_env_score + five_lead_delta_net_env_score
			gen y0_y2_delta_net_env_score = lead_delta_net_env_score + two_lead_delta_net_env_score + three_lead_delta_net_env_score
			gen y3_y4_delta_net_env_score = four_lead_delta_net_env_score + five_lead_delta_net_env_score
			
			drop lead_delta_net_env_score two_lead_delta_net_env_score ///
			three_lead_delta_net_env_score four_lead_delta_net_env_score ///
			five_lead_delta_net_env_score
			
			
		gen delta_net_emp_score = d.net_env_score
		gen lead_delta_net_emp_score = f.delta_net_emp_score
		gen two_lead_delta_net_emp_score = f2.delta_net_emp_score
		gen three_lead_delta_net_emp_score = f3.delta_net_emp_score
		gen four_lead_delta_net_emp_score = f4.delta_net_emp_score
		gen five_lead_delta_net_emp_score = f5.delta_net_emp_score
		
			gen y0_y4_delta_net_emp_score = lead_delta_net_emp_score + two_lead_delta_net_emp_score ///
			+ three_lead_delta_net_emp_score + four_lead_delta_net_emp_score + five_lead_delta_net_emp_score
			gen y0_y2_delta_net_emp_score = lead_delta_net_emp_score + two_lead_delta_net_emp_score + three_lead_delta_net_emp_score
			gen y3_y4_delta_net_emp_score = four_lead_delta_net_emp_score + five_lead_delta_net_emp_score
			
			drop lead_delta_net_emp_score two_lead_delta_net_emp_score ///
			three_lead_delta_net_emp_score four_lead_delta_net_emp_score ///
			five_lead_delta_net_emp_score
			
			
		gen delta_net_hum_score = d.net_env_score
		gen lead_delta_net_hum_score = f.delta_net_hum_score
		gen two_lead_delta_net_hum_score = f2.delta_net_hum_score
		gen three_lead_delta_net_hum_score = f3.delta_net_hum_score
		gen four_lead_delta_net_hum_score = f4.delta_net_hum_score
		gen five_lead_delta_net_hum_score = f5.delta_net_hum_score
		
			gen y0_y4_delta_net_hum_score = lead_delta_net_hum_score + two_lead_delta_net_hum_score ///
			+ three_lead_delta_net_hum_score + four_lead_delta_net_hum_score + five_lead_delta_net_hum_score
			gen y0_y2_delta_net_hum_score = lead_delta_net_hum_score + two_lead_delta_net_hum_score + three_lead_delta_net_hum_score
			gen y3_y4_delta_net_hum_score = four_lead_delta_net_hum_score + five_lead_delta_net_hum_score
			
			drop lead_delta_net_hum_score two_lead_delta_net_hum_score ///
			three_lead_delta_net_hum_score four_lead_delta_net_hum_score ///
			five_lead_delta_net_hum_score
			
	
	replace year = year + 1
	sort firm_id year
	
	save "$temp\KLD_clean.dta", replace
	
	use "V:\COMPUSTAT\Securities\Annual Updates\Fundamentals Annually 1950-2015.dta", clear
	keep cusip che at mkvalt fyear
	gen che_at = che/at
	rename fyear year
	replace cusip = substr(cusip,1,8)
	duplicates drop cusip year, force
	save "$temp\COMPUSTAT_clean.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	sort firm_id dealnumber dist
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1
	
	
	gen year = yofd(deal_date)
	sort group1 dist
	by group1: carryforward year, replace
	drop if year < 1991																	//Define time period, lower bound
	drop if year > 2007																	//Define time period, upper bound
	drop year
	*drop if n_obs <48
	
	gen year = yofd(deal_date)
	merge m:1 cusip year using "$temp\KLD_clean.dta"
	drop _merge
	merge m:1 cusip year using "$temp\COMPUSTAT_clean.dta"
	sort cusip year
	drop if dealnumber ==.
	drop _merge
	
	
	
	xtile size_cluster = mkvalt if dist==0, nq(2)										// 1 indicates below median (small firms), 2 indicates above median (large firms)
	xtile cash_cluster = che_at if dist==0, nq(2)										// 1 indicates below median (low cash ratio), 2 indicates above median (high cash ratio)
	
	
	sort group1 dist
	by group1: carryforward size_cluster, replace
	by group1: carryforward cash_cluster, replace

	
	gen sic2 = string(siccode)
	replace sic2 = substr(sic2,1,2)
	destring sic2, replace
	

	* net gov score
	*gen net_gov_score = cgov_str_num - cgov_con_num
	sort group1 dist
	by group1: carryforward net_gov_score, replace
	
	
	*net environmental score 
	*gen net_env_score = env_str_num - env_con_num
	sort group1 dist
	by group1: carryforward net_env_score, replace
	
	*net social score	
	*gen net_hum_score = hum_str_num - hum_con_num
	sort group1 dist
	by group1: carryforward net_hum_score, replace
	
	*net employee score	
	*gen net_emp_score = emp_str_num - emp_con_num
	sort group1 dist
	by group1: carryforward net_emp_score, replace
	
	*total core
	gen tot_net_score = net_gov_score + net_env_score + net_hum_score + net_emp_score
	sort group1 dist
	by group1: carryforward tot_net_score, replace
	
	* Dropping financial institutions
	
	drop if 60 <= sic2 <=67 &  91 <= sic2 <= 99 & sic2 == 49
	
	sort group1 dist
	tsset group1 dist
	
	
	* positive net governance score
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 /*& net_gov_score > 0*/ & size_cluster ==2 /*, lag(3)*/			// change score type!!!
	
	
		/*& net_gov_score > 0*/ 
	
		gen alpha_pos_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_pos_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_pos_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_pos_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		gen obs_reg = e(N)
		
		gen beta_smb_pos_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_pos_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_pos_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_pos_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_pos_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_pos_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_pos_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_pos_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_pos_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		gen beta_rmw_pos_12m = _b[rmw] if dist==0 & n_obs >=12
		gen se_beta_rmw_pos_12m = _se[rmw] if dist==0 & n_obs >=12
		gen t_beta_rmw_pos_12m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=12
		
		gen beta_cma_pos_12m = _b[cma] if dist==0 & n_obs >=12
		gen se_beta_cma_pos_12m = _se[cma] if dist==0 & n_obs >=12
		gen t_beta_cma_pos_12m = _b[cma]/_se[cma] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 /*& net_gov_score > 0*/ & size_cluster ==2 /*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		
		gen beta_mktrf_pos_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_pos_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_pos_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_pos_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_pos_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_pos_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_pos_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_pos_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_pos_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_pos_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_pos_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_pos_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		gen beta_rmw_pos_24m = _b[rmw] if dist==0 & n_obs >=24
		gen se_beta_rmw_pos_24m = _se[rmw] if dist==0 & n_obs >=24
		gen t_beta_rmw_pos_24m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=24
		
		gen beta_cma_pos_24m = _b[cma] if dist==0 & n_obs >=24
		gen se_beta_cma_pos_24m = _se[cma] if dist==0 & n_obs >=24
		gen t_beta_cma_pos_24m = _b[cma]/_se[cma] if dist==0 & n_obs >=24

		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 /*& net_gov_score > 0*/ & size_cluster ==2 /*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		
		gen beta_mktrf_pos_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_pos_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_pos_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_pos_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_pos_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_pos_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_pos_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_pos_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_pos_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_pos_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_pos_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_pos_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		gen beta_rmw_pos_36m = _b[rmw] if dist==0 & n_obs >=36
		gen se_beta_rmw_pos_36m = _se[rmw] if dist==0 & n_obs >=36
		gen t_beta_rmw_pos_36m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=36
		
		gen beta_cma_pos_36m = _b[cma] if dist==0 & n_obs >=36
		gen se_beta_cma_pos_36m = _se[cma] if dist==0 & n_obs >=36
		gen t_beta_cma_pos_36m = _b[cma]/_se[cma] if dist==0 & n_obs >=36
	
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 /*& net_gov_score > 0*/ & size_cluster ==2 /*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		
		gen beta_mktrf_pos_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_pos_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_pos_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_pos_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_pos_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_pos_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_pos_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_pos_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_pos_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_pos_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_pos_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_pos_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48
		
		gen beta_rmw_pos_48m = _b[rmw] if dist==0 & n_obs >=48
		gen se_beta_rmw_pos_48m = _se[rmw] if dist==0 & n_obs >=48
		gen t_beta_rmw_pos_48m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=48
		
		gen beta_cma_pos_48m = _b[cma] if dist==0 & n_obs >=48
		gen se_beta_cma_pos_48m = _se[cma] if dist==0 & n_obs >=48
		gen t_beta_cma_pos_48m = _b[cma]/_se[cma] if dist==0 & n_obs >=48
	
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		gen se_alpha =.
		gen se_alpha_sq = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd rmw cma if dist == `i' /*& net_gov_score > 0*/ & size_cluster ==2																							
				replace se_alpha = _se[_cons] if dist ==`i'
				
						}
				 
		replace se_alpha_sq = se_alpha^2
		egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
		egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
		egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
		egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
		gen t_alpha_pos_12m = alpha_pos_12m/sqrt(sum_se_alpha_sq_12m)
		gen t_alpha_pos_24m = alpha_pos_24m/sqrt(sum_se_alpha_sq_24m)
		gen t_alpha_pos_36m = alpha_pos_36m/sqrt(sum_se_alpha_sq_36m)
		gen t_alpha_pos_48m = alpha_pos_48m/sqrt(sum_se_alpha_sq_48m)
				
		drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m
		
		
	* negative net governance score
	
		* 12 months
	
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 /*& net_gov_score <= 0*/ & size_cluster ==1 /*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_neg_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_neg_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_neg_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_neg_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_neg_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_neg_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_neg_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_neg_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_neg_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_neg_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_neg_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_neg_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		gen beta_rmw_neg_12m = _b[rmw] if dist==0 & n_obs >=12
		gen se_beta_rmw_neg_12m = _se[rmw] if dist==0 & n_obs >=12
		gen t_beta_rmw_neg_12m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=12
		
		gen beta_cma_neg_12m = _b[cma] if dist==0 & n_obs >=12
		gen se_beta_cma_neg_12m = _se[cma] if dist==0 & n_obs >=12
		gen t_beta_cma_neg_12m = _b[cma]/_se[cma] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 /*& net_gov_score <= 0 */& size_cluster ==1 /*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		
		gen beta_mktrf_neg_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_neg_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_neg_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_neg_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_neg_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_neg_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_neg_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_neg_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_neg_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_neg_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_neg_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_neg_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		gen beta_rmw_neg_24m = _b[rmw] if dist==0 & n_obs >=24
		gen se_beta_rmw_neg_24m = _se[rmw] if dist==0 & n_obs >=24
		gen t_beta_rmw_neg_24m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=24
		
		gen beta_cma_neg_24m = _b[cma] if dist==0 & n_obs >=24
		gen se_beta_cma_neg_24m = _se[cma] if dist==0 & n_obs >=24
		gen t_beta_cma_neg_24m = _b[cma]/_se[cma] if dist==0 & n_obs >=24

		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 /*& net_gov_score <= 0 */ & size_cluster ==1 /*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		
		gen beta_mktrf_neg_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_neg_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_neg_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_neg_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_neg_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_neg_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_neg_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_neg_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_neg_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_neg_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_neg_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_neg_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		gen beta_rmw_neg_36m = _b[rmw] if dist==0 & n_obs >=36
		gen se_beta_rmw_neg_36m = _se[rmw] if dist==0 & n_obs >=36
		gen t_beta_rmw_neg_36m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=36
		
		gen beta_cma_neg_36m = _b[cma] if dist==0 & n_obs >=36
		gen se_beta_cma_neg_36m = _se[cma] if dist==0 & n_obs >=36
		gen t_beta_cma_neg_36m = _b[cma]/_se[cma] if dist==0 & n_obs >=36
	
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 /*& net_gov_score <= 0*/ & size_cluster ==1 /*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		
		gen beta_mktrf_neg_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_neg_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_neg_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_neg_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_neg_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_neg_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_neg_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_neg_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_neg_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_neg_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_neg_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_neg_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48
		
		gen beta_rmw_neg_48m = _b[rmw] if dist==0 & n_obs >=48
		gen se_beta_rmw_neg_48m = _se[rmw] if dist==0 & n_obs >=48
		gen t_beta_rmw_neg_48m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=48
		
		gen beta_cma_neg_48m = _b[cma] if dist==0 & n_obs >=48
		gen se_beta_cma_neg_48m = _se[cma] if dist==0 & n_obs >=48
		gen t_beta_cma_neg_48m = _b[cma]/_se[cma] if dist==0 & n_obs >=48
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		replace se_alpha =.
		replace se_alpha_sq = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd rmw cma if dist == `i' /*& net_gov_score <= 0 */& size_cluster ==1																							
				replace se_alpha = _se[_cons] if dist ==`i'
				
						}
				 
		replace se_alpha_sq = se_alpha^2
		egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
		egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
		egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
		egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
		gen t_alpha_neg_12m = alpha_neg_12m/sqrt(sum_se_alpha_sq_12m)
		gen t_alpha_neg_24m = alpha_neg_24m/sqrt(sum_se_alpha_sq_24m)
		gen t_alpha_neg_36m = alpha_neg_36m/sqrt(sum_se_alpha_sq_36m)
		gen t_alpha_neg_48m = alpha_neg_48m/sqrt(sum_se_alpha_sq_48m)
				
		drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m
		
		
		
	*Delta CARs
		gen delta_pos_0_12 = alpha_pos_12m
		gen delta_pos_12_24 = alpha_pos_24m - alpha_pos_12m
		gen delta_pos_24_36 = alpha_pos_36m - alpha_pos_24m
		gen delta_pos_36_48 = alpha_pos_48m - alpha_pos_36m
		gen delta_neg_0_12 = alpha_neg_12m
		gen delta_neg_12_24 = alpha_neg_24m - alpha_neg_12m
		gen delta_neg_24_36 = alpha_neg_36m - alpha_neg_24m
		gen delta_neg_36_48 = alpha_neg_48m - alpha_neg_36m
		
	
	*Mean absolute performance 1-48 months after announcement
		gen cum_ret = ln(1+retrf) if dist > 0
		by group1: replace cum_ret = sum(cum_ret) if dist > 0
		replace cum_ret = exp(cum_ret)-1
			
			*positive score
		gen pos_mean_performance = .
		forvalues i = 1(1)48{
			sum cum_ret if dist == `i' & net_gov_score > 0
			replace pos_mean_performance = r(mean) if dist ==`i'
			}
			
			*negative score
		gen neg_mean_performance = .
		forvalues i = 1(1)48{
			sum cum_ret if dist == `i' & net_gov_score < 0
			replace neg_mean_performance = r(mean) if dist ==`i'
			}
			
			
			
			
			
		
	*** Total Social Responsbility Scores
	
	use "J:\Sascha\Buyback Anomalies\KLD.dta", clear
	drop if cusip == ""
	duplicates drop year cusip, force
	save "$temp\KLD_raw.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	
	sort firm_id dealnumber dist
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1
	
	
	gen year = yofd(deal_date)
	sort group1 dist
	by group1: carryforward year, replace
	drop if year < 1991																	//Define time period, lower bound
	drop if year > 2007																	//Define time period, upper bound
	drop year
	*drop if n_obs <48
	
	gen year = yofd(deal_date)
	merge m:1 cusip year using "$temp\KLD_raw.dta"
	sort cusip month
	drop if dealnumber ==.
	drop _merge companyid ticker companyname
	
	**Diversity																			// All indicators are constructed as done by Kecskes et al. 2016
	
	*strength: women and minorities
	gen wom_and_min = (div_str_a+ div_str_b+ div_str_c+ div_str_e- div_con_a- div_con_b)
	
	*strength: work-life balance
	gen wrklf_bal = div_str_d
	
	*strength: disabled people
	gen dis_ppl = div_str_f
	
	*Strength: gays and lesbians
	gen gay_and_les = div_str_g
	
	*other strengths minus other concerns
	gen other_str_div = div_str_x - div_con_x
	
	* toatal diversity score
	gen div_tot = wom_and_min + wrklf_bal + dis_ppl + gay_and_les + other_str_div 
	
	
	
	**Employee relations
	
	
	*strength: union relations
	gen union_rel = emp_str_a - emp_con_a
	
	*strength: employee profit sharing
	gen empl_prof_shr = emp_str_c + emp_str_d
	
	*strength: retirement benefits
	gen ret_benf = emp_str_f - emp_con_d
	
	*strength: health and safety
	gen hlth_and_sfty = emp_str_g - emp_con_b
	
	*other strengths minus other concerns
	gen other_str_emp = emp_str_x - emp_con_x
	
	*total employee score
	gen empl_tot = union_rel + empl_prof_shr + ret_benf + hlth_and_sfty + other_str_emp
	
	
	**Community
	
	*strength: charity
	gen charity = com_str_a + com_str_b + com_str_f + com_str_g
	
	*strength: support for housing
	gen sup_housing = com_str_c
	
	*strength: support for housing
	gen sup_edu = com_str_d
	
	*other strengths minus other concerns
	gen other_str_com = com_str_x - com_con_x
	
	*concern: negative economic impact
	gen neg_eco_imp = com_con_b
	
	*total community score
	gen com_tot = charity + sup_housing + sup_edu + other_str_com - neg_eco_imp
	
	
	** Environment
	
	*strenght: products and services
	gen prod_serv = env_str_a
	
	*strenght: pollution prevention
	gen pol_prev = env_str_b
	
	*strenght: recycling
	gen recycl = env_str_c
	
	*strenght: clean energy usage
	gen cln_ener = env_str_d
	
	*other strengths minus other concerns
	gen other_str_env = env_str_x - env_con_x
	
	*concern: legal and regulatory problems
	gen lgl_regl = env_con_a + env_con_b
	
	*concern: excessive pollution
	gen ex_pol = env_con_d
	
	*total environment score
	gen env_tot = prod_serv + pol_prev + recycl + cln_ener + other_str_env - lgl_regl - ex_pol
	
	
	
	*total score
	gen kld_total = div_tot + empl_tot + com_tot + env_tot
	
	sort group1 kld_total dist
	by group1: carryforward kld_total, replace
	
	
	sort group1 dist
	tsset group1 dist
	
	* positive total KLD score
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd if dist <=12 & dist > 0 & kld_total > 0
	
		gen alpha_pos_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_pos_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_pos_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_pos_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_pos_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_pos_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_pos_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_pos_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_pos_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_pos_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_pos_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_pos_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_pos_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd if dist <=24 & dist > 0 & kld_total > 0
	
		gen alpha_pos_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		
		
		gen beta_mktrf_pos_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_pos_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_pos_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_pos_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_pos_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_pos_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_pos_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_pos_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_pos_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_pos_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_pos_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_pos_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd if dist <=36 & dist > 0 & kld_total > 0
	
		gen alpha_pos_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		
		
		gen beta_mktrf_pos_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_pos_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_pos_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_pos_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_pos_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_pos_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_pos_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_pos_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_pos_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_pos_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_pos_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_pos_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & kld_total > 0
	
		gen alpha_pos_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		
		
		gen beta_mktrf_pos_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_pos_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_pos_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_pos_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_pos_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_pos_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_pos_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_pos_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_pos_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_pos_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_pos_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_pos_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48		
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		gen se_alpha =.
		gen se_alpha_sq = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd if dist == `i' & kld_total > 0																							
				replace se_alpha = _se[_cons] if dist ==`i'
				
						}
				 
		replace se_alpha_sq = se_alpha^2
		egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
		egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
		egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
		egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
		gen t_alpha_pos_12m = alpha_pos_12m/sqrt(sum_se_alpha_sq_12m)
		gen t_alpha_pos_24m = alpha_pos_24m/sqrt(sum_se_alpha_sq_24m)
		gen t_alpha_pos_36m = alpha_pos_36m/sqrt(sum_se_alpha_sq_36m)
		gen t_alpha_pos_48m = alpha_pos_48m/sqrt(sum_se_alpha_sq_48m)
				
		drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m
		
		
	* negative total KLD score
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & kld_total < 0
	
		gen alpha_neg_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_neg_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_neg_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_neg_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_neg_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_neg_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_neg_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_neg_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_neg_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_neg_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_neg_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_neg_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_neg_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd if dist <=24 & dist > 0 & kld_total < 0
	
		gen alpha_neg_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		
		
		gen beta_mktrf_neg_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_neg_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_neg_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_neg_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_neg_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_neg_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_neg_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_neg_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_neg_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_neg_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_neg_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_neg_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd if dist <=36 & dist > 0 & kld_total < 0
	
		gen alpha_neg_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		
		
		gen beta_mktrf_neg_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_neg_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_neg_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_neg_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_neg_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_neg_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_neg_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_neg_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_neg_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_neg_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_neg_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_neg_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & kld_total < 0
	
		gen alpha_neg_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		
		
		gen beta_mktrf_neg_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_neg_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_neg_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_neg_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_neg_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_neg_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_neg_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_neg_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_neg_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_neg_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_neg_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_neg_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48		
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		replace se_alpha =.
		replace se_alpha_sq = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd if dist == `i' & kld_total < 0																							
				replace se_alpha = _se[_cons] if dist ==`i'
				
						}
				 
		replace se_alpha_sq = se_alpha^2
		egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
		egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
		egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
		egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
		gen t_alpha_neg_12m = alpha_neg_12m/sqrt(sum_se_alpha_sq_12m)
		gen t_alpha_neg_24m = alpha_neg_24m/sqrt(sum_se_alpha_sq_24m)
		gen t_alpha_neg_36m = alpha_neg_36m/sqrt(sum_se_alpha_sq_36m)
		gen t_alpha_neg_48m = alpha_neg_48m/sqrt(sum_se_alpha_sq_48m)
				
		drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m
		
		
		
		
	
	g year = year(deal_date)
	
	keep cusip deal_date year alpha_12m alpha_24m alpha_36m alpha_48m
	sort cusip year
	
	save "$data\repurchase_alphas.dta", replace	
	
*	use "$data\repurchase_cars.dta", clear
	
*	sort dealnumber
	
	
	
	use "$data\Compustat_sample.dta", replace
	
	sort cusip year
	merge 1:n cusip year using "$data\repurchase_cars.dta"
	keep if _merge == 3
	drop _merge
	
	sort cusip year
	merge n:n cusip year using "$data\repurchase_alphas.dta"
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
	
	
	