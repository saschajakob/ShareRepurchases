
set rmsg on
set more off

************************************************************************
*
*	JOB:		format_compustat.do
*	PROJECT:	The effects of permanent and transitory shocks on investment and cash
*	INPUT:		Compustat and other files
*	OUTPUT:		Sample
*
*	DESCRIPTION: This job creates a panel data set with compustat data
*				 at the annual frequency
*
*************************************************************************

set more 1
set matsize 800
set scrollbufsize 500000		
capture program drop _all
clear
clear matrix

**	0.i	Philip computer
	global 	codes	"D:\Dropbox\work\CFshocks_investment\codes"
	global 	data	"D:\Dropbox\work\CFshocks_investment\data"	
	global	comp    "D:\Dropbox\work\data\comp"
	global  temp	"C:/Users/valta/Desktop/temp"
	
	global  data2	"D:\Dropbox\work\SBB\Data"

*********************************
**** Begin: Utility Programs ****
*********************************

  **** Winsor program (winsor varname [1,5])
  capture program drop winsor
  program define winsor
    quiet sum `1', detail
    replace `1' = r(p1)  if `1' ~= . & (`1' < r(p1))  & `2' == 1
    replace `1' = r(p99) if `1' ~= . & (`1' > r(p99)) & `2' == 1
    replace `1' = r(p5)  if `1' ~= . & (`1' < r(p5))  & `2' == 5
    replace `1' = r(p95) if `1' ~= . & (`1' > r(p95)) & `2' == 5
  end
  
*******************************
**** End: Utility Programs ****
*******************************

***************************************************************
**** Begin: Extract and Format of ANNUAL compustat data ****
***************************************************************
  
**** Load the annual compustat data
		
	use	gvkey cusip tic sic sich conm datadate fyear fyr indfmt datafmt popsrc consol scf emp ni		/*
	*/	che at ppent sale dltt dlc csho prcc_f capx xrd ib dp oiadp oibdp ceq prstkc	xint txc txt dvc/*
	*/ 	ivch aqc fuseo sppe siv ivstch ivaco chech dv dltis dltr dlcch sstk wcapc recch invch apalch txach aoloch fiao /*
	*/	ibc xidoc dpc txdc esubc sppiv fopo fsrco exre tstkc prstkc sstk	/*
	*/  using "$data\compann_1970_2015.dta", clear 
	
	destring gvkey, replace
	destring sic, replace
	sort gvkey fyear
	
	* Drop duplicate observations based on gvkey and fiscal year
	duplicates list gvkey fyear
	duplicates drop gvkey fyear, force
		
**** Create calendar date, year and month variables
	
	g date = .
	replace date = mdy(12,31,fyear) 	if fyr == 12
	replace date = mdy(6,30,fyear) 		if fyr == 6
	replace date = mdy(5,31,fyear+1) 	if fyr == 5
	replace date = mdy(9,30,fyear) 		if fyr == 9
	replace date = mdy(8,31,fyear) 		if fyr == 8
	replace date = mdy(10,31,fyear) 	if fyr == 10
	replace date = mdy(7,31,fyear) 		if fyr == 7
	replace date = mdy(11,30,fyear) 	if fyr == 11
	replace date = mdy(3,31,fyear+1) 	if fyr == 3
	replace date = mdy(4,30,fyear+1) 	if fyr == 4
	replace date = mdy(1,31,fyear+1) 	if fyr == 1
	replace date = mdy(2,29,fyear+1) 	if fyr == 2 & mod(fyear-1903,4) == 0
	replace date = mdy(2,28,fyear+1) 	if fyr == 2 & mod(fyear-1903,4) ~= 0
  	
	g year	= year(date)
  	g month	= month(date)
	
	* Drop duplicate observations based on gvkey and calendar year
*	duplicates list gvkey year
*	duplicates drop gvkey year, force
	
**** Apply some filters to the data
	
	* Only keep years starting in 1975 until 2010
	keep if (fyear>=1989)
*	keep if (fyear<=2010)
	keep if (indfmt=="INDL") & (datafmt=="STD") & (popsrc=="D") & (consol=="C")
	drop indfmt datafmt popsrc consol

	* Drop regulated industries
	drop if sic>=6000 & sic<7000
	drop if sic>=4900 & sic<5000
	drop if sic>=9000
	
	keep if (gvkey!=.)
	keep if (fyear!=.)
	
	drop if prcc_f < 1
	drop if sale < 10
	
*	drop if ev < 1
*	drop if asset_growth > 1
*	drop if sales_growth > 2
	
	
	
**** Define variables	
	sort gvkey year
	g	sic4				= sic
	g	sic3=floor(sic4/10)		
	g	sic2=floor(sic4/100)		


	* Compute lagged values
	sort gvkey fyear
	g atl1 		= at[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1
	g ppentl1 	= ppent[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1
	g salel1 	= sale[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1	
	
	* Size
	g	l_a					= log(at)
	g	me					= csho*prcc_f	
	g	l_me				= log(me)
	g	ev					= me + at - ceq
		
	* Stock variables	
	g	cash_at 			= che / at
	g	ppe_at				= ppent/at
	g	td					= dltt+dlc
	g	td_at				= td/at

	* Investment variables
	g	capex_at				= capx/atl1
	g	capex_cap			= capx/ppentl1
	
	replace xrd				=0 if xrd==.
	g	rd_sale				= xrd/salel1
	g	rd_at				= xrd/atl1
	
	g 	aqc_at				= aqc/at
	
	g 	div_d				= .
	replace div_d = 1 if dvc > 0
	replace div_d = 0 if dvc == 0
	replace div_d = . if dvc == .
	
	g	emp_at	= emp/at*1000
	
	* Compute different cash flow variables
	g	cf1					= ib + dp
	g	cf2 				= oibdp
	g	cf3					= oibdp - xint - txt - dvc
	
	g	cf1_at				= cf1/atl1		
	g	cf2_at				= cf2/atl1
	g	cf3_at				= cf3/atl1
	
	g	cf1_cap				= cf1/ppentl1	
	
	
	g 	ebit_margin			= oiadp/sale
	g 	ebitda_margin		= oibdp/sale	
	
	g	ebit_at				= oiadp/at
	
	* Other variables
	g	mb				= ev/at
	g 	sales_growth	= (sale - salel1) / salel1
	g 	asset_growth	= (at - atl1) / atl1	
	g 	ppe_growth		= (ppent - ppentl1) / ppentl1	

	g	cl				= ppent/emp
	
	g	roa				= ni/at
	g	roe				= ni/ceq
	
	

		
		
**** Winsorize variables
	local vlist l_a me ev l_me cash_at ppe_at td_at capex_at capex_cap rd_sale rd_at cf1_at cf2_at cf3_at cf1_cap mb aqc_at /*
	*/			asset_growth sales_growth roa roe ebit_margin ebitda_margin emp_at ebit_at
	
	foreach v of local vlist {
		winsor `v' 1
		}	

	
**** Create lags and leads of variables
	
	* One period lag
	sort gvkey fyear
	local vlist l_a me ev l_me cash_at ppe_at td_at capex_at capex_cap rd_sale rd_at cf1_at cf2_at cf3_at cf1_cap mb aqc_at /*
	*/			asset_growth sales_growth roa roe ebit_margin ebitda_margin emp_at ebit_at

		
	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}
	
	* One period lead
	sort gvkey year
	local vlist l_a me ev l_me cash_at ppe_at td_at capex_at capex_cap rd_sale rd_at cf1_at cf2_at cf3_at cf1_cap mb aqc_at /*
	*/			asset_growth sales_growth roa roe ebit_margin ebitda_margin emp_at ebit_at
		
	local vlistf1
	foreach v of local vlist {
		g `v'f1 = `v'[_n+1] if gvkey[_n]==gvkey[_n+1] & fyear[_n]==fyear[_n+1]-1
		local vlistf1 `vlistf1' `v'f1
		}
	
	* Two period lead
	sort gvkey year
	local vlist sales_growth cf2_at ebitda_margin ebit_margin capex_at rd_sale emp_at mb roe roa ebit_at
		
	local vlistf2
	foreach v of local vlist {
		g `v'f2 = `v'[_n+2] if gvkey[_n]==gvkey[_n+2] & fyear[_n]==fyear[_n+2]-2
		local vlistf2 `vlistf2' `v'f2
		}
	
	* Three period lead
	sort gvkey year
	local vlist sales_growth cf2_at ebitda_margin ebit_margin capex_at rd_sale emp_at mb roe roa ebit_at
		
	local vlistf3
	foreach v of local vlist {
		g `v'f3 = `v'[_n+3] if gvkey[_n]==gvkey[_n+3] & fyear[_n]==fyear[_n+3]-3
		local vlistf3 `vlistf3' `v'f3
		}	

	* Four period lead
	sort gvkey year
	local vlist sales_growth cf2_at ebitda_margin ebit_margin capex_at rd_sale emp_at mb roe roa ebit_at
		
	local vlistf4
	foreach v of local vlist {
		g `v'f4 = `v'[_n+4] if gvkey[_n]==gvkey[_n+4] & fyear[_n]==fyear[_n+4]-4
		local vlistf4 `vlistf4' `v'f4
		}	
			
	* Five period lead
	sort gvkey year
	local vlist sales_growth cf2_at ebitda_margin ebit_margin capex_at rd_sale emp_at mb roe roa ebit_at
		
	local vlistf5
	foreach v of local vlist {
		g `v'f5 = `v'[_n+5] if gvkey[_n]==gvkey[_n+5] & fyear[_n]==fyear[_n+5]-5
		local vlistf5 `vlistf5' `v'f5
		}	
				
	
	* Drop variables
	drop	che at ppent sale dltt dlc prcc_f capx xrd ib dp oiadp oibdp ceq prstkc 	/*
				*/ 	ivch aqc fuseo sppe siv ivstch ivaco chech dv dltis dltr dlcch sstk wcapc recch invch apalch txach aoloch fiao /*
				*/	ibc xidoc dpc txdc esubc sppiv fopo fsrco exre
	
/*
	tsset gvkey fyear
	
	g sales_growthf2 = f2.sales_growth
	g sales_growthf3 = f3.sales_growth	
	g sales_growthf4 = f4.sales_growth	
	g sales_growthf5 = f5.sales_growth
	
	g cf2_atf2	= f2.cf2_at
	g cf2_atf3	= f3.cf2_at
	g cf2_atf4	= f4.cf2_at	
	g cf2_atf5	= f5.cf2_at	
	
	g ebit_marginf2 = f2.ebit_margin
	g ebit_marginf3 = f3.ebit_margin
	g ebit_marginf4 = f4.ebit_margin
	g ebit_marginf5 = f5.ebit_margin
	
	g ebitda_marginf2 = f2.ebitda_margin
	g ebitda_marginf3 = f3.ebitda_margin
	g ebitda_marginf4 = f4.ebitda_margin
	g ebitda_marginf5 = f5.ebitda_margin	
	
	g capex_atf2 = f2.capex_at	
	g capex_atf3 = f3.capex_at	
	g capex_atf4 = f4.capex_at	
	g capex_atf5 = f5.capex_at		
	
	g rd_salef2 = f2.rd_sale	
	g rd_salef3 = f3.rd_sale	
	g rd_salef4 = f4.rd_sale	
	g rd_salef5 = f5.rd_sale	
	
	g emp_atf2 = f2.emp_at	
	g emp_atf3 = f3.emp_at	
	g emp_atf4 = f4.emp_at	
	g emp_atf5 = f5.emp_at		
	
	g mbf2 = f2.mb	
	g mbf3 = f3.mb	
	g mbf4 = f4.mb	
	g mbf5 = f5.mb			
	
*/	
	
	g str8 cusip8 = cusip
	drop cusip
	rename cusip8 cusip
**** Save the dataset 
	sort cusip fyear
	
**** There are 143,386 observations in this dataset
	count
		
	save "$data2\Compustat_sample2.dta", replace
			
***************************************************************
**** End: Extract and Format of ANNUAL compustat data ****
***************************************************************

	**** Compute CARs
	
	* +1/-1 CARs
	use "$data2/repurchase_AR.dta", clear		
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
	use "$data2/repurchase_AR.dta", clear		
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
	use "$data2/repurchase_AR.dta", clear		
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
	
	gen deal_year = yofd(deal_date)	
	tostring deal_year, replace
	g deal_month = month(deal_date)
	tostring deal_month, replace
	g str1 zeros = "0"
	
	g yearmo = deal_year + deal_month if deal_month == "10" | deal_month == "11" | deal_month == "12"
	replace yearmo = deal_year + zeros + deal_month if yearmo == ""
	
	destring yearmo, replace
	
	keep dealnumber yearmo car_11_ff car_11_m car_33_ff car_33_m car_55_ff car_55_m
	
	sort yearmo
	merge m:1 yearmo using "$data2\sentiment.dta"
	drop if _merge == 2
	drop _merge
	
	sort dealnumber
	
	save "$data2\CARs_dealnumber.dta", replace		


	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear

	* Drop financial institutions
	
	drop if siccode >= 6000 & siccode < 7000
	drop if siccode >= 4900 & siccode < 5000
	drop if siccode >= 9100
	
	g fyear = year(deal_date)
	
	sort firm_id dealnumber dist
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1	
	
	sort cusip fyear
	
	merge m:1 cusip fyear using "$data2\Compustat_sample2.dta"
	drop if _merge == 2
	
	sort dealnumber dist 
	
	g tag = 1 if _merge == 3
	egen tag2 = mean(tag), by(dealnumber)
	keep if tag2 == 1
	drop _merge tag tag2
	
	egen year_id = group(fyear)
	
	* Make size groups every year
	
	forval i= 1(1)26 {
	xtile size_cluster_`i' = me if dist==0 & year_id == `i', nq(2)		
	}
	
	egen size_bin = rowmean(size_cluster_1-size_cluster_26)
	drop size_cluster_1-size_cluster_26
	
	by dealnumber: carryforward size_bin, replace

	
	forval i= 1(1)26 {
	xtile cash_cluster_`i' = cash_at if dist==0 & year_id == `i', nq(2)		
	}
	
	egen cash_bin = rowmean(cash_cluster_1-cash_cluster_26)
	drop cash_cluster_1-cash_cluster_26
	
	by dealnumber: carryforward cash_bin, replace	
*	by dealnumber: carryforward year, replace
	
	* Merge with CARs
	sort dealnumber
	merge m:1 dealnumber using "$data2\CARs_dealnumber.dta"
	drop if _merge == 2
	drop _merge
	
	
	* Merge with KLD
	sort cusip year
	merge m:1 cusip year using "$data2\KLD_clean.dta"
	drop if _merge == 2
	drop _merge	
	
	gen net_gov_score = cgov_str_num - cgov_con_num
	gen net_env_score = env_str_num - env_con_num
	gen net_hum_score = hum_str_num - hum_con_num
	gen net_emp_score = emp_str_num - emp_con_num	
	
	* Merge with cost of equity and VP ratio
	
	
	* Define the panel variables for Fama-MacBeth
	sort group1 dist
	tsset group1 dist	
	
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0
	est store full
	* Alpha is 18.32% (=0.0038159*48)
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 & size_bin == 1
	est store small
	* Alpha is 22.63% (=0.0047138*48)
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 & size_bin == 2	
	est store large
	* Alpha is 13.74% (=0.0028635*48)
	
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 & cash_bin == 1
	est store low_cash
	* Alpha is 11.62% (=0.0024199*48)
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 & cash_bin == 2	
	est store high_cash
	* Alpha is 24.72% (=0.0051503*48)

	estout full small large low_cash high_cash using "Table_alpha.txt", replace 	///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table: Fama-MacBeth regressions of excess returns on six factors") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 		
	
	gen posCAR = 1 if car_55_ff > 0 & dist == 0
	replace posCAR = 0 if car_55_ff <= 0 & dist == 0
	
	* Sales growth
	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5, stat(mean p50 n) c(s)

	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if size_bin == 2, stat(mean p50 n) c(s)

	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if cash_bin == 2, stat(mean p50 n) c(s)	

	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if posCAR == 0, stat(mean p50 n) c(s)		
	
	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if net_gov_score <= 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	tabstat sales_growth sales_growthf1 sales_growthf2 sales_growthf3 sales_growthf4 sales_growthf5 if net_gov_score > 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	
	* EBIT margin
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5, stat(mean p50 n) c(s)

	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if size_bin == 2, stat(mean p50 n) c(s)

	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if cash_bin == 2, stat(mean p50 n) c(s)
	
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if posCAR == 0, stat(mean p50 n) c(s)	
	
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if net_gov_score <= 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	tabstat ebit_margin ebit_marginf1 ebit_marginf2 ebit_marginf3 ebit_marginf4 ebit_marginf5 if net_gov_score > 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	
	
	* Investment
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5, stat(mean p50 n) c(s)

	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if size_bin == 2, stat(mean p50 n) c(s)

	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if cash_bin == 2, stat(mean p50 n) c(s)	
	
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if posCAR == 0, stat(mean p50 n) c(s)		
	
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if net_gov_score <= 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	tabstat capex_at capex_atf1 capex_atf2 capex_atf3 capex_atf4 capex_atf5 if net_gov_score > 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	
	
	* Employment
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5, stat(mean p50 n) c(s)

	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if size_bin == 2, stat(mean p50 n) c(s)

	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if cash_bin == 2, stat(mean p50 n) c(s)		
	
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if posCAR == 0, stat(mean p50 n) c(s)			
	
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if net_gov_score <= 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	tabstat emp_at emp_atf1 emp_atf2 emp_atf3 emp_atf4 emp_atf5 if net_gov_score > 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	
	
	* ROE
	tabstat roe roef1 roef2 roef3 roef4 roef5, stat(mean p50 n) c(s)

	tabstat roe roef1 roef2 roef3 roef4 roef5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat roe roef1 roef2 roef3 roef4 roef5 if size_bin == 2, stat(mean p50 n) c(s)

	tabstat roe roef1 roef2 roef3 roef4 roef5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat roe roef1 roef2 roef3 roef4 roef5 if cash_bin == 2, stat(mean p50 n) c(s)	
	
	tabstat roe roef1 roef2 roef3 roef4 roef5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat roe roef1 roef2 roef3 roef4 roef5 if posCAR == 0, stat(mean p50 n) c(s)			
	
	tabstat roe roef1 roef2 roef3 roef4 roef5 if net_gov_score <= 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	tabstat roe roef1 roef2 roef3 roef4 roef5 if net_gov_score > 0 & net_gov_score !=., stat(mean p50 n) c(s)		
	
	* ROCE
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5, stat(mean p50 n) c(s)
	
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if size_bin == 2, stat(mean p50 n) c(s)

	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if cash_bin == 2, stat(mean p50 n) c(s)		
	
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if posCAR == 0, stat(mean p50 n) c(s)		

	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if net_gov_score <= 0 & net_gov_score !=., stat(mean p50 n) c(s)	
	tabstat ebit_at ebit_atf1 ebit_atf2 ebit_atf3 ebit_atf4 ebit_atf5 if net_gov_score > 0 & net_gov_score !=., stat(mean p50 n) c(s)			
	
	
	
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5, stat(mean p50 n) c(s)
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5 if size_bin == 2, stat(mean p50 n) c(s)
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5 if cash_bin == 2, stat(mean p50 n) c(s)			
	
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat mb mbf1 mbf2 mbf3 mbf4 mbf5 if posCAR == 0, stat(mean p50 n) c(s)			
	
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5, stat(mean p50 n) c(s)
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5 if size_bin == 2, stat(mean p50 n) c(s)
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5 if cash_bin == 2, stat(mean p50 n) c(s)			
	
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat rd_sale rd_salef1 rd_salef2 rd_salef3 rd_salef4 rd_salef5 if posCAR == 0, stat(mean p50 n) c(s)	
	

	
	
		
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5, stat(mean p50 n) c(s)
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5 if size_bin == 1, stat(mean p50 n) c(s)
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5 if size_bin == 2, stat(mean p50 n) c(s)
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5 if cash_bin == 1, stat(mean p50 n) c(s)	
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5 if cash_bin == 2, stat(mean p50 n) c(s)		
	
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5 if posCAR == 1, stat(mean p50 n) c(s)	
	tabstat roa roaf1 roaf2 roaf3 roaf4 roaf5 if posCAR == 0, stat(mean p50 n) c(s)	
	
	
	
	
	