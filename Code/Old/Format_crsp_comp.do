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
*	DESCRIPTION: This job puts together the final sample. 
*
*************************************************************************


* Define paths

global data		"C:\Users\valta\Dropbox\work\SBB\Data"
global temp		"C:\Users\valta\Desktop\temp"														

global data2	"J:\Sascha\Buyback Anomalies"

clear
clear matrix

set matsize 800
set scrollbufsize 500000		
capture program drop _all

	
	
	********************************************
	**** Beginn: Format and clean CRSP data	****
	********************************************
	use "V:\CRSP\Securities\Monthly\1926-2016.dta", clear
	
	keep permno date ncusip cusip shrcd ret shrout prc sprtrn vol cfacpr cfacshr
	
	* We only keep share codes 10 and 11
	keep if shrcd == 10 | shrcd == 11
		
	g year = year(date)
	drop if year < 1985
	drop if ncusip == ""
	
	duplicates drop permno date, force
	egen firm_id = group(permno)
	gen month = mofd(date)
	format %tm month
	tsset firm_id month, monthly
	
	
	* Calculate for every stock and time period the past 6 months return
	
	gen retl1 = l.ret
	gen retl2 = l2.ret
	gen retl3 = l3.ret
	gen retl4 = l4.ret
	gen retl5 = l5.ret
	gen retl6 = l6.ret
	
	sort permno date
	bysort permno: g time = _n
	
	egen six_m_sum_ret= rowtotal(retl1 retl2 retl3 retl4 retl5 retl6)															// sum of past six monthly returns prior to announcement date
	gen six_m_cum_ret = (1+retl1)*(1+retl2)*(1+retl3)*(1+retl4)*(1+retl5)*(1+retl6)-1		
	replace six_m_sum_ret = . if time == 1
	
	drop time firm_id
	
	* Merge with Fama/French and liquidity factors
	
	sort month	
	merge m:1 month using "V:\Fama French & Liqudity Factors\Pastor Stambaugh\01Aug1962 - 31Dec2017.dta"
	keep if _merge == 3
	drop _merge ps_innov ps_level
		
	sort month
	merge m:1 month using "J:\Sascha\Buyback Anomalies\ff6f.dta"
	keep if _merge == 3
	drop _merge	
	
	* Compute idiosyncratic volatility as (1 - R2)
	egen firm_id = group(permno)
	egen month_id =group(month)
	sort firm_id month_id

	g retrf = ret - rf
	
	rangestat (reg) retrf mktrf smb hml, interval(month_id -60 -1) by(firm_id)

	gen ivol = 1-reg_r2
	
	* Only keep the ivols if at least 30 observations
	replace ivol = . if reg_nobs < 30
	
	drop firm_id month_id reg_nobs reg_r2 reg_adj_r2 b_mktrf b_smb b_hml b_cons se_mktrf se_smb se_hml se_cons dateff cusip
	
	rename ncusip cusip
	
	g mkvalt = abs(prc)*shrout
	
	sort permno month
	sort permno year
	
	save "$data2/crsp_monthly_clean2.dta", replace

	
	
	****************************************************
	**** Beginn: Format and clean Compustat data	****
	****************************************************
	
 	use	gvkey linkprim liid linktype lpermno lpermco linkdt linkenddt datadate fyear fyr indfmt consol popsrc datafmt cusip		/*
	*/	che at sale csho prcc_f ceq dvc dv dlc dltt cogs /*
	*/  using "V:\CRSP\CRSP_Comp_merged_1950_2017.dta", clear  
	
	* Drop duplicate observations based on gvkey and fiscal year
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
  	
	g calyear	= year(date)
*  	g calmonth	= month(date)
	format %td date
	
	* Drop duplicate observations based on gvkey and fiscal year
	duplicates drop gvkey fyear, force
	
	* Drop duplicate observations based on gvkey and calender year
	* These duplicates occur when a firm changes its fiscal year end
	* We only keep the last calender year observation of the duplicates 
/*	duplicates tag gvkey calyear, g(tag)
	sort gvkey fyear
	bysort gvkey calyear tag: g tag2 = _n
	replace tag2 = . if tag == 0
	drop if tag2 == 1
	drop tag tag2
*/	
**** Apply some filters to the data
	
	drop if at == .
	drop if sale == .
	
	keep if (fyear>=1985)
*	keep if (fyear<=2010)
	keep if (indfmt=="INDL") & (datafmt=="STD") & (popsrc=="D") & (consol=="C")
	drop indfmt datafmt popsrc consol 
  
	keep if linktype == "LU" | linktype == "LC"
	keep if linkprim == "P" | linkprim == "C"
  
	* Define variables used for the analysis
	g	cash_at 			= che / at
	g	mb					= (csho*prcc_f + at - ceq) / at
	g 	dvc_mkvalt			= dvc / (csho*prcc_f)
	g 	dv_mkvalt			= dv / (csho*prcc_f)	
	
	g 	debt_at				= (dlc + dltt)/at
	g	debt_mv				= (dlc + dltt)/(csho*prcc_f + at - ceq)
	g	debt_cap			= (dlc + dltt)/(csho*prcc_f)
	
	g	netdebt_at			= (dlc + dltt - che)/at
	g	netdebt_mv			= (dlc + dltt - che)/(csho*prcc_f + at - ceq)
	g	netdebt_cap			= (dlc + dltt - che)/(csho*prcc_f)	
	
	g	grossprofit_at		= (sale - cogs)/at
	
	keep gvkey linkprim linktype lpermno lpermco linkdt linkenddt fyear cusip calyear fyr	/*
	*/	cash_at mb dvc dv dvc_mkvalt dv_mkvalt debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at
	
	winsor2 debt_at, replace cuts(1 99)
	winsor2 debt_mv, replace cuts(1 99)
	winsor2 debt_cap, replace cuts(1 99)	
	winsor2 netdebt_at, replace cuts(1 99)
	winsor2 netdebt_mv, replace cuts(1 99)
	winsor2 netdebt_cap, replace cuts(1 99)	
	winsor2 grossprofit_at, replace cuts(1 99)
	
	* We keep only the years after valid link
	g begyear = year(linkdt)
	keep if fyear > begyear
	
	* We keep only the years before link became invalid
	g endyear = year(linkenddt)
	keep if fyear < endyear | linkenddt == .
	
	rename lpermno permno
	
*	rename fyear year
	keep permno gvkey calyear fyear cash_at mb dvc dv dvc_mkvalt dv_mkvalt calyear fyr debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at
	
	* One year lag
	sort permno fyear
	local vlist cash_at mb dvc dvc_mkvalt dv_mkvalt debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at
		
	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if permno[_n]==permno[_n-1] & fyear[_n]==fyear[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}
	
	rename fyear year
	sort permno year
	save "$data2/compustat_clean2.dta", replace
	
	
	******************************************
	**** Merge with CRSP data ****
	******************************************
		
	use "$data2/compustat_clean2.dta", clear	
	
	merge 1:m permno year using "$data2/crsp_monthly_clean2.dta"
	keep if _merge == 3
	drop _merge
	
	duplicates drop permno month, force
	sort permno month
	
	
	save "$data2/crsp_comp_clean2.dta", replace
	
	
	
	
  
 
  
  
