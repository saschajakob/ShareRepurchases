*version 14.2
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

*******************************************************************************
*						COMPUSTAT Frequency
*
					scalar frequency= "annually"
*					scalar frequency= "quarterly"
*
*******************************************************************************

* Define paths

*global data		"C:\Users\valta\Dropbox\work\SBB\Data"
global temp		"C:\Users\\`=c(username)'\Desktop\temp"
*global data2	"A:\Sascha\Buyback Anomalies"
global data2	"W:\Sascha\Buyback Anomalies"

clear
clear matrix
*set matsize 800
set scrollbufsize 500000
capture program drop _all
global F9 "browse;"

**** Format the sdc data (country names)

	*use "C:\Users\\`=c(username)'\Dropbox\Buybacks\Data\Share Buybacks raw data\SDC_repurchase_1985_2019.dta", clear				// updated till 2019, 24'712 lines in the raw file
	use "D:\Dropbox\work\Buybacks\Data\Share Buybacks raw data\SDC_repurchase_1985_2019.dta", clear
	
	drop if cusip8 == ""
	drop v7

	tab corporateaction
	keep if corporateaction == ""																// Drop corporate actions other than share repurchases

	keep daterepurchaseauthorizedbyboard totalsharesauthorized name dealnumber numberofsecuritiesoutstanding ///
		  cusip cusip8 pctsharesauthorizedforrepurchase totalpercentofsharesauth totalvalueauthorizedmil

	destring pctsharesauthorizedforrepurchase, replace force

	replace pctsharesauthorizedforrepurchase = 100 if pctsharesauthorizedforrepurchase > 100 & pctsharesauthorizedforrepurchase != .
	replace totalpercentofsharesauth = 100 if totalpercentofsharesauth > 100 & totalpercentofsharesauth != .

	gen date = date(daterepurchaseauthorizedbyboard, "DMY")
	format date %d

	gen month = mofd(date)
	format month %tm

	gen ayear = year(date)

	sort cusip8 date

	duplicates drop cusip8 date, force

	keep daterepurchaseauthorizedbyboard totalsharesauthorized cusip dealnumber cusip8 totalpercentofsharesauth date month ayear totalvalueauthorizedmil
	sort cusip8 date
	egen sequence = rank(month), u by(cusip8)										// Sequence number of announcement within same company, first announcement = 1

	drop if ayear < 1985

	rename cusip cusip6
	rename cusip8 cusip

	* Drop the multiple observations when the firm announces more than one buyback in a given month
	duplicates drop cusip month, force
	sort cusip month
	count

*	merge m:1 cusip using  "$data2/S&P_500_const.dta"

*	drop if _merge ==2
*	drop _merge
*	drop gvkey

	save "$data2/sdc_repurchase_clean.dta", replace															// _N = 19'699 (17'407 in 2015)

	********************************************
	**** Beginn: Format and clean CRSP data	****
	********************************************
	use "V:\CRSP\Securities\Monthly\CRSP_monthly_1970_2019.dta", clear													// updated till 2019

	keep permno date ncusip cusip shrcd ret shrout prc sprtrn hsiccd

	g year = year(date)
	drop if year < 1985
	drop if ncusip == ""

	duplicates drop permno date, force
	egen firm_id = group(permno)
	gen month = mofd(date)
	format %tm month
	tsset firm_id month, monthly

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

*	sort month
*	merge m:1 month using "V:\Fama French & Liqudity Factors\Pastor Stambaugh\1962-2018.dta"						            // updated till 2018
*	keep if _merge == 3
*	drop _merge ps_innov ps_level

	sort month
	merge m:1 month using "V:\Fama French & Liqudity Factors\Factory Mothly\FamaFrench_1926_2019.dta" 											// updated till 2019
	keep if _merge == 3
	drop _merge

	* Compute idiosyncratic volatility
	egen firm_id = group(permno)
	egen month_id =group(month)
	sort firm_id month_id

	g retrf = ret - rf

	rangestat (reg) retrf mktrf smb hml, interval(month_id -60 -1) by(firm_id)

	gen ivol = 1-reg_r2																						// relative IVOL

	* Only keep the ivols if at least 30 observations
	replace ivol = . if reg_nobs < 30

	drop firm_id month_id reg_nobs reg_r2 reg_adj_r2 b_mktrf b_smb b_hml b_cons se_mktrf se_smb se_hml se_cons cusip

	rename ncusip cusip

	g mkvalt = abs(prc)*shrout
	
	** Merge with daily volume data
	sort permno month
	merge 1:1 permno month using "$data2/crsp_vol_sd.dta"
	drop if _merge == 2
	drop _merge
	
	sort cusip month 
	
	** Create Fama-French industries
	
	ffind hsiccd, g(ff12) type(12)
	ffind hsiccd, g(ff17) type(17)	
	ffind hsiccd, g(ff48) type(48)
	
	save "$data2/crsp_monthly_clean.dta", replace

	*************************************
	**** Merge with SDC buyback data ****
	*************************************

	use "$data2/crsp_monthly_clean.dta", clear

	merge 1:1 cusip month using "$data2/sdc_repurchase_clean.dta"

	* Keep only firms for which we have share buybacks

	g buyback = 1 if _merge == 3
	egen tag2 = mean(buyback), by(permno)
	keep if tag2 == 1
	replace buyback = 0 if buyback == .

	drop tag2 _merge
	sort permno month
	sort permno year
	g yearqtr = qofd(date)
	format yearqtr %tq

	save "$data2/crsp_sdc_clean.dta", replace

	****************************************************
	**** Beginn: Format and clean Compustat data	****
	****************************************************

if frequency == "annually" {
	*Annual Data
 	use	gvkey linkprim liid linktype lpermno lpermco linkdt linkenddt datadate fyear fyr indfmt consol popsrc datafmt cusip		/*
	*/	che at act sale csho prcc_f ceq dvc dv dlc dltt cogs oiadp oibdp capx xrd act lct txt xint sic ni /*
	*/  using "V:\CRSP\CRSP_Comp_1950_2019.dta", clear  													// updated till 2019

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
	format %td date

	* Drop duplicate observations based on gvkey and fiscal year
	duplicates drop gvkey fyear, force
	}

		if frequency == "quarterly" {

	*Load Quaterly Data
	use gvkey linkprim liid linktype lpermno lpermco linkdt linkenddt datadate fyear fyr fqtr indfmt consol popsrc datafmt cusip  datafqtr    ///
	cheq atq actq saleq cshoq cshopq prccq ceq dlcq dlttq cogsq oiadpq oibdpq capxy xrdq actq lctq txtq xintq sic niq dvpspq dvy ///
	using "C:\Temp\CCM_quarterly_1965_2018.dta", clear

	rename (cheq atq actq saleq cshoq cshopq prccq ceqq dlcq dlttq cogsq oiadpq oibdpq capxy xrdq actq lctq xintq niq) ///
			(che at act sale csho cshop prcc_f ceq dlc dltt cogs oiadp oibdp capx xrd act lct xint ni)	///

	g date = datadate
	format date %td
	g quarter = quarter(datadate)
	g dataquarter = qofd(datadate)
	format dataquarter %tq
	duplicates drop gvkey dataquarter, force

	g dvc = dvpspq*csho

	destring gvkey, replace
	sort gvkey dataquarter
	tsset gvkey dataquarter, q
	sort gvkey dataquarter
	bysort gvkey: generate dv = dvy-l.dvy
	g dq = substr(datafqtr,5,2)
	replace dv = dvy if dq=="Q1"
	drop dq
	g calyear	= year(date)
	g fyearqtr = yq(fyear, fqtr)
	format fyearqtr %tq
	}

**** Apply some filters to the data

	drop if at == .
	drop if sale == .

	keep if (fyear>=1965)
	keep if (indfmt=="INDL") & (datafmt=="STD") & (popsrc=="D") & (consol=="C")
	drop indfmt datafmt popsrc consol

	keep if linktype == "LU" | linktype == "LC"
	keep if linkprim == "P" | linkprim == "C"

	* Define variables used for the analysis
	g	cash_at 			= che / at
	g 	ex_che2				= (act - dlc -che)/at
	g	nat					= at - che
	g 	cash_nat			= che / nat
	g	mb					= (csho*prcc_f + at - ceq) / at
	g	mtb					= (csho*prcc_f)/ceq
	g 	dvc_mkvalt			= dvc / (csho*prcc_f)
	g 	dv_mkvalt			= dv / (csho*prcc_f)
	g   div_d				= 1 if dv > 0
		replace div_d		= 0 if dv <=0
	g   fcf					= (oiadp - xint - dv)/at
	g   cflow				= (oibdp - xint - dv - txt)/nat
	g	nwc					= (act -lct -che)/at
	g   rds					= xrd/sale
		replace rds 		= 0 if xrd ==.											// as in Opler 1999
	g   capex				= capx/at

	g 	debt_at				= (dlc + dltt)/at
	g	debt_mv				= (dlc + dltt)/(csho*prcc_f + at - ceq)
	g	debt_cap			= (dlc + dltt)/(csho*prcc_f)

	g	netdebt_at			= (dlc + dltt - che)/at
	g	netdebt_mv			= (dlc + dltt - che)/(csho*prcc_f + at - ceq)
	g	netdebt_cap			= (dlc + dltt - che)/(csho*prcc_f)

	g	grossprofit_at		= (sale - cogs)/at
	g	roa					= ni/at
	g	ebitda_a			= oibdp/at


	if frequency =="annually" {

	destring gvkey, replace
	sort gvkey fyear
	xtset gvkey fyear
	rangestat (sd) cflow, interval(fyear  -10 -1) by(gvkey)
	rename cflow_sd cflow_vol_10
	rangestat (sd) cflow, interval(fyear  -5 -1) by(gvkey)
	rename cflow_sd cfflow_vol_5

	gen sic2 = substr(sic,1,2)
	gen sic3 = substr(sic,1,3)

	egen industry_sigma = mean(cflow_vol_10), by(fyear sic3)

	egen industry_leverage = median(netdebt_at), by(fyear sic3)						// SIC 2 as in Dittmar 2000
	gen lev_dev = netdebt_at - industry_leverage
	gen lev_dev_d = 1 if netdebt_at >= industry_leverage
		replace lev_dev_d = 0 if netdebt_at < industry_leverage


	keep gvkey linkprim linktype lpermno lpermco linkdt linkenddt fyear cusip calyear fyr	///
		cash_at ex_che2 mb mtb rds dvc roa ebitda_a nwc dv at dvc_mkvalt dv_mkvalt debt_at debt_mv debt_cap netdebt_at ///
		netdebt_mv netdebt_cap grossprofit_at fcf cflow capex div_d sic2 sic3 industry_sigma cash_nat nat industry_leverage lev_dev lev_dev_d

	}

	if frequency == "quarterly"{
	destring gvkey, replace
	sort gvkey dataquarter
	xtset gvkey dataquarter
	rangestat (sd) cflow, interval(fyearqtr  -40 -1) by(gvkey)
	rename cflow_sd cflow_vol_10
	rangestat (sd) cflow, interval(fyearqtr  -20 -1) by(gvkey)
	rename cflow_sd cflow_vol_5

	gen sic2 = substr(sic,1,2)
	gen sic3 = substr(sic,1,3)

	egen industry_sigma = mean(cflow_vol_10), by(fyearqtr sic3)

	egen industry_leverage = median(netdebt_at), by(fyearqtr sic2)						// SIC 2 as in Dittmar 2000
	gen lev_dev = netdebt_at - industry_leverage
	gen lev_dev_d = 1 if netdebt_at >= industry_leverage
		replace lev_dev_d = 0 if netdebt_at < industry_leverage


	keep gvkey linkprim linktype lpermno lpermco linkdt linkenddt fyear fqtr fyearqtr cusip calyear fyr	///
		cash_at ex_che2 mb mtb rds dvc roa ebitda_a nwc dv at dvc_mkvalt dv_mkvalt debt_at debt_mv debt_cap netdebt_at ///
		netdebt_mv netdebt_cap grossprofit_at fcf cflow capex div_d sic2 sic3 industry_sigma cash_nat nat industry_leverage lev_dev lev_dev_d dataquarter

	}

	winsor2 at, replace cuts(1 99)
	winsor2 ex_che2, replace cuts(1 99)
	winsor2 debt_at, replace cuts(1 99)						// Why do we not winsorize che/at?
	winsor2 debt_mv, replace cuts(1 99)
	winsor2 mtb, replace cuts(1 99)
	winsor2 nwc, replace cuts(1 99)
	winsor2 fcf, replace cuts(1 99)
	winsor2 cflow, replace cuts(1 99)
	winsor2 rds, replace cuts(1 99)
	winsor2 capex, replace cuts(1 99)
	winsor2 debt_cap, replace cuts(1 99)
	winsor2 netdebt_at, replace cuts(1 99)
	winsor2 netdebt_mv, replace cuts(1 99)
	winsor2 netdebt_cap, replace cuts(1 99)
	winsor2 grossprofit_at, replace cuts(1 99)
	winsor2 industry_sigma, replace cuts(1 99)
	winsor2 cash_nat, replace cuts(1 99)
	winsor2 roa, replace cuts(1 99)
	winsor2 ebitda_a, replace cuts(1 99)
	winsor2 lev_dev, replace cuts(1 99)
	winsor2 industry_leverage, replace cuts(1 99)

	* We keep only the years after valid link
	g begyear = year(linkdt)
	keep if fyear > begyear

	* We keep only the years before link became invalid
	g endyear = year(linkenddt)
	keep if fyear < endyear | linkenddt == .

	rename lpermno permno

		if frequency == "annually"{
	keep gvkey permno calyear roa ebitda_a capex fyear rds cash_at ex_che2 at mb mtb dvc nwc dv dvc_mkvalt dv_mkvalt calyear fyr ///
	debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at fcf cflow div_d sic2 sic3 industry_sigma cash_nat ///
	nat industry_leverage lev_dev lev_dev_d
	}

	if frequency == "quarterly"{
	keep gvkey permno calyear roa ebitda_a capex fyear fqtr fyearqtr rds cash_at ex_che2 at mb mtb dvc nwc dv dvc_mkvalt dv_mkvalt calyear fyr ///
	debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at fcf cflow div_d sic2 sic3 industry_sigma cash_nat ///
	nat industry_leverage lev_dev lev_dev_d dataquarter
	}

	* Opler 1999 to estimate excess cash
	gen log_cash = ln(cash_nat)
	gen size = ln(at)
	gen ex_che =.


	if frequency == "annually"{
	forval i = 1985(1)2018 {
	reg log_cash mtb size cflow nwc capex debt_at industry_sigma rds div_d if fyear ==`i'
	predict fit if fyear ==`i', xb
	replace ex_che = exp(log_cash)-exp(fit) if fyear ==`i'
	drop fit
	}
	drop log_cash size nat cash_nat nwc cflow capex rds
	winsor2 ex_che, replace cuts(1 99)
	}

	if frequency == "quarterly" {
	forval i = 100(1)235 {
	reg log_cash mtb size cflow nwc capex debt_at industry_sigma rds div_d if fyearqtr ==`i'
	predict fit if fyearqtr ==`i', xb
	replace ex_che = exp(log_cash)-exp(fit) if fyearqtr ==`i'
	drop fit
	}
	drop log_cash size nat cash_nat nwc cflow capex rds
	winsor2 ex_che, replace cuts(1 99)
	}

	* age

	egen first_date = min(fyear), by(permno)
	g age = fyear - first_date +1														// does not consider lifetime prior to going public
	drop first_date

	merge m:1 permno using "$data2\IPO_year.dta"
	drop if _merge ==2
	drop _merge

	g age2 = fyear - IPOyear
	drop IPOyear begdat

	keep if (fyear>=1985)
	* One year lag
	if frequency == "annually"{
	sort permno fyear
	local vlist cash_at at mb mtb dvc dvc_mkvalt dv_mkvalt fcf debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at div_d ex_che ex_che2 roa ebitda_a lev_dev lev_dev_d industry_leverage age

	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if permno[_n]==permno[_n-1] & fyear[_n]==fyear[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}

	rename fyear year
	sort permno year
	save "$data2/compustat_clean.dta", replace
	}

	* One quarter lag
	if frequency == "quarterly"{
	sort permno fyearqtr
	local vlist cash_at at mb mtb dvc dvc_mkvalt dv_mkvalt fcf debt_at debt_mv debt_cap netdebt_at netdebt_mv netdebt_cap grossprofit_at div_d ex_che ex_che2 roa ebitda_a lev_dev lev_dev_d industry_leverage age

	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if permno[_n]==permno[_n-1] & fyearqtr[_n]==fyearqtr[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}

	rename fyear year
	rename fyearqtr yearqtr
	sort permno yearqtr
	save "$data2/compustat_clean_q.dta", replace
	}


	use "$data2/S&P_500_const.dta", clear
	destring gvkey, replace force
	drop cusip
	sort gvkey
	save "$data2/sp500_constitutents.dta", replace
	
	use "$data2/compustat_clean.dta", clear
	sort gvkey
	
	merge m:1 gvkey using  "$data2/sp500_constitutents.dta"
	drop if _merge == 2
	drop _merge
	
	drop conm
	
	sort permno year
	save "$data2/compustat_clean.dta", replace
	
	
	******************************************
	**** Merge with CRSP/SDC buyback data ****
	******************************************
	if frequency == "annually"{
	use "$data2/compustat_clean.dta", clear
	merge 1:m permno year using "$data2/crsp_sdc_clean.dta"
	keep if _merge == 3
	drop _merge
	}

	if frequency == "quarterly"{
	use "$data2/compustat_clean_q.dta", clear
	merge 1:m permno yearqtr using "$data2/crsp_sdc_clean.dta"
	keep if _merge == 3
	drop _merge
	drop yearqtr
	}

	gen qdate = qofd(date)
	format qdate %tq
	sort cusip qdate month

	**** Create an S&P 500 variable
	g sp500stock = 0
	replace sp500stock = 1 if from_1 <= date & thru_1 >= date
	replace sp500stock = 1 if from_2 <= date & thru_2 >= date
	replace sp500stock = 1 if from_3 <= date & thru_3 >= date
	replace sp500stock = 1 if from_4 <= date & thru_4 >= date	
	
	if frequency == "annually" {
	save "$data2/crsp_sdc_comp_clean.dta", replace
	}
	if frequency == "quarterly" {
	save "$data2/crsp_sdc_comp_clean_q.dta", replace
	}

	

	
	
	****************************************************
	**** Beginn: Format and clean Ownership data	****
	****************************************************

	use "V:\13F ownership data\ownership.dta", clear

	gen month = mofd(rdate)
	format month %tm
	gen year =yofd(rdate)
	format year %ty
	keep if year >=1985

	keep rdate month cusip shrout top5instown top10instown numinstblockowners instblockown numinstowners maxinstown instown instown_perc

	gen top10instown_perc = top10instown/(shrout*1000)								 // top 10 holdings in %
	la var top10instown_perc "holdings of top 10 holders in %"

	gen qdate = qofd(rdate)
	format qdate %tq

	* One quarter lag
	sort cusip qdate
	local vlist top10instown_perc

	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if cusip[_n]==cusip[_n-1] & qdate[_n]==qdate[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}

	save "$data2/13f_clean.dta", replace												// updated till 2018

	****************************************************
	**** Beginn: Format and clean Short interest data	****
	****************************************************

	use "V:\Short interest\CRSP_short_interest_1973-2018.dta", clear
	drop cusip sic  datadate splitadjdate cik naics
	keep if day <= 22
	destring iid gvkey, replace
	drop if iid >=90																	// above 90 is ADR
	drop iid shortintadj day

	sort gvkey month
	save "$data2/SI_data.dta", replace

 	*******************************
	**** Merge with other data ****
	*******************************

	if frequency == "annually"{
	use "$data2/crsp_sdc_comp_clean.dta", clear
	}
	if frequency == "quarterly"{
	use "$data2/crsp_sdc_comp_clean_q.dta", clear
	destring gvkey, replace
	}

	merge m:1 cusip qdate using "$data2/13f_clean.dta"
	keep if _merge == 3 																	// Why do we keep only merged observations?
	drop _merge

	merge 1:1 gvkey month permno using "$data2/SI_data.dta"
	drop if _merge == 2
	drop _merge
	xtset permno month
	g si = shortint / (shrout*1000)
	g si_delta = l.shortint/l4.shortint-1
	rangestat (mean) si, interval(month -3 -1) by(permno)

	merge m:1 permno month using "V:\Amihud Measure\amihud_measure.dta", ///
		keepusing(amihud_1M amihud_3M amihud_6M amihud_1Y)
	keep if _merge == 3
	drop _merge

	sort permno month

	merge 1:1 permno month using "$data2/crsp_vol_sd.dta"
	drop if _merge == 2
	drop _merge




if frequency == "annually"{
	save "$data2/sample_buyback_portfolio.dta", replace
	save "C:\Users\\`=c(username)'\Dropbox\Buybacks\Data\sample_buyback_portfolio.dta", replace   // Main data set, additionally stored on dropbox
	distinct dealnumber
	}

	if frequency == "quarterly"{
	save "$data2/sample_buyback_portfolio_q.dta", replace
	save "C:\Users\\`=c(username)'\Dropbox\Buybacks\Data\sample_buyback_portfolio_q.dta", replace   // Main data set, additionally stored on dropbox
	distinct dealnumber
	}


	***********************************************************************************************
	*** Final data set contains 12'377 (Y) / 12'254 (Q) distinct share repurchase announcements ***
	***********************************************************************************************
