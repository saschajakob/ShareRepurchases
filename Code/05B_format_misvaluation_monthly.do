version 14.2
set rmsg on
set more off

************************************************************************
*
*	JOB:		Format CRSP and Compustat data
*	PROJECT:	Negative effects of M&As on rivals
*
*	DESCRIPTION: Compute misvaluation measures based on Rhodes-Kropf and Viswanathan
*
*************************************************************************


* Define paths

global data		"A:\Sascha\Buyback Anomalies"
global temp		"C:\Users\sajakob\Desktop\temp"

clear
clear matrix

set matsize 800
set scrollbufsize 500000
capture program drop _all



	********************************************
	**** Beginn: Format and clean CRSP data	****
	********************************************
	use "V:\CRSP\Securities\Monthly\1926-2018.dta", clear

	keep permno date ncusip cusip shrcd ret shrout prc sprtrn siccd

	g year = year(date)
	keep if (year>=1977)
	drop if ncusip == ""

	duplicates drop permno date, force
	egen firm_id = group(permno)
	gen month = mofd(date)
	format %tm month
	tsset firm_id month, monthly


	* Merge with Fama/French and liquidity factors

	sort month
	merge m:1 month using "$data\ff6f.dta"
	keep if _merge == 3
	drop _merge

	* Compute idiosyncratic volatility as 1 - Rsquared using the Fama-French 3-factor model
	egen month_id =group(month)
	sort firm_id month_id

	g retrf = ret - rf

	rangestat (reg) retrf mktrf smb hml, interval(month_id -60 -1) by(firm_id)


	drop firm_id month_id reg_nobs reg_r2 reg_adj_r2 b_mktrf b_smb b_hml b_cons se_mktrf se_smb se_hml se_cons dateff cusip

	g mkvalt = abs(prc)*shrout


	sort permno month

	save "$temp/crsp_tmp.dta", replace

	******************************************
	**** End: Format and clean CRSP data  ****
	******************************************

	*************************************************
	**** Beginn: Format and clean Compustat data ****
	*************************************************

 	use 	gvkey linkprim liid linktype lpermno lpermco linkdt linkenddt datadate fyear fyr indfmt consol popsrc datafmt cusip 	sic 	/*
	*/  	che at sale csho prcc_f ceq dvc dv dlc dltt txdb ppent capx ni  /*
	*/  using "V:\CRSP\CRSP_Comp_merged_1950_2018.dta", clear

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
  	g month	= mofd(datadate)
	format %td date
	format %tm month


	* Drop duplicate observations based on gvkey and fiscal year
	duplicates drop gvkey fyear, force
	duplicates drop gvkey month, force


	tostring fyear, g(tmp1)
	tostring fyr, g(tmp2)
	g str1 tmp3 = "0"
	g str1 tmp5 = "/"

	g tmp4 = tmp1 + tmp5 + tmp3 + tmp2 if fyr < 10
	replace tmp4 = tmp1 + tmp5 + tmp2 if fyr >= 10

	g month_true = monthly(tmp4, "YM")
	format %tm month_true
	drop tmp1 tmp2 tmp3 tmp4 tmp5

	* Create a new month index for match with CRSP that leads by three month
	*g month = month_true + 3
	*format %tm month

	**** Apply some filters to the data and prepare for match

	drop if at == .
	drop if sale == .

	keep if (fyear>=1985)
*	keep if (fyear<=2010)
	keep if (indfmt=="INDL") & (datafmt=="STD") & (popsrc=="D") & (consol=="C")
	drop indfmt datafmt popsrc consol

	keep if linktype == "LU" | linktype == "LC"
	keep if linkprim == "P" | linkprim == "C"

	* We keep only the years after valid link
	g begyear = year(linkdt)
	keep if fyear > begyear

	* We keep only the years before link became invalid
	g endyear = year(linkenddt)
	keep if fyear < endyear | linkenddt == .

	rename lpermno permno

	drop linkprim liid linktype lpermco linkdt linkenddt cusip begyear endyear date

	sort permno month

	g ceq_l1 = ceq[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1

	expand 12
	sort permno fyear
	egen temp_id = group(permno fyear)
	egen temp_rank = rank(fyear), by(temp_id) unique
	g temp_month = month -(temp_rank - 1)
	format temp_month %tm
	sort permno temp_month
	replace month = temp_month
	drop temp_month temp_id temp_rank
	sort permno month datadate
	duplicates drop permno month, force
	g m_index = mofd(datadate)
	format %tm m_index
	replace ceq_l1 = ceq if month == m_index
	drop m_index

	save "$temp/comp_tmp.dta", replace


	**********************************************
	**** End: Format and clean Compustat data ****
	**********************************************

	**************************************************
	**** Beginn: Merge and variables construction ****
	**************************************************

	use "$temp/comp_tmp.dta", clear

	merge 1:1 permno month using "$temp/crsp_tmp.dta"
	keep if _merge == 3
	drop _merge

	sort gvkey fyear

	* Create Fama-French 12 industry classification

	ffind siccd, newvar(ff12) type(12)


	* Define other variables
	* Rescale CRSP market cap, which is in thousands, to millions
	replace mkvalt = mkvalt/1000

	g mv_at = (mkvalt + at - txdb - ceq)/at
	g ln_mkvalt = ln(mkvalt)
	g ln_bkvalt = ln(ceq)
	g ln_bkvalt_l1 = ln(ceq_l1)
	g mk_bk = ln_mkvalt - ln_bkvalt_l1

	winsor2 mv_at, replace cuts(1 99)
	*winsor2 mk_bk, replace cuts(1 99)
	*winsor2 ln_mkvalt, replace cuts(1 99)
	*winsor2 ln_bkvalt, replace cuts(1 99)

	tabstat mv_at ln_mkvalt ln_bkvalt, stats(n mean sd p50 p1 p5 p95 p99) c(s)

	egen year_id = group(fyear)
	egen month_id = group(month)

	* Run the cross-sectional regressions as in Rhodes-Kropf, Robinson, and Viswanathan (2005) and capture regression estimates

	** Model 1:

	* These alphas are estimated for every industry-year
	g alpha0_jt = .
	g alpha1_jt = .


	*sum year_id, det
	 sum month_id, det

	forval i = 1(1)`r(max)' {

		forval j = 1(1)12 {

		qui capture reg ln_mkvalt ln_bkvalt_l1 if month_id == `i' & ff12 == `j'
		if _rc != 0{
		continue
		}

		replace alpha0_jt = _b[_cons] if month_id == `i' & ff12 == `j'
		replace alpha1_jt = _b[ln_bkvalt_l1] if month_id == `i' & ff12 == `j'

		}
	}

	* We take the time-series average of the alphas for every industry
	egen alpha_bar0_j = mean(alpha0_jt), by(ff12)
	egen alpha_bar1_j = mean(alpha1_jt), by(ff12)

	egen alpha_bar0_j_RKRV = mean(alpha0_jt) if fyear <= 2000, by(ff12)
	egen alpha_bar1_j_RKRV = mean(alpha1_jt) if fyear <= 2000, by(ff12)

	* Look at summary stats
	tabstat alpha_bar0_j alpha_bar1_j, stats(mean) by(ff12) c(v)

	tabstat alpha_bar0_j_RKRV alpha_bar1_j_RKRV, stats(mean) by(ff12) c(v)

	* Generate fundamental value variables
	g v_alpha_jt = alpha0_jt + alpha1_jt * ln_bkvalt
	g v_alpha_bar_j = alpha_bar0_j + alpha_bar1_j * ln_bkvalt_l1

	* Generate misvaluation variables
	g RKRV_firm_error = ln_mkvalt - v_alpha_jt
	g RKRV_sector_error = v_alpha_jt - v_alpha_bar_j
	g RKRV_longrun_vb = v_alpha_bar_j - ln_bkvalt_l1

	tabstat RKRV_firm_error RKRV_sector_error RKRV_longrun_vb, stats(n mean sd p50 p1 p5 p95 p99) c(s)


	keep gvkey permno  month ln_mkvalt ln_bkvalt ln_bkvalt_l1 mk_bk alpha0_jt alpha1_jt alpha_bar0_j alpha_bar1_j alpha_bar0_j_RKRV alpha_bar1_j_RKRV v_alpha_jt v_alpha_bar_j RKRV_firm_error RKRV_sector_error RKRV_longrun_vb

	sort permno month

	save "$data/RKRV_misvaluation_monthly.dta", replace
