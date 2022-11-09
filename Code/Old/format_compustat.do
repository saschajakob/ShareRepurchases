
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
	global 	codes	"C:\Users\valta\Dropbox\work\CFshocks_investment\codes"
	global 	data	"C:\Users\valta\Dropbox\work\CFshocks_investment\data"	
	global	comp    "C:\Users\valta\Dropbox\work\data\comp"
	global  temp	"C:/Users/valta/Desktop/temp"
	
	global  data2	"C:\Users\valta\Dropbox\work\SBB\Data"

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
	*/	che at ppent sale dltt dlc csho prcc_f capx xrd ib dp oibdp ceq prstkc	xint txc txt dvc/*
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
	keep if (fyear>=1990)
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
	
	* Compute different cash flow variables
	g	cf1					= ib + dp
	g	cf2 				= oibdp
	g	cf3					= oibdp - xint - txt - dvc
	
	g	cf1_at				= cf1/atl1		
	g	cf2_at				= cf2/atl1
	g	cf3_at				= cf3/atl1
	
	g	cf1_cap				= cf1/ppentl1	
	
	* Other variables
	g	mb				= ev/at
	g 	sales_growth	= (sale - salel1) / salel1
	g 	asset_growth	= (at - atl1) / atl1	
	
	g	cl				= ppent/emp
	
	g	roa				= ni/at
	g	roe				= ni/ceq
	
	
	* Repurchase variables
	
	g tstkcl1 = tstkc[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1
	
	g temp = tstkc-tstkcl1
	g temp1 = prstkc-sstk  /* Purchase of common and preferred stock - Sale of common and preferred stock */
	
	g net_rep = temp
	replace net_rep = 0 if temp < 0
	replace net_rep = 0 if temp1 < 0 & net_rep !=.	
	
	g mel1 	= me[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1	
	g net_rep_me = net_rep/mel1
	
	g buy = 1 if net_rep > 0
	replace buy = 0 if net_rep == 0
	replace buy = . if net_rep == .
	
	g net_rep2 = temp1
	replace net_rep2 = 0 if temp1 < 0
	
	g net_rep2_me = net_rep2/mel1
	
	g buy2 = 1 if net_rep2 > 0
	replace buy2 = 0 if net_rep2 == 0
	replace buy2 = . if net_rep2 == .	
	
	g rep = prstkc/atl1
	
	* Merge with KLD data
	
	g str8 cusip2 = cusip
	
	drop cusip
	rename cusip2 cusip
	
	sort cusip year
	duplicates drop cusip year, force
	
	merge 1:1 cusip year using "$data2\KLD_clean.dta"
	keep if _merge == 3
	drop _merge
	
	g sr_index = div_str_num - div_con_num + emp_str_num - emp_con_num + com_str_num - com_con_num + env_str_num - env_con_num
	
	* Merge with governance data
	
	sort gvkey year
	merge 1:1 gvkey year using "$data2\GIM_index.dta"
	drop if _merge == 2
	drop _merge
	
	sort cusip year
	merge 1:1 cusip year using "$data2\eindex.dta"
	drop if _merge == 2
	drop _merge
		
		
**** Winsorize variables
	local vlist l_a me ev l_me cash_at ppe_at td_at capex_at capex_cap rd_sale rd_at cf1_at cf2_at cf3_at cf1_cap mb aqc_at /*
	*/			asset_growth sales_growth net_rep net_rep_me net_rep2 net_rep2_me rep roa roe
	
	foreach v of local vlist {
		winsor `v' 1
		}	

	
	drop mel1
**** Create lags and leads of variables
	
	* One period lag
	sort gvkey fyear
	local vlist l_a me ev l_me cash_at ppe_at td_at capex_at capex_cap rd_sale rd_at cf1_at cf2_at cf3_at cf1_cap mb aqc_at /*
	*/			asset_growth sales_growth net_rep net_rep_me net_rep2 net_rep2_me buy buy2 roa roe env_str_num env_con_num com_str_num com_con_num hum_con_num emp_str_num /*
	*/			emp_con_num div_str_num div_con_num pro_str_num pro_con_num cgov_str_num cgov_con_num hum_str_num rep gim gim_complete eindex sr_index

		
	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if gvkey[_n]==gvkey[_n-1] & fyear[_n]==fyear[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}
	
	* One period lead
	sort gvkey year
	local vlist l_a me ev l_me cash_at ppe_at td_at capex_at capex_cap rd_sale rd_at cf1_at cf2_at cf3_at cf1_cap mb aqc_at /*
	*/			asset_growth sales_growth net_rep net_rep_me net_rep2 net_rep2_me buy buy2 roa roe env_str_num env_con_num com_str_num com_con_num hum_con_num emp_str_num /*
	*/			emp_con_num div_str_num div_con_num pro_str_num pro_con_num cgov_str_num cgov_con_num hum_str_num rep gim gim_complete eindex sr_index
		
	local vlistf1
	foreach v of local vlist {
		g `v'f1 = `v'[_n+1] if gvkey[_n]==gvkey[_n+1] & fyear[_n]==fyear[_n+1]-1
		local vlistf1 `vlistf1' `v'f1
		}
	
	
	* Drop variables
	drop	che at ppent sale dltt dlc prcc_f capx xrd ib dp oibdp ceq prstkc 	/*
				*/ 	ivch aqc fuseo sppe siv ivstch ivaco chech dv dltis dltr dlcch sstk wcapc recch invch apalch txach aoloch fiao /*
				*/	ibc xidoc dpc txdc esubc sppiv fopo fsrco exre
	

	
	g str8 cusip8 = cusip
	drop cusip
	rename cusip8 cusip
**** Save the dataset 
	sort cusip fyear
	
**** There are 21,619 observations in this dataset
	count
		
	save "$data2\Compustat_sample.dta", replace
			
***************************************************************
**** End: Extract and Format of ANNUAL compustat data ****
***************************************************************
