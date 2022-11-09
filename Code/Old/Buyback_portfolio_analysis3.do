version 14.2
set rmsg on
set more off

************************************************************************
*
*	JOB:		Analysis buyback portfolio
*	PROJECT:	SBB
*	INPUT:		Final sample
*	OUTPUT:		Descriptive statistics and analysis
*
*	DESCRIPTION: This job analyzes the buyback sample 
*
*************************************************************************


* Define paths

global data		"D:\Dropbox\work\Buybacks\Data"
global temp		"C:\Users\valta\Desktop\temp"														

global data2	"J:\Sascha\Buyback Anomalies"

clear
clear matrix

set matsize 800
set scrollbufsize 500000		
capture program drop _all

cd $data


	************************************
	**** Begin performance analysis ****
	************************************

*	use "$data2/sample_returns_all_36m_cashretprofitivol.dta"
	
	* Load data for mega stocks
	use "$data2/sample_returns_all_36m_mega_cashretprofitivol_index.dta"

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc rf mktrf smb hml umd cma rmw ps_vwf, by(month)
	
	g month2 = month
	format month2 
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_mega
	rename pf_ret36m_tc ret_tc_mega
	sort month
	
	save "$temp/ret_mega.dta", replace
	
	* Load data for large stocks
	use "$data2/sample_returns_all_36m_large_cashretprofitivol_index.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_large
	rename pf_ret36m_tc ret_tc_large
	sort month	
		
	save "$temp/ret_large.dta", replace	
	
	* Load data for mid stocks
	use "$data2/sample_returns_all_36m_mid_cashretprofitivol_index.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_mid
	rename pf_ret36m_tc ret_tc_mid
	sort month	
		
	save "$temp/ret_mid.dta", replace		
	
	* Load data for small stocks
	use "$data2/sample_returns_all_36m_small_cashretprofitivol_index.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_small
	rename pf_ret36m_tc ret_tc_small
	sort month	
		
	save "$temp/ret_small.dta", replace			
	
	* Load data for overall sample stocks
	use "$data2/sample_returns_all_36m_cashretprofitivol.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_overall
	rename pf_ret36m_tc ret_tc_overall
	sort month	
		
	save "$temp/ret_overall.dta", replace			
	
	use "$data2/sample_returns_all_36m_500bm_cashretprofitivol.dta", clear
	
	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_500m
	rename pf_ret36m_tc ret_tc_500m
	sort month	
		
	save "$temp/ret_500m.dta", replace	
	
	
	use "$temp/ret_overall.dta", clear	

	merge 1:1 month using "$temp/ret_mega.dta"
	drop _merge
	
	merge 1:1 month using "$temp/ret_large.dta"
	drop _merge	

	merge 1:1 month using "$temp/ret_mid.dta"
	drop _merge	
	
	merge 1:1 month using "$temp/ret_small.dta"
	drop _merge		
	
	merge 1:1 month using "$temp/ret_500m.dta"
	drop _merge		
	
	merge 1:1 month using index_returns.dta
	keep if _merge == 3
	drop _merge
	
	keep if month >= 479	
	keep if month >= 587  // 2009 onwards		
	
	egen month_id = group(month)
	
*	g ret_tc_mega_rf = ret_tc_mega - rf
*	g ret_tc_large_rf = ret_tc_large - rf
*	g ret_tc_mid_rf = ret_tc_mid - rf
*	g ret_tc_small_rf = ret_tc_small - rf
	
	tabstat ret_tc_overall ret_tc_mega ret_tc_large ret_tc_mid ret_tc_small ret_tc_500m sp500 russel3000 if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)
	

	replace month_id = month_id-1
	tsset month_id
	
	*** plotting perfomance indexed at 100 ***
/*
	* Overall portfolio
	g ri_overall = .
	replace ri36m = ln(1+pf_ret36m)
	gen temp =.
	forvalues x =2(1)253{
		 sum pf_ret36m if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri36m = 100*(1+(exp(temp)-1))
	replace ri36m = 100 if month_id==1
	drop temp	
*/	

	* Overall stocks
	g ri_overall = .
	replace ri_overall = ln(1+ret_overall)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_overall if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_overall = 100*(1+(exp(temp)-1))
	replace ri_overall = 100 if month_id==1
	drop temp


	* Mega stocks
	g ri_mega = .
	replace ri_mega = ln(1+ret_mega)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_mega if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_mega = 100*(1+(exp(temp)-1))
	replace ri_mega = 100 if month_id==1
	drop temp
	
	* Large stocks
	g ri_large = .
	replace ri_large = ln(1+ret_large)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_large if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_large = 100*(1+(exp(temp)-1))
	replace ri_large = 100 if month_id ==1
	drop temp

	* Mid stocks
	g ri_mid = .
	replace ri_mid = ln(1+ret_mid)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_mid if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_mid = 100*(1+(exp(temp)-1))
	replace ri_mid = 100 if month_id ==1
	drop temp	
	
	* Small stocks
	g ri_small = .
	replace ri_small = ln(1+ret_small)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_small if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_small = 100*(1+(exp(temp)-1))
	replace ri_small = 100 if month_id ==1
	drop temp	
	
	* SP500
	g risp500 = ln(1+sp500)
	gen temp =.
	forvalues x =2(1)253{
		 sum sp500 if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace risp500 = 100*(1+(exp(temp)-1))
	replace risp500 = 100 if month_id==1
	drop temp	
	
	* Russel 3000
	g rirus3000 = ln(1+russel3000)
	gen temp =.
	forvalues x =2(1)253{
		 sum russel3000 if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace rirus3000 = 100*(1+(exp(temp)-1))
	replace rirus3000 = 100 if month_id==1
	drop temp	
	
	
	* Plot portfolio performance using regular scale
	
	* Plot portfolio performance using log scale
	line ri_overall ri_mega ri_large ri_mid ri_small rirus3000 month, yscale(log)
	
	
	
	
	
	
	