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
	use "$data2/sample_36m_mega.dta"

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
	use "$data2/sample_36m_large.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_large
	rename pf_ret36m_tc ret_tc_large
	sort month	
		
	save "$temp/ret_large.dta", replace	
	
	* Load data for mid stocks
	use "$data2/sample_36m_mid.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_mid
	rename pf_ret36m_tc ret_tc_mid
	sort month	
		
	save "$temp/ret_mid.dta", replace		
	
	* Load data for small stocks
	use "$data2/sample_36m_small.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_small
	rename pf_ret36m_tc ret_tc_small
	sort month	
		
	save "$temp/ret_small.dta", replace			
	
	* Load data for overall sample stocks
	use "$data2/sample_36m_full.dta", clear

	* Collapse the data to the portfolio level
	collapse pf_ret36m pf_ret36m_tc, by(month)
	
	keep if month >= 419
	drop if month > 671
	
	rename pf_ret36m ret_full
	rename pf_ret36m_tc ret_tc_full
	sort month	
		
	save "$temp/ret_full.dta", replace			
	
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
	
	use "$temp/ret_overall.dta", clear	

	merge 1:1 month using "$temp/ret_mega.dta"
	drop _merge
	
	merge 1:1 month using "$temp/ret_large.dta"
	drop _merge	

	merge 1:1 month using "$temp/ret_mid.dta"
	drop _merge	
	
	merge 1:1 month using "$temp/ret_small.dta"
	drop _merge		
	
	merge 1:1 month using "$temp/ret_full.dta"
	drop _merge		
	
	merge 1:1 month using index_returns.dta
	keep if _merge == 3
	drop _merge
	
	keep if month >= 479	
	* keep if month >= 587  // 2009 onwards		
	
	egen month_id = group(month)
	
	g ret_tc_mega_rf = ret_tc_mega - rf
	g ret_tc_large_rf = ret_tc_large - rf
	g ret_tc_mid_rf = ret_tc_mid - rf
	g ret_tc_small_rf = ret_tc_small - rf
	g ret_tc_full_rf = ret_tc_full - rf	
	
	g ret_mega_rf = ret_mega - rf
	g ret_large_rf = ret_large - rf
	g ret_mid_rf = ret_mid - rf
	g ret_small_rf = ret_small - rf
	g ret_full_rf = ret_full - rf		
	
	g sp500_rf = sp500 - rf
	g russel3000_rf = russel3000 - rf		
	
	
	**** STATS WITH TRANSACTION COSTS
	tabstat ret_tc_overall ret_tc_full ret_tc_mega ret_tc_large ret_tc_mid ret_tc_small sp500 russel3000 if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)
	
	**** STATS WITHOUT TRANSACTION COSTS
	tabstat ret_overall ret_full ret_mega ret_large ret_mid ret_small sp500 russel3000 if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)	
	
	
	**** VOLATILITY
	egen sd_ret_mega = sd(ret_mega) if month_id > 1
	egen sd_ret_large = sd(ret_large) if month_id > 1
	egen sd_ret_mid = sd(ret_mid) if month_id > 1
	egen sd_ret_small = sd(ret_small) if month_id > 1
	egen sd_ret_full = sd(ret_full) if month_id > 1	

	egen sd_sp500 = sd(sp500) if month_id > 1	
	egen sd_russel3000 = sd(russel3000) if month_id > 1
	
	g sda_ret_mega = sd_ret_mega*sqrt(12)
	g sda_ret_large = sd_ret_large*sqrt(12)
	g sda_ret_mid = sd_ret_mid*sqrt(12)	
	g sda_ret_small = sd_ret_small*sqrt(12)
	g sda_ret_full = sd_ret_full*sqrt(12)	
	
	g sda_sp500 = sd_sp500*sqrt(12)		
	g sda_russel3000 = sd_russel3000*sqrt(12)	
	
	* Display annualized standard deviation of portfolio returns
	*tabstat sda_pf_ret12m sda_pf_ret24m sda_pf_ret36m sda_ret_sp500, stats(mean)  c(s)	
	tabstat sda_ret_full sda_ret_mega sda_ret_large sda_ret_mid sda_ret_small sda_sp500 sda_russel3000 if month_id > 1, stats(mean n)  c(s)
	
	
	**** SHARPE RATIO
	egen mean_ret_full_rf = mean(ret_full_rf) if month_id > 1
	egen mean_ret_mega_rf = mean(ret_mega_rf) if month_id > 1
	egen mean_ret_large_rf = mean(ret_large_rf) if month_id > 1
	egen mean_ret_mid_rf = mean(ret_mid_rf) if month_id > 1
	egen mean_ret_small_rf = mean(ret_small_rf) if month_id > 1	
		
	egen mean_sp500_rf = mean(sp500_rf) if month_id > 1
	egen mean_russel3000_rf = mean(russel3000_rf) if month_id > 1	
	
	egen sd_ret_full_rf = sd(ret_full_rf) if month_id > 1		
	egen sd_ret_mega_rf = sd(ret_mega_rf) if month_id > 1
	egen sd_ret_large_rf = sd(ret_large_rf) if month_id > 1
	egen sd_ret_mid_rf = sd(ret_mid_rf) if month_id > 1
	egen sd_ret_small_rf = sd(ret_small_rf) if month_id > 1

	egen sd_sp500_rf = sd(sp500_rf) if month_id > 1	
	egen sd_russel3000_rf = sd(russel3000_rf) if month_id > 1
	
	g sr_ret_full = mean_ret_full_rf/sd_ret_full_rf*sqrt(12)
	g sr_ret_mega = mean_ret_mega_rf/sd_ret_mega_rf*sqrt(12)
	g sr_ret_large = mean_ret_large_rf/sd_ret_large_rf*sqrt(12)
	g sr_ret_mid = mean_ret_mid_rf/sd_ret_mid_rf*sqrt(12)
	g sr_ret_small = mean_ret_small_rf/sd_ret_small_rf*sqrt(12)
	g sr_sp500 = mean_sp500_rf/sd_sp500_rf*sqrt(12)
	g sr_russel3000 = mean_russel3000_rf/sd_russel3000_rf*sqrt(12)	
	
	tabstat sr_ret_full sr_ret_mega sr_ret_large sr_ret_mid sr_ret_small sr_sp500 sr_russel3000 if month_id > 1, stats(mean n)  c(s)
	
	
	**** INFORMATION RATIO
	g ret_full_r3000 = ret_full - russel3000	
	g ret_mega_r3000 = ret_mega - russel3000	
	g ret_large_r3000 = ret_large - russel3000
	g ret_mid_r3000 = ret_mid - russel3000
	g ret_small_r3000 = ret_small - russel3000

	egen mean_ret_full_r3000 = mean(ret_full_r3000) if month_id > 1	
	egen mean_ret_mega_r3000 = mean(ret_mega_r3000) if month_id > 1
	egen mean_ret_large_r3000 = mean(ret_large_r3000) if month_id > 1	
	egen mean_ret_mid_r3000 = mean(ret_mid_r3000) if month_id > 1
	egen mean_ret_small_r3000 = mean(ret_small_r3000) if month_id > 1
	
	egen sd_ret_full_r3000 = sd(ret_full_r3000) if month_id > 1	
	egen sd_ret_mega_r3000 = sd(ret_mega_r3000) if month_id > 1
	egen sd_ret_large_r3000 = sd(ret_large_r3000) if month_id > 1	
	egen sd_ret_mid_r3000 = sd(ret_mid_r3000) if month_id > 1
	egen sd_ret_small_r3000 = sd(ret_small_r3000) if month_id > 1	
	
	g ir_ret_full = mean_ret_full_r3000/sd_ret_full_r3000*sqrt(12)
	g ir_ret_mega = mean_ret_mega_r3000/sd_ret_mega_r3000*sqrt(12)
	g ir_ret_large = mean_ret_large_r3000/sd_ret_large_r3000*sqrt(12)
	g ir_ret_mid = mean_ret_mid_r3000/sd_ret_mid_r3000*sqrt(12)
	g ir_ret_small = mean_ret_small_r3000/sd_ret_small_r3000*sqrt(12)

	tabstat ir_ret_full ir_ret_mega ir_ret_large ir_ret_mid ir_ret_small if month_id > 1, stats(mean n)  c(s)
	
	
	**** ESTIMATE MARKET BETAS
	g beta_full = .
	g beta_mega = .
	g beta_large = .
	g beta_mid = .
	g beta_small = .	
	
	reg ret_full russel3000 if month_id > 1
	replace beta_full = _b[russel3000]
	
	reg ret_mega russel3000 if month_id > 1
	replace beta_mega = _b[russel3000]
	
	reg ret_large russel3000 if month_id > 1
	replace beta_large = _b[russel3000]
	
	reg ret_mid russel3000 if month_id > 1
	replace beta_mid = _b[russel3000]
	
	reg ret_small russel3000 if month_id > 1	
	replace beta_small = _b[russel3000]	
	
	tabstat beta_full beta_mega beta_large beta_mid beta_small, stats(mean n)  c(s)
	
	
	**** COMPUTE THE MAX DRAWDOWNS PER PORTFOLIO
	** Full
	replace month_id = month_id-1
	tsset month_id
	
	g cum_full = .
	replace cum_full = (1+ret_full) if month_id == 1
	replace cum_full = l.cum_full*(1+ret_full) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_full = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_full) if month_id <= `i'
		replace max_cum_full = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_full = (max_cum_full - cum_full)/max_cum_full
	egen max_drawdown_full = max(drawdown_full)		
	
	** Mega
	g cum_mega = .
	replace cum_mega = (1+ret_mega) if month_id == 1
	replace cum_mega = l.cum_mega*(1+ret_mega) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_mega = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_mega) if month_id <= `i'
		replace max_cum_mega = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_mega = (max_cum_mega - cum_mega)/max_cum_mega
	egen max_drawdown_mega = max(drawdown_mega)	
	
	** Large
	g cum_large = .
	replace cum_large = (1+ret_large) if month_id == 1
	replace cum_large = l.cum_large*(1+ret_large) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_large = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_large) if month_id <= `i'
		replace max_cum_large = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_large = (max_cum_large - cum_large)/max_cum_large
	egen max_drawdown_large = max(drawdown_large)		
	
	** Mid
	g cum_mid = .
	replace cum_mid = (1+ret_mid) if month_id == 1
	replace cum_mid = l.cum_mid*(1+ret_mid) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_mid = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_mid) if month_id <= `i'
		replace max_cum_mid = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_mid = (max_cum_mid - cum_mid)/max_cum_mid
	egen max_drawdown_mid = max(drawdown_mid)	
	
	** Small
	g cum_small = .
	replace cum_small = (1+ret_small) if month_id == 1
	replace cum_small = l.cum_small*(1+ret_small) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_small = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_small) if month_id <= `i'
		replace max_cum_small = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_small = (max_cum_small - cum_small)/max_cum_small
	egen max_drawdown_small = max(drawdown_small)		
	
	** Russel 3000
	g cum_r3000 = .
	replace cum_r3000 = (1+russel3000) if month_id == 1
	replace cum_r3000 = l.cum_r3000*(1+russel3000) if month_id > 1
	
	g max_cum_r3000 = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_r3000) if month_id <= `i'
		replace max_cum_r3000 = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_r3000 = (max_cum_r3000 - cum_r3000)/max_cum_r3000
	egen max_drawdown_r3000 = max(drawdown_r3000)	
	
	* S&P500
	g cum_sp500 = .
	replace cum_sp500 = (1+sp500) if month_id == 1
	replace cum_sp500 = l.cum_sp500*(1+sp500) if month_id > 1
	
	g max_cum_sp500 = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_sp500) if month_id <= `i'
		replace max_cum_sp500 = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_sp500 = (max_cum_sp500 - cum_sp500)/max_cum_sp500
	egen max_drawdown_sp500 = max(drawdown_sp500)		

	tabstat max_drawdown_full max_drawdown_mega max_drawdown_large max_drawdown_mid max_drawdown_small max_drawdown_r3000 max_drawdown_sp500 if month_id > 1, stats(mean n)  c(s)
	
	
	**** ESTIMATE ALPHAS USING MULTIFACTOR MODEL

	
	global controls = "mktrf smb hml umd" 
*	global controls = "mktrf smb hml umd rmw cma ps_vwf" 

	reg ret_full_rf $controls if month_id >=1
	est store full
	
	reg ret_mega_rf $controls if month_id >=1
	est store mega
	
	reg ret_large_rf $controls if month_id >=1
	est store large
	
	reg ret_mid_rf $controls if month_id >=1
	est store mid
	
	reg ret_small_rf $controls if month_id >=1
	est store small
	
	estout full mega large mid small using "$temp/results_alpha_retrf.txt", replace 	///
		   mlabels("full" "mega" "large" "mid" "small") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 
	
	global controls = "mktrf smb hml umd" 
*	global controls = "mktrf smb hml umd rmw cma ps_vwf" 

	reg ret_tc_full_rf $controls if month_id >=1
	est store full
	
	reg ret_tc_mega_rf $controls if month_id >=1
	est store mega
	
	reg ret_tc_large_rf $controls if month_id >=1
	est store large
	
	reg ret_tc_mid_rf $controls if month_id >=1
	est store mid
	
	reg ret_tc_small_rf $controls if month_id >=1
	est store small
	
	estout full mega large mid small using "$temp/results_alpha_retrf_tc.txt", replace 	///
		   mlabels("full" "mega" "large" "mid" "small") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
	
/*	
	**** Estimate alphas for subperiods
	global controls = "mktrf smb hml umd" 	
	*global controls = "mktrf smb hml umd rmw cma ps_vwf" 	
	*1995-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 420
	est store top_1995
	*1996-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 432
	est store top_1996
	*1997-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 444
	est store top_1997
	*1998-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 456
	est store top_1998	
	*1999-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 468
	est store top_1999
	*2000-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 480
	est store top_2000
	
	*2001-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 492
	est store top_2001	
	*2002-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 504
	est store top_2002
	*2003-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 516
	est store top_2003
	*2004-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 528
	est store top_2004	
	*2005-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 540
	est store top_2005
	
	*2006-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 552
	est store top_2006	
	*2007-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 564
	est store top_2007
	*2008-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 576
	est store top_2008
	*2009-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 588
	est store top_2009	
	*2010-2015
	reg top_pf_ret36m_tcrf $controls if month2 >= 600
	est store top_2010	
	
	estout top_1995 top_1996 top_1997 top_1998 top_1999 top_2000 top_2001 top_2002 top_2003 top_2004 top_2005 top_2006 top_2007 top_2008 top_2009 top_2010 using "$temp/results_alpha_year.txt", replace 	///
		   cells(b(star fmt(%9.4f)) se(par) t(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
*/	
	save "$temp/test.dta", replace	
	
	
	
	
	
	

*	replace month_id = month_id-1
*	tsset month_id
	
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
	g ri_full = .
	replace ri_full = ln(1+ret_full)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_full if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_full = 100*(1+(exp(temp)-1))
	replace ri_full = 100 if month_id==1
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
	line ri_full ri_mega ri_large ri_mid ri_small rirus3000 month, yscale(log)
	
	
	
	
	
	
	