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
	
	* Load data 
*	use "$data2/pf_all_crsp.dta"
*	use "$data2/pf_mega_crsp.dta"
*	use "$data2/pf_large_crsp.dta"	
*	use "$data2/pf_mid_crsp.dta"	
*	use "$data2/sample_36m_full_vw_impact.dta"	
	use "$data2/sample_index_vw_impact_100m.dta", clear
*	use "$data2/sample_index_vw_impact_500m.dta", clear	
*	use "$data2/sample_small_vw_impact.dta", clear
*	use "$data2/sample_mid_vw_impact.dta", clear
*	use "$data2/sample_large_vw_impact.dta", clear
*	use "$data2/sample_full_vw_impact.dta", clear
	
	* Collapse the data to the portfolio level
	collapse pf_ret36m n_obs36m pf_ret36m_tc pf_ret36m_tc_tic pf_ret36m_tc_tic_val rf mktrf smb hml umd cma rmw ps_vwf,  by(month)
	
	g month2 = month
	format month2 
	
	keep if month >= 419
	drop if month > 671
	
	sort month
	
	rename pf_ret36m 				ret_vw
	rename pf_ret36m_tc 			ret_tc_vw
	rename pf_ret36m_tc_tic 		ret_tc_tic_vw	
	rename pf_ret36m_tc_tic_val 	pf_val_vw
	
	save "$temp/ret_vw.dta", replace
	
	
*	use "$data2/sample_index_ew_impact_500m.dta", clear	
	use "$data2/sample_index_ew_impact_100m.dta", clear
*	use "$data2/sample_small_ew_impact.dta", clear	
*	use "$data2/sample_mid_ew_impact.dta", clear
*	use "$data2/sample_large_ew_impact.dta", clear	
*	use "$data2/sample_full_ew_impact.dta", clear	
	
	* Collapse the data to the portfolio level
	collapse pf_ret36m n_obs36m pf_ret36m_tc pf_ret36m_tc_tic pf_ret36m_tc_tic_val,  by(month)
	
	g month2 = month
	format month2 
	
	keep if month >= 419
	drop if month > 671
	
	sort month
	
	rename pf_ret36m 				ret_ew
	rename pf_ret36m_tc 			ret_tc_ew
	rename pf_ret36m_tc_tic 		ret_tc_tic_ew	
	rename pf_ret36m_tc_tic_val 	pf_val_ew
		
	save "$temp/ret_ew.dta", replace	
	
	* Load data 
	use "$temp/ret_ew.dta", clear

	merge 1:1 month using "$temp/ret_vw.dta"
	keep if _merge == 3
	drop _merge	
	
	
	merge 1:1 month using index_returns.dta
	keep if _merge == 3
	drop _merge	
	
	
	keep if month >= 479	
	* keep if month >= 587  // 2009 onwards		
	
	egen month_id = group(month)
	
	g ret_vw_rf = ret_vw - rf
	g ret_tc_vw_rf = ret_tc_vw - rf	
	g ret_tc_tic_vw_rf = ret_tc_tic_vw - rf	
	
	g ret_ew_rf = ret_ew - rf
	g ret_tc_ew_rf = ret_tc_ew - rf	
	g ret_tc_tic_ew_rf = ret_tc_tic_ew - rf	
	
	
	g sp500_rf = sp500 - rf
	g russel3000_rf = russel3000 - rf		
	
	
	**** STATS WITHOUT TRANSACTION COSTS
	tabstat ret_ew ret_vw sp500 russel3000 if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)	
	tabstat ret_tc_ew ret_tc_vw if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)	
	tabstat ret_tc_tic_ew ret_tc_tic_vw if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)		
	
	**** VOLATILITY
	egen sd_ret_ew = sd(ret_ew) if month_id > 1
	egen sd_ret_vw = sd(ret_vw) if month_id > 1

	egen sd_sp500 = sd(sp500) if month_id > 1	
	egen sd_russel3000 = sd(russel3000) if month_id > 1
	
	g sda_ret_ew = sd_ret_ew*sqrt(12)
	g sda_ret_vw = sd_ret_vw*sqrt(12)
	
	g sda_sp500 = sd_sp500*sqrt(12)		
	g sda_russel3000 = sd_russel3000*sqrt(12)	
	
	* Display annualized standard deviation of portfolio returns
	*tabstat sda_pf_ret12m sda_pf_ret24m sda_pf_ret36m sda_ret_sp500, stats(mean)  c(s)	
	tabstat sda_ret_ew sda_ret_vw sda_sp500 sda_russel3000 if month_id > 1, stats(mean n)  c(s)
	
	
	**** SHARPE RATIO
	egen mean_ret_ew_rf = mean(ret_ew_rf) if month_id > 1
	egen mean_ret_vw_rf = mean(ret_vw_rf) if month_id > 1
		
	egen mean_sp500_rf = mean(sp500_rf) if month_id > 1
	egen mean_russel3000_rf = mean(russel3000_rf) if month_id > 1	
	
	egen sd_ret_ew_rf = sd(ret_ew_rf) if month_id > 1
	egen sd_ret_vw_rf = sd(ret_vw_rf) if month_id > 1

	egen sd_sp500_rf = sd(sp500_rf) if month_id > 1	
	egen sd_russel3000_rf = sd(russel3000_rf) if month_id > 1
	
	g sr_ret_ew = mean_ret_ew_rf/sd_ret_ew_rf*sqrt(12)
	g sr_ret_vw = mean_ret_vw_rf/sd_ret_vw_rf*sqrt(12)
	g sr_sp500 = mean_sp500_rf/sd_sp500_rf*sqrt(12)
	g sr_russel3000 = mean_russel3000_rf/sd_russel3000_rf*sqrt(12)	
	
	tabstat sr_ret_ew sr_ret_vw sr_sp500 sr_russel3000 if month_id > 1, stats(mean n)  c(s)
	
	
	**** INFORMATION RATIO
	g ret_ew_r3000 = ret_ew - russel3000	
	g ret_vw_r3000 = ret_vw - russel3000
	
	egen mean_ret_ew_r3000 = mean(ret_ew_r3000) if month_id > 1
	egen mean_ret_vw_r3000 = mean(ret_vw_r3000) if month_id > 1	

	egen sd_ret_ew_r3000 = sd(ret_ew_r3000) if month_id > 1
	egen sd_ret_vw_r3000 = sd(ret_vw_r3000) if month_id > 1	
	
	g ir_ret_ew = mean_ret_ew_r3000/sd_ret_ew_r3000*sqrt(12)
	g ir_ret_vw = mean_ret_vw_r3000/sd_ret_vw_r3000*sqrt(12)

	tabstat ir_ret_ew ir_ret_vw if month_id > 1, stats(mean n)  c(s)
	
	
	**** ESTIMATE MARKET BETAS
	g beta_ew = .
	g beta_vw = .
	
	reg ret_ew russel3000 if month_id > 1
	replace beta_ew = _b[russel3000]
	
	reg ret_vw russel3000 if month_id > 1
	replace beta_vw = _b[russel3000]
	
	tabstat beta_ew beta_vw,  stats(mean n)  c(s)
	
	
	**** COMPUTE THE MAX DRAWDOWNS PER PORTFOLIO
	
	
	** ew
	replace month_id = month_id-1
	tsset month_id
	
	g cum_ew = .
	replace cum_ew = (1+ret_ew) if month_id == 1
	replace cum_ew = l.cum_ew*(1+ret_ew) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_ew = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_ew) if month_id <= `i'
		replace max_cum_ew = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_ew = (max_cum_ew - cum_ew)/max_cum_ew
	egen max_drawdown_ew = max(drawdown_ew)	
	
	** vw
	g cum_vw = .
	replace cum_vw = (1+ret_vw) if month_id == 1
	replace cum_vw = l.cum_vw*(1+ret_vw) if month_id > 1
	
	* Compute the maximum cumulative return
	g max_cum_vw = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_vw) if month_id <= `i'
		replace max_cum_vw = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_vw = (max_cum_vw - cum_vw)/max_cum_vw
	egen max_drawdown_vw = max(drawdown_vw)		
	
	
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

	tabstat max_drawdown_ew max_drawdown_vw max_drawdown_r3000 max_drawdown_sp500 if month_id > 1, stats(mean n)  c(s)
	
	
	**** ESTIMATE ALPHAS USING MULTIFACTOR MODEL

	
	global controls = "mktrf smb hml umd" 
*	global controls = "mktrf smb hml umd rmw cma ps_vwf" 

	
	reg ret_ew_rf $controls if month_id >=1
	est store ew
	
	reg ret_tc_ew_rf $controls if month_id >=1
	est store tc_ew
	
	reg ret_tc_tic_ew_rf $controls if month_id >=1
	est store tc_tic_ew	
	
	reg ret_vw_rf $controls if month_id >=1
	est store vw
	
	reg ret_tc_vw_rf $controls if month_id >=1
	est store tc_vw
	
	reg ret_tc_tic_vw_rf $controls if month_id >=1
	est store tc_tic_vw	
	
	
	
	estout ew tc_ew tc_tic_ew vw tc_vw tc_tic_vw using "$temp/results_alpha_retrf.txt", replace 	///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 
	
/*	
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
*/	
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
	
	
	use "$temp/test.dta", clear		
	
	
	

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
*/

	* Equal-weighted stocks
	g ri_ew = .
	replace ri_ew = ln(1+ret_ew)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_ew if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_ew = 100*(1+(exp(temp)-1))
	replace ri_ew = 100 if month_id==1
	drop temp
	
	* Equal-weighted stocks after transaction costs
	g ri_ew_tc_tic = .
	replace ri_ew_tc_tic = ln(1+ret_tc_tic_ew)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_tc_tic_ew if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_ew_tc_tic = 100*(1+(exp(temp)-1))
	replace ri_ew_tc_tic = 100 if month_id==1
	drop temp	
	
	* Value-weighted stocks
	g ri_vw = .
	replace ri_vw = ln(1+ret_vw)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_vw if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_vw = 100*(1+(exp(temp)-1))
	replace ri_vw = 100 if month_id ==1
	drop temp
	
	* Value-weighted stocks after transaction costs
	g ri_vw_tc_tic = .
	replace ri_vw_tc_tic = ln(1+ret_tc_tic_vw)
	gen temp =.
	forvalues x =2(1)253{
		 sum ret_tc_tic_vw if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri_vw_tc_tic = 100*(1+(exp(temp)-1))
	replace ri_vw_tc_tic = 100 if month_id ==1
	drop temp	
	
/*
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
*/	
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
	line ri_ew rirus3000 month, yscale(log)
	line ri_ew ri_vw rirus3000 month, yscale(log)
	
	
	line ri_ew ri_ew_tc_tic rirus3000 month, yscale(log)
	
	
	line ri_vw ri_vw_tc_tic rirus3000 month, yscale(log)
	