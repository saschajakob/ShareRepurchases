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



	

	************************************
	**** Begin performance analysis ****
	************************************

*	use "$data2/sample_returns_all.dta", clear
*	use "$data2/sample_returns_all_36m.dta", clear	
*	use "$data2/sample_returns_all_36m_100m.dta"
*	use "$data2/sample_returns_all_36m_100m_ret.dta"
*	use "$data2/sample_returns_all_36m_100bm.dta"
	use "$data2/sample_returns_all_36m_100bm_cashretprofitivol.dta"
*	use "$data2/sample_returns_all_36m_100m_cashretprofitivol.dta"
*	use "$data2/sample_returns_all_36m_100m_cashretprofitivol.dta"
*	use "$data2/sample_returns_all_36m_100m_cashretprofitpayout.dta"	
*	use "$data2/sample_returns_all_36m_250bm.dta"
*	use "$data2/sample_returns_all_36m_250m_ret.dta"
*	use "$data2/sample_returns_all_36m_250bm_ret.dta"
*	use "$data2/sample_returns_all_36m_500bm.dta"
*	use "$data2/sample_returns_all_36m_50m.dta"
*	use "$data2/sample_returns_all_36m_75m.dta"
*	use "$data2/sample_returns_all_36m_400bm_cashretprofitivol.dta"
*	use "$data2/sample_returns_all_36m_500bm_cashretprofitivol.dta"
*	use "$data2/sample_returns_all_36m_500m_cashretprofitivol.dta"
*	use "$data2/sample_returns_all_36m_500m_cashretprofitivol_index15.dta"	
*	use "$data2/sample_returns_all_36m_500bm_cashretprofitivol_index15.dta"	
*	use "$data2/sample_returns_all_36m_500m_cashretprofitivol_index14.dta"		
*	use "$data2/sample_returns_all_36m_500m_cashretprofitivol_index17.dta"	
*	use "$data2/sample_returns_all_36m_500bm_cashretprofitivol_index17.dta"	
*	use "$data2/sample_returns_all_36m_500bm_cashretprofitivol_index14.dta"	
*	use "$data2/sample_returns_all_36m_mega_cashretprofitivol_index.dta"
*	use "$data2/sample_returns_all_36m_large_cashretprofitivol_index.dta"
*	use "$data2/sample_returns_all_36m_mid_cashretprofitivol_index.dta"
*	use "$data2/sample_returns_all_36m_small_cashretprofitivol_index.dta"
*	use "$data2/sample_returns_all_36m_cashretprofitivol.dta"	
	
	* There are 11'327 buybacks in the data set

	distinct dealnumber
	
/*	
	* Collapse the data to the portfolio level
	collapse qdate pf_ret12m top_pf_ret12m bottom_pf_ret12m n_obs12m top_n_obs12m bottom_n_obs12m pf_ret24m top_pf_ret24m bottom_pf_ret24m /*
	*/	n_obs24m top_n_obs24m bottom_n_obs24m pf_ret36m top_pf_ret36m bottom_pf_ret36m n_obs36m top_n_obs36m bottom_n_obs36m /*
	*/ pf_ret12m_tc top_pf_ret12m_tc bottom_pf_ret12m_tc pf_ret24m_tc top_pf_ret24m_tc bottom_pf_ret24m_tc /*
	*/ pf_ret36m_tc top_pf_ret36m_tc bottom_pf_ret36m_tc /*
	*/ rf mktrf smb hml umd cma rmw ps_vwf sprtrn (sum) buyback, by(month)
*/

	* Collapse the data to the portfolio level
	collapse qdate year pf_ret36m top_pf_ret36m bottom_pf_ret36m n_obs36m top_n_obs36m bottom_n_obs36m /*
	*/ top_debt_at36m bottom_debt_at36m debt_at36m top_netdebt_at36m bottom_netdebt_at36m netdebt_at36m /*
	*/ top_debt_mv36m bottom_debt_mv36m debt_mv36m top_netdebt_cap36m bottom_netdebt_cap36m netdebt_cap36m /*
	*/ top_mkvalt36m top_mkvalt_med36m bottom_mkvalt36m bottom_mkvalt_med36m mkvalt36m mkvalt_med36m /*
	*/ pf_ret36m_tc top_pf_ret36m_tc bottom_pf_ret36m_tc /*
	*/ rf mktrf smb hml umd cma rmw ps_vwf sprtrn (sum) buyback, by(month)
	
	g month2 = month
	format month2 
	
	* Drop observations with missing returns
	* drop if pf_ret12m == .
	
	* Drop observations after December 2015
	
	keep if month >= 419
	drop if month > 671
	
	cd "$data"
	
	sort month 
	merge 1:1 month using index_returns.dta
	keep if _merge == 3
	drop _merge
	
	
	**** Figure showing the number of share buybacks by month
*	two bar buyback month, color(sand) tlabel(1995m1(60)2015m1, labsize(small)) ylabel(#6, angle(horizontal) labsize(small)) xtitle("Year-month", size(small)) yline(0) ytitle("Number of buyback announcements", size(small)) /// 
*		legend(off)  title("Number of share buyback announcements over time", size(small)) scheme(s1mono)		
	
	**** Additional filters
	* Keep observations from January 2000 onwards
		keep if month >= 479
	*	keep if month >= 587  //2009
	*   keep if month >= 515  // 2003 onwards
	*   keep if month >= 539  // 2005 onwards
	*   keep if month >= 527  // 2004 onwards
	*	keep if month >= 587  // 2009 onwards	
	
	egen month_id = group(month)
	
	**** Generate additional return variables
	* Generate excess returns over risk-free rate
	*g pf_ret12mrf = pf_ret12m - rf
	*g pf_ret24mrf = pf_ret24m - rf
	g pf_ret36mrf = pf_ret36m - rf	
	
	*g top_pf_ret12mrf = top_pf_ret12m - rf
	*g top_pf_ret24mrf = top_pf_ret24m - rf	
	g top_pf_ret36mrf = top_pf_ret36m - rf	
	
	*g bottom_pf_ret12mrf = bottom_pf_ret12m - rf	
	*g bottom_pf_ret24mrf = bottom_pf_ret24m - rf
	g bottom_pf_ret36mrf = bottom_pf_ret36m - rf	

	g pf_ret36m_tcrf = pf_ret36m_tc - rf		
	g top_pf_ret36m_tcrf = top_pf_ret36m_tc - rf		
	g bottom_pf_ret36m_tcrf = bottom_pf_ret36m_tc - rf	
	
	g ret_sp500rf = sprtrn - rf
	
	* Generate excess returns over S&P500	
	*g pf_ret12msp = pf_ret12m - sprtrn
	*g pf_ret24msp = pf_ret24m - sprtrn
	g pf_ret36msp = pf_ret36m - sprtrn	
	
	*g top_pf_ret12msp = top_pf_ret12m - sprtrn
	*g top_pf_ret24msp = top_pf_ret24m - sprtrn	
	g top_pf_ret36msp = top_pf_ret36m - sprtrn	
	
	*g bottom_pf_ret12msp = bottom_pf_ret12m - sprtrn	
	*g bottom_pf_ret24msp = bottom_pf_ret24m - sprtrn
	g bottom_pf_ret36msp = bottom_pf_ret36m - sprtrn		
	
	
	**** Display descriptive statistisc
	* Show distributinal characteristics of monthly portfolio returns
	*tabstat top_pf_ret12m top_pf_ret24m top_pf_ret36m sprtrn, stats(n mean sd min med max iqr sk k) c(s)
	*tabstat pf_ret12m pf_ret24m pf_ret36m sprtrn, stats(n mean sd min med max iqr sk k) c(s)

	tabstat top_pf_ret36m top_pf_ret36m_tc sprtrn if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)
	tabstat pf_ret36m pf_ret36m_tc sprtrn if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)
	tabstat bottom_pf_ret36m bottom_pf_ret36m_tc sprtrn if month_id > 1, stats(n mean sd min med max iqr sk k) c(s)
	
	* Compute annualized volatility
	*egen sd_top_pf_ret12m = sd(top_pf_ret12m)
	*egen sd_top_pf_ret24m = sd(top_pf_ret24m)
	egen sd_top_pf_ret36m = sd(top_pf_ret36m) if month_id > 1
	egen sd_ret_sp500 = sd(ret_sp500) if month_id > 1
	
	*g sda_pf_ret12m = sd_top_pf_ret12m*sqrt(12)
	*g sda_pf_ret24m = sd_top_pf_ret24m*sqrt(12)
	g sda_pf_ret36m = sd_top_pf_ret36m*sqrt(12)
	g sda_ret_sp500 = sd_ret_sp500*sqrt(12)		
	
	* Display annualized standard deviation of portfolio returns
	*tabstat sda_pf_ret12m sda_pf_ret24m sda_pf_ret36m sda_ret_sp500, stats(mean)  c(s)	
	tabstat sda_pf_ret36m sda_ret_sp500 if month_id > 1, stats(mean)  c(s)
	
	**** SHARPE RATIO
	* Compute Sharpe ratios
	*egen mean_top_pf_ret12mrf = mean(top_pf_ret12mrf)
	*egen mean_top_pf_ret24mrf = mean(top_pf_ret24mrf)
	egen mean_top_pf_ret36mrf = mean(top_pf_ret36mrf) if month_id > 1
	egen mean_ret_sp500rf = mean(ret_sp500rf) if month_id > 1
	
	*egen sd_top_pf_ret12mrf = sd(top_pf_ret12mrf)
	*egen sd_top_pf_ret24mrf = sd(top_pf_ret24mrf)
	egen sd_top_pf_ret36mrf = sd(top_pf_ret36mrf) if month_id > 1
	egen sd_ret_sp500rf = sd(ret_sp500rf) if month_id > 1	
	
	*g sr_top_pf_ret12mrf = mean_top_pf_ret12mrf/sd_top_pf_ret12mrf
	*g sr_top_pf_ret24mrf = mean_top_pf_ret24mrf/sd_top_pf_ret24mrf
	g sr_top_pf_ret36mrf = mean_top_pf_ret36mrf/sd_top_pf_ret36mrf	
	g sr_ret_sp500rf = mean_ret_sp500rf/sd_ret_sp500rf	
	
	*replace sr_top_pf_ret12mrf = sr_top_pf_ret12mrf*sqrt(12)
	*replace sr_top_pf_ret24mrf = sr_top_pf_ret24mrf*sqrt(12)
	replace sr_top_pf_ret36mrf = sr_top_pf_ret36mrf*sqrt(12)	
	replace sr_ret_sp500rf = sr_ret_sp500rf*sqrt(12)	
		
	* Display Sharpe ratios and annualized vol
	*tabstat sr_top_pf_ret12mrf sr_top_pf_ret24mrf sr_top_pf_ret36mrf sr_ret_sp500rf, stats(mean)  c(s)
	tabstat sr_top_pf_ret36mrf sr_ret_sp500rf if month_id > 1, stats(mean)  c(s)
	
	**** VaR
	*egen mean_top_pf_ret12m = mean(top_pf_ret12m)
	*egen mean_top_pf_ret24m = mean(top_pf_ret24m)
	*egen mean_top_pf_ret36m = mean(top_pf_ret36m)	
	*egen mean_ret_sp500 = mean(ret_sp500)
	
	*g VaR_top_pf_ret12m = -1.645 * sd_top_pf_ret12m + mean_top_pf_ret12m
	*g VaR_top_pf_ret24m = -1.645 * sd_top_pf_ret24m + mean_top_pf_ret24m
	*g VaR_top_pf_ret36m = -1.645 * sd_top_pf_ret36m + mean_top_pf_ret36m
	*g VaR_ret_sp500 = -1.645 * sd_ret_sp500 + mean_ret_sp500
	
	* Display monthly Value-at-Risk
	*tabstat VaR_top_pf_ret12m VaR_top_pf_ret24m VaR_top_pf_ret36m VaR_ret_sp500, stats(mean)  c(s)
	*tabstat VaR_top_pf_ret36m VaR_ret_sp500 if month_id > 1, stats(mean)  c(s)
	
	**** INFORMATION RATIO
	* Compute Information ratio using the S&P500 as benchmark
	*egen mean_top_pf_ret12msp = mean(top_pf_ret12msp)
	*egen mean_top_pf_ret24msp = mean(top_pf_ret24msp)
	egen mean_top_pf_ret36msp = mean(top_pf_ret36msp) if month_id > 1	
	
	*egen sd_top_pf_ret12msp = sd(top_pf_ret12msp)
	*egen sd_top_pf_ret24msp = sd(top_pf_ret24msp)
	egen sd_top_pf_ret36msp = sd(top_pf_ret36msp) if month_id > 1		
	
	*g ir_top_pf_ret12msp = mean_top_pf_ret12msp/sd_top_pf_ret12msp
	*g ir_top_pf_ret24msp = mean_top_pf_ret24msp/sd_top_pf_ret24msp
	g ir_top_pf_ret36msp = mean_top_pf_ret36msp/sd_top_pf_ret36msp	
	
	*replace ir_top_pf_ret12msp = ir_top_pf_ret12msp*sqrt(12)
	*replace ir_top_pf_ret24msp = ir_top_pf_ret24msp*sqrt(12)
	replace ir_top_pf_ret36msp = ir_top_pf_ret36msp*sqrt(12)		
	
	* tabstat ir_top_pf_ret12msp ir_top_pf_ret24msp ir_top_pf_ret36msp, stats(mean)  c(s)	
	tabstat ir_top_pf_ret36msp if month_id > 1, stats(mean)  c(s)
	
	
	**** ESTIMATE MARKET BETAS
	* Compute beta using S&P500
	
	*g top_beta12m = .
	*g top_beta24m = .
	g top_beta36m = .
	
	*reg top_pf_ret12m sprtrn
	*replace top_beta12m = _b[sprtrn]
	
	*reg top_pf_ret24m sprtrn
	*replace top_beta24m = _b[sprtrn]
	
	reg top_pf_ret36m sprtrn if month_id > 1	
	replace top_beta36m = _b[sprtrn]
	
	* Display betas
	*tabstat top_beta12m top_beta24m top_beta36m, stats(mean)  c(s)	
	tabstat top_beta36m, stats(mean)  c(s)
	
	
	**** COMPUTE THE MAX DRAWDOWNS PER PORTFOLIO
	* Compute the max drawdown
	* 1. Compute cumulative returns
	* Top 12m portfolio
	* egen month_id = group(month)
	*tsset month_id
/*	g cum_top_pf_ret12m = .
	replace cum_top_pf_ret12m = (1+top_pf_ret12m) if month_id == 1
	replace cum_top_pf_ret12m = l.cum_top_pf_ret12m*(1+top_pf_ret12m) if month_id > 1
	
	* 2. Compute the maximum cumulative return
	g max_cum_top_pf_ret12m = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_top_pf_ret12m) if month_id <= `i'
		replace max_cum_top_pf_ret12m = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_top_pf_ret12m = (max_cum_top_pf_ret12m - cum_top_pf_ret12m)/max_cum_top_pf_ret12m
	egen max_drawdown_top_pf_ret12m = max(drawdown_top_pf_ret12m)	
	
	* Top 24m portfolio
	tsset month_id
	g cum_top_pf_ret24m = .
	replace cum_top_pf_ret24m = (1+top_pf_ret24m) if month_id == 1
	replace cum_top_pf_ret24m = l.cum_top_pf_ret24m*(1+top_pf_ret24m) if month_id > 1
	
	* 2. Compute the maximum cumulative return
	g max_cum_top_pf_ret24m = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_top_pf_ret24m) if month_id <= `i'
		replace max_cum_top_pf_ret24m = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_top_pf_ret24m = (max_cum_top_pf_ret24m - cum_top_pf_ret24m)/max_cum_top_pf_ret24m
	egen max_drawdown_top_pf_ret24m = max(drawdown_top_pf_ret24m)		
*/	
	* Top 36m portfolio
	replace month_id = month_id-1
	tsset month_id
	
	g cum_top_pf_ret36m = .
	replace cum_top_pf_ret36m = (1+top_pf_ret36m) if month_id == 1
	replace cum_top_pf_ret36m = l.cum_top_pf_ret36m*(1+top_pf_ret36m) if month_id > 1
	
	* 2. Compute the maximum cumulative return
	g max_cum_top_pf_ret36m = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_top_pf_ret36m) if month_id <= `i'
		replace max_cum_top_pf_ret36m = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_top_pf_ret36m = (max_cum_top_pf_ret36m - cum_top_pf_ret36m)/max_cum_top_pf_ret36m
	egen max_drawdown_top_pf_ret36m = max(drawdown_top_pf_ret36m)		
	
	
	* S&P500
	g cum_sp500 = .
	replace cum_sp500 = (1+sprtrn) if month_id == 1
	replace cum_sp500 = l.cum_sp500*(1+sprtrn) if month_id > 1
	
	g max_cum_sp500 = .
	su month_id, det

	forval i = 1(1)`r(max)' {
		
		egen temp = max(cum_sp500) if month_id <= `i'
		replace max_cum_sp500 = temp if month_id == `i' 
		drop temp
	}
	
	g drawdown_sp500 = (max_cum_sp500 - cum_sp500)/max_cum_sp500
	egen max_drawdown_sp500 = max(drawdown_sp500)		

	* Display the max drawdowns by portfolio
	*tabstat max_drawdown_top_pf_ret12m max_drawdown_top_pf_ret24m max_drawdown_top_pf_ret36m max_drawdown_sp500, stats(mean)  c(s)		
	tabstat max_drawdown_top_pf_ret36m max_drawdown_sp500 if month_id > 1, stats(mean)  c(s)
	
	**** ESTIMATE ALPHAS USING MULTIFACTOR MODEL

/*
	global controls = "mktrf smb hml umd" 
*	global controls = "mktrf smb hml umd rmw cma ps_vwf" 
	
	*reg pf_ret12m $controls 
	*est store full_12m
		
	*reg pf_ret24m $controls
	*est store full_24m
		
	reg pf_ret36m $controls
	est store full_36m
		
	*reg top_pf_ret12m $controls 
	*est store top_12m
		
	*reg top_pf_ret24m $controls
	*est store top_24m
		
	reg top_pf_ret36m $controls
	est store top_36m
		
	*reg bottom_pf_ret12m $controls
	*est store bottom_12m
		
	*reg bottom_pf_ret24m $controls
	*est store bottom_24m
		
	reg bottom_pf_ret36m $controls
	est store bottom_36m
	
/*	estout full_12m full_24m full_36m top_12m top_24m top_36m bottom_12m bottom_24m bottom_36m using "$temp/results_alpha_ret.txt", replace 	///
		   mlabels("full12m" "full24m" "full36m" "top12m" "top24m" "top36m" "bot12m" "bot24m" "bot36m") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 
*/	
	estout full_36m top_36m bottom_36m using "$temp/results_alpha_ret.txt", replace 	///
		   mlabels("full36m" "top36m" "bot36m") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
*/	
	global controls = "mktrf smb hml umd" 
*	global controls = "mktrf smb hml umd rmw cma ps_vwf" 
	
	*reg pf_ret12mrf $controls 
	*est store full_12m
		
	*reg pf_ret24mrf $controls
	*est store full_24m
		
	reg pf_ret36m_tcrf $controls if month_id >=1
	est store full_36m
		
	*reg top_pf_ret12mrf $controls 
	*est store top_12m
		
	*reg top_pf_ret24mrf $controls
	*est store top_24m
		
	*reg top_pf_ret36mrf $controls if month_id >=1
	*est store top_36m

	reg top_pf_ret36m_tcrf $controls if month_id >=1
	est store top_36m_tc	
	
	*reg bottom_pf_ret12mrf $controls
	*est store bottom_12m
		
	*reg bottom_pf_ret24mrf $controls
	*est store bottom_24m
		
	reg bottom_pf_ret36m_tcrf $controls if month_id >=1
	est store bottom_36m
	
/*	estout full_12m full_24m full_36m top_12m top_24m top_36m bottom_12m bottom_24m bottom_36m using "$temp/results_alpha_retrf.txt", replace 	///
		   mlabels("full12m" "full24m" "full36m" "top12m" "top24m" "top36m" "bot12m" "bot24m" "bot36m") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
*/	
	estout full_36m top_36m_tc bottom_36m using "$temp/results_alpha_retrf.txt", replace 	///
		   mlabels("full36m" "top36m_tc" "bot36m") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 
	
	
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
	
	save "$temp/test.dta", replace
	use "$temp/test.dta", clear
	
	*** plotting perfomance indexed at 100 ***

	* Overall portfolio
	g ri36m = .
	replace ri36m = ln(1+pf_ret36m)
	gen temp =.
	forvalues x =2(1)253{
		 sum pf_ret36m if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace ri36m = 100*(1+(exp(temp)-1))
	replace ri36m = 100 if month_id==1
	drop temp
	
	* Top portfolio
	g top_ri36m = .
	replace top_ri36m = ln(1+top_pf_ret36m)
	gen temp =.
	forvalues x =2(1)253{
		 sum top_pf_ret36m if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace top_ri36m = 100*(1+(exp(temp)-1))
	replace top_ri36m = 100 if month_id ==1
	drop temp

	* Bottom portfolio
	g bottom_ri36m = .
	replace bottom_ri36m = ln(1+bottom_pf_ret36m)
	gen temp =.
	forvalues x =2(1)253{
		 sum bottom_pf_ret36m if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*(`x'-1) if month_id == `x'
	}
	replace bottom_ri36m = 100*(1+(exp(temp)-1))
	replace bottom_ri36m = 100 if month_id ==1
	drop temp	
	
	* SP500
	g risp500 = ln(1+sprtrn)
	gen temp =.
	forvalues x =2(1)253{
		 sum sprtrn if month_id > 0 & month_id <= `x'
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
	* twoway (line /*ri36m*/ top_ri36m risp500 month, sort)
	
	* Plot portfolio performance using log scale
	line top_ri36m bottom_ri36m ri36m risp500 rirus3000 month, yscale(log)
	
	
	* Number of observations over time
	twoway (line top_n_obs36m month, sort)
	
	* Financial leverage over time
	twoway (line top_debt_at36m bottom_debt_at36m month, sort)
	twoway (line top_netdebt_at36m bottom_netdebt_at36m month, sort)
	twoway (line top_debt_mv36m bottom_debt_mv36m month, sort)
	twoway (line top_netdebt_cap36m bottom_netdebt_cap36m month, sort)
	
	* Market cap over time
	twoway (line top_mkvalt36m month, sort)
	twoway (line top_mkvalt_med36m month, sort)
	
	collapse (mean) mkvalt36m top_mkvalt36m mkvalt_med36m top_mkvalt_med36m  , by(year)
	
	
	
	
	