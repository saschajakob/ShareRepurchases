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
global temp		"C:\Users\\`=c(username)'\Desktop\temp"													

global data2	"A:\Sascha\Buyback Anomalies"

clear
clear matrix

set matsize 800
set scrollbufsize 500000		
capture program drop _all


	*************************************************************
	**** Beginn: Load data set and computation of indicators ****
	*************************************************************		

	use "$data2/sample_buyback_portfolio.dta", replace		
	
	*** Merge with entropy file ***
	merge 1:1 gvkey permno month dealnumber using "$data2/buyback_entropy.dta"
	drop _merge

	sort permno month
	
	
	browse permno year buyback ayear qdate cash_atl1 dvcl1 top10instown_percl1 ivol totalvalueauthorizedmil mkvalt entropy_H entropy_H_binomial p_SBB entropy_I

	**** Create/define sorting variables
	
	* 1: Cash to asset ratio. Take one year lag: cash_atl1 (this is the last cash-to-asset ratio observable based on annual data
	
	* 2: Idiosyncratic volatility. Take the one month lag: ivoll1
	
	sort permno month
	local vlist ivol mkvalt
		
	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if permno[_n]==permno[_n-1] & month[_n]==month[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}	
	
	* 3: Top 10% of institutional investors. Take the one quarter lag: top10instown_percl1
	
	* 4: Total payout. Sum of last year's dividend yield and the buyback yield
	
	g buyback_yield = totalvalueauthorizedmil/(mkvaltl1/1000)
	g payout_yield = dvc_mkvaltl1 + buyback_yield
	
	sort permno month
	bysort permno: carryforward dealnumber, replace
	
	sort dealnumber month
	by dealnumber: carryforward payout_yield, gen(payout_yield_all)
	
	tabstat cash_atl1 ivol top10instown_percl1 payout_yield, stats(n mean sd p50 min p25 p75 max) c(s)
	
*	drop if cash_atl1 == .
*	drop if ivoll1 == .
*	drop if top10instown_percl1 == .
*	drop if payout_yield == .
	
	distinct dealnumber
	* 12'377 buyback announcements
	
	sort permno month
	g mth = month(date)
	drop if year <1992
	sort month permno
	egen time =group(month)
	sort permno month
	
	save "$data2/sample_buyback_portfolio_entropy.dta", replace
	save "C:\Users\\`=c(username)'\Dropbox\Buybacks\Data\sample_buyback_portfolio_entropy.dta", replace   // Main data set, additionally stored on dropbox$
	export delimited "C:\Users\sajakob\Dropbox\Buybacks\Codes\Portfolio Backtesting\sample_buyback_portfolio_entropy.csv", replace
	
