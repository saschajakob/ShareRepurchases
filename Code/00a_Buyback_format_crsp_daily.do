*version 14.2
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

global data		"D:\Dropbox\work\Buybacks\Data"
global temp		"C:\Users\valta\Desktop\temp"														

global data2	"W:\Sascha\Buyback Anomalies"

clear
clear matrix

set matsize 800
set scrollbufsize 500000		
capture program drop _all

	cd "$data"
	
	********************************************
	**** Beginn: Format and clean CRSP data	****
	********************************************
	
	use	permno date ncusip bidlo askhi prc vol ret shrout cfacpr cfacshr using "V:\CRSP\Securities\Daily\crsp_daily_1985_2018.dta", clear  		

	
	* We only keep share codes 10 and 11
	*keep if shrcd == 10 | shrcd == 11	
	
	sort permno
	merge m:1 permno using permno_with_buyback.dta
	keep if _merge == 3
	drop _merge
		
	duplicates drop permno date, force
	
	replace prc = abs(prc)
	
	* Create variables for volume in shares: VOL is the total number of shares of a stock sold on day
	g vol_shares = vol
	* Create variables for volume in dollars	
	g vol_dollar = vol*prc
	
	* Compute average volume over past three months
	egen firm_id = group(permno)
	egen day_id =group(date)
	sort firm_id day_id
	
	rangestat (mean) vol_shares, interval(day_id -60 -1) by(firm_id)
	rangestat (mean) vol_dollar, interval(day_id -60 -1) by(firm_id)
	rangestat (count) vol_shares, interval(day_id -60 -1) by(firm_id)
	
	replace vol_shares_mean = . if vol_shares_count < 30
	replace vol_dollar_mean = . if vol_shares_count < 30
	
	* Compute the past volatility
	rangestat (sd) ret, interval(day_id -60 -1) by(firm_id)
	rangestat (count) ret, interval(day_id -60 -1) by(firm_id)	
	
	replace ret_sd = . if ret_count < 30
	
	g dayofmonth = day(date)
	gen month = mofd(date)
	format %tm month	
	
	egen max_day = max(dayofmonth), by(month)
	
	* Keep only the last observation per month
	
	keep if dayofmonth == max_day
	
	drop if vol_shares == .
	drop if ret_sd == .
	
	keep permno prc ret shrout vol_shares_mean vol_dollar_mean ret_sd month
	
	duplicates drop permno month, force
	sort permno month
	
	
	
	save "$data2/crsp_vol_sd.dta", replace

