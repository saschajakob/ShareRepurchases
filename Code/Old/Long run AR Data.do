		
	* Prior 6-month raw returns
	
	use "V:\CRSP\Securities\Monthly\1926-2016.dta", clear
	keep date cusip ret
	duplicates drop cusip date, force
	egen firm_id = group(cusip)
	gen month = mofd(date)
	format %tm month
	tsset firm_id month, monthly
	
	
	
	gen lag_1 = l.ret
	gen lag_2 = l2.ret
	gen lag_3 = l3.ret
	gen lag_4 = l4.ret
	gen lag_5 = l5.ret
	gen lag_6 = l6.ret
	
	save "$temp\returns.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\repurchase_cars.dta", clear
	gen month = mofd(deal_date)
	format %tm month
	
	merge 1:1 cusip month using "$temp\returns.dta"
	keep if _merge == 3
	drop _merge date
	egen six_m_sum_ret= rowtotal(lag_1 lag_2 lag_3 lag_4 lag_5 lag_6)															// sum of past six monthly returns prior to announcement date
	gen six_m_cum_ret = (1+lag_1)*(1+lag_2)*(1+lag_3)*(1+lag_4)*(1+lag_5)*(1+lag_6)-1											// six month cumulative return prior to announcement date
	
	tabstat six_m_sum_ret, by(year)																								
	tabstat six_m_cum_ret, by(year)																								

	preserve
	collapse (mean) car_11_m car_11_ff car_33_m car_33_ff car_55_m car_55_ff pctsharesauthorizedforrepurchase six_m_sum_ret six_m_cum_ret , by(year)	
	save "raw_returns.dta", replace
	restore 
	
	preserve
	keep dealnumber cusip deal_date six_m_sum_ret six_m_cum_ret 
	save "six_m_ret.dta", replace
	restore
	
	
	
	use "V:\Fama French & Liqudity Factors\Factory Mothly\01Jul1926 - 31Jul2017", clear
	gen year = yofd(dateff)
	drop if year <= 1975
	drop year
	gen month = mofd(dateff)
	format %tm month
	save "$temp\ff4f.dta", replace
	
	import delimited "J:\Sascha\Buyback Anomalies\FF5F.csv", clear
	gen date2 = date(date,"DMY")
	format date2 %td
	gen month = mofd(date2)
	drop date2
	format month %tm
	drop date rf hml smb mktrf
	replace rmw = rmw/100
	replace cma = cma/100
	merge 1:1 month using "$temp\ff4f.dta"
	keep if _merge ==3
	drop _merge
	
	gen ln_mktrf = ln(1+mktrf)
	save "$temp\ff6f.dta", replace
	
	tsset month
	rolling ln_mrp = r(mean), window(60) clear: sum ln_mktrf						// MRP
	drop start
	gen mrp = exp(ln_mrp)-1
	drop ln_mrp
	rename end month
	merge 1:1 month using "$temp\ff6f.dta"
	drop ln_mktrf _merge
	save "$temp\ff6f.dta", replace
	
	
	use "V:\CRSP\Securities\Monthly\1926-2016.dta", clear
	keep date cusip ret prc
	replace prc = abs(prc)
	gen year = yofd(date)
	drop if year <= 1980
	drop year
	gen month = mofd(date)
	format %tm month
	
	merge m:1 month using "$temp\ff6f.dta"
	keep if _merge==3
	drop _merge
	gen retrf = ret - rf
	duplicates drop cusip month, force
	sort cusip month
	save "$temp\monthly_data.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\repurchase_ar.dta", clear
	keep cusip deal_date dealnumber siccode
	gen month = mofd(deal_date)
	format %tm month
	duplicates drop cusip deal_date month, force
	
	merge 1:1 cusip month using "$temp\monthly_data.dta"
	
	
	sort cusip month
	egen firm_id = group(cusip) 

	gsort firm_id +month
	by firm_id: carryforward deal_date, gen(announcement_date)
	gsort firm_id -month
	by firm_id: carryforward announcement_date, replace
	sort firm_id month
	drop if announcement_date ==.
	gen deal_month = mofd(announcement_date)
	format %tm deal_month
	drop announcement_date
	drop _merge
	gen dist = month - deal_month
	by firm_id: carryforward dealnumber, replace
	by firm_id: carryforward siccode, replace
	
	merge m:1 dealnumber cusip deal_date using "J:\Sascha\Buyback Anomalies\six_m_ret.dta"
	drop _merge
	
	egen month_id = group(month)
	sort firm_id month_id
	rangestat (reg) retrf mktrf, interval(month_id -60 -1) by(firm_id)
	drop reg_r2 reg_adj_r2 b_cons se_mktrf se_cons 
	
	save "long_run_ar.dta", replace
