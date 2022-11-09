set more off

if c(os) == "Windows" local projectpath "J:/Sascha/Buyback Anomalies"
if c(os) == "MacOSX"  local projectpath "~/Dropbox/data.dta"

//global  temp	"C:/Users/valta/Desktop/temp"		


**********************
*** Preparing Data ***
**********************
	

//use "J:/Sascha/Buyback Anomalies/index.dta", clear
//use "J:/Sascha/Buyback Anomalies/index2.dta", clear
use "J:/Sascha/Buyback Anomalies/index3.dta", clear

*use "`projectpath'/index3.dta", clear
	
	keep if inrange(year, 1997,2015)
	//drop if inrange(year, 2008,2009)

	gen index_rank = governance_rank + payout_rank + cash_rank + ivol_rank  	//if div_dummy ==0											    	 	//highest alpha and information ratio
	//gen index_rank = governance_rank + payout_rank + cash_rank + ivol_rank + pre_ex_eva_rank + momentum_rank + size_rank	     						// higheste expected excess return and sharpe ratio
	//gen index_rank = mtb_rank + size_rank  + momentum_rank  + ivol_rank 																				// Peyer and Vermaelen
	
	sort dealnumber month
	by dealnumber: gen missing_rank = 1 if dist==0 & index_rank ==.
	by dealnumber: replace missing_rank = 0 if dist==0 & index_rank !=.
	by dealnumber: carryforward missing_rank, replace
	
	sort group1 dist
	tsset group1 dist
	
	keep if missing_rank ==0
	
	sort firm_id month
	egen SBB_count = count(deal_date), by(firm)
	//keep if SBB_count > 1
	
	hist index_rank if dist ==0 /*& div_dummy ==0*/ & n_obs >=12, freq norm
	
	bysort year: sum mkvalt if index_rank >=16 & mkvalt !=. & dist >0 & dist <=24,d
	
	sort group1 dist
	tsset group1 dist
	
	merge m:1 month using "V:/Fama French & Liqudity Factors/Pastor Stambaugh/01Aug1962 - 31Dec2016.dta"
	
	gen quarter = qofd(date)
	format quarter %tq
	
	gen qi = quarter(date)
	gen mi = month(date)
	gen sequence =.
	replace sequence = 1 if mi ==1 | mi ==4 | mi== 7 | mi == 10
	replace sequence = 2 if mi ==2 | mi ==5 | mi== 8 | mi == 11
	replace sequence = 3 if mi ==3 | mi ==6 | mi== 9 | mi == 12
	
	
	*-----------------------------------------------------------------------------------------------------------*
	
*** Event time approach ***

//sort group1 dist
	
	//xtfmb retrf mktrf smb hml umd rmw cma ps_vwf if dist <=48 & dist > 0 //& index_rank >=26
	//display _b[_cons]*48

 
/*	*** graph ***
	
	gen top_alpha =.
	reg retrf mktrf smb hml umd rmw cma ps_vwf if dist ==1 & index_rank >=16
	replace top_alpha = _b[_cons] if dist ==1
	sort group1 dist
	
		forvalues i = 2(1)48{														
				xtfmb retrf mktrf smb hml umd rmw cma ps_vwf if dist <= `i' & dist > 0 & index_rank >=16																						
				replace top_alpha = _b[_cons]*`i' if dist == `i'
						}
		
	gen full_alpha =.
	reg retrf mktrf smb hml umd rmw cma ps_vwf if dist ==1
	replace full_alpha = _b[_cons] if dist ==1
	sort group1 dist
	
		forvalues i = 2(1)48{														
				xtfmb retrf mktrf smb hml umd rmw cma ps_vwf if dist <= `i' & dist > 0 																						
				replace full_alpha = _b[_cons]*`i' if dist == `i'
						}

	gen bottom_alpha =.
	reg retrf mktrf smb hml umd rmw cma ps_vwf if dist ==1 & index_rank <=8
	replace bottom_alpha = _b[_cons] if dist ==1
	sort group1 dist
	
		forvalues i = 2(1)48{														
				xtfmb retrf mktrf smb hml umd rmw cma ps_vwf if dist <= `i' & dist > 0 & index_rank <=8																						
				replace bottom_alpha = _b[_cons]*`i' if dist == `i'
						}					
						
	replace top_alpha = 0 if dist ==0
	replace full_alpha = 0 if dist ==0
	replace bottom_alpha = 0 if dist ==0	
	
	twoway (line top_alpha full_alpha bottom_alpha dist, sort) if dist <=48
	*/
	
	
	
	
	
*** Fama French Calendar Approach ***

* constructing portfolios *
	
	sort firm_id month
	tsset firm_id month
	
		*full sample

		egen obs = count(retrf) if dist >= 1 & dist <=12 & inrange(year,2000,2015) & sequence == 1, by(month)			// selection, counts only firms whith return observation in rebalancing month
		replace obs =. if retrf ==. 																					// excluding firms that have no return observation in the rebalancing month
			gen weight =.
			*** at the beginning of first month ***
			replace weight = 1/obs if sequence == 1 																	// equal weighting in the rebalancing month
			*** at the beginning of second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 														// change of weight from the beginning of first to second month
			replace weight = l.weight if l.retrf ==. & sequence ==2
			egen t1_weight = total(weight) if sequence == 2, by(month)													// normalization of new weight in order to add up to one
			replace weight = weight/t1_weight if sequence == 2 															// replacing weight with newly normalized weight
			*** at the beginning of third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen portfolio_12m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=12
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace portfolio_12m = portfolio_12m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
		
		
		
		egen obs = count(retrf) if dist >= 1 & dist <=24 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen portfolio_24m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=24
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace portfolio_24m = portfolio_24m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		

		egen obs = count(retrf) if dist >= 1 & dist <=36 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen portfolio_36m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=36
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace portfolio_36m = portfolio_36m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
	
		* high ranks

		egen obs = count(retrf) if dist >= 1 & dist <=12 & index_rank >=26 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen top_portfolio_12m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=12 & index_rank >=26
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace top_portfolio_12m = top_portfolio_12m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
		
		
		
		egen obs = count(retrf) if dist >= 1 & dist <=24 & index_rank >=26 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen top_portfolio_24m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=24 & index_rank >=26
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace top_portfolio_24m = top_portfolio_24m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
		
		
		
		egen obs = count(retrf) if dist >= 1 & dist <=36 & index_rank >=26 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen top_portfolio_36m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=36 & index_rank >=26
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace top_portfolio_36m = top_portfolio_36m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
		
		
		
		
		* low ranks
		egen obs = count(retrf) if dist >= 1 & dist <=12 & index_rank <=8 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen bottom_portfolio_12m = sum(pw_retrf), by(month)
			
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=12 & index_rank <=8
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace bottom_portfolio_12m = bottom_portfolio_12m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
		
		
		
		egen obs = count(retrf) if dist >= 1 & dist <=24 & index_rank <=8 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen bottom_portfolio_24m = sum(pw_retrf), by(month)
		
		
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=24 & index_rank <=8
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace bottom_portfolio_24m = bottom_portfolio_24m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
		
		
		
		egen obs = count(retrf) if dist >= 1 & dist <=36 & index_rank <=8 & inrange(year,2000,2015), by(month)
		replace obs =. if retrf ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs if sequence == 1 
			*** second month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==2 
			egen t1_weight = total(weight) if sequence == 2, by(month)
			replace weight = weight/t1_weight if sequence == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if sequence ==3 
			egen t2_weight = total(weight) if sequence == 3, by(month)
			replace weight = weight/t2_weight if sequence == 3 
			
			gen pw_retrf = retrf * weight
		egen bottom_portfolio_36m = sum(pw_retrf), by(month)
		
			*rebalancing costs
			gen ind = 1 if dist >= 1 & dist <=36 & index_rank <=8
			replace ind = 0 if ind==.
			by firm_id: gen w_diff = abs(weight - l.weight) if ind==1
			by firm_id: replace w_diff = weight if ind==1 & sequence==1 & l.weight ==.
			by firm_id: replace w_diff = l.weight if ind== 0 & sequence ==1
			
			replace w_diff =. if sequence !=1
			egen tw_diff = total(w_diff), by(month)
			gen tc = tw_diff *0.0040																					// assuming transaction costs of 20 basis points
			replace bottom_portfolio_36m = bottom_portfolio_36m - tc																	
			
		drop obs pw_retrf t1_weight t2_weight weight w_diff tw_diff ind tc
		sort firm_id month
		tsset firm_id month
		
	 
		drop if year < 2000 | year ==.
		bysort month: sum retrf if index_rank >=16 & dist>0 & dist <=12 & inrange(year,2000,2015)
		by month: egen obs12m = count(retrf) if index_rank >=16 & dist>0 & dist <=12 & inrange(year,2000,2014)
		by month: egen obs24m = count(retrf) if index_rank >=16 & dist>0 & dist <=24 & inrange(year,2000,2014)
		by month: egen obs36m = count(retrf) if index_rank >=16 & dist>0 & dist <=36 & inrange(year,2000,2014)
		twoway (line obs12m obs24m obs36m month, sort)
		
		//replace top_portfolio_12m = top_portfolio_12m - 0.00075 if sequence ==1
	collapse portfolio_12m portfolio_24m portfolio_36m top_portfolio_12m  top_portfolio_24m top_portfolio_36m ///
		bottom_portfolio_12m bottom_portfolio_24m bottom_portfolio_36m mktrf smb hml umd cma rmw ps_vwf, by(month)
		

		reg portfolio_12m mktrf smb hml umd rmw cma ps_vwf 
		estimate store full_12m
		
		reg portfolio_24m mktrf smb hml umd rmw cma ps_vwf
		estimate store full_24m
		
		reg portfolio_36m mktrf smb hml umd rmw cma ps_vwf
		estimate store full_36m
		
		reg top_portfolio_12m mktrf smb hml umd rmw cma ps_vwf 
		estimate store top_12m
		
		reg top_portfolio_24m mktrf smb hml umd rmw cma ps_vwf
		estimate store top_24m
		
		reg top_portfolio_36m mktrf smb hml umd rmw cma ps_vwf
		estimate store top_36m
		
		reg bottom_portfolio_12m mktrf smb hml umd rmw cma ps_vwf
		estimate store bottom_12m
		
		reg bottom_portfolio_24m mktrf smb hml umd rmw cma ps_vwf
		estimate store bottom_24m
		
		reg bottom_portfolio_36m mktrf smb hml umd rmw cma ps_vwf
		estimate store bottom_36m
	
	estout full_12m full_24m full_36m top_12m top_24m top_36m bottom_12m bottom_24m bottom_36m, cells(b(star fmt(4)) t(par fmt(2)))
	
	sum portfolio_12m portfolio_24m portfolio_36m top_portfolio_12m top_portfolio_24m top_portfolio_36m ///
	bottom_portfolio_12m bottom_portfolio_24m bottom_portfolio_36m
	
	
	*** plotting perfomance indexed at 100 ***
	egen month_id = group(month)

	replace portfolio_12m = ln(1+portfolio_12m)
	gen p12m_ret =.
	forvalues x =1(1)192{
		 sum portfolio_12m if month_id > 0 & month_id <= `x'
		 replace p12m_ret = r(mean)*`x' if month_id == `x'
	}
	replace portfolio_12m = 100*(1+(exp(p12m_ret)-1))
	drop p12m_ret
	
	
	replace portfolio_24m = ln(1+portfolio_24m)
	gen p24m_ret =.
	forvalues x =1(1)192{
		 sum portfolio_24m if month_id > 0 & month_id <= `x'
		 replace p24m_ret = r(mean)*`x' if month_id == `x'
	}
	replace portfolio_24m = 100*(1+(exp(p24m_ret)-1))
	drop p24m_ret
	
	
	replace portfolio_36m = ln(1+portfolio_36m)
	gen p36m_ret =.
	forvalues x =1(1)192{
		 sum portfolio_36m if month_id > 0 & month_id <= `x'
		 replace p36m_ret = r(mean)*`x' if month_id == `x'
	}
	replace portfolio_36m = 100*(1+(exp(p36m_ret)-1))
	drop p36m_ret
	
		replace top_portfolio_12m = ln(1+top_portfolio_12m)
	gen p12m_ret =.
	forvalues x =1(1)192{
		 sum top_portfolio_12m if month_id > 0 & month_id <= `x'
		 replace p12m_ret = r(mean)*`x' if month_id == `x'
	}
	replace top_portfolio_12m = 100*(1+(exp(p12m_ret)-1))
	drop p12m_ret
	
	
	replace top_portfolio_24m = ln(1+top_portfolio_24m)
	gen p24m_ret =.
	forvalues x =1(1)192{
		 sum top_portfolio_24m if month_id > 0 & month_id <= `x'
		 replace p24m_ret = r(mean)*`x' if month_id == `x'
	}
	replace top_portfolio_24m = 100*(1+(exp(p24m_ret)-1))
	drop p24m_ret
	
	
	replace top_portfolio_36m = ln(1+top_portfolio_36m)
	gen p36m_ret =.
	forvalues x =1(1)192{
		 sum top_portfolio_36m if month_id > 0 & month_id <= `x'
		 replace p36m_ret = r(mean)*`x' if month_id == `x'
	}
	replace top_portfolio_36m = 100*(1+(exp(p36m_ret)-1))
	drop p36m_ret
	
		replace bottom_portfolio_12m = ln(1+bottom_portfolio_12m)
	gen p12m_ret =.
	forvalues x =1(1)192{
		 sum bottom_portfolio_12m if month_id > 0 & month_id <= `x'
		 replace p12m_ret = r(mean)*`x' if month_id == `x'
	}
	replace bottom_portfolio_12m = 100*(1+(exp(p12m_ret)-1))
	drop p12m_ret
	
	
	replace bottom_portfolio_24m = ln(1+bottom_portfolio_24m)
	gen p24m_ret =.
	forvalues x =1(1)192{
		 sum bottom_portfolio_24m if month_id > 0 & month_id <= `x'
		 replace p24m_ret = r(mean)*`x' if month_id == `x'
	}
	replace bottom_portfolio_24m = 100*(1+(exp(p24m_ret)-1))
	drop p24m_ret
	
	
	replace bottom_portfolio_36m = ln(1+bottom_portfolio_36m)
	gen p36m_ret =.
	forvalues x =1(1)192{
		 sum bottom_portfolio_36m if month_id > 0 & month_id <= `x'
		 replace p36m_ret = r(mean)*`x' if month_id == `x'
	}
	replace bottom_portfolio_36m = 100*(1+(exp(p36m_ret)-1))
	drop p36m_ret
	
		replace mktrf = ln(1+mktrf)
	gen mkt_ret =.
	forvalues x =1(1)192{
		 sum mktrf if month_id > 0 & month_id <= `x'
		 replace mkt_ret = r(mean)*`x' if month_id == `x'
	}
	replace mktrf = 100*(1+(exp(mkt_ret)-1))
	drop mkt_ret
	
	
	
	twoway (line portfolio_12m portfolio_24m portfolio_36m top_portfolio_12m top_portfolio_24m top_portfolio_36m bottom_portfolio_12m bottom_portfolio_24m bottom_portfolio_36m mktrf month, sort)
	twoway (line portfolio_24m top_portfolio_24m  mktrf month, sort)
