
set more off

global  temp	"C:\Users\valta\Desktop\temp"		


**********************
*** Preparing Data ***
**********************
	
	use "V:\COMPUSTAT\Securities\Annual Updates\Fundamentals Annually 1950-2015.dta", clear
	keep cusip che at fyear csho prcc_c ni seq ceq re
	gen che_at = che/at
	gen mkvalt = csho*prcc_c
	rename fyear year
	replace cusip = substr(cusip,1,8)
	duplicates drop cusip year, force
	save "$temp\COMPUSTAT_clean.dta", replace
	
	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	sort firm_id dealnumber dist
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1
	
	
	gen year = yofd(deal_date)
	sort group1 dist
	by group1: carryforward year, replace
	gsort group1 -dist
	by group1: carryforward year, replace
	sort group1 dist
	drop if dist < -36																	
	drop year

	
	gen year = yofd(deal_date)
	merge m:1 cusip year using "J:\Sascha\Buyback Anomalies\KLD_clean.dta"
	drop _merge
	sort firm_id month
	format month %tm
	replace year = dofm(month)
	format year %td
	replace year = yofd(year)
	format year %ty
	merge m:1 cusip year using "$temp\COMPUSTAT_clean.dta"
	gsort group1 -dist
	by group1: carryforward dealnumber, replace
	sort cusip year	
	drop if dealnumber ==.
	drop _merge
	sort firm_id month
	

	
	forvalues i = 1991(1)2007{
	xtile size_cluster_`i' = mkvalt if dist==0 & year == `i', nq(2)										// 1 indicates below median (small firms), 2 indicates above median (large firms)
	xtile cash_cluster_`i' = che_at if dist==0 & year == `i', nq(2)										// 1 indicates below median (low cash ratio), 2 indicates above median (high cash ratio)
	}
	
	egen size_cluster = rowtotal(size_cluster_1991 size_cluster_1992 size_cluster_1993 ///
	size_cluster_1994 size_cluster_1995 size_cluster_1996 size_cluster_1997 ///
	size_cluster_1998 size_cluster_1999 size_cluster_2000 size_cluster_2001 ///
	size_cluster_2002 size_cluster_2003 size_cluster_2004 size_cluster_2005 ///
	size_cluster_2006 size_cluster_2007) if dist==0
	
	egen cash_cluster = rowtotal(cash_cluster_1991 cash_cluster_1992 cash_cluster_1993 ///
	cash_cluster_1994 cash_cluster_1995 cash_cluster_1996 cash_cluster_1997 ///
	cash_cluster_1998 cash_cluster_1999 cash_cluster_2000 cash_cluster_2001 ///
	cash_cluster_2002 cash_cluster_2003 cash_cluster_2004 cash_cluster_2005 ///
	cash_cluster_2006 cash_cluster_2007) if dist==0
	
	drop size_cluster_1991 size_cluster_1992 size_cluster_1993 ///
	size_cluster_1994 size_cluster_1995 size_cluster_1996 size_cluster_1997 ///
	size_cluster_1998 size_cluster_1999 size_cluster_2000 size_cluster_2001 ///
	size_cluster_2002 size_cluster_2003 size_cluster_2004 size_cluster_2005 ///
	size_cluster_2006 size_cluster_2007 ///
	cash_cluster_1991 cash_cluster_1992 cash_cluster_1993 ///
	cash_cluster_1994 cash_cluster_1995 cash_cluster_1996 cash_cluster_1997 ///
	cash_cluster_1998 cash_cluster_1999 cash_cluster_2000 cash_cluster_2001 ///
	cash_cluster_2002 cash_cluster_2003 cash_cluster_2004 cash_cluster_2005 ///
	cash_cluster_2006 cash_cluster_2007

	
	sort group1 dist
	by group1: carryforward size_cluster, replace
	by group1: carryforward cash_cluster, replace

	
	gen sic2 = string(siccode)
	replace sic2 = substr(sic2,1,2)
	destring sic2, replace
	

	* net gov score
	*gen net_gov_score = cgov_str_num - cgov_con_num
	sort group1 dist
	by group1: carryforward net_gov_score, replace
	
	
	*net environmental score 
	*gen net_env_score = env_str_num - env_con_num
	sort group1 dist
	by group1: carryforward net_env_score, replace
	
	*net social score	
	*gen net_hum_score = hum_str_num - hum_con_num
	sort group1 dist
	by group1: carryforward net_hum_score, replace
	
	*net employee score	
	*gen net_emp_score = emp_str_num - emp_con_num
	sort group1 dist
	by group1: carryforward net_emp_score, replace
	
	*total core
	gen tot_net_score = net_gov_score + net_env_score + net_hum_score + net_emp_score
	sort group1 dist
	by group1: carryforward tot_net_score, replace
	
	* Dropping financial institutions
	
	drop if 60 <= sic2 <=67 &  91 <= sic2 <= 99 & sic2 == 49
	
	sort group1 dist
	tsset group1 dist
	
	
	
	
	***********************************************
	***********************************************
	*** Comparison to measure of value creation ***
	***********************************************
	***********************************************
	save "$temp\test1.dta", replace

	use "$temp\test1.dta", clear
	
	sort firm_id month
	by firm_id: egen n_deals = count(deal_date)
	drop if n_deals < 1																//drop firms according to deals
	sort dealnumber month
	by dealnumber: egen max_dist = count(dist) if dist >=0
	gen rev_dist = dist - max_dist
	replace rev_dist =-99 if n_deals==1
	sort firm_id month
	by firm_id: egen rank_deal = rank(deal_date)
	sort dealnumber month
	by dealnumber: carryforward rank_deal, replace
	sort firm_id month
	replace rev_dist =-99 if rank_deal==n_deals
	replace rev_dist=0 if dist==0
	replace rev_dist=dist if dist<0
	drop rank_deal max_dist n_deals
	
	drop if firm_id==17368
	*by firm_id: egen max_dist = max(dist)
	*drop if max_dist < 48
	*drop if net_gov_score < 0
	
	*** Distribution of returns ***
	
	preserve
	winsor2 ret, replace cuts(0.5 99.5)
	*histogram ret if dist >=0 & dist <=48, frequency normal     
	histogram ret if rev_dist >=-24 & rev_dist <=0
	restore
	

	
	*** Cumulative raw return subsequent to a SBB ***
	
		sort group1 dist
		gen ln_ret = ln(1+ret)
		by group1: gen cum_ln_ret = sum(ln_ret) if dist>=1 & rev_dist <=-24
		gen postSBB_cum_ret =exp(cum_ln_ret)-1 if rev_dist <=-24
		
	
	*** Cumulative raw return prior to a SBB ***
	
		sort group1 dist
		gen ln_ret_p = ln(1+ret)
		by group1: gen cum_ln_ret_p = sum(ln_ret_p) if rev_dist >= -24 & rev_dist <=0 & dist > 48
		gen preSBB_cum_ret =exp(cum_ln_ret_p)-1 if rev_dist >= -24 & dist >=48
		
		
	
	
	
	merge m:1 month using "J:\Sascha\Buyback Anomalies\socgen.dta"
	keep if _merge==3
	drop _merge
	drop  mth
	
	gen ke = lt10y + b_mktrf*mrp_sg														// Cost of equity
	sort firm_id month
	tsset firm_id month
	
	*merge m:1 year using "J:\Sascha\Buyback Anomalies\market_roe.dta"
	
	*sort firm_id month
	
	drop if year < 1990																	//Define time period, lower bound
	drop if year > 2015	
	
	
	save "$temp\test.dta", replace

	use "$temp\test.dta", clear
	
	
	sort firm_id month_id
	tsset firm_id month_id
	drop if seq<1																		// drop if total shareholder equity lower than 1 million
	by firm_id: gen roe = ni/l12.seq													// static ROE

	winsor2 roe, replace cuts(2 98) trim
	sort firm_id month_id
	tsset firm_id month_id
	
	*******************************
	*** Generating rolling ROEs ***
	*******************************
		
		rangestat (sum) ni (count) ni, interval(month_id -11 0) by(firm_id)
		gen ni_w = ni_sum/ni_count if ni !=.
		rangestat (sum) seq (count) seq, interval(month_id -11 0) by(firm_id)
		gen seq_w = seq_sum/seq_count if seq !=.
		gen roe_w = ni_w/l12.seq_w
	
	
	drop if year < 1991																//Define time period, lower bound
	drop if year > 2015	 
	
	
	***********************
	*** Generating EVAs ***
	***********************
	
		gen mktcap = csho*prc
		gen eva = (roe_w-ke)*seq_w														// choose type of equity here
		gen impl_mktcap = eva/ke
		gen impl_mktcap_g = impl_mktcap/l.impl_mktcap -1
		replace impl_mktcap_g = impl_mktcap_g *(-1) if impl_mktcap < 0 & impl_mktcap*l.impl_mktcap>0
		winsor2 impl_mktcap_g, replace cuts(2 98) trim
		gen ex_eva = roe_w-ke
		winsor2 ex_eva, replace cuts(0.1 99.9) trim										//extreme outlier
		sort month
		by month: egen mth_avg_ex = mean(ex_eva)
		by month: egen mth_med_ex = median(ex_eva)
		gen ex_mkteva_ew = mktroe_ew - (mrp_sg + lt10y)	
		gen ew_ex_eva_spread = ex_eva - ex_mkteva_ew
		sort firm_id month
		tsset firm_id month
		gen eva_g = eva/l.eva -1
		replace eva_g = abs(eva_g) if eva > l.eva
		replace eva_g = eva_g *(-1) if eva < 0 & l.eva < 0 & eva < l.eva
		
		twoway (line ex_mkteva_ew month, sort) (line mth_avg_ex month, sort) (line mth_med_ex month, sort)
		
		
	*drop if mktcap > 250															// confine market cap
	*drop if mktcap < 250
	
	*********************************
	*** Equally weighted measures ***
	*********************************
	
	
	*** Implicit market cap ***
	
		sort group1 dist
		gen ln_impl_mktcap_g = ln(1+impl_mktcap_g)
		by group1: gen cum_ln_impl_mktcap_g = sum(ln_impl_mktcap_g) if dist>=1
		gen postSBB_cum_ln_impl_mktcap_g =exp(cum_ln_impl_mktcap_g)-1
		winsor2 postSBB_cum_ln_impl_mktcap_g, replace cuts(1 99) trim
	
	gen avg_cum_ret =.
	replace avg_cum_ret=.
	gen avg_cum_impl_mktcap_g =.
	replace avg_cum_impl_mktcap_g=.
	
	
	forvalues i=1(1)48{
		sum postSBB_cum_ret if dist== `i' & rev_dist <=-24 /*& net_gov_score >0*/ ,d
		replace avg_cum_ret = r(mean) if dist== `i' & rev_dist <=-24 //& net_gov_score >0
		}
	forvalues i=1(1)48{
		sum postSBB_cum_ln_impl_mktcap_g if dist== `i' /*& net_gov_score >0*/ ,d
		replace avg_cum_impl_mktcap_g = r(mean) if dist== `i' //& net_gov_score >0	
		}
		
	twoway (line avg_cum_ret dist, sort) (line avg_cum_impl_mktcap_g dist, sort)  if dist<=48 & dist >=1
	
	
	
	gen avg_cum_pre_ret =.
	replace avg_cum_pre_ret=.

	
	forvalues i=-24(1)0{
		sum preSBB_cum_ret if rev_dist== `i' & dist >=48/*& net_gov_score >0*/ ,d
		replace avg_cum_pre_ret = r(mean) if rev_dist== `i' & dist >= 48 //& net_gov_score >0
		}

	twoway (line avg_cum_pre_ret rev_dist, sort)   if rev_dist >= -24 & rev_dist <=0 & dist >=48
	

	gen avg_exc_eva =.
	replace avg_exc_eva =.
	
	
	forvalues i=1(1)48{
		sum ex_eva if dist== `i' & rev_dist <= -24 /*& net_gov_score >0*/,d
		replace avg_exc_eva = r(mean) if dist== `i' & rev_dist <= -24 //& net_gov_score >0
		}
		
	gen avg_pre_exc_eva =.
	replace avg_pre_exc_eva =.
	
	
	forvalues i=-24(1)0{
		sum ex_eva if rev_dist== `i' & dist >=48/*& net_gov_score >0*/,d
		replace avg_pre_exc_eva = r(mean) if dist== `i' & dist >= 48 //& net_gov_score >0
		}	
		
	
		
	sort group1 dist
	
	by group1: gen sum_ex_eva = sum(ex_eva) if dist >=1
	by group1: gen sum_ew_ex_eva_spread = sum(ew_ex_eva_spread) if dist >=1
	
	replace sum_ex_eva =. if ex_eva==.
	replace sum_ew_ex_eva_spread =. if ew_ex_eva_spread==.
	
	gen avg_sum_ex_eva =.
	gen avg_sum_ew_ex_eva_spread =.
	replace avg_sum_ex_eva =.
	replace avg_sum_ew_ex_eva_spread=.
	
	forvalues i=1(1)48{
		sum sum_ex_eva if dist== `i'/*& net_gov_score >0*/,d 
		replace avg_sum_ex_eva = r(mean) if dist== `i' //& net_gov_score >0
		}
		
	forvalues i=1(1)48{
		sum sum_ew_ex_eva_spread if dist== `i'/*& net_gov_score >0*/,d 
		replace avg_sum_ew_ex_eva_spread = r(mean) if dist== `i' //& net_gov_score >0
		}	
	
	twoway (line avg_sum_ex_eva dist, sort) (line avg_cum_ret dist, sort) if dist<=48 & dist >=1
	twoway (line avg_sum_ex_eva dist, sort) (line avg_sum_ew_ex_eva_spread dist, sort) (line avg_cum_ret dist, sort) if dist<=48 & dist >=1
	
	
	
	
	********************************
	*** Market weighted measures ***
	********************************
	
	gen ex_mkteva_mw = mktroe_mw - (mrp_sg + lt10y)
	
	sort dist
	by dist: egen dist_total_mktcap = total(mktcap) if rev_dist <=-24
	sort firm_id month
	gen dist_mkt_w = mktcap/dist_total_mktcap if rev_dist <=-24
	
	sort rev_dist
	by rev_dist: egen rev_dist_total_mktcap = total(mktcap)
	sort firm_id month
	gen rev_dist_mkt_w = mktcap/rev_dist_total_mktcap
	
	gen mkt_w_ret = dist_mkt_w*ret if rev_dist <=-24
	gen mkt_w_ex_eva = dist_mkt_w*ex_eva if rev_dist <=-24
	sort dist
	by dist: egen mkt_w_avg_ret = total(mkt_w_ret) if rev_dist <=-24
	by dist: egen mkt_w_avg_ex_eva = total(mkt_w_ex_eva) if rev_dist <=-24
	
	gen mkt_w_pre_ret = rev_dist_mkt_w*ret
	gen mkt_w_pre_ex_eva = rev_dist_mkt_w*ex_eva
	sort rev_dist
	by rev_dist: egen mkt_w_avg_pre_ret = total(mkt_w_pre_ret)
	by rev_dist: egen mkt_w_avg_pre_ex_eva = total(mkt_w_pre_ex_eva)
	
	sort group1 dist
	gen ln_mkt_w_avg_ret = ln(1+mkt_w_avg_ret) if rev_dist <=-24
	*drop if dist<0
	*drop if dist >48
	by group1: gen cum_ln_mkt_w_avg_ret = sum(ln_mkt_w_avg_ret) if dist>=1 &  rev_dist <=-24
	by group1: gen sum_mkt_w_avg_ex_eva = sum(mkt_w_avg_ex_eva) if dist>=1 &  rev_dist <=-24
	gen cum_mkt_w_avg_ret =exp(cum_ln_mkt_w_avg_ret)-1 if rev_dist <=-24
	
	
	
		forvalues i=1(1)48{
		sum cum_mkt_w_avg_ret if dist== `i'/*& net_gov_score >0*/ & rev_dist <=-24,d 
		replace cum_mkt_w_avg_ret = r(mean) if dist== `i' & rev_dist <=-24 //& net_gov_score >0
		}
		forvalues i=1(1)48{
		sum sum_mkt_w_avg_ex_eva if dist== `i' & rev_dist <=-24 /*& net_gov_score >0*/,d 
		replace sum_mkt_w_avg_ex_eva = r(mean) if dist== `i' &  rev_dist <=-24 //& net_gov_score >0
		}		
		
		twoway (line cum_mkt_w_avg_ret dist, sort) (line sum_mkt_w_avg_ex_eva dist, sort) (line avg_sum_ex_eva dist, sort) (line avg_cum_ret dist, sort) if dist<=48 & dist >=1 & rev_dist <=-24
		twoway (line mkt_w_avg_ex_eva dist, sort) if dist<=48 & dist >=1 & rev_dist <=-24
		
	sort month
	by month: egen m_total_mktcap = total(mktcap)
	sort firm_id month
	gen m_mkt_w = mktcap/m_total_mktcap
	
	gen m_mkt_w_ret = m_mkt_w*ret
	gen m_mkt_w_ex_eva = m_mkt_w*ex_eva
	sort month
	by month: egen m_mkt_w_avg_ret = total(m_mkt_w_ret)
	by month: egen m_mkt_w_avg_ex_eva = total(m_mkt_w_ex_eva)
	twoway (line ex_mkteva_mw month, sort) (line m_mkt_w_avg_ex_eva month, sort) 
	
	
	
	************************************
	***Relative-to-market performance***
	************************************
	
	*** returns ***
	
	gen ex_mkt_ret = retrf - mktrf
	sort group1 dist
	gen ln_ex_mkt_ret = ln(1+ex_mkt_ret)
	by group1: gen cum_ln_ex_mkt_ret = sum(ln_ex_mkt_ret) if dist>=1 & rev_dist <=-24
	gen postSBB_cum_ex_mkt_ret =exp(cum_ln_ex_mkt_ret)-1 if rev_dist <=-24
	
	gen avg_cum_ex_mkt_ret =.
	replace avg_cum_ex_mkt_ret=.
	
		forvalues i=1(1)48{
		sum postSBB_cum_ex_mkt_ret if dist== `i' & rev_dist <=-24 /*& net_gov_score >0*/ ,d
		replace avg_cum_ex_mkt_ret = r(mean) if dist== `i' & rev_dist <=-24 //& net_gov_score >0
		}
		
	twoway (line avg_cum_ret dist, sort) (line avg_cum_ex_mkt_ret dist, sort)  if dist<=48 & dist >=1
	
	*** Excess EVA spread***
	
	*** By month ***
	
	*** market weighted ***
	
		gen mw_ex_eva_spread = ex_eva - ex_mkteva_mw
		sort month
		gen  m_mw_ex_eva_spread = m_mkt_w*mw_ex_eva_spread
		by month: egen m_mw_avg_ex_eva_spread = total(m_mw_ex_eva_spread)
		twoway (line m_mw_avg_ex_eva_spread month, sort)
		
	*** equally weighted ***
	
		by month: egen m_ew_avg_ex_eva_spread = mean(ew_ex_eva_spread)
		by month: egen m_ew_med_ex_eva_spread = median(ew_ex_eva_spread)
		twoway (line m_ew_avg_ex_eva_spread month, sort) (line m_ew_med_ex_eva_spread month, sort)
		twoway (line m_ew_avg_ex_eva_spread month, sort) (line m_mw_avg_ex_eva_spread month, sort) (line m_ew_med_ex_eva_spread month, sort)
		
	
	*** By event time ***
	
	*** market weighted ***
	
		sort dist
		gen dist_mw_ex_eva_spread = dist_mkt_w*mw_ex_eva_spread if rev_dist <=-24
		by dist: egen dist_mw_avg_ex_eva_spread = total(dist_mw_ex_eva_spread) if rev_dist <=-24
		twoway (line dist_mw_avg_ex_eva_spread dist, sort)if dist<=48 & dist >=1 & rev_dist <=-24

		
		sort rev_dist
		gen rev_dist_mw_ex_eva_spread = rev_dist_mkt_w*mw_ex_eva_spread
		by rev_dist: egen rev_dist_mw_avg_ex_eva_spread = total(rev_dist_mw_ex_eva_spread)
		twoway (line rev_dist_mw_avg_ex_eva_spread rev_dist, sort)if rev_dist>=-24 & rev_dist <=0

	*** equally weighted ***
		sort dist
		by dist: egen dist_ew_avg_ex_eva_spread = mean(ew_ex_eva_spread)
		by dist: egen dist_ew_med_ex_eva_spread = median(ew_ex_eva_spread)
		
		sort rev_dist
		by rev_dist: egen rev_dist_ew_avg_ex_eva_spread = mean(ew_ex_eva_spread)
		twoway (line rev_dist_ew_avg_ex_eva_spread rev_dist, sort) if rev_dist >=-24 & rev_dist <=0
	
		twoway (line dist_ew_avg_ex_eva_spread dist, sort) (line dist_ew_med_ex_eva_spread dist, sort)if dist<=48 & dist >=1
		twoway (line dist_ew_avg_ex_eva_spread dist, sort) (line dist_mw_avg_ex_eva_spread dist, sort) (line dist_ew_med_ex_eva_spread dist, sort)if dist<=48 & dist >=1
		
	*** Number of firms in event time***
	
		gen dist_n_firms =.
		forvalues i=1(1)48{
			sum dist_mw_ex_eva_spread if dist== `i' & rev_dist <=-24
			replace dist_n_firms = r(N) if dist== `i' & rev_dist <=-24
		 }
		twoway (line dist_n_firms dist, sort)if dist<=48 & dist >=1 & rev_dist <=-24
	