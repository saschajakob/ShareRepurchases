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
global temp		"C:\Users\valta\Desktop\temp"														

global data2	"J:\Sascha\Buyback Anomalies"

clear
clear matrix

set matsize 800
set scrollbufsize 500000		
capture program drop _all


	*************************************************************
	**** Beginn: Load data set and computation of indicators ****
	*************************************************************		

	use "$data2/sample_buyback_portfolio.dta", replace		

	sort permno month
	
	* browse permno year buyback ayear qdate cash_atl1 dvcl1 top10instown_percl1 ivol totalvalueauthorizedmil mkvalt

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
	
	* Take this out
	* 3: Top 10% of institutional investors. Take the one quarter lag: top10instown_percl1
	
	* 3: Past 6 months returns: six_m_sum_ret 
	
	* 4: Total payout. Sum of last year's dividend yield and the buyback yield
	
	g buyback_yield = totalvalueauthorizedmil/(mkvaltl1/1000)
	g payout_yield = dvc_mkvaltl1 + buyback_yield
	
	* Summarize the four sorting variables
	
	tabstat cash_atl1 ivol top10instown_percl1 payout_yield, stats(n mean sd p50 min p25 p75 max) c(s)
	
*	drop if cash_atl1 == .
*	drop if ivoll1 == .
*	drop if top10instown_percl1 == .
*	drop if payout_yield == .
	
	distinct dealnumber
	* 11,327 buyback announcements
	
	
	**************************************************
	**** Beginn: Computation of portfolio returns ****
	**************************************************			
	
	**** Create a portfolio that moves forward at the quarterly frequency. Every quarter, look back on the past 12, 24, or 36 months
	**** and based on all firms that did a share buyback during that period sort based on indicators.
	
	sort permno month
	bysort permno: carryforward dealnumber, replace
	
	sort dealnumber month
	by dealnumber: carryforward payout_yield, gen(payout_yield_all)
	
	g time2 = mofd(date)
	
	* We set December 1994 as the starting time	
	* December 2015 is time2 = 671
	* December 1995 is time2 = 419
	g pf_time = time2 - 419

	g pf_ret12m = .
	g top_pf_ret12m = .
	g bottom_pf_ret12m = .
	g n_obs12m = .
	g top_n_obs12m = .
	g bottom_n_obs12m = .	

	g pf_ret24m = .
	g top_pf_ret24m = .
	g bottom_pf_ret24m = .
	g n_obs24m = .
	g top_n_obs24m = .
	g bottom_n_obs24m = .		

	g pf_ret36m = .
	g top_pf_ret36m = .
	g bottom_pf_ret36m = .
	g n_obs36m = .
	g top_n_obs36m = .
	g bottom_n_obs36m = .			
	
	* Create variables for returns AFTER transaction costs
	g pf_ret12m_tc = .
	g top_pf_ret12m_tc = .
	g bottom_pf_ret12m_tc = .

	g pf_ret24m_tc = .
	g top_pf_ret24m_tc = .
	g bottom_pf_ret24m_tc = .

	g pf_ret36m_tc = .
	g top_pf_ret36m_tc = .
	g bottom_pf_ret36m_tc = .
	
	* Create variables for returns AFTER transaction costs and trading impact
	g pf_ret12m_tc_tic = .
	g top_pf_ret12m_tc_tic = .
	g bottom_pf_ret12m_tc_tic = .

	g pf_ret24m_tc_tic = .
	g top_pf_ret24m_tc_tic = .
	g bottom_pf_ret24m_tc_tic = .

	g pf_ret36m_tc_tic = .
	g top_pf_ret36m_tc_tic = .
	g bottom_pf_ret36m_tc_tic = .
	
	* Create variables for leverage
	g top_debt_at36m = .
	g bottom_debt_at36m = .	
	g debt_at36m = .	
	*
	g top_netdebt_at36m = .
	g bottom_netdebt_at36m = .
	g netdebt_at36m = .	
	*
	g top_debt_mv36m = .
	g bottom_debt_mv36m = .
	g debt_mv36m = .	
	*
	g top_netdebt_cap36m = .
	g bottom_netdebt_cap36m = .	
	g netdebt_cap36m = .	
	
	* Create variables for market cap	
	g top_mkvalt36m = .
	g top_mkvalt_med36m = .
	*
	g bottom_mkvalt36m = .
	g bottom_mkvalt_med36m = .
	*
	g mkvalt36m = .
	g mkvalt_med36m = .
	

	****************************************************************************
	
	***************** BUYBACKS OVER THE LAST 36 MONTHS**************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w4_top =.
	g quarter_w_diff_bottom =.
	g quarter_w4_bottom =.
	g quarter_w_diff =.	
	g quarter_w4=.
	g pf_ret36m_val = .
	g pf_ret36m_tc_val = .
	g pf_ret36m_tc_tic_val = .
	replace pf_ret36m_tc_tic_val = 100000 if time2 ==419													// chose initial portfolio size
	
	
	forval i = 0(3)252 {
	*forval i = 0(3)9 {
	
	
	g dist = pf_time - `i'
	
	g tag = 1 if buyback == 1 & dist <= 0 & dist >= -36
	egen tag2 = mean(tag), by(permno)
	g tag3 = 1 if dist <= 0 & dist >= -36 & tag2 == 1
/*	
	* Create the index - Add a constraint of minimum market cap of 100m
	egen q_cash = xtile(cash_atl1) if dist==0 & tag3 == 1, nq(5)		
	egen q_ivol = xtile(ivol) if dist==0 & tag3 == 1, nq(5) 		
*	egen q_owner = xtile(top10instown_percl1) if dist==0 & tag3 == 1, nq(5)	
	egen q_ret = xtile(six_m_cum_ret) if dist == 0 & tag3 == 1, nq(5) 	
*	egen q_payout = xtile(payout_yield_all) if dist==0 & tag3 == 1 & mkvalt > 100000, nq(5)	
	egen q_profit = xtile(grossprofit_atl1) if dist==0 & tag3 == 1, nq(5)
	
	
	gen cash_rank =. 
	replace cash_rank = 1 if q_cash ==1
	replace cash_rank = 2 if q_cash ==2
	replace cash_rank = 3 if q_cash ==3
	replace cash_rank = 4 if q_cash ==4
	replace cash_rank = 5 if q_cash ==5
	
	gen ivol_rank =. 
	replace ivol_rank = 1 if q_ivol ==1
	replace ivol_rank = 2 if q_ivol ==2
	replace ivol_rank = 3 if q_ivol ==3
	replace ivol_rank = 4 if q_ivol ==4
	replace ivol_rank = 5 if q_ivol ==5	
/*
	gen owner_rank =. 
	replace owner_rank = 1 if q_owner ==5
	replace owner_rank = 2 if q_owner ==4
	replace owner_rank = 3 if q_owner ==3
	replace owner_rank = 4 if q_owner ==2
	replace owner_rank = 5 if q_owner ==1		

*/
	g ret_rank = .
	replace ret_rank = 1 if q_ret ==5
	replace ret_rank = 2 if q_ret ==4
	replace ret_rank = 3 if q_ret ==3
	replace ret_rank = 4 if q_ret ==2
	replace ret_rank = 5 if q_ret ==1		
	
/*	
	gen payout_rank =. 
	replace payout_rank = 1 if q_payout ==5
	replace payout_rank = 2 if q_payout ==4
	replace payout_rank = 3 if q_payout ==3
	replace payout_rank = 4 if q_payout ==2
	replace payout_rank = 5 if q_payout ==1		
*/
	
	gen profit_rank =. 
	replace profit_rank = 1 if q_profit ==1
	replace profit_rank = 2 if q_profit ==2
	replace profit_rank = 3 if q_profit ==3
	replace profit_rank = 4 if q_profit ==4
	replace profit_rank = 5 if q_profit ==5		
	
*/	
	
	_pctile mkvalt if dist == 0 & tag3 == 1, percentiles(15, 30, 75)

	*g insample = 1 if mkvalt >= r(r3) & tag3 == 1 
	*g insample = 1 if mkvalt < r(r3) & mkvalt >=r(r2) & tag3 == 1 
	*g insample = 1 if mkvalt < r(r2) & mkvalt >=r(r1) & tag3 == 1 
	*g insample = 1 if mkvalt < r(r1) & tag3 == 1 
	g insample = 1 if tag3 == 1 
	
	sort permno month
	by permno: carryforward insample, replace
	
	/*	
	*gen index = cash_rank + ivol_rank + owner_rank + payout_rank
	*gen index = cash_rank + ivol_rank + ret_rank + payout_rank if mkvalt > 500000
	*gen index = cash_rank + profit_rank + ret_rank + ivol_rank if mkvalt >= r(r3)
	*gen index = cash_rank + profit_rank + ret_rank + ivol_rank if mkvalt < r(r3) & mkvalt >=r(r2)
	*gen index = cash_rank + profit_rank + ret_rank + ivol_rank if mkvalt < r(r2) & mkvalt >=r(r1)
	gen index = cash_rank + profit_rank + ret_rank + ivol_rank	
	*gen index = cash_rank + profit_rank + ret_rank + ivol_rank if mkvalt < r(r1)	
	*gen index = cash_rank + profit_rank + ret_rank + payout_rank
	*gen index = cash_rank + profit_rank + payout_rank + ivol_rank
	
	sort permno month
	by permno: carryforward index, replace
*/	
	* Compute returns and number of observations for overall portfolio
	egen temp = mean(ret) if dist == 1 & insample == 1
	egen obs1 = count(ret) if dist == 1 & insample == 1 
	egen obs2 = count(ret) if dist == 2 & insample == 1	
	egen obs3 = count(ret) if dist == 3 & insample == 1
	egen obs4 = count(ret) if dist == 4 & insample == 1

	
	* Compute average and median market cap
	egen mtemp1 = mean(mkvalt) if dist == 1 & insample == 1, by(month)
	egen mtemp2 = mean(mkvalt) if dist == 2 & insample == 1, by(month)
	egen mtemp3 = mean(mkvalt) if dist == 3 & insample == 1, by(month)
	
	replace mkvalt36m = mtemp1 if dist == 1
	replace mkvalt36m = mtemp2 if dist == 2
	replace mkvalt36m = mtemp3 if dist == 3		
	*
	egen mtemp4 = median(mkvalt) if dist == 1 & insample == 1, by(month)
	egen mtemp5 = median(mkvalt) if dist == 2 & insample == 1, by(month)
	egen mtemp6 = median(mkvalt) if dist == 3 & insample == 1, by(month)
	
	replace mkvalt_med36m = mtemp4 if dist == 1
	replace mkvalt_med36m = mtemp5 if dist == 2
	replace mkvalt_med36m = mtemp6 if dist == 3			
	
	
	tsset permno month
		replace obs1 =. if ret ==. 
			gen weight =.
			*** first month ***
			replace weight = 1/obs1 if dist == 1 
			*** second month ***
			replace weight = l.weight*(1+l.ret) if dist ==2 
			egen t1_weight = total(weight) if dist == 2, by(month)
			replace weight = weight/t1_weight if dist == 2 
			*** third month ***
			replace weight = l.weight*(1+l.retrf) if dist ==3 
			egen t2_weight = total(weight) if dist == 3, by(month)
			replace weight = weight/t2_weight if dist == 3
			*** next quarter ***
			replace weight = 1/obs1 if dist == 4
			replace quarter_w4 = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace pf_ret36m = temp2 if dist >= 1 & dist <= 3

			egen temp3 = mean(obs1), by(month)
			replace n_obs36m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace n_obs36m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace n_obs36m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace n_obs36m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff if dist==1 & tag2 ==1															// relevant weight difference fro end of quarter to beginning of next quarter
			replace w_diff = weight if dist ==1 & quarter_w_diff ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4 if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points	
			
				*trading impact
				gen ic = sqrt(w_diff*l.pf_ret36m_tc_tic_val/vol_dollar_mean)*ret_sd if dist==1
				egen tic = total(ic), by(month)
				replace tic = tic/n_obs36m if dist==1
				
			replace pf_ret36m_tc = pf_ret36m - tc if dist==1
			replace pf_ret36m_tc = pf_ret36m if dist==2 | dist==3
			replace pf_ret36m_tc_tic = pf_ret36m - tc - tic if dist==1
			replace pf_ret36m_tc_tic = pf_ret36m if dist==2 | dist==3
			
			replace pf_ret36m_tc_tic_val = l.pf_ret36m_tc_tic_val*(1+pf_ret36m_tc_tic) if dist==1
			replace pf_ret36m_tc_tic_val = l.pf_ret36m_tc_tic_val*(1+pf_ret36m_tc_tic) if dist==2
			replace pf_ret36m_tc_tic_val = l.pf_ret36m_tc_tic_val*(1+pf_ret36m_tc_tic) if dist==3
	
	* Drop all variables of look and rerun three months later	
	drop dist tag tag2 tag3 insample temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff mtemp1-mtemp6 ic tic
		
	}
	
	drop quarter_w_diff_top quarter_w4_top quarter_w_diff_bottom quarter_w4_bottom quarter_w_diff quarter_w4
	
	save "$temp/sample_36m_full.dta", replace
	save "$data2/sample_36m_full.dta", replace	
	****************************************************************************
	
