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
	
	* We set December 1995 as the starting time	
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
	
/*		
	***************************************************************************
	***************** Rolling portfolio construction **************************
	***************************************************************************
	
	****************** BUYBACKS OVER THE LAST 12 MONTHS************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w4_top =.
	g quarter_w_diff_bottom =.
	g quarter_w4_bottom =.
	g quarter_w_diff =.	
	g quarter_w4=.
	
	
	forval i = 0(3)252 {
	
	g dist = pf_time - `i'
	*g dist = pf_time - 3
	
	g tag = 1 if buyback == 1 & dist <= 0 & dist >= -12
	egen tag2 = mean(tag), by(permno)
	g tag3 = 1 if dist <= 0 & dist >= -12 & tag2 == 1
	
	* Create the index
	egen q_cash = xtile(cash_atl1) if dist==0 & tag3 == 1, nq(5)	
	egen q_ivol = xtile(ivol) if dist==0 & tag3 == 1, nq(5)	
	egen q_owner = xtile(top10instown_percl1) if dist==0 & tag3 == 1, nq(5)	
	egen q_payout = xtile(payout_yield_all) if dist==0 & tag3 == 1, nq(5)	
	
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
	
	gen owner_rank =. 
	replace owner_rank = 1 if q_owner ==5
	replace owner_rank = 2 if q_owner ==4
	replace owner_rank = 3 if q_owner ==3
	replace owner_rank = 4 if q_owner ==2
	replace owner_rank = 5 if q_owner ==1		
	
	gen payout_rank =. 
	replace payout_rank = 1 if q_payout ==5
	replace payout_rank = 2 if q_payout ==4
	replace payout_rank = 3 if q_payout ==3
	replace payout_rank = 4 if q_payout ==2
	replace payout_rank = 5 if q_payout ==1			
	
	gen index = cash_rank + ivol_rank + owner_rank + payout_rank
	
	sort permno month
	by permno: carryforward index, replace
	
	* Compute returns and number of observations for TOP portfolio
	egen temp = mean(ret) if index >=16 & dist == 1 & index != .
	egen obs1 = count(ret) if index >=16 & dist == 1 & index != . 
	egen obs2 = count(ret) if index >=16 & dist == 2 & index != . 	
	egen obs3 = count(ret) if index >=16 & dist == 3 & index != .
	egen obs4 = count(ret) if index >=16 & dist == 4 & index != .

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
			replace weight = 1/obs4 if dist == 4
			replace quarter_w4_top = weight if dist ==4 
			
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace top_pf_ret12m = temp2 if dist >= 1 & dist <= 3
			
			egen temp3 = mean(obs1), by(month)
			replace top_n_obs12m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace top_n_obs12m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace top_n_obs12m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace top_n_obs12m = temp6 if dist == 4
		
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff_top = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff_top if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff_top ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4_top if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace top_pf_ret12m_tc = top_pf_ret12m - tc if dist==1
			replace top_pf_ret12m_tc = top_pf_ret12m  if dist==2 | dist==3
			
			drop temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff
	
* Compute returns and number of observations for BOTTOM portfolio
	egen temp = mean(ret) if index <=8 & dist == 1 & index != .
	egen obs1 = count(ret) if index <=8 & dist == 1 & index != . 
	egen obs2 = count(ret) if index <=8 & dist == 2 & index != . 	
	egen obs3 = count(ret) if index <=8 & dist == 3 & index != . 
	egen obs4 = count(ret) if index <=8 & dist == 4 & index != .

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
			replace weight = 1/obs4 if dist == 4
			replace quarter_w4_bottom = weight if dist ==4 
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace bottom_pf_ret12m = temp2 if dist >= 1 & dist <= 3
		
			egen temp3 = mean(obs1), by(month)
			replace bottom_n_obs12m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace bottom_n_obs12m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace bottom_n_obs12m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace bottom_n_obs12m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff_bottom = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff_bottom if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff_bottom ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4_bottom if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace bottom_pf_ret12m_tc = bottom_pf_ret12m - tc if dist==1
			replace bottom_pf_ret12m_tc = bottom_pf_ret12m if dist==2 | dist==3

			drop temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff	
		
	* Compute returns and number of observations for TOTAL portfolio
	egen temp = mean(ret) if dist == 1 & index != .
	egen obs1 = count(ret) if dist == 1 & index != . 
	egen obs2 = count(ret) if dist == 2 & index != . 	
	egen obs3 = count(ret) if dist == 3 & index != .
	egen obs4 = count(ret) if dist == 4 & index != .
	
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
			replace weight = 1/obs4 if dist == 4
			replace quarter_w4 = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace pf_ret12m = temp2 if dist >= 1 & dist <= 3
			
			egen temp3 = mean(obs1), by(month)
			replace n_obs12m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace n_obs12m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace n_obs12m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace n_obs12m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4 if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace pf_ret12m_tc = pf_ret12m - tc if dist==1
			replace pf_ret12m_tc = pf_ret12m if dist==2 | dist==3
		
	* Drop all variables of loop and rerun three months later
	drop dist tag tag2 tag3 q_cash q_ivol q_owner q_payout cash_rank ivol_rank owner_rank payout_rank index temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff
	
	}
	
	drop quarter_w_diff_top quarter_w4_top quarter_w_diff_bottom quarter_w4_bottom quarter_w_diff quarter_w4
	save "$temp/sample_returns_all_12m.dta", replace
	***************************************************************************

	**************** BUYBACKS OVER THE LAST 24 MONTHS**************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w4_top =.
	g quarter_w_diff_bottom =.
	g quarter_w4_bottom =.
	g quarter_w_diff =.	
	g quarter_w4=.	
	
	forval i = 0(3)252 {
	
	g dist = pf_time - `i'
	
	g tag = 1 if buyback == 1 & dist <= 0 & dist >= -24
	egen tag2 = mean(tag), by(permno)
	g tag3 = 1 if dist <= 0 & dist >= -24 & tag2 == 1
	
	* Create the index
	egen q_cash = xtile(cash_atl1) if dist==0 & tag3 == 1, nq(5)	
	egen q_ivol = xtile(ivol) if dist==0 & tag3 == 1, nq(5)	
	egen q_owner = xtile(top10instown_percl1) if dist==0 & tag3 == 1, nq(5)	
	egen q_payout = xtile(payout_yield_all) if dist==0 & tag3 == 1, nq(5)	
	
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
	
	gen owner_rank =. 
	replace owner_rank = 1 if q_owner ==5
	replace owner_rank = 2 if q_owner ==4
	replace owner_rank = 3 if q_owner ==3
	replace owner_rank = 4 if q_owner ==2
	replace owner_rank = 5 if q_owner ==1		
	
	gen payout_rank =. 
	replace payout_rank = 1 if q_payout ==5
	replace payout_rank = 2 if q_payout ==4
	replace payout_rank = 3 if q_payout ==3
	replace payout_rank = 4 if q_payout ==2
	replace payout_rank = 5 if q_payout ==1			
	
	gen index = cash_rank + ivol_rank + owner_rank + payout_rank
	
	sort permno month
	by permno: carryforward index, replace
	
	* Compute returns and number of observations for TOP portfolio
	egen temp = mean(ret) if index >=16 & dist == 1 & index != .
	egen obs1 = count(ret) if index >=16 & dist == 1 & index != . 
	egen obs2 = count(ret) if index >=16 & dist == 2 & index != . 	
	egen obs3 = count(ret) if index >=16 & dist == 3 & index != .
	egen obs4 = count(ret) if index >=16 & dist == 4 & index != .

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
			replace weight = 1/obs4 if dist == 4
			replace quarter_w4_top = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace top_pf_ret24m = temp2 if dist >= 1 & dist <= 3
			
			egen temp3 = mean(obs1), by(month)
			replace top_n_obs24m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace top_n_obs24m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace top_n_obs24m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace top_n_obs24m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff_top = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff_top if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff_top ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4_top if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace top_pf_ret24m_tc = top_pf_ret24m - tc if dist==1
			replace top_pf_ret24m_tc = top_pf_ret24m  if dist==2 | dist==3
		
	drop temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff
	
* Compute returns and number of observations for BOTTOM portfolio
	egen temp = mean(ret) if index <=8 & dist == 1 & index != .
	egen obs1 = count(ret) if index <=8 & dist == 1 & index != . 
	egen obs2 = count(ret) if index <=8 & dist == 2 & index != . 	
	egen obs3 = count(ret) if index <=8 & dist == 3 & index != .
	egen obs4 = count(ret) if index <=8 & dist == 4 & index != .

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
			replace weight = 1/obs4 if dist == 4
			replace quarter_w4_bottom = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace bottom_pf_ret24m = temp2 if dist >= 1 & dist <= 3
			
			egen temp3 = mean(obs1), by(month)
			replace bottom_n_obs24m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace bottom_n_obs24m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace bottom_n_obs24m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace bottom_n_obs24m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff_bottom = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff_bottom if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff_bottom ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4_bottom if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace bottom_pf_ret24m_tc = bottom_pf_ret24m - tc if dist==1
			replace bottom_pf_ret24m_tc = bottom_pf_ret24m if dist==2 | dist==3
		
	drop temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff		
	
		
	* Compute returns and number of observations for TOTAL portfolio
	egen temp = mean(ret) if dist == 1 & index != .
	egen obs1 = count(ret) if dist == 1 & index != . 
	egen obs2 = count(ret) if dist == 2 & index != . 	
	egen obs3 = count(ret) if dist == 3 & index != . 
	egen obs4 = count(ret) if dist == 4 & index != .
	
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
			replace weight = 1/obs4 if dist == 4
			replace quarter_w4 = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace pf_ret24m = temp2 if dist >= 1 & dist <= 3

			egen temp3 = mean(obs1), by(month)
			replace n_obs24m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace n_obs24m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace n_obs24m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace n_obs24m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4 if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace pf_ret24m_tc = pf_ret24m - tc if dist==1
			replace pf_ret24m_tc = pf_ret24m if dist==2 | dist==3
	
	* Drop all variables of loop and rerun three months later
	drop dist tag tag2 tag3 q_cash q_ivol q_owner q_payout cash_rank ivol_rank owner_rank payout_rank index temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff
	
	}
	
	drop quarter_w_diff_top quarter_w4_top quarter_w_diff_bottom quarter_w4_bottom quarter_w_diff quarter_w4
	save "$temp/sample_returns_all_24m.dta", replace	

*/	
	****************************************************************************
	
	***************** BUYBACKS OVER THE LAST 36 MONTHS**************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w4_top =.
	g quarter_w_diff_bottom =.
	g quarter_w4_bottom =.
	g quarter_w_diff =.	
	g quarter_w4=.	
	
	forval i = 0(3)252 {
	*forval i = 0(3)9 {
	
	g dist = pf_time - `i'
	
	g tag = 1 if buyback == 1 & dist <= 0 & dist >= -36
	egen tag2 = mean(tag), by(permno)
	g tag3 = 1 if dist <= 0 & dist >= -36 & tag2 == 1
	
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
	
	
	*_pctile mkvalt if dist == 0, percentiles(15, 30, 75)
	
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
	
	* Compute returns and number of observations for TOP portfolio
	egen temp = mean(ret) if index >=16 & dist == 1 & index != .
	egen obs1 = count(ret) if index >=16 & dist == 1 & index != . 
	egen obs2 = count(ret) if index >=16 & dist == 2 & index != . 	
	egen obs3 = count(ret) if index >=16 & dist == 3 & index != . 
	egen obs4 = count(ret) if index >=16 & dist == 4 & index != .

	* Compute leverage ratios
	egen ltemp1 = mean(debt_at) if index >=16 & dist == 1 & index != ., by(month)
	egen ltemp2 = mean(debt_at) if index >=16 & dist == 2 & index != ., by(month)
	egen ltemp3 = mean(debt_at) if index >=16 & dist == 3 & index != ., by(month)
	
	replace top_debt_at36m = ltemp1 if dist == 1
	replace top_debt_at36m = ltemp2 if dist == 2
	replace top_debt_at36m = ltemp3 if dist == 3		
	*
	egen ltemp4 = mean(netdebt_at) if index >=16 & dist == 1 & index != ., by(month)
	egen ltemp5 = mean(netdebt_at) if index >=16 & dist == 2 & index != ., by(month)
	egen ltemp6 = mean(netdebt_at) if index >=16 & dist == 3 & index != ., by(month)
	
	replace top_netdebt_at36m = ltemp4 if dist == 1
	replace top_netdebt_at36m = ltemp5 if dist == 2
	replace top_netdebt_at36m = ltemp6 if dist == 3		
	*
	egen ltemp7 = mean(debt_mv) if index >=16 & dist == 1 & index != ., by(month)
	egen ltemp8 = mean(debt_mv) if index >=16 & dist == 2 & index != ., by(month)
	egen ltemp9 = mean(debt_mv) if index >=16 & dist == 3 & index != ., by(month)
	
	replace top_debt_mv36m = ltemp7 if dist == 1
	replace top_debt_mv36m = ltemp8 if dist == 2
	replace top_debt_mv36m = ltemp9 if dist == 3		
	*
	egen ltemp10 = mean(netdebt_cap) if index >=16 & dist == 1 & index != ., by(month)
	egen ltemp11 = mean(netdebt_cap) if index >=16 & dist == 2 & index != ., by(month)
	egen ltemp12 = mean(netdebt_cap) if index >=16 & dist == 3 & index != ., by(month)
	
	replace top_netdebt_cap36m = ltemp10 if dist == 1
	replace top_netdebt_cap36m = ltemp11 if dist == 2
	replace top_netdebt_cap36m = ltemp12 if dist == 3			
	
	* Compute average and median market cap
	egen mtemp1 = mean(mkvalt) if index >=16 & dist == 1 & index != ., by(month)
	egen mtemp2 = mean(mkvalt) if index >=16 & dist == 2 & index != ., by(month)
	egen mtemp3 = mean(mkvalt) if index >=16 & dist == 3 & index != ., by(month)
	
	replace top_mkvalt36m = mtemp1 if dist == 1
	replace top_mkvalt36m = mtemp2 if dist == 2
	replace top_mkvalt36m = mtemp3 if dist == 3		
	*
	egen mtemp4 = median(mkvalt) if index >=16 & dist == 1 & index != ., by(month)
	egen mtemp5 = median(mkvalt) if index >=16 & dist == 2 & index != ., by(month)
	egen mtemp6 = median(mkvalt) if index >=16 & dist == 3 & index != ., by(month)
	
	replace top_mkvalt_med36m = mtemp4 if dist == 1
	replace top_mkvalt_med36m = mtemp5 if dist == 2
	replace top_mkvalt_med36m = mtemp6 if dist == 3			
	
	
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
			replace quarter_w4_top = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace top_pf_ret36m = temp2 if dist >= 1 & dist <= 3

			egen temp3 = mean(obs1), by(month)
			replace top_n_obs36m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace top_n_obs36m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace top_n_obs36m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace top_n_obs36m = temp6 if dist == 4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff_top = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff_top if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff_top ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4_top if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace top_pf_ret36m_tc = top_pf_ret36m - tc if dist==1
			replace top_pf_ret36m_tc = top_pf_ret36m  if dist==2 | dist==3
		
	drop temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff ltemp1-ltemp12	mtemp1-mtemp6
	
	* Compute returns and number of observations for BOTTOM portfolio
	egen temp = mean(ret) if index <=8 & dist == 1 & index != .
	egen obs1 = count(ret) if index <=8 & dist == 1 & index != . 
	egen obs2 = count(ret) if index <=8 & dist == 2 & index != . 	
	egen obs3 = count(ret) if index <=8 & dist == 3 & index != . 
	egen obs4 = count(ret) if index <=8 & dist == 4 & index != .

	* Compute leverage ratios
	egen ltemp1 = mean(debt_at) if index <=8 & dist == 1 & index != ., by(month)
	egen ltemp2 = mean(debt_at) if index <=8 & dist == 2 & index != ., by(month)
	egen ltemp3 = mean(debt_at) if index <=8 & dist == 3 & index != ., by(month)
	
	replace bottom_debt_at36m = ltemp1 if dist == 1
	replace bottom_debt_at36m = ltemp2 if dist == 2
	replace bottom_debt_at36m = ltemp3 if dist == 3		
	*
	egen ltemp4 = mean(netdebt_at) if index <=8 & dist == 1 & index != ., by(month)
	egen ltemp5 = mean(netdebt_at) if index <=8 & dist == 2 & index != ., by(month)
	egen ltemp6 = mean(netdebt_at) if index <=8 & dist == 3 & index != ., by(month)
	
	replace bottom_netdebt_at36m = ltemp4 if dist == 1
	replace bottom_netdebt_at36m = ltemp5 if dist == 2
	replace bottom_netdebt_at36m = ltemp6 if dist == 3		
	*
	egen ltemp7 = mean(debt_mv) if index <=8 & dist == 1 & index != ., by(month)
	egen ltemp8 = mean(debt_mv) if index <=8 & dist == 2 & index != ., by(month)
	egen ltemp9 = mean(debt_mv) if index <=8 & dist == 3 & index != ., by(month)
	
	replace bottom_debt_mv36m = ltemp7 if dist == 1
	replace bottom_debt_mv36m = ltemp8 if dist == 2
	replace bottom_debt_mv36m = ltemp9 if dist == 3		
	*
	egen ltemp10 = mean(netdebt_cap) if index <=8 & dist == 1 & index != ., by(month)
	egen ltemp11 = mean(netdebt_cap) if index <=8 & dist == 2 & index != ., by(month)
	egen ltemp12 = mean(netdebt_cap) if index <=8 & dist == 3 & index != ., by(month)
	
	replace bottom_netdebt_cap36m = ltemp10 if dist == 1
	replace bottom_netdebt_cap36m = ltemp11 if dist == 2
	replace bottom_netdebt_cap36m = ltemp12 if dist == 3			
	
	* Compute average and median market cap
	egen mtemp1 = mean(mkvalt) if index <=8 & dist == 1 & index != ., by(month)
	egen mtemp2 = mean(mkvalt) if index <=8 & dist == 2 & index != ., by(month)
	egen mtemp3 = mean(mkvalt) if index <=8 & dist == 3 & index != ., by(month)
	
	replace bottom_mkvalt36m = mtemp1 if dist == 1
	replace bottom_mkvalt36m = mtemp2 if dist == 2
	replace bottom_mkvalt36m = mtemp3 if dist == 3		
	*
	egen mtemp4 = median(mkvalt) if index <=8 & dist == 1 & index != ., by(month)
	egen mtemp5 = median(mkvalt) if index <=8 & dist == 2 & index != ., by(month)
	egen mtemp6 = median(mkvalt) if index <=8 & dist == 3 & index != ., by(month)
	
	replace bottom_mkvalt_med36m = mtemp4 if dist == 1
	replace bottom_mkvalt_med36m = mtemp5 if dist == 2
	replace bottom_mkvalt_med36m = mtemp6 if dist == 3	
	
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
			replace quarter_w4_bottom = weight if dist ==4
			
			replace temp = ret * weight
			egen temp2 = sum(temp), by(month)	
			replace bottom_pf_ret36m = temp2 if dist >= 1 & dist <= 3

			egen temp3 = mean(obs1), by(month)
			replace bottom_n_obs36m = temp3 if dist == 1
			egen temp4 = mean(obs2), by(month)
			replace bottom_n_obs36m = temp4 if dist == 2
			egen temp5 = mean(obs3), by(month)			
			replace bottom_n_obs36m = temp5 if dist == 3
			egen temp6 = mean(obs4), by(month)			
			replace bottom_n_obs36m = temp6 if dist ==4
			
			*rebalancing costs
			by permno: gen w_diff = abs(weight - l.weight)
			by permno: replace w_diff = weight if temp3 !=. & l.weight ==.
			replace quarter_w_diff_bottom = w_diff if dist==4
			replace w_diff=. if dist==4
			replace w_diff=quarter_w_diff_bottom if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff_bottom ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4_bottom if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace bottom_pf_ret36m_tc = bottom_pf_ret36m - tc if dist==1
			replace bottom_pf_ret36m_tc = bottom_pf_ret36m if dist==2 | dist==3
		
	drop temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff ltemp1-ltemp12	mtemp1-mtemp6
	
		
	* Compute returns and number of observations for TOTAL portfolio
	egen temp = mean(ret) if dist == 1 & index != .
	egen obs1 = count(ret) if dist == 1 & index != . 
	egen obs2 = count(ret) if dist == 2 & index != . 	
	egen obs3 = count(ret) if dist == 3 & index != . 
	egen obs4 = count(ret) if dist == 4 & index != .
	
	* Compute leverage ratios
	egen ltemp1 = mean(debt_at) if dist == 1 & index != ., by(month)
	egen ltemp2 = mean(debt_at) if dist == 2 & index != ., by(month)
	egen ltemp3 = mean(debt_at) if dist == 3 & index != ., by(month)
	
	replace debt_at36m = ltemp1 if dist == 1
	replace debt_at36m = ltemp2 if dist == 2
	replace debt_at36m = ltemp3 if dist == 3		
	*
	egen ltemp4 = mean(netdebt_at) if dist == 1 & index != ., by(month)
	egen ltemp5 = mean(netdebt_at) if dist == 2 & index != ., by(month)
	egen ltemp6 = mean(netdebt_at) if dist == 3 & index != ., by(month)
	
	replace netdebt_at36m = ltemp4 if dist == 1
	replace netdebt_at36m = ltemp5 if dist == 2
	replace netdebt_at36m = ltemp6 if dist == 3		
	*
	egen ltemp7 = mean(debt_mv) if dist == 1 & index != ., by(month)
	egen ltemp8 = mean(debt_mv) if dist == 2 & index != ., by(month)
	egen ltemp9 = mean(debt_mv) if dist == 3 & index != ., by(month)
	
	replace debt_mv36m = ltemp7 if dist == 1
	replace debt_mv36m = ltemp8 if dist == 2
	replace debt_mv36m = ltemp9 if dist == 3		
	*
	egen ltemp10 = mean(netdebt_cap) if dist == 1 & index != ., by(month)
	egen ltemp11 = mean(netdebt_cap) if dist == 2 & index != ., by(month)
	egen ltemp12 = mean(netdebt_cap) if dist == 3 & index != ., by(month)
	
	replace netdebt_cap36m = ltemp10 if dist == 1
	replace netdebt_cap36m = ltemp11 if dist == 2
	replace netdebt_cap36m = ltemp12 if dist == 3			
	
	* Compute average and median market cap
	egen mtemp1 = mean(mkvalt) if dist == 1 & index != ., by(month)
	egen mtemp2 = mean(mkvalt) if dist == 2 & index != ., by(month)
	egen mtemp3 = mean(mkvalt) if dist == 3 & index != ., by(month)
	
	replace mkvalt36m = mtemp1 if dist == 1
	replace mkvalt36m = mtemp2 if dist == 2
	replace mkvalt36m = mtemp3 if dist == 3		
	*
	egen mtemp4 = median(mkvalt) if dist == 1 & index != ., by(month)
	egen mtemp5 = median(mkvalt) if dist == 2 & index != ., by(month)
	egen mtemp6 = median(mkvalt) if dist == 3 & index != ., by(month)
	
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
			replace w_diff=quarter_w_diff if dist==1 & tag2 ==1
			replace w_diff = weight if dist ==1 & quarter_w_diff ==. & tag2 ==1                                         // new
			replace w_diff = quarter_w4 if tag2 ==. & dist == 1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace pf_ret36m_tc = pf_ret36m - tc if dist==1
			replace pf_ret36m_tc = pf_ret36m if dist==2 | dist==3
			
	* Drop all variables of look and rerun three months later
	drop dist tag tag2 tag3 q_cash q_profit q_ret q_ivol cash_rank profit_rank ret_rank ivol_rank index temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff	ltemp1-ltemp12 mtemp1-mtemp6
	
	}
	
	drop quarter_w_diff_top quarter_w4_top quarter_w_diff_bottom quarter_w4_bottom quarter_w_diff quarter_w4
	
	save "$temp/sample_returns_all_36m_cashretprofitivol.dta", replace
	save "$data2/sample_returns_all_36m_cashretprofitivol.dta", replace	
	****************************************************************************
	
