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
	
	browse permno year buyback ayear qdate cash_atl1 dvcl1 top10instown_percl1 ivol totalvalueauthorizedmil mkvalt

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
	
	***************************************************************************
	***************** Rolling portfolio construction **************************
	***************************************************************************
	
	****************** BUYBACKS OVER THE LAST 12 MONTHS************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w_diff_bottom =.
	g quarter_w_diff =.	
	
	
	forval i = 0(3)252 {
	
	g dist = pf_time - `i'
	*g dist = pf_time - 0
	
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
			replace w_diff=quarter_w_diff_top if dist==1
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
			replace w_diff=quarter_w_diff_bottom if dist==1
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
			replace w_diff=quarter_w_diff if dist==1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace pf_ret12m_tc = pf_ret12m - tc if dist==1
			replace pf_ret12m_tc = pf_ret12m if dist==2 | dist==3
		
	* Drop all variables of look and rerun three months later
	drop dist tag tag2 tag3 q_cash q_ivol q_owner q_payout cash_rank ivol_rank owner_rank payout_rank index temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff
	
	}
	
	drop quarter_w_diff_top quarter_w_diff_bottom quarter_w_diff
	save "$temp/sample_returns_all_12m.dta", replace
	***************************************************************************

	**************** BUYBACKS OVER THE LAST 24 MONTHS**************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w_diff_bottom =.
	g quarter_w_diff =.	
	
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
			replace w_diff=quarter_w_diff_top if dist==1
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
			replace w_diff=quarter_w_diff_bottom if dist==1
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
			replace w_diff=quarter_w_diff if dist==1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace pf_ret24m_tc = pf_ret24m - tc if dist==1
			replace pf_ret24m_tc = pf_ret24m if dist==2 | dist==3
	
	* Drop all variables of look and rerun three months later
	drop dist tag tag2 tag3 q_cash q_ivol q_owner q_payout cash_rank ivol_rank owner_rank payout_rank index temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff
	
	}
	
	drop quarter_w_diff_top quarter_w_diff_bottom quarter_w_diff
	save "$temp/sample_returns_all_24m.dta", replace	
	****************************************************************************
	
	***************** BUYBACKS OVER THE LAST 36 MONTHS**************************
	
	* quarter transition weight variable
	g quarter_w_diff_top =.
	g quarter_w_diff_bottom =.
	g quarter_w_diff =.	
	
	forval i = 0(3)252 {
	
	g dist = pf_time - `i'
	
	g tag = 1 if buyback == 1 & dist <= 0 & dist >= -36
	egen tag2 = mean(tag), by(permno)
	g tag3 = 1 if dist <= 0 & dist >= -36 & tag2 == 1
	
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
			replace weight = 1/obs1 if dist == 4
			
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
			replace w_diff=quarter_w_diff_top if dist==1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace top_pf_ret36m_tc = top_pf_ret36m - tc if dist==1
			replace top_pf_ret36m_tc = top_pf_ret36m  if dist==2 | dist==3
		
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
			replace weight = 1/obs1 if dist == 4
			
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
			replace w_diff=quarter_w_diff_bottom if dist==1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace bottom_pf_ret36m_tc = bottom_pf_ret36m - tc if dist==1
			replace bottom_pf_ret36m_tc = bottom_pf_ret36m if dist==2 | dist==3
		
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
			replace weight = 1/obs1 if dist == 4
			
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
			replace w_diff=quarter_w_diff if dist==1
			egen tw_diff = total(w_diff), by(month)
			replace tw_diff =1 if pf_time ==1 & pf_time == dist
			gen tc = tw_diff *0.0020																					// assuming transaction costs of 20 basis points
			replace pf_ret36m_tc = pf_ret36m - tc if dist==1
			replace pf_ret36m_tc = pf_ret36m if dist==2 | dist==3
			
	* Drop all variables of look and rerun three months later
	drop dist tag tag2 tag3 q_cash q_ivol q_owner q_payout cash_rank ivol_rank owner_rank payout_rank index temp temp2 temp3 temp4 temp5 temp6 obs1 obs2 obs3 obs4 weight t1_weight t2_weight tc tw_diff w_diff	
	
	}
	
	drop quarter_w_diff_top quarter_w_diff_bottom quarter_w_diff
	save "$temp/sample_returns_all_36m.dta", replace
	****************************************************************************
	
	use "$temp/sample_returns_all_36m.dta", clear
	
	*** saving complete data set
	save "$data2/sample_returns_all.dta", replace	
	****************************************************************************

	
	************************************
	**** Begin performance analysis ****
	************************************

	use "$data2/sample_returns_all.dta", clear
	* There are 11'327 buybacks in the data set
	distinct dealnumber
	
	* Collapse the data to the portfolio level
	collapse qdate pf_ret12m top_pf_ret12m bottom_pf_ret12m n_obs12m top_n_obs12m bottom_n_obs12m pf_ret24m top_pf_ret24m bottom_pf_ret24m /*
	*/	n_obs24m top_n_obs24m bottom_n_obs24m pf_ret36m top_pf_ret36m bottom_pf_ret36m n_obs36m top_n_obs36m bottom_n_obs36m /*
	*/ pf_ret12m_tc top_pf_ret12m_tc bottom_pf_ret12m_tc pf_ret24m_tc top_pf_ret24m_tc bottom_pf_ret24m_tc /*
	*/ pf_ret36m_tc top_pf_ret36m_tc bottom_pf_ret36m_tc /*
	*/ rf mktrf smb hml umd cma rmw ps_vwf sprtrn (sum) buyback, by(month)
	
	* Drop observations with missing returns
	*drop if pf_ret12m == .
	* Drop observations after December 2015
	
		keep if month >= 419	// December 1994
		drop if month > 671
	
	**** Figure showing the number of share buybacks by month
	two bar buyback month, color(sand) tlabel(1995m1(60)2015m1, labsize(small)) ylabel(#6, angle(horizontal) labsize(small)) xtitle("Year-month", size(small)) yline(0) ytitle("Number of buyback announcements", size(small)) /// 
		legend(off)  title("Number of share buyback announcements over time", size(small)) scheme(s1mono)		
	
	**** Additional filters
	* Keep observations from January 2000 onwards
	*	keep if month >= 480  //2000
		keep if month >= 588  //2009
	
	**** Generate additional return variables
	* Generate excess returns over risk-free rate
	g pf_ret12mrf = pf_ret12m - rf
	g pf_ret24mrf = pf_ret24m - rf
	g pf_ret36mrf = pf_ret36m - rf	
	
	g top_pf_ret12mrf = top_pf_ret12m - rf
	g top_pf_ret24mrf = top_pf_ret24m - rf	
	g top_pf_ret36mrf = top_pf_ret36m - rf	
	
	g bottom_pf_ret12mrf = bottom_pf_ret12m - rf	
	g bottom_pf_ret24mrf = bottom_pf_ret24m - rf
	g bottom_pf_ret36mrf = bottom_pf_ret36m - rf	

	g ret_sp500rf = sprtrn - rf
	
	* Generate excess returns over S&P500	
	g pf_ret12msp = pf_ret12m - sprtrn
	g pf_ret24msp = pf_ret24m - sprtrn
	g pf_ret36msp = pf_ret36m - sprtrn	
	
	g top_pf_ret12msp = top_pf_ret12m - sprtrn
	g top_pf_ret24msp = top_pf_ret24m - sprtrn	
	g top_pf_ret36msp = top_pf_ret36m - sprtrn	
	
	g bottom_pf_ret12msp = bottom_pf_ret12m - sprtrn	
	g bottom_pf_ret24msp = bottom_pf_ret24m - sprtrn
	g bottom_pf_ret36msp = bottom_pf_ret36m - sprtrn		
	
	
	**** Display descriptive statistisc
	* Show distributinal characteristics of monthly portfolio returns
	tabstat top_pf_ret12m top_pf_ret24m top_pf_ret36m sprtrn, stats(n mean sd min med max iqr sk k) c(s)
	tabstat pf_ret12m pf_ret24m pf_ret36m sprtrn, stats(n mean sd min med max iqr sk k) c(s)
	
	* Compute annualized volatility
	egen sd_top_pf_ret12m = sd(top_pf_ret12m)
	egen sd_top_pf_ret24m = sd(top_pf_ret24m)
	egen sd_top_pf_ret36m = sd(top_pf_ret36m)
	egen sd_ret_sp500 = sd(ret_sp500)		
	
	g sda_pf_ret12m = sd_top_pf_ret12m*sqrt(12)
	g sda_pf_ret24m = sd_top_pf_ret24m*sqrt(12)
	g sda_pf_ret36m = sd_top_pf_ret36m*sqrt(12)
	g sda_ret_sp500 = sd_ret_sp500*sqrt(12)		
	
	* Display annualized standard deviation of portfolio returns
	tabstat sda_pf_ret12m sda_pf_ret24m sda_pf_ret36m sda_ret_sp500, stats(mean)  c(s)	
	
	**** SHARPE RATIO
	* Compute Sharpe ratios
	egen mean_top_pf_ret12mrf = mean(top_pf_ret12mrf)
	egen mean_top_pf_ret24mrf = mean(top_pf_ret24mrf)
	egen mean_top_pf_ret36mrf = mean(top_pf_ret36mrf)	
	egen mean_ret_sp500rf = mean(ret_sp500rf)	
	
	egen sd_top_pf_ret12mrf = sd(top_pf_ret12mrf)
	egen sd_top_pf_ret24mrf = sd(top_pf_ret24mrf)
	egen sd_top_pf_ret36mrf = sd(top_pf_ret36mrf)
	egen sd_ret_sp500rf = sd(ret_sp500rf)	
	
	g sr_top_pf_ret12mrf = mean_top_pf_ret12mrf/sd_top_pf_ret12mrf
	g sr_top_pf_ret24mrf = mean_top_pf_ret24mrf/sd_top_pf_ret24mrf
	g sr_top_pf_ret36mrf = mean_top_pf_ret36mrf/sd_top_pf_ret36mrf	
	g sr_ret_sp500rf = mean_ret_sp500rf/sd_ret_sp500rf	
	
	replace sr_top_pf_ret12mrf = sr_top_pf_ret12mrf*sqrt(12)
	replace sr_top_pf_ret24mrf = sr_top_pf_ret24mrf*sqrt(12)
	replace sr_top_pf_ret36mrf = sr_top_pf_ret36mrf*sqrt(12)	
	replace sr_ret_sp500rf = sr_ret_sp500rf*sqrt(12)	
		
	* Display Sharpe ratios and annualized vol
	tabstat sr_top_pf_ret12mrf sr_top_pf_ret24mrf sr_top_pf_ret36mrf sr_ret_sp500rf, stats(mean)  c(s)
	
	*** VaR
	egen mean_top_pf_ret12m = mean(top_pf_ret12m)
	egen mean_top_pf_ret24m = mean(top_pf_ret24m)
	egen mean_top_pf_ret36m = mean(top_pf_ret36m)	
	egen mean_ret_sp500 = mean(ret_sp500)
	
	g VaR_top_pf_ret12m = -1.645 * sd_top_pf_ret12m + mean_top_pf_ret12m
	g VaR_top_pf_ret24m = -1.645 * sd_top_pf_ret24m + mean_top_pf_ret24m
	g VaR_top_pf_ret36m = -1.645 * sd_top_pf_ret36m + mean_top_pf_ret36m
	g VaR_ret_sp500 = -1.645 * sd_ret_sp500 + mean_ret_sp500
	
	* Display monthly Value-at-Risk
	tabstat VaR_top_pf_ret12m VaR_top_pf_ret24m VaR_top_pf_ret36m VaR_ret_sp500, stats(mean)  c(s)
	
	
	**** INFORMATION RATIO
	* Compute Information ratio using the S&P500 as benchmark
	egen mean_top_pf_ret12msp = mean(top_pf_ret12msp)
	egen mean_top_pf_ret24msp = mean(top_pf_ret24msp)
	egen mean_top_pf_ret36msp = mean(top_pf_ret36msp)	
	
	egen sd_top_pf_ret12msp = sd(top_pf_ret12msp)
	egen sd_top_pf_ret24msp = sd(top_pf_ret24msp)
	egen sd_top_pf_ret36msp = sd(top_pf_ret36msp)		
	
	g ir_top_pf_ret12msp = mean_top_pf_ret12msp/sd_top_pf_ret12msp
	g ir_top_pf_ret24msp = mean_top_pf_ret24msp/sd_top_pf_ret24msp
	g ir_top_pf_ret36msp = mean_top_pf_ret36msp/sd_top_pf_ret36msp	
	
	replace ir_top_pf_ret12msp = ir_top_pf_ret12msp*sqrt(12)
	replace ir_top_pf_ret24msp = ir_top_pf_ret24msp*sqrt(12)
	replace ir_top_pf_ret36msp = ir_top_pf_ret36msp*sqrt(12)		
	
	tabstat ir_top_pf_ret12msp ir_top_pf_ret24msp ir_top_pf_ret36msp, stats(mean)  c(s)	
	
	
	**** ESTIMATE MARKET BETAS
	* Compute beta using S&P500
	
	g top_beta12m = .
	g top_beta24m = .
	g top_beta36m = .
	
	reg top_pf_ret12m sprtrn
	replace top_beta12m = _b[sprtrn]
	
	reg top_pf_ret24m sprtrn
	replace top_beta24m = _b[sprtrn]
	
	reg top_pf_ret36m sprtrn	
	replace top_beta36m = _b[sprtrn]
	
	* Display betas
	tabstat top_beta12m top_beta24m top_beta36m, stats(mean)  c(s)	
	
	
	**** COMPUTE THE MAX DRAWDOWNS PER PORTFOLIO
	* Compute the max drawdown
	* 1. Compute cumulative returns
	* Top 12m portfolio
	egen month_id = group(month)
	tsset month_id
	g cum_top_pf_ret12m = .
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
	
	* Top 36m portfolio
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
	tabstat max_drawdown_top_pf_ret12m max_drawdown_top_pf_ret24m max_drawdown_top_pf_ret36m max_drawdown_sp500, stats(mean)  c(s)		
	
	
	**** ESTIMATE ALPHAS USING MULTIFACTOR MODEL


*	global controls = "mktrf smb hml umd" 
	global controls = "mktrf smb hml umd rmw cma ps_vwf" 
	
	reg pf_ret12m $controls 
	est store full_12m
		
	reg pf_ret24m $controls
	est store full_24m
		
	reg pf_ret36m $controls
	est store full_36m
		
	reg top_pf_ret12m $controls 
	est store top_12m
		
	reg top_pf_ret24m $controls
	est store top_24m
		
	reg top_pf_ret36m $controls
	est store top_36m
		
	reg bottom_pf_ret12m $controls
	est store bottom_12m
		
	reg bottom_pf_ret24m $controls
	est store bottom_24m
		
	reg bottom_pf_ret36m $controls
	est store bottom_36m
	
	estout full_12m full_24m full_36m top_12m top_24m top_36m bottom_12m bottom_24m bottom_36m using "$temp/results_alpha_ret.txt", replace 	///
		   mlabels("full12m" "full24m" "full36m" "top12m" "top24m" "top36m" "bot12m" "bot24m" "bot36m") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 
	
	*global controls = "mktrf smb hml umd" 
	global controls = "mktrf smb hml umd rmw cma ps_vwf" 
	
	reg pf_ret12mrf $controls 
	est store full_12m
		
	reg pf_ret24mrf $controls
	est store full_24m
		
	reg pf_ret36mrf $controls
	est store full_36m
		
	reg top_pf_ret12mrf $controls 
	est store top_12m
		
	reg top_pf_ret24mrf $controls
	est store top_24m
		
	reg top_pf_ret36mrf $controls
	est store top_36m
		
	reg bottom_pf_ret12mrf $controls
	est store bottom_12m
		
	reg bottom_pf_ret24mrf $controls
	est store bottom_24m
		
	reg bottom_pf_ret36mrf $controls
	est store bottom_36m
	
	estout full_12m full_24m full_36m top_12m top_24m top_36m bottom_12m bottom_24m bottom_36m using "$temp/results_alpha_retrf.txt", replace 	///
		   mlabels("full12m" "full24m" "full36m" "top12m" "top24m" "top36m" "bot12m" "bot24m" "bot36m") num  ///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table. Dependent variable is the portfolio return.") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
	
	
	*** plotting perfomance indexed at 100 ***
	egen month_id = group(month)

	g ri36m = .
*	replace ri36m = 100 if month == 419
	replace ri36m = ln(1+pf_ret36m)
	gen temp =.
	forvalues x =1(1)252{
		 sum pf_ret36m if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*`x' if month_id == `x'
	}
	replace ri36m = 100*(1+(exp(temp)-1))
	drop temp
	
	
	g top_ri36m = .
	replace top_ri36m = ln(1+top_pf_ret36m)
	gen temp =.
	forvalues x =1(1)252{
		 sum top_pf_ret36m if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*`x' if month_id == `x'
	}
	replace top_ri36m = 100*(1+(exp(temp)-1))
	drop temp
	
	
	g risp500 = ln(1+sprtrn)
	gen temp =.
	forvalues x =1(1)252{
		 sum sprtrn if month_id > 0 & month_id <= `x'
		 replace temp = r(mean)*`x' if month_id == `x'
	}
	replace risp500 = 100*(1+(exp(temp)-1))
	drop temp	
	
	g month2 = 
	
	* Plot regular scale
	twoway (line /*ri36m*/ top_ri36m risp500 month, sort)
	
	* Plot log scale
	line /*ri36m*/ top_ri36m risp500 month, yscale(log)
	
	
	
	twoway (line top_n_obs12m top_n_obs24m top_n_obs36m month, sort)
	
	twoway (line top_n_obs36m month, sort)
