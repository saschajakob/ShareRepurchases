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

	use "$data2/crsp_comp_clean2.dta", replace		

	sort permno month
	
	* browse permno year buyback ayear qdate cash_atl1 dvcl1 top10instown_percl1 ivol totalvalueauthorizedmil mkvalt

	**** Create/define sorting variables
	
	* 1: Cash to asset ratio. Take one year lag: cash_atl1 (this is the last cash-to-asset ratio observable based on annual data)
	
	* 2: Gross profitability. Take one year lag: grossprofit_atl1 (this is the last profitability ratio observable based on annual data)
	
	* 3: Past 6-months return: Take the cumulative return over the last six months: six_m_cum_ret
	
	* 4: Idiosyncratic volatility. Take the one month lag: ivoll1
	
	sort permno month
	local vlist ivol
		
	local vlistl1
	foreach v of local vlist {
		g `v'l1 = `v'[_n-1] if permno[_n]==permno[_n-1] & month[_n]==month[_n-1]+1
		local vlistl1 `vlistl1' `v'l1
		}	
	
	* Summarize the four sorting variables
	
	tabstat cash_atl1 grossprofit_atl1 six_m_cum_ret ivol, stats(n mean sd p50 min p25 p75 max) c(s)
	
*	drop if cash_atl1 == .
*	drop if grossprofit_atl1 == .
*	drop if six_m_cum_ret == .
*	drop if ivol == .
	
	distinct permno
	* 16,321 distinct securities
	
	
	**************************************************
	**** Beginn: Computation of portfolio returns ****
	**************************************************			
	
	**** Create a portfolio that moves forward at the quarterly frequency.
	
	g time2 = mofd(date)
	
	* We set December 1995 as the starting time	
	* December 2015 is time2 = 671
	* December 1994 is time2 = 419 - start portfolio on January 1995
	g pf_time = time2 - 419

	g pfret_ew = .
	g pfret_vw = .
	*
	g pf_obs = .
	*
	g pf_mkvalt = .
	g pf_mkvalt_med = .	
	*


	****************************************************************************
	
	***************** BUYBACKS OVER THE LAST 36 MONTHS**************************
	
	* quarter transition weight variable
	*g quarter_w_diff =.	
	*g quarter_w4=.	
	
	forval i = 0(3)252 {
	*forval i = 0(3)9 {
	
	g dist = pf_time - `i'
	

	* Create the index
	egen q_cash = xtile(cash_atl1) if dist==0, nq(5)		
	egen q_ivol = xtile(ivol) if dist==0, nq(5) 		
	egen q_ret = xtile(six_m_cum_ret) if dist == 0, nq(5) 	
	egen q_profit = xtile(grossprofit_atl1) if dist==0, nq(5)
	
	
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

	g ret_rank = .
	replace ret_rank = 1 if q_ret ==5
	replace ret_rank = 2 if q_ret ==4
	replace ret_rank = 3 if q_ret ==3
	replace ret_rank = 4 if q_ret ==2
	replace ret_rank = 5 if q_ret ==1		
	
	
	gen profit_rank =. 
	replace profit_rank = 1 if q_profit ==1
	replace profit_rank = 2 if q_profit ==2
	replace profit_rank = 3 if q_profit ==3
	replace profit_rank = 4 if q_profit ==4
	replace profit_rank = 5 if q_profit ==5		
	

	* Compute percentiles for the firms in the sample 
	_pctile mkvalt if dist == 0, percentiles(15, 30, 75)

	*g insample = 1 if dist == 0
	*g insample = 1 if mkvalt >= r(r3) 
	*g insample = 1 if mkvalt < r(r3) & mkvalt >=r(r2) 
	g insample = 1 if mkvalt < r(r2) & mkvalt >=r(r1) 
	*g insample = 1 if mkvalt < r(r1)
	
	sort permno month
	by permno: carryforward insample, replace
	

	gen index = cash_rank + profit_rank + ret_rank + ivol_rank if insample == 1
	
	sort permno month
	by permno: carryforward index, replace

	* Compute equal-weighted returns
	egen temp_ew = mean(ret) if index >=16 & dist == 1 & insample == 1
	
	* Compute value-weighted returns
	egen totmk = total(mkvalt) if index >=16 & dist == 1 & insample == 1
	g weight_vw = mkvalt/totmk
	g temp_vw = weight*ret	
	
	
	egen obs1 = count(ret) if index >=16 & dist == 1 & insample == 1 
	egen obs2 = count(ret) if index >=16 & dist == 2 & insample == 1	
	egen obs3 = count(ret) if index >=16 & dist == 3 & insample == 1
	
	* Compute average and median market cap
	egen mtemp1 = mean(mkvalt) if dist == 1 & insample == 1, by(month)
	egen mtemp2 = mean(mkvalt) if dist == 2 & insample == 1, by(month)
	egen mtemp3 = mean(mkvalt) if dist == 3 & insample == 1, by(month)
	
	replace pf_mkvalt = mtemp1 if dist == 1
	replace pf_mkvalt = mtemp2 if dist == 2
	replace pf_mkvalt = mtemp3 if dist == 3		
	*
	egen mtemp4 = median(mkvalt) if dist == 1 & insample == 1, by(month)
	egen mtemp5 = median(mkvalt) if dist == 2 & insample == 1, by(month)
	egen mtemp6 = median(mkvalt) if dist == 3 & insample == 1, by(month)
	
	replace pf_mkvalt_med = mtemp4 if dist == 1
	replace pf_mkvalt_med = mtemp5 if dist == 2
	replace pf_mkvalt_med = mtemp6 if dist == 3			
	
	
	tsset permno month
	replace obs1 =. if ret ==. 
	
	* Equal-weighted returns
	gen weight_ew =.
	
	*** first month ***
	replace weight_ew = 1/obs1 if dist == 1 
	*** second month ***
	replace weight_ew = l.weight_ew*(1+l.ret) if dist ==2 
	egen t1_weight_ew = total(weight_ew) if dist == 2, by(month)
	replace weight_ew = weight_ew/t1_weight_ew if dist == 2 
	*** third month ***
	replace weight_ew = l.weight_ew*(1+l.ret) if dist ==3 
	egen t2_weight_ew = total(weight_ew) if dist == 3, by(month)
	replace weight_ew = weight_ew/t2_weight_ew if dist == 3
	*** next quarter ***
	*replace weight = 1/obs1 if dist == 4
	*replace quarter_w4 = weight if dist ==4
	
	replace temp_ew = ret * weight_ew
	egen temp2_ew = sum(temp_ew), by(month)	
	replace pfret_ew = temp2_ew if dist >= 1 & dist <= 3
			
	* Value-weighted returns			
	
	*** second month ***	
	replace weight_vw = l.weight_vw*(1+l.ret) if dist ==2 
	egen t1_weight_vw = total(weight_vw) if dist == 2, by(month)
	replace weight_vw = weight_vw/t1_weight_vw if dist == 2 
	*** third month ***
	replace weight_vw = l.weight_vw*(1+l.ret) if dist ==3 
	egen t2_weight_vw = total(weight_vw) if dist == 3, by(month)
	replace weight_vw = weight_vw/t2_weight_vw if dist == 3
	*** next quarter ***
	*egen totmk2 = total(mkvalt) if dist == 4		
	*replace weight = mkvalt/totmk2 if dist == 4
	*replace quarter_w4 = weight if dist ==4
			
	replace temp_vw = ret * weight_vw
	egen temp2_vw = sum(temp_vw), by(month)	
	replace pfret_vw = temp2_vw if dist >= 1 & dist <= 3			
			
			
	* Number of observations			
	egen temp3 = mean(obs1), by(month)
	replace pf_obs = temp3 if dist == 1
	egen temp4 = mean(obs2), by(month)
	replace pf_obs = temp4 if dist == 2
	egen temp5 = mean(obs3), by(month)			
	replace pf_obs = temp5 if dist == 3
			
						
/*			
			
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
*/
	
	* Drop all variables of look and rerun three months later	
	drop dist q_cash q_profit q_ret q_ivol cash_rank profit_rank ret_rank ivol_rank index insample temp_ew temp_vw totmk temp2_ew temp2_vw temp3 temp4 temp5 mtemp1-mtemp6 obs1 obs2 obs3 weight_ew weight_vw t1_weight_ew t1_weight_vw t2_weight_ew t2_weight_vw  
		
	}
	
	*drop quarter_w_diff_top quarter_w4_top quarter_w_diff_bottom quarter_w4_bottom quarter_w_diff quarter_w4
	
	save "$temp/pf_mid_crsp.dta", replace
	save "$data2/pf_mid_crsp.dta", replace	
	****************************************************************************
	
