


*************************************
*
* IBES DATA
*
*************************************


* Office desktop
	global 	data	"C:\Users\valta\Dropbox\work\SBB\Data"
	global 	comp	"C:\Users\valta\Dropbox\work\CFshocks_investment\data"	
    global  temp	"C:/Users/valta/Desktop/temp"	
	
* Laptop
	global 	data	"C:\Users\Philip Valta\Dropbox\work\SBB\Data"
	global 	comp	"C:\Users\Philip Valta\Dropbox\work\CFshocks_investment\data"	
    global  temp	"C:/Users/Philip Valta/Desktop/temp"	
	
*********************************
**** Begin: Utility Programs ****
*********************************

**** Winsor program (winsor varname [1,5])
  capture program drop winsor
  program define winsor
    quiet sum `1', detail
    replace `1' = r(p1)  if `1' ~= . & (`1' < r(p1))  & `2' == 1
    replace `1' = r(p99) if `1' ~= . & (`1' > r(p99)) & `2' == 1
    replace `1' = r(p5)  if `1' ~= . & (`1' < r(p5))  & `2' == 5
    replace `1' = r(p95) if `1' ~= . & (`1' > r(p95)) & `2' == 5
  end
  
*******************************
**** End: Utility Programs ****
*******************************
  
  
  use "$data/IBES_1990_2015.dta", clear
  
 **** Clean and prepare the IBES data
  
  drop if cusip == ""
*  drop if fpedats == .
  drop if measure == "ENT"  
  
  drop if ticker =="CAD"
  drop if ticker =="SAP5"
  drop if ticker == "DOWI"
  drop if ticker == "SAP4"
  keep if curcode == "USD"
  keep if usfirm == 1
  
  keep ticker cusip cname statpers fpedats measure fpi meanest
  
  keep if measure == "EPS" 	
  drop if fpi == "5"
  drop if fpi == "4" 
  
  sort cusip fpi statpers
  
  * Reshape the data
  cd "$temp"
  save tmp1, replace
  
  use tmp1, clear
  keep ticker cusip cname statpers fpi meanest
  keep if fpi == "1"
  duplicates drop cusip statpers, force
  sort cusip statpers
  rename meanest feps1
  drop fpi
  save tmp2, replace
  
  use tmp1, clear
  keep ticker cusip cname statpers fpi meanest 
  keep if fpi == "2"
  duplicates drop cusip statpers, force
  sort cusip statpers
  rename meanest feps2
  drop fpi
  save tmp3, replace
  
  use tmp1, clear
  keep ticker cusip cname statpers fpi meanest
  keep if fpi == "3"
  duplicates drop cusip statpers, force
  sort cusip statpers
  rename meanest feps3
  drop fpi 
  save tmp4, replace 
  
  use tmp1, clear
  keep ticker cusip cname statpers fpi meanest
  keep if fpi == "0"
  duplicates drop cusip statpers, force 
  sort cusip statpers
  rename meanest epsltg
  drop fpi  
  save tmp5, replace  

  merge 1:1 cusip statpers using tmp2.dta
*  keep if _merge == 3
  drop _merge
  
  merge 1:1 cusip statpers using tmp3.dta
*  keep if _merge == 3
  drop _merge
  
  merge 1:1 cusip statpers using tmp4.dta 
  drop _merge
  
  * Drop firms for which there is no feps1 available
  drop if feps1 == .
  
  * Subsitute missing EPS forecasts for the previous horizon forecast and compound at long-term growth rate
  replace feps2 = feps1*(1+epsltg/100) if feps2 == .
  replace feps2 = feps1 if feps2 == .
  
  replace feps3 = feps2*(1+epsltg/100) if feps3 == .
  replace feps3 = feps2 if feps3 == .
 
 
  g yr = year(statpers) 
  g mth = month(statpers) 
 
  
  * Merge with yield and MRP from Societe Generale
  cd "$data"
  sort yr mth
  merge m:1 yr mth using socgen.dta
  keep if _merge == 3
  drop _merge
 
   * Merge with FF factors
  cd "$data"
  sort yr mth
  merge m:1 yr mth using ff_factors.dta
  keep if _merge == 3
  drop _merge
 
  cd "$temp"
  save tmp_ibes.dta, replace
 
 
    cd "$data"
  
    use CRSP_monthly_1980_2016.dta, replace 
   
    keep if shrcd == 10 | shrcd == 11
   
    duplicates drop cusip date, force
   
    g mth = month(date)
    g yr = year(date)
   
    g price = abs(prc)
   
    replace price = price/cfacpr
   
    keep cusip yr mth price ret
   
    sort cusip yr mth
   
    merge m:m yr mth using ff_factors.dta
    keep if _merge == 3
    drop _merge
   
    cd "$temp" 
    save crsp_price.dta, replace
 
 
 
  * Merge with CRSP prices data
  use tmp_ibes.dta
  sort cusip yr mth
  merge 1:1 cusip yr mth using crsp_price.dta
  keep if _merge == 3
  drop _merge
  save tmp_ibes.dta, replace 
 
 
 * Get Compustat data
  	use	tic gvkey cusip datadate fyear fyr at bkvlps dvpsx_f dvpsp_f adjex_f epsfi epsfx epspi epspx ni ceq seq teq using "$comp\compann_1970_2015.dta", clear 
	
	drop gvkey
	g str8 tmp = cusip
	drop cusip
	rename tmp cusip
	
	duplicates drop cusip fyear, force
	egen cusip_id = group(cusip)
	g yr	= year(datadate)
	duplicates drop cusip yr, force
	
	tsset cusip_id fyear
	
	* Actual and lagged book value per share
	g bps0 = bkvlps/adjex_f
*	winsor bps0 1
	drop if bps0 < 0
	g bpsl1 = l1.bps0
	g bpsf1_eff = f1.bps0
	g bpsf2_eff = f2.bps0	
	
	drop if bps0 ==.
	
	* Actual dividend per share
	g dps0 = dvpsx_f/adjex_f
	
	* Actual earnings per share
	g eps0 = epspx/adjex_f
	g epsd0 = epsfx/adjex_f
	

	winsor dps0 1
	winsor eps0 1
	winsor epsd0 1
	
	
	* Ratio k
	
	g k = dps0/eps0
	g kd = dps0/epsd0
	
	replace k = dps0/(0.06*at) if eps0 <=0
	replace kd = dps0/(0.06*at) if epsd0 <=0	
	drop if k > 1
	drop if kd > 1	
*	replace k = 1 if k > 1

	g roe1 = ni/ceq
	g roe2 = ni/seq
	g roe3 = ni/teq
	
	winsor roe1 1
	winsor roe2 1
	winsor roe3 1
		
	keep tic cusip datadate yr fyear bps0 bpsl1 dps0 eps0 epsd0 k kd bpsf1_eff bpsf2_eff roe1 roe2 roe3

	sort cusip yr
	
	save tmp_comp.dta, replace
  
    * Merge IBES and Compustat
    use tmp_ibes.dta, clear
  
	sort yr
	merge m:1 cusip yr using tmp_comp.dta
	keep if _merge == 3
	drop _merge
	
	save tmp_ibes_comp.dta, replace
    
  * Get CRSP data and compute betas
  
  
    use crsp_price.dta, clear 
   
    drop if ret == .
  
    egen n_obs = count(ret), by(cusip)
   
    drop if n_obs < 60
   
*   merge m:1 yr mth using Tbill90days.dta
*   drop _merge
   
    egen cusip_id = group(cusip)    
    egen yr_mth = group(yr mth)   
   
    sort cusip_id yr_mth
    tsset cusip_id yr_mth
   
    replace ret = ret*100
    winsor ret 1   
   
    g retrf = ret-rf
   
    tabstat retrf mktrf smb hml, stats(n mean p50 sd min max) c(s)
   
    rangestat (reg) retrf mktrf smb hml, interval(yr_mth -59 0) by(cusip_id)

    save betas.dta, replace
    cd "$temp"
   
    use betas.dta, clear
    drop reg_nobs reg_r2 reg_adj_r2 b_smb b_hml b_cons se_mktrf se_smb se_hml se_cons
   
    rename b_mktrf beta_ff
   
    sort cusip_id yr_mth
    tsset cusip_id yr_mth  
    rangestat (reg) retrf mktrf, interval(yr_mth -59 0) by(cusip_id)
  
    rename b_mktrf beta_mkt
   
    keep if reg_nobs >=30

    keep cusip yr mth mktrf smb hml rf retrf beta_ff beta_mkt
   
    tabstat beta_ff beta_mkt, stats(n mean p50 sd min max) c(s)
   
    sort cusip yr mth
   
    save betas.dta, replace
   
    * Calculate values based on residual income model 
    cd "$temp"   
    use tmp_ibes_comp.dta, clear
	
	sort cusip yr mth
	
	merge 1:1 cusip yr mth using betas.dta
	keep if _merge == 3
	drop _merge
		
	sort yr mth
	merge m:1 yr mth using "$data\3monthTbill.dta"
	keep if _merge == 3
	drop _merge
	
	sort cusip yr mth

	* Basic
	g bpsf1 = bps0 + (1-k)*feps1
	g bpsf2 = bpsf1 + (1-k)*feps2	
	
	* Diluted
*	g bpsdf1 = bps0 + (1-kd)*feps1
*	g bpsdf2 = bpsf1 + (1-kd)*feps2	
	
	* Basic
	g avg_bps10 = (bpsl1+bps0)/2
	g avg_bps01 = (bps0+bpsf1)/2	
	g avg_bps12 = (bpsf1+bpsf2)/2	

	* Effective
	g avg_bps01_eff = (bps0+bpsf1_eff)/2	
	g avg_bps12_eff = (bpsf1_eff+bpsf2_eff)/2		
	
	
	* Diluted
*	g avg_bpsd10 = (bpsl1+bps0)/2
*	g avg_bpsd01 = (bps0+bpsdf1)/2	
*	g avg_bpsd12 = (bpsdf1+bpsdf2)/2	
	
	* Basic
	g froe1 = feps1/avg_bps10
	g froe2 = feps2/avg_bps01
	g froe3 = feps3/avg_bps12	

	g froe1_eff = feps1/avg_bps10
	g froe2_eff = feps2/avg_bps01_eff
	g froe3_eff = feps3/avg_bps12_eff
	
	* Diluted
*	g froed1 = feps1/avg_bpsd10
*	g froed2 = feps2/avg_bpsd01
*	g froed3 = feps3/avg_bpsd12
		
	replace mktrf = mktrf/100
	replace rf = rf/100
	replace tb3ms = tb3ms/100
	g re1_ff = tb3ms + beta_ff*mrp_sg
	g re2_ff = lt10y + beta_ff*mrp_sg
	
	replace re1_ff = 0.05 if re1_ff < 0.05
	replace re1_ff = 0.2 if re1_ff > 0.2	

	replace re2_ff = 0.05 if re2_ff < 0.05
	replace re2_ff = 0.2 if re2_ff > 0.2		
	
*	replace re_ff = 0 if re_ff < 0
	
	g vtA1 = ((froe1 - re1_ff)*bps0)/(1+re1_ff)
	g vtA2 = ((froe2 - re1_ff)*bpsf1)/(1+re1_ff)^2
	g vtA3 = ((froe3 - re1_ff)*bpsf2)/((1+re1_ff)^2*re1_ff)
	g vt_tb3 = vtA1 + vtA2 + vtA3
 
	g vtB1 = ((froe1 - re2_ff)*bps0)/(1+re2_ff)
	g vtB2 = ((froe2 - re2_ff)*bpsf1)/(1+re2_ff)^2
	g vtB3 = ((froe3 - re2_ff)*bpsf2)/((1+re2_ff)^2*re2_ff)
	g vt_10y = vtB1 + vtB2 + vtB3
 	
	replace vt_tb3 = 0 if vt_tb3 < 0 & vt_tb3 != .
	replace vt_10y = 0 if vt_10y < 0 & vt_10y != .
		
	g vp_tb3 = vt_tb3/price
	g vp_10y = vt_10y/price 
 
	g vtA1_eff = ((froe1_eff - re1_ff)*bps0)/(1+re1_ff)
	g vtA2_eff = ((froe2_eff - re1_ff)*bpsf1_eff)/(1+re1_ff)^2
	g vtA3_eff = ((froe3_eff - re1_ff)*bpsf2_eff)/((1+re1_ff)^2*re1_ff)
	g vt_tb3_eff = vtA1_eff + vtA2_eff + vtA3_eff
 
	g vtB1_eff = ((froe1_eff - re2_ff)*bps0)/(1+re2_ff)
	g vtB2_eff = ((froe2_eff - re2_ff)*bpsf1_eff)/(1+re2_ff)^2
	g vtB3_eff = ((froe3_eff - re2_ff)*bpsf2_eff)/((1+re2_ff)^2*re2_ff)
	g vt_10y_eff = vtB1_eff + vtB2_eff + vtB3_eff


	replace vt_tb3_eff = 0 if vt_tb3_eff < 0 & vt_tb3_eff != .
	replace vt_10y_eff = 0 if vt_10y_eff < 0 & vt_10y_eff != .	
	
	g vp_tb3_eff = vt_tb3_eff/price
	g vp_10y_eff = vt_10y_eff/price  
 
 	winsor vp_tb3		1
	winsor vp_10y		1
*	winsor vp_tb3_eff	1
*	winsor vp_10y_eff	1
	
	
    tabstat vp_tb3 vp_10y vp_tb3_eff vp_10y_eff, stats(n mean p50 sd min p1 p99 max) c(s)	
	
	cd "$data"
    save ResInc_20170802.dta, replace
	
    use ResInc_20170802.dta, clear
	
	sort cusip fyear
	merge m:1 cusip fyear using Compustat_sample.dta
	keep if _merge == 3
	drop _merge
	
	save ResInc_sample.dta, replace
	
*************************
**** Begin: Analysis ****
*************************	
	cd "$data2"		
    use ResInc_sample.dta, clear	
	
	
	* Compute the ratio of ROE to cost of equity

	g roe_re = roe/re2_ff
	
	* Compute the difference between ROE and the cost of equity
	
	g roe_minus_re = roe - re2_ff
	
	
	
	g sr_dummy = 1 if sr_index >0
	replace sr_dummy = 0 if sr_index <=0	
	replace sr_dummy = . if sr_index == .

	* Descriptive statistics
	
	tabstat roe re2_ff roe_re vp_10y net_rep_me net_rep2_me rep buy buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, stats(mean n sd p5 p50 p95) c(s)
	
	bysort buy: su mb, d
	
	
	
	* Repurchase variables are rep, net_rep_me, net_rep2_me, buy, buy2
	
	* Work with data at the annual level?
	
	pwcorr roe_re rep net_rep_me net_rep2_me buy buy2 mb vp_10y, sig
	
	* Explain roe_re
	reg roe_re net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg roe_re net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg roe_re rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg roe_re buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)		
	reg roe_re buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	
	reghdfe roe_re net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)	
	reghdfe roe_re net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe roe_re rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe roe_re buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe roe_re buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)

	reghdfe roe_re net_rep_mel1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)	
	est store reg1
	reghdfe roe_re net_rep2_mel1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)
	est store reg2
	reghdfe roe_re repl1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)
	est store reg3
	reghdfe roe_re buyl1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)
	est store reg4
	reghdfe roe_re buy2l1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)	
	est store reg5
	
	estout reg1 reg2 reg3 reg4 reg5 using "Table2.txt", replace 	///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table 2. ROE regressions. All specifications include firm and fiscal year FE (Clustering of SE's at the firm level") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
	
	
	
	
	* Explain VP
	reg vp_10y net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg vp_10y net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg vp_10y l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg vp_10y buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)		
	reg vp_10y buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	
	reghdfe vp_10y net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)	
	reghdfe vp_10y net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe vp_10y rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe vp_10y buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe vp_10y buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)

	reghdfe vp_10y net_rep_mel1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)	
	est store reg1
	reghdfe vp_10y net_rep2_mel1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)
	est store reg2
	reghdfe vp_10y repl1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)
	est store reg3
	reghdfe vp_10y buyl1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)
	est store reg4	
	reghdfe vp_10y buy2l1 l_al1 mbl1 cf3_atl1 cash_atl1 div_d td_atl1, a(gvkey fyear) vce(cl gvkey)	
	est store reg5	
	
	estout reg1 reg2 reg3 reg4 reg5 using "Table3.txt", replace 	///
		   cells(b(star fmt(%9.4f)) se(par)) stats(N r2 r2_a, fmt(%9.3g %9.0g) labels(Observations R2 "Adjusted R2")) ///
		   legend  label varlabels(_cons Constant) abbrev starl(* 0.1 ** 0.05 *** 0.01) ///
		   title("Table 3. VP regressions. All specifications include firm and fiscal year FE (Clustering of SE's at the firm level") ///	
		   varwidth(45) modelwidth(10) delimiter("") style(fixed) drop() wrap 	
	
	
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d, cluster(gvkey)
	
	reg net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)		
	reg buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	
	reghdfe net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)	
	reghdfe net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)
	reghdfe buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(gvkey fyear) vce(cl gvkey)

	reghdfe net_rep_mef1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(cusip) vce(cl gvkey)	
	reghdfe net_rep2_mef1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(cusip) vce(cl gvkey)	
	reghdfe repf1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(cusip) vce(cl gvkey)
	
	* Low ESG score
	reg net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, cluster(gvkey)	
	reg net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, cluster(gvkey)	
	reg rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, cluster(gvkey)	
	reg buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, cluster(gvkey)		
	reg buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, cluster(gvkey)	
	
	areg net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)	
	areg net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)	
	areg rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)	
	areg buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)		
	areg buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)	

	areg net_rep_mef1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)	
	areg net_rep2_mef1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)	
	areg repf1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index < 0, a(cusip) cluster(gvkey)		
	
	* High ESG score
	reg net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, cluster(gvkey)	
	reg net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, cluster(gvkey)	
	reg rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, cluster(gvkey)	
	reg buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, cluster(gvkey)		
	reg buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, cluster(gvkey)	
	
	areg net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)	
	areg net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)	
	areg rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)	
	areg buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)		
	areg buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)	

	areg net_rep_mef1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)	
	areg net_rep2_mef1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)	
	areg repf1 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d if sr_index >= 0, a(cusip) cluster(gvkey)		
	
		
	reghdfe net_rep_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d, a(fyear) vce(cl gvkey)	
	reg net_rep2_me roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg rep roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)	
	reg buy roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)		
	reg buy2 roe_re l_al1 mbl1 cf3_atl1 cash_atl1 div_d i.fyear, cluster(gvkey)			
		
	
	

	* Only keep Dow Jones 30 firms
	
	keep if tic == "GS" | tic == "MMM" | tic == "BA" | tic == "UNH" | tic == "IBM" | tic == "HD" | tic == "AAPL" | tic == "MCD" | tic == "JNJ" | tic == "TRV" /*
	*/	  | tic == "UTX"| tic == "DIS" | tic == "CVX" | tic == "CAT" | tic == "V" | tic == "PG" | tic == "JPM" | tic == "XOM" | tic == "AXP" | tic == "DD" /*
	*/   | tic == "WMT" | tic == "MSFT" | tic == "MRK" | tic == "NKE" | tic == "VZ" | tic == "KO" | tic == "INTC" | tic == "PFE" | tic == "CSCO" | tic == "GE"
	
	
   tabstat vp_tb3, stats(n mean p50 sd min max) by(tic)
   tabstat vp_10y, stats(n mean p50 sd min max) by(tic)
   tabstat vp_tb3_eff, stats(n mean p50 sd min max) by(tic)
   tabstat vp_10y_eff, stats(n mean p50 sd min max) by(tic) 
   
   
   

