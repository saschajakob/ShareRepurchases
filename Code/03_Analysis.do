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
global temp		"/Users/saschajakob/Desktop/Paper 1/Temp"

*global data2	"A:\Sascha\Buyback Anomalies"
global data2 	"/Users/saschajakob/Desktop/Paper 1"

clear
clear matrix

set matsize 800
set scrollbufsize 500000
capture program drop _all


	*************************************************************
	****************  Beginn: Load data set  ********************
	*************************************************************
	use "$data2/cash_quarterly.dta", clear
	gen ccm_yq = quarterly(datacqtr,"YQ")
	format ccm_yq %tq
	duplicates drop gvkey ccm_yq,force
	destring gvkey,replace
	xtset gvkey ccm_yq
	g cta = cheq/atq
	winsor2 cta, replace cuts(1 99)


	g cta_l1 = l1.cta
	g cta_l2 = l2.cta
	g cta_l3 = l3.cta
	g cta_l4 = l4.cta
	g cta_l5 = l5.cta
	g cta_l6 = l6.cta
	g cta_l7 = l7.cta
	g cta_l8 = l8.cta
	g cta_l9 = l9.cta
	g cta_l10 = l10.cta
	g cta_l11= l11.cta
	g cta_l12 = l12.cta

	g cta_f1 = f1.cta
	g cta_f2 = f2.cta
	g cta_f3 = f3.cta
	g cta_f4 = f4.cta
	g cta_f5 = f5.cta
	g cta_f6 = f6.cta
	g cta_f7 = f7.cta
	g cta_f8 = f8.cta
	g cta_f9 = f9.cta
	g cta_f10 = f10.cta
	g cta_f11 = f11.cta
	g cta_f12 = f12.cta

	egen post_cash_mean_1y = rowmean(cta_l1 cta_l2 cta_l3 cta_l4)
	egen post_cash_mean_2y = rowmean(cta_l5 cta_l6 cta_l7 cta_l8)
	egen post_cash_mean_3y = rowmean( cta_l9 cta_l10 cta_l11 cta_l12)


	egen pre_cash_mean_1y = rowmean(cta_f1 cta_f2 cta_f3 cta_f4)
	egen pre_cash_mean_2y = rowmean(cta_f5 cta_f6 cta_f7 cta_f8)
	egen pre_cash_mean_3y = rowmean(cta_f9 cta_f10 cta_f11 cta_f12)

	g cash_change_1y = post_cash_mean_1y - pre_cash_mean_1y
	g cash_change_2y = post_cash_mean_2y - pre_cash_mean_1y
	g cash_change_3y = post_cash_mean_3y - pre_cash_mean_1y

	keep gvkey ccm_yq cta post_cash_mean_1y pre_cash_mean_1y cash_change_1y post_cash_mean_2y pre_cash_mean_2y cash_change_2y post_cash_mean_3y pre_cash_mean_3y cash_change_3y

	save "$temp/cash.dta", replace


	use "$data2/sample_buyback_portfolio_entropy.dta", clear
	drop if year < 1995
	sort dealnumber time
	merge m:1 dealnumber using "$data2/stock_entropy.dta"
	drop if _merge == 2
	drop _merge
	*Yearly*
	*merge m:1 permno year using "$data2/RKRV_misvaluation_yearly.dta"
	*Monthly*
	merge 1:1 permno month using "$data2/RKRV_misvaluation_monthly_3M.dta"
	drop if _merge == 2
	drop _merge



	*merge m:1 dealnumber  using "$data2/decomp_data_single_gamma_5.dta"
	*merge m:1 dealnumber  using "$data2/decomp_data_single_gamma_mispricing.dta"
	*merge m:1 dealnumber  using "$data2/decomp_data_single_gamma_alternative2.dta"
	merge m:1 dealnumber  using "$data2/decomp_data_single_gamma_alternative3.dta"

	*merge m:1 dealnumber  using "$data2/decomp_data_single_gamma_mispricing2.dta"


	*merge m:1 dealnumber  using "A:\Sascha\Buyback Anomalies\decomp_data_single_gamma_uval.dta"
	*merge m:1 dealnumber  using "A:\Sascha\Buyback Anomalies\decomp_data_single_gamma_fundamental.dta"
	*merge m:1 dealnumber  using "A:\Sascha\Buyback Anomalies\decomp_data_single_gamma_oval.dta"


	*merge m:1 dealnumber  using "A:\Sascha\Buyback Anomalies\decomp_data.dta"
	*merge m:1 dealnumber  using "A:\Sascha\Buyback Anomalies\decomp_data_single_sample.dta"
	*merge m:1 dealnumber  using "A:\Sascha\Buyback Anomalies\decomp_data_single_VAR.dta"

	drop if _merge ==2
	drop _merge
	merge m:1 dealnumber  using "$data2/announcement_AR.dta"
		drop if _merge ==2
	drop _merge

	gen ccm_q = quarterly(ccm_yq, "YQ")
	format ccm_q %tq
	drop ccm_yq
	gen ccm_yq = ccm_q
	drop ccm_q
	format ccm_yq %tq

	merge m:1 permno ccm_yq using "$data2/return_decomp_data_1980_2019.dta", keepusing(rep_q lq_ret c_lq_ret lroe_q c_lroe_q lbtm_q c_lbtm_q q_ret btm_q roe_q)
		drop if _merge ==2
	drop _merge

	merge m:1 gvkey ccm_yq using "$temp/cash.dta"
	drop if _merge==2
	drop _merge


	**************************************************************************
	************************* Begin analysis *********************************
	**************************************************************************
	sort permno month
	g dist = .
	replace dist = 0 if buyback==1

	sort dealnumber month
	by dealnumber: g dealmonth = month if dist==0
	by dealnumber: carryforward dealmonth, replace

	replace dist = month - dealmonth
	drop dealmonth
	drop if dist ==.

	sort dealnumber dist
	tsset dealnumber dist

	replace rep_q = . if dist != 0
	drop if cf_var_change_scaled==.


	egen vol_12m = sd(retrf) if dist > 0 & dist <=12, by(dealnumber)
	replace vol_12m = vol_12m[_n+1] if dist==0
	replace vol_12m =. if dist !=0

	egen vol_24m = sd(retrf) if dist > 0 & dist <=24, by(dealnumber)
	replace vol_24m = vol_24m[_n+1] if dist==0
	replace vol_24m =. if dist !=0

	egen vol_36m = sd(retrf) if dist > 0 & dist <=36, by(dealnumber)
	replace vol_36m = vol_36m[_n+1] if dist==0
	replace vol_36m =. if dist !=0

	g pre_var_ratio = sqrt(pre_disc_var)/sqrt(pre_cf_var) if dist ==0
	g post_var_ratio = sqrt(post_disc_var)/sqrt(post_cf_var) if dist ==0
	g delta_var_ratio = post_var_ratio/pre_var_ratio-1

	g ln_fcf = ln(fcf)
	g ln_age = ln(age)

	g RKRV_error = RKRV_firm_error + RKRV_sector_error

	bysort year: egen size_cluster = xtile(mkvalt) if dist == 0, nq(2)
	bysort dealnumber: carryforward size_cluster, replace

	egen error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5)
	bysort dealnumber: carryforward error_cluster, replace

	egen total_error_cluster = xtile(RKRV_error) if dist == 0, nq(5)
	bysort dealnumber: carryforward error_cluster, replace

	egen industry_error_cluster = xtile(RKRV_sector_error) if dist == 0, nq(5)
	bysort dealnumber: carryforward industry_error_cluster, replace

	bysort size_cluster: egen size_error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5)
	bysort dealnumber: carryforward size_error_cluster, replace

	egen cash_change_cluster = xtile(cash_change_3y) if dist == 0, nq(5)
	bysort dealnumber: carryforward cash_change_cluster, replace

	egen cash_cluster = xtile(cta) if dist == 0, nq(5)
	bysort dealnumber: carryforward cash_cluster, replace





	/*replace cf_var_change_scaled = .
	replace disc_var_change_scaled =.
	egen cf_var_mean = mean(pre_cf_var) if dist ==0
	egen disc_var_mean = mean(pre_disc_var) if dist ==0
	replace cf_var_change_scaled = cf_var_change_abs/cf_var_mean if dist == 0
	replace disc_var_change_scaled = disc_var_change_abs/disc_var_mean if dist == 0*/


	**************************** Summary statistics *******************************

	bysort permno: egen rep_count = count(dealnumber) if dist ==0
	tab rep_count

		est clear
		eststo clear
		eststo: estpost tabstat cf_var_change_scaled cf_lvl_change_abs disc_var_change_scaled disc_lvl_change_abs q_ret btm_q roe_q RKRV_firm_error ln_mkvalt if dist == 0, statistics(count mean p50 sd) columns(statistics)
		esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\sum_stat.tex", replace nomtitle nonumber cells("count(fmt(%9.2f)label(Obs.)) mean(fmt(%9.2f)label(Mean)) p50(fmt(%9.2f)label(Median)) sd(fmt(%9.2f)label(sd))") ///
	noobs coeflabels(cf_var_change_scaled "$ \Delta\mathbb{VAR}[\eta_{cf}]/mean(\mathbb{VAR}[\eta_{cf,pre}]) $" cf_lvl_change_abs "$ \Delta\eta_{cf} $" disc_var_change_scaled "$ \Delta\mathbb{VAR}[\eta_{r}]/mean(\mathbb{VAR}[\eta_{cf,pre}]) $" disc_lvl_change_abs "$ \Delta\eta_{r} $" q_ret "$ r_{q} $" btm_q "$ BM-Ratio_{q} $" roe_q "$ RoE_{q} $" RKRV_firm_error "$ RKRV pricing error $" ln_mkvalt "Market Cap") nonumber title(Summary statistics)


	*********************************************************************************

***table 3

	sum pre_cf_lvl post_cf_lvl cf_lvl_change_abs if dist ==0
	sum pre_disc_lvl post_disc_lvl disc_lvl_change_abs if dist ==0
	sum pre_cf_var post_cf_var cf_var_change_scaled if dist ==0
	sum pre_disc_var post_disc_var disc_var_change_scaled if dist ==0


*** table 4
	bysort error_cluster: ttest cf_var_change_scaled==0 if dist==0
	bysort error_cluster: ttest disc_var_change_scaled==0 if dist==0

	preserve
	keep if error_cluster==1 | error_cluster==5
	ttest cf_var_change_scaled if dist==0, by(error_cluster)
	ttest disc_var_change_scaled if dist==0, by(error_cluster)
	restore

	sum btm_q if dist==0 & industry_error_cluster ==5

	bysort error_cluster: ttest car_11_m ==0 if dist==0 & year >= 1995
	bysort error_cluster: ttest car_33_m ==0 if dist==0 & year >= 1995
	bysort error_cluster: ttest car_55_m ==0 if dist==0 & year >= 1995

	xtfmb retrf mktrf smb hml umd rmw cma if dist <= 12 & dist > 0 & error_cluster ==5
	di _b[_cons]*12

	xtfmb retrf mktrf smb hml umd rmw cma if dist <= 24 & dist > 0 & error_cluster ==5
	di _b[_cons]*24

	xtfmb retrf mktrf smb hml umd rmw cma if dist <= 36 & dist > 0 & error_cluster ==5
	di _b[_cons]*36

	*** calendar time Approach
	egen calendar_id = group(error_cluster month)
	bysort calendar_id: egen calendar_ret_12m = mean(retrf) if dist <=12 & dist>0
	bysort calendar_id: egen calendar_ret_24m = mean(retrf) if dist <=24 & dist>0
	bysort calendar_id: egen calendar_ret_36m = mean(retrf) if dist <=36 & dist>0
	drop if error_cluster==.

	preserve
	collapse (mean) calendar_ret_12m calendar_ret_24m calendar_ret_36m mktrf smb hml umd rmw cma, by(month error_cluster)

	forval i = 1(1)5{
		reg calendar_ret_36m mktrf smb hml umd rmw cma if error_cluster==`i'
	}
	restore

	preserve
	keep if industry_error_cluster == 1 | industry_error_cluster == 5
	ttest disc_lvl_change_abs_1q if dist==0, by(industry_error_cluster)
	ttest disc_lvl_change_abs_1y if dist==0, by(industry_error_cluster)
	ttest disc_lvl_change_abs_2y if dist==0, by(industry_error_cluster)
	ttest disc_lvl_change_abs_3y if dist==0, by(industry_error_cluster)
	restore




*Table 5

	ttest cf_lvl_change_abs_1q_1q==0 if dist==0
	ttest disc_lvl_change_abs_1q_1q==0 if dist==0

	ttest cf_lvl_change_abs_1q_1y==0 if dist==0
	ttest disc_lvl_change_abs_1q_1y==0 if dist==0

	ttest cf_lvl_change_abs_2y==0 if dist==0
	ttest disc_lvl_change_abs_2y==0 if dist==0

	ttest cf_lvl_change_abs_3y==0 if dist==0
	ttest disc_lvl_change_abs_3y==0 if dist==0


	bysort error_cluster: ttest cf_lvl_change_abs_1q_1q==0 if dist==0
	bysort error_cluster: ttest cf_lvl_change_abs_1q_1y==0 if dist==0
	bysort error_cluster: ttest cf_lvl_change_abs_2y==0 if dist==0
	bysort error_cluster: ttest cf_lvl_change_abs_3y==0 if dist==0

	preserve
	keep if error_cluster == 1 | error_cluster == 5
	ttest cf_lvl_change_abs_1q_1q if dist==0, by(error_cluster)
	ttest cf_lvl_change_abs_1q_1y if dist==0, by(error_cluster)
	ttest cf_lvl_change_abs_2y if dist==0, by(error_cluster)
	ttest cf_lvl_change_abs_3y if dist==0, by(error_cluster)
	restore

	bysort error_cluster: ttest disc_lvl_change_abs_1q_1q==0 if dist==0
	bysort error_cluster: ttest disc_lvl_change_abs_1q_1y==0 if dist==0
	bysort error_cluster: ttest disc_lvl_change_abs_2y==0 if dist==0
	bysort error_cluster: ttest disc_lvl_change_abs_3y==0 if dist==0

	preserve
	keep if error_cluster == 1 | error_cluster == 5
	ttest disc_lvl_change_abs_1q_1q if dist==0, by(error_cluster)
	ttest disc_lvl_change_abs_1q_1y if dist==0, by(error_cluster)
	ttest disc_lvl_change_abs_2y if dist==0, by(error_cluster)
	ttest disc_lvl_change_abs_3y if dist==0, by(error_cluster)
	restore




	bysort error_cluster: ttest disc_lvl_change_abs_1q_1y==0 if dist==0
	bysort error_cluster: ttest disc_lvl_change_abs_2q_1y==0 if dist==0
	bysort error_cluster: ttest disc_lvl_change_abs_3q_1y==0 if dist==0
	bysort error_cluster: ttest disc_lvl_change_abs_1y_1y==0 if dist==0

	preserve
	keep if error_cluster == 1 | error_cluster == 5
	ttest disc_lvl_change_abs_1q_1y if dist==0, by(error_cluster)
	ttest disc_lvl_change_abs_2q_1y if dist==0, by(error_cluster)
	ttest disc_lvl_change_abs_3q_1y if dist==0, by(error_cluster)
	ttest disc_lvl_change_abs_1y_1y if dist==0, by(error_cluster)
	restore




	bysort industry_error_cluster: sum disc_lvl_change_abs_1q_1q if dist==0
	bysort industry_error_cluster: sum disc_lvl_change_abs_1q_1y if dist==0
	bysort industry_error_cluster: sum disc_lvl_change_abs_2y if dist==0
	bysort industry_error_cluster: sum disc_lvl_change_abs_3y if dist==0

	bysort industry_error_cluster: sum disc_lvl_change_abs_2q_1y if dist==0
	bysort industry_error_cluster: sum disc_lvl_change_abs_3q_1y if dist==0
	bysort industry_error_cluster: sum disc_lvl_change_abs_1y_1y if dist==0

*table 6
bysort error_cluster: ttest pre_cf_lvl_1q==0 if dist ==0
bysort error_cluster: ttest post_cf_lvl_1y==0 if dist ==0
bysort error_cluster: ttest pre_disc_lvl_1q==0 if dist ==0
bysort error_cluster: ttest post_disc_lvl_1y==0 if dist ==0

bysort industry_error_cluster: ttest pre_disc_lvl_1q==0 if dist ==0
bysort industry_error_cluster: ttest post_disc_lvl_1y==0 if dist ==0

*Table 8

	bysort error_cluster: sum pre_disc_fit_1q post_disc_fit_1q post_disc_fit_1y post_disc_fit_2y post_disc_fit_3y if dist ==0

	bysort error_cluster: ttest post_disc_fit_1q == pre_disc_fit_1q if dist==0
	bysort error_cluster: ttest post_disc_fit_1y == pre_disc_fit_1q if dist==0
	bysort error_cluster: ttest post_disc_fit_2y == pre_disc_fit_1q if dist==0
	bysort error_cluster: ttest post_disc_fit_3y == pre_disc_fit_1q if dist==0

	bysort error_cluster: ttest post_cf_fit_1q == pre_cf_fit_1q if dist==0
	bysort error_cluster: ttest post_cf_fit_1y == pre_cf_fit_1q if dist==0
	bysort error_cluster: ttest post_cf_fit_2y == pre_cf_fit_1q if dist==0
	bysort error_cluster: ttest post_cf_fit_3y == pre_cf_fit_1q if dist==0





	bysort industry_error_cluster: sum pre_disc_fit_1q post_disc_fit_1q post_disc_fit_1y post_disc_fit_2y post_disc_fit_3y if dist ==0

	bysort industry_error_cluster: ttest post_disc_fit_1q == pre_disc_fit_1q if dist==0
	bysort industry_error_cluster: ttest post_disc_fit_1y == pre_disc_fit_1q if dist==0
	bysort industry_error_cluster: ttest post_disc_fit_2y == pre_disc_fit_1q if dist==0
	bysort industry_error_cluster: ttest post_disc_fit_3y == pre_disc_fit_1q if dist==0


*** size and misrpicing

* table 9

sum cf_var_change_scaled if size_cluster ==2 & dist == 0

ttest disc_lvl_chan ==0 if dist==0 & size_error_cluster ==1 & size_cluster ==1
ttest cf_var_change_scaled ==0 if dist==0 & size_error_cluster ==5 & size_cluster ==1
ttest cf_var_change_scaled ==0 if dist==0 & size_error_cluster ==1 & size_cluster ==2
ttest cf_var_change_scaled ==0 if dist==0 & size_error_cluster ==5 & size_cluster ==2


ttest disc_var_change_scaled ==0 if dist==0 & size_error_cluster ==1 & size_cluster ==1
ttest disc_var_change_scaled ==0 if dist==0 & size_error_cluster ==5 & size_cluster ==1
ttest disc_var_change_scaled ==0 if dist==0 & size_error_cluster ==1 & size_cluster ==2
ttest disc_var_change_scaled ==0 if dist==0 & size_error_cluster ==5 & size_cluster ==2


eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <= 36 & dist > 0 & size_error_cluster ==5 & size_cluster==2
di _b[_cons]*36


bysort size_cluster: sum disc_lvl_change_abs_1q_1q disc_lvl_change_abs_1q_1y disc_lvl_change_abs_2y disc_lvl_change_abs_3y if dist==0
bysort size_cluster: ttest disc_lvl_change_abs_1q_1q == 0 if dist == 0
bysort size_cluster: ttest disc_lvl_change_abs_1q_1y == 0 if dist == 0
bysort size_cluster: ttest disc_lvl_change_abs_2y == 0 if dist == 0
bysort size_cluster: ttest disc_lvl_change_abs_3y == 0 if dist == 0

bysort size_error_cluster: sum disc_lvl_change_abs_1q_1q disc_lvl_change_abs_1q_1y disc_lvl_change_abs_2y disc_lvl_change_abs_3y if dist==0 & size_cluster ==1
bysort size_error_cluster: sum disc_lvl_change_abs_1q_1q disc_lvl_change_abs_1q_1y disc_lvl_change_abs_2y disc_lvl_change_abs_3y if dist==0 & size_cluster ==2

*small
bysort size_error_cluster: ttest disc_lvl_change_abs_1q_1q == 0 if dist ==0 & size_cluster==1
bysort size_error_cluster: ttest disc_lvl_change_abs_1q_1y == 0 if dist ==0 & size_cluster==1
bysort size_error_cluster: ttest disc_lvl_change_abs_2y== 0 if dist ==0 & size_cluster==1
bysort size_error_cluster: ttest disc_lvl_change_abs_3y == 0 if dist ==0 & size_cluster==1

preserve
keep if size_error_cluster == 1 | size_error_cluster == 5
ttest disc_lvl_change_abs_1q_1q if dist == 0 & size_cluster==1, by(size_error_cluster)
ttest disc_lvl_change_abs_1q_1y if dist == 0 & size_cluster==1, by(size_error_cluster)
ttest disc_lvl_change_abs_2y if dist == 0 & size_cluster==1, by(size_error_cluster)
ttest disc_lvl_change_abs_3y if dist == 0 & size_cluster==1, by(size_error_cluster)
restore


*large
bysort size_error_cluster: ttest disc_lvl_change_abs_1q_1q == 0 if dist ==0 & size_cluster==2
bysort size_error_cluster: ttest disc_lvl_change_abs_1q_1y == 0 if dist ==0 & size_cluster==2
bysort size_error_cluster: ttest disc_lvl_change_abs_2y== 0 if dist ==0 & size_cluster==2
bysort size_error_cluster: ttest disc_lvl_change_abs_3y == 0 if dist ==0 & size_cluster==2

preserve
keep if size_error_cluster == 1 | size_error_cluster == 5
ttest disc_lvl_change_abs_1q_1q if dist == 0 & size_cluster==2, by(size_error_cluster)
ttest disc_lvl_change_abs_1q_1y if dist == 0 & size_cluster==2, by(size_error_cluster)
ttest disc_lvl_change_abs_2y if dist == 0 & size_cluster==2, by(size_error_cluster)
ttest disc_lvl_change_abs_3y if dist == 0 & size_cluster==2, by(size_error_cluster)
restore

* table 10
egen tag = xtile(mtb) if dist==0, nq(5)
bysort tag: ttest cf_var_change_scaled==0 if dist==0
bysort tag: ttest disc_var_change_scaled==0 if dist==0






	ttest disc_lvl_change_abs_1y_1y ==0 if dist==0 & error_cluster ==1


	sum vol_36m pre_var_ratio post_var_ratio delta_var_ratio if dist==0 & error_cluster==1


	preserve
	drop if error_cluster ==3
	ttest cf_var_change_scaled if dist==0, by(error_cluster)
	restore

	ttest car_55_m ==0 if dist==0 & error_cluster==5


* cash
set linesize 80
log using cash_model_3, replace

bysort cash_cluster: sum cta if dist==0
bysort cash_cluster: sum RKRV_firm_error if dist==0
bysort cash_cluster: sum cash_change_3y if dist==0
bysort cash_cluster: ttest car_33_m==0 if dist==0
xtfmb retrf mktrf smb hml umd rmw cma if dist <= 36 & dist > 0 & cash_cluster ==1
xtfmb retrf mktrf smb hml umd rmw cma if dist <= 36 & dist > 0 & cash_cluster ==5
bysort cash_cluster: ttest disc_var_change_scaled==0 if dist==0

bysort cash_change_cluster: sum cash_change_3y if dist==0
bysort cash_change_cluster: sum RKRV_firm_error if dist==0
bysort cash_change_cluster: ttest car_33_m==0 if dist==0
xtfmb retrf mktrf smb hml umd rmw cma if dist <= 36 & dist > 0 & cash_change_cluster ==1
xtfmb retrf mktrf smb hml umd rmw cma if dist <= 36 & dist > 0 & cash_change_cluster ==5
bysort cash_change_cluster: ttest disc_var_change_scaled==0 if dist==0

reghdfe disc_var_change_scaled cta cash_change_1y cash_change_2y cash_change_3y RKRV_firm_error RKRV_sector_error ln_mkvalt ln_age debt_atl1   if dist ==0, a(sic2 year) vce(cluster ccm_yq)

reghdfe disc_lvl_change_abs_3y disc_var_change_scaled cta cash_change_1y cash_change_2y cash_change_3y RKRV_firm_error RKRV_sector_error ln_mkvalt ln_age debt_atl1   if dist ==0, a(sic2 year) vce(cluster ccm_yq)

log close



	la var cf_var_change_scaled "$\Delta$ Cash Flow Variance"
	la var disc_var_change_scaled "$\Delta$ Discount Rate Variance"
	la var RKRV_firm_error "RKRV Mispricing"
	la var fcfl1 "FCF$_{t-1}"
	la var totalpercentofsharesauth "Repurchase Size"
	la var ln_mkvalt "Market Capitalization (Log)"
	la var debt_atl1 "Leverage$_{t-1}"
	la var ln_age "Age (Log)"

	est clear
	eststo clear

		eststo reg1: reghdfe cf_var_change_scaled c.RKRV_firm_error##c.fcfl1 totalpercentofsharesauth ln_mkvalt ln_age debt_atl1   if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		margins, dydx(fcfl1) at(RKRV_firm_error=(-2(1)2)) vsquish

		eststo reg2: reghdfe disc_var_change_scaled c.RKRV_firm_error##c.fcfl1 totalpercentofsharesauth ln_mkvalt ln_age debt_atl1   if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		 margins, dydx(fcfl1) at(RKRV_firm_error=(-2(1)2)) vsquish


	esttab reg1 reg2 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\moments_reg.tex", label replace booktabs width(1\hsize) title(Regression of Second Moments) ///
			mtitles("$\Delta$ Cash Flow Variance" "$\Delta$ Discount Rate Variance") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) nonumber scalars("r2 Avg. R-squared") sfmt(%9.2f)


			eststo reg1: reghdfe cf_var_change_scaled i.error_cluster##c.fcfl1 totalpercentofsharesauth ln_mkvalt ln_age debt_atl1   if dist ==0, a(sic2 year) vce(cluster ccm_yq)
			eststo reg1: reghdfe disc_var_change_scaled i.error_cluster##c.fcfl1 totalpercentofsharesauth ln_mkvalt ln_age debt_atl1   if dist ==0, a(sic2 year) vce(cluster ccm_yq)



*** SHort term car Regression

	la var pre_var_ratio "Variance Ratio"

	est clear
	eststo clear
		eststo: reghdfe car_11_m i.error_cluster cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#i.error_cluster c.disc_var_change_scaled#i.error_cluster pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		eststo: reghdfe car_33_m i.error_cluster cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#i.error_cluster c.disc_var_change_scaled#i.error_cluster pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		eststo: reghdfe car_55_m i.error_cluster cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#i.error_cluster c.disc_var_change_scaled#i.error_cluster pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)


		est clear
		eststo clear
		eststo: reghdfe car_11_m RKRV_firm_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_firm_error c.disc_var_change_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		margins, dydx(disc_var_change_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

		eststo: reghdfe car_33_m RKRV_firm_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_firm_error c.disc_var_change_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		margins, dydx(disc_var_change_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

		eststo: reghdfe car_55_m RKRV_firm_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_firm_error c.disc_var_change_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
		margins, dydx(disc_var_change_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\st_car_regressions.tex", label replace booktabs width(1\hsize) title(Drivers of short-term CARs) ///
			mtitles("$\pm$ 1 day" "$\pm$ 3 days" "$\pm$ 5 days" ) b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) nonumber substitute("\caption{Drivers of short-term CARs}" "\caption{Drivers of short-term CARs} \label{table:st_car_reg} \caption*{\footnotesize This tables reports the estimation from the following regression: \begin{center} $CAR_{i,t,[\pm d]} = \alpha + \beta_1 RKRV_{i,t} + \beta_2 \Delta\mathbb{VAR}[\eta_{cf}]_{i,t} + \beta_3 RKRV_{i,t} \times \Delta\mathbb{VAR}[\eta_{cf}]_{i,t} + VarRatio_{i,t}  + \varepsilon_{i,t},$\end{center} where $CAR_{i,t,[\pm d]}$ denotes the cumulative abnormal announcement return of stock $i$ in repurchase quarter $t$ over a event window of $\pm d$ days around the repurchase announcement. $RKRV_{i,t}$ measures the stocks mispricing at the time of the repurchase announcement using the method of \cite{Rhodes-Kropf2005} which we lay out in section III. $\Delta\mathbb{VAR}[\eta_{cf}]_{i,t}$ denotes the change in cash flow news variance scaled by the cross-sectional average cash flow variance prior the repurchase announcements. The ratio between the variance of discount rate news and the variance of cash flow news is represented by $ VarRatio_{i,t}$, measured as the quotient of the second moment of cash flow news divided by the second moment of cash flow news prior to the announcement. All cash flow and discount rate moments are estimated using the VAR method for all repurchasing firms using the method we outline in section III. We add year and industry fixed effects at the 2-digit industry level and standard errors are clustered at the repurchase-quarter level. t-stats are reported in parentheses.}") scalars("r2 Avg. R-squared") sfmt(%9.2f)








********************************************************************************
******************* Chnages in moment as a function of CAR *********************
********************************************************************************

preserve

	egen CAR_cluster_1 = xtile(car_11_m) if dist == 0, nq(5) by(year)
	egen CAR_cluster_3 = xtile(car_33_m) if dist == 0, nq(5) by(year)
	egen CAR_cluster_5 = xtile(car_55_m) if dist == 0, nq(5) by(year)


	* Change in moments as a function of CAR

	* Scaled

	tabstat cf_lvl_change_abs disc_lvl_change_abs if year >=1996 & dist ==0, by(CAR_cluster_1) statistics(mean) columns(v)

	 tabstat cf_lvl_change_scaled cf_var_change_scaled disc_lvl_change_scaled disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_cluster_1) statistics(mean) columns(v)
	 tabstat cf_lvl_change_scaled cf_var_change_scaled disc_lvl_change_scaled disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_cluster_3) statistics(mean) columns(v)
	 tabstat cf_lvl_change_scaled cf_var_change_scaled disc_lvl_change_scaled disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_cluster_5) statistics(mean) columns(v)

	 est clear
	 eststo clear
	 eststo CAR1: estpost tabstat  cf_var_change_scaled  disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_cluster_1) statistics(mean) columns(v)
	 eststo CAR3: estpost tabstat  cf_var_change_scaled  disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_cluster_3) statistics(mean) columns(v)
	 eststo CAR5: estpost tabstat  cf_var_change_scaled  disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_cluster_5) statistics(mean) columns(v)
	 /*esttab CAR1 CAR3 CAR5 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\AR_moments.tex",  replace noobs nonumber cells("cf_var_change_scaled(fmt(%9.3f)label(Cash Flow)) disc_var_change_scaled(fmt(%9.3f)label(Discount Rate))") label title(Changes in cash flow and discount rate variance conditional on announcement returns) substitute("&\multicolumn{2}{c}{}     &\multicolumn{2}{c}{}     &\multicolumn{2}{c}{}     \\" "&\multicolumn{2}{c}{$[-1;+1]$}     &\multicolumn{2}{c}{$[-3;+3]$}     &\multicolumn{2}{c}{$[-5;+5]$}     \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7}" "\caption{Changes in cash flow and discount rate variance conditional on announcement returns}" "\caption{Changes in cash flow and discount rate variance conditional on announcement returns} \label{table:AR_moments}")*/


	* Absolute
	est clear
	eststo clear
	eststo: estpost tabstat cf_lvl_change_abs cf_var_change_abs disc_lvl_change_abs disc_var_change_abs if year >=1996 & dist ==0, by(CAR_cluster_1) statistics(mean) columns(v)
	eststo: estpost tabstat cf_lvl_change_abs cf_var_change_abs disc_lvl_change_abs disc_var_change_abs if year >=1996 & dist ==0, by(CAR_cluster_3) statistics(mean) columns(v)
	eststo: estpost tabstat cf_lvl_change_abs cf_var_change_abs disc_lvl_change_abs disc_var_change_abs if year >=1996 & dist ==0, by(CAR_cluster_5) statistics(mean) columns(v)

restore

********************************************************************************
*************************** Analysis of moments ********************************
********************************************************************************

preserve

	est clear
	eststo clear
	eststo: estpost tabstat pre_cf_lvl post_cf_lvl cf_lvl_change_abs pre_disc_lvl post_disc_lvl disc_lvl_change_abs if dist == 0, statistics(mean)  columns(v)
	/*esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\level_overview.tex",  replace noobs nonumber cells("pre_cf_lvl(fmt(%9.4f)label($\eta_{cf,pre}$)) post_cf_lvl(fmt(%9.4f)label($\eta_{cf,post}$)) cf_lvl_change_scaled(fmt(%9.4f)label($\Delta\eta_{cf}$)) pre_disc_lvl(fmt(%9.4f)label($\eta_{r,pre}$)) post_disc_lvl(fmt(%9.4f)label($\eta_{r,post}$)) disc_lvl_change_scaled(fmt(%9.4f)label($\Delta\eta_{r,pre}$))") label title(Cash flow and discount rate level) substitute("\caption{Cash flow and discount rate level}" "\caption{Cash flow and discount rate level}  \label{table:level_overview}" "&\multicolumn{6}{c}{}                                                         \\" "&\multicolumn{3}{c}{Cash Flow News} &  \multicolumn{3}{c}{Discount Rate News} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}")*/

restore


egen tag = xtile(mtb) if dist==0, nq(5)

preserve

	est clear
	eststo clear
	eststo: estpost tabstat pre_cf_var post_cf_var cf_var_change_scaled pre_disc_var post_disc_var disc_var_change_scaled if dist == 0, statistics(mean)  columns(v)
	/*esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\variance_overview_fundamental.tex",  replace noobs nonumber cells("pre_cf_var(fmt(%9.4f)label($\mathbb{VAR}[\eta_{cf,pre}]$)) post_cf_var(fmt(%9.4f)label($\mathbb{VAR}[\eta_{cf,post}]$)) cf_var_change_scaled(fmt(%9.4f)label($\Delta\mathbb{VAR}[\eta_{cf,pre}]$)) pre_disc_var(fmt(%9.4f)label($\mathbb{VAR}[\eta_{r,pre}]$)) post_disc_var(fmt(%9.4f)label($\mathbb{VAR}[\eta_{r,post}]$)) disc_var_change_scaled(fmt(%9.4f)label($\Delta\mathbb{VAR}[\eta_{r,pre}]$))") label title(Cash flow and discount rate variance - fundamental) substitute("\caption{Cash flow and discount rate variance - fundamental}" "\caption{Cash flow and discount rate variance - fundamental}  \label{table:variance_overview_fundamental}" "&\multicolumn{6}{c}{}                                                         \\" "&\multicolumn{3}{c}{Cash Flow News} &  \multicolumn{3}{c}{Discount Rate News} \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}")*/

restore

********************************************************************************
************* Change in moments conditional on mispricing **********************
********************************************************************************


********* firm specific **********************
	preserve

	/*egen firm_error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5)
	la var cf_lvl_change_scaled  "Cash flow level"
	la var cf_var_change_scaled "Cash flow variance"
	la var disc_lvl_change_scaled "Discount rate level"
	la var disc_var_change_scaled "Discount rate variance"*/

	est clear
	eststo clear
	eststo: estpost tabstat cf_lvl_change_scaled cf_var_change_scaled disc_lvl_change_scaled disc_var_change_scaled if dist==0, statistics(mean) by(error_cluster) columns(statistics) listwise


	est clear
	eststo clear
	eststo: estpost tabstat pre_cf_lvl_1q post_cf_lvl_1y if dist==0, statistics(mean) by(error_cluster) columns(statistics) listwise

	est clear
	eststo clear
	eststo: estpost tabstat pre_disc_lvl_1q post_disc_lvl_1y if dist==0, statistics(mean) by(error_cluster) columns(statistics) listwise


	est clear
	eststo clear
	eststo: estpost tabstat pre_cf_var post_cf_var if dist==0, statistics(mean) by(error_cluster) columns(statistics) listwise

	est clear
	eststo clear
	eststo: estpost tabstat pre_disc_var post_disc_var if dist==0, statistics(mean) by(error_cluster) columns(statistics) listwise

	/*esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\mispricing_decomposition_overview.tex",  main(mean %9.3f)  nostar unstack noobs nonote nomtitle nonumber label replace title(Change in moments conditional on mispricing) 			substitute("\caption{Change in moments conditional on mispricing}" "\caption{Change in moments conditional on mispricing}  \label{table:moments_mispricing_overview}" "                    &           1&           2&           3&       Total\\"     "Change in moment                &           Undervalued &           Fundamental &           Overvalued &       Full sample\\")*/

	restore



	********* industry specific *****************************

	preserve

	la var cf_var_change_scaled "Cash flow variance"
	la var disc_var_change_scaled "Discount rate variance"

	est clear
	eststo clear
	egen tag = xtile(RKRV_sector_error) if dist == 0, nq(5)
	eststo: estpost tabstat  cf_var_change_scaled  disc_var_change_scaled if dist==0, statistics(mean) by(tag) columns(statistics)
	/*esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\sector_decomposition_overview.tex",  main(mean %9.3f)  nostar unstack noobs nonote nomtitle nonumber label replace title(Change in moments conditional on sector mispricing) 			substitute("\caption{Change in moments conditional on sector mispricing}" "\caption{Change in moments conditional on sector mispricing}  \label{table:sector_mispricing_overview}" "                    &           1&           2&           3&       Total\\"     "Change in moment                &           Undervalued &           Fundamental &           Overvalued &       Full sample\\")*/

	restore

	************ BM *********************************************

	preserve

	la var cf_var_change_scaled "Cash flow variance"
	la var disc_var_change_scaled "Discount rate variance"

	est clear
	eststo clear
	egen tag = xtile(mtb) if dist==0, nq(5)
	eststo: estpost tabstat  cf_var_change_scaled  disc_var_change_scaled if dist==0, statistics(mean n) by(tag) columns(statistics)
	/*esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\mtb_decomposition_overview.tex",  main(mean %9.3f)  nostar unstack noobs nonote nomtitle nonumber label replace title(Change in moments conditional on MTB-ratio) 			substitute("\caption{Change in moments conditional on MTB-ratio}" "\caption{Change in moments conditional on MTB-ratio}  \label{table:mtb_mispricing_overview}" "                    &           1&           2&           3&       Total\\"     "Change in moment                &           Undervalued &           Fundamental &           Overvalued &       Full sample\\")*/

	restore

	***************** past return ********************************

	preserve

	la var cf_var_change_scaled "Cash flow variance"
	la var disc_var_change_scaled "Discount rate variance"

	est clear
	eststo clear
	egen tag = xtile(six_m_cum_ret) if dist==0, nq(5)
	eststo: estpost tabstat  cf_var_change_scaled  disc_var_change_scaled if dist==0, statistics(mean) by(tag) columns(statistics)
	/*esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\past_ret_decomposition_overview.tex",  main(mean %9.3f)  nostar unstack noobs nonote nomtitle nonumber label replace title(Change in moments conditional on past returns) 			substitute("\caption{Change in moments conditional on past returns}" "\caption{Change in moments conditional on past returns}  \label{table:ret_mispricing_overview}" "                    &           1&           2&           3&       Total\\"     "Change in moment                &           Undervalued &           Fundamental &           Overvalued &       Full sample\\")*/

	restore




********************************************************************************





********************************************************************************
******************* Announcment returns for CF and DR news *********************
********************************************************************************
preserve

	egen entropy_cluster = xtile(entropy_H) if dist == 0, nq(5) by(year)
	egen binomial_entropy_cluster = xtile(entropy_H_binomial) if dist == 0, nq(5) by(year)
	egen information_cluster = xtile(entropy_I) if dist == 0, nq(5) by(year)
	egen size_cluster = xtile(mkvalt) if dist == 0, nq(5) by(year)
	egen ML_stock_entropy_cluster = xtile(ML_entropy) if dist == 0, nq(5) by(year)
	egen KT_stock_entropy_cluster = xtile(KT_Entropy) if dist == 0, nq(5) by(year)
	egen ivol_cluster = xtile(ivol) if dist == 0, nq(5) by(year)
	egen error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5) 				/* not yearly since we are interested in the absolute degree of misspricing. The measure itself is already yearly and relative to the cross-section */
	egen cf_var_cluster = xtile(cf_var_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_var_cluster = xtile(disc_var_change_scaled) if dist == 0, nq(5) by(year)
	egen cf_lvl_cluster = xtile(cf_lvl_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_lvl_cluster = xtile(disc_lvl_change_scaled) if dist == 0, nq(5) by(year)


	* Announcment return as a function of mispricing table C.1
	est clear
	eststo clear
	eststo: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(error_cluster) statistics(mean) columns(v)
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\ST_CAR_RKRV.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term 		CARs and mispricing) substitute("\caption{Short-term CARs and mispricing}" "\caption{Short-term CARs and mispricing}  \label{table:ST_CAR_RKRV}")

	ttest car_55_m == 0 if dist == 0 & error_cluster==1
	ttest car_55_m == 0 if dist == 0 & error_cluster==2
	ttest car_55_m == 0 if dist == 0 & error_cluster==3
	ttest car_55_m == 0 if dist == 0 & error_cluster==4
	ttest car_55_m == 0 if dist == 0 & error_cluster==5
	ttest car_55_m == 0 if dist == 0


	* Announcment return as a function of CF level
	est clear
	eststo clear
	eststo: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(cf_lvl_cluster) statistics(mean) columns(v)
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\ST_CAR_CF_LVL.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term 		CARs and cash flow level) substitute("\caption{Short-term CARs and cash flow level}" "\caption{Short-term CARs and cash flow level}  \label{table:ST_CAR_CF_LVL}")


	* Announcment return as a function of CF variance
	est clear
	eststo clear
	eststo: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(cf_var_cluster) statistics(mean) columns(v)
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\ST_CAR_CF_VAR.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term 		CARs and cash flow variance) substitute("\caption{Short-term CARs and cash flow variance}" "\caption{Short-term CARs and cash flow variance}  \label{table:ST_CAR_CF_VAR}")


	* Announcment return as a function of DR level
	est clear
	eststo clear
	eststo: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(disc_lvl_cluster) statistics(mean) columns(v)
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\ST_CAR_DISC_LVL.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term CARs and discount rate level) substitute("\caption{Short-term CARs and discount rate level}" "\caption{Short-term CARs and discount rate level}  \label{table:ST_CAR_DISC_LVL}")


	* Announcment return as a function of DR variance
	est clear
	eststo clear
	eststo: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(disc_var_cluster) statistics(mean) columns(v)
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\ST_CAR_DISC_VAR.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term CARs and discount rate variance) substitute("\caption{Short-term CARs and discount rate variance}" "\caption{Short-term CARs and discount rate variance}  \label{table:ST_CAR_DISC_VAR}")


restore
*********************************************************************************





********************************************************************************
******* Announcment returns for CF and DR news conditional on mispricing *******
********************************************************************************
preserve


	*** Undervalued ***

egen mispricing_indicator = xtile(RKRV_firm_error) if dist == 0, nq(3)


keep if mispricing_indicator == 1


	egen entropy_cluster = xtile(entropy_H) if dist == 0, nq(5) by(year)
	egen binomial_entropy_cluster = xtile(entropy_H_binomial) if dist == 0, nq(5) by(year)
	egen information_cluster = xtile(entropy_I) if dist == 0, nq(5) by(year)
	egen size_cluster = xtile(mkvalt) if dist == 0, nq(5) by(year)
	egen ML_stock_entropy_cluster = xtile(ML_entropy) if dist == 0, nq(5) by(year)
	egen KT_stock_entropy_cluster = xtile(KT_Entropy) if dist == 0, nq(5) by(year)
	egen ivol_cluster = xtile(ivol) if dist == 0, nq(5) by(year)
	egen error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5) 				/* not yearly since we are interested in the absolute degree of misspricing */
	egen cf_var_cluster = xtile(cf_var_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_var_cluster = xtile(disc_var_change_scaled) if dist == 0, nq(5) by(year)
	egen cf_lvl_cluster = xtile(cf_lvl_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_lvl_cluster = xtile(disc_lvl_change_scaled) if dist == 0, nq(5) by(year)



	* Announcment return as a function of CF level
	est clear
	eststo clear

	eststo uval_cf1: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(cf_lvl_cluster) statistics(mean) columns(v)
	eststo uval_cf2: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(cf_var_cluster) statistics(mean) columns(v)


	* Announcment return as a function of DR

	eststo uval_dr1: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(disc_lvl_cluster) statistics(mean) columns(v)
	eststo uval_dr2: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(disc_var_cluster) statistics(mean) columns(v)


restore

	*** Overvalued ***

preserve

egen mispricing_indicator = xtile(RKRV_firm_error) if dist == 0, nq(3)


keep if mispricing_indicator == 3


	egen entropy_cluster = xtile(entropy_H) if dist == 0, nq(5) by(year)
	egen binomial_entropy_cluster = xtile(entropy_H_binomial) if dist == 0, nq(5) by(year)
	egen information_cluster = xtile(entropy_I) if dist == 0, nq(5) by(year)
	egen size_cluster = xtile(mkvalt) if dist == 0, nq(5) by(year)
	egen ML_stock_entropy_cluster = xtile(ML_entropy) if dist == 0, nq(5) by(year)
	egen KT_stock_entropy_cluster = xtile(KT_Entropy) if dist == 0, nq(5) by(year)
	egen ivol_cluster = xtile(ivol) if dist == 0, nq(5) by(year)
	egen error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5) 				/* not yearly since we are interested in the absolute degree of misspricing */
	egen cf_var_cluster = xtile(cf_var_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_var_cluster = xtile(disc_var_change_scaled) if dist == 0, nq(5) by(year)
	egen cf_lvl_cluster = xtile(cf_lvl_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_lvl_cluster = xtile(disc_lvl_change_scaled) if dist == 0, nq(5) by(year)



	* Announcment return as a function of CF level
	eststo oval_cf1: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(cf_lvl_cluster) statistics(mean) columns(v)
	eststo oval_cf2: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(cf_var_cluster) statistics(mean) columns(v)

	* Announcment return as a function of DR level
	eststo oval_dr1: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(disc_lvl_cluster) statistics(mean) columns(v)
	eststo oval_dr2: estpost tabstat car_11_m car_33_m car_55_m if year >=1996 & dist ==0, by(disc_var_cluster) statistics(mean) columns(v)


restore


	* Generating tables *

	esttab uval_cf1 oval_cf1 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\cf_lvl_mispricing.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term CARs and cash flow level conditional on mispricing) 		substitute("&\multicolumn{3}{c}{}                  &\multicolumn{3}{c}{}                  \\" "&\multicolumn{3}{c}{Undervalued}                  &\multicolumn{3}{c}{Overvalued}                  \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}" "\caption{Short-term CARs and cash flow level conditional on mispricing}" "\caption{Short-term CARs and cash flow level conditional on mispricing} \label{table:cf_lvl_mispricing}" "Total" "Full Sample")

		esttab uval_cf2 oval_cf2 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\cf_var_mispricing.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term CARs and cash flow variance conditional on mispricing) 		substitute("&\multicolumn{3}{c}{}                  &\multicolumn{3}{c}{}                  \\" "&\multicolumn{3}{c}{Undervalued}                  &\multicolumn{3}{c}{Overvalued}                  \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}" "\caption{Short-term CARs and cash flow variance conditional on mispricing}" "\caption{Short-term CARs and cash flow variance conditional on mispricing} \label{table:cf_var_mispricing}" "Total" "Full Sample")

		esttab uval_dr1 oval_dr1 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\dr_lvl_mispricing.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term CARs and discount rate level conditional on mispricing) 		substitute("&\multicolumn{3}{c}{}                  &\multicolumn{3}{c}{}                  \\" "&\multicolumn{3}{c}{Undervalued}                  &\multicolumn{3}{c}{Overvalued}                  \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}" "\caption{Short-term CARs and discount rate level conditional on mispricing}" "\caption{Short-term CARs and discount rate level conditional on mispricing} \label{table:dr_lvl_mispricing}" "Total" "Full Sample")

		esttab uval_dr2 oval_dr2 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\dr_var_mispricing.tex",  replace noobs nonumber cells("car_11_m(fmt(%9.3f)label([-1;+1])) car_33_m(fmt(%9.3f)label([-3;+3])) car_55_m(fmt(%9.3f)label([-5;+5]))") label title(Short-term CARs and discount rate variance conditional on mispricing) 		substitute("&\multicolumn{3}{c}{}                  &\multicolumn{3}{c}{}                  \\" "&\multicolumn{3}{c}{Undervalued}                  &\multicolumn{3}{c}{Overvalued}                  \\ \cmidrule(lr){2-4} \cmidrule(lr){5-7}" "\caption{Short-term CARs and discount rate variance conditional on mispricing}" "\caption{Short-term CARs and discount rate variance conditional on mispricing} \label{table:dr_var_mispricing}" "Total" "Full Sample")

********************************************************************************


la var ML_entropy "Stock efficiency"
la var ivol "IVOL"
la var ret_sd "Stock volatility"
la var instown_per "Institutional ownership"
la var amihud_6M "Amihud measure"
la var ln_mkvalt " Market cap (log)"
la var si_mean "Short interest"
la var age "Firm age"
la var RKRV_firm_error "Firm specific error"
la var RKRV_sector_error "Sector error"
la var RKRV_longrun_vb  "Longrun error"
la var cf_lvl_change_scaled  "Cash flow level change"
la var cf_var_change_rel "Cash flow variance change"
la var disc_lvl_change_scaled "Discount rate level change"
la var disc_var_change_scaled "Discount rate variance change"
la var error_cluster "Mispricing Cluster"



********************************************************************************
************************ Stock-level alphas ************************************
********************************************************************************

g mkt_adj_ret = retrf - mktrf

g ln_retrf = ln(1+mkt_adj_ret)
bys dealnumber: egen max_dist=max(dist)

sort dealnumber dist

by dealnumber: egen ret_12m = total(ln_retrf) if dist>0 & dist<=12 & max_dist>=12
replace ret_12m = exp(ret_12m)-1
by dealnumber: replace ret_12m = ret_12m[_n+1] if dist==0
replace ret_12m =. if dist !=0

by dealnumber: egen ret_24m = total(ln_retrf) if dist>0 & dist<=24 & max_dist>=24
replace ret_24m = exp(ret_24m)-1
by dealnumber: replace ret_24m = ret_24m[_n+1] if dist==0
replace ret_24m =. if dist !=0

by dealnumber: egen ret_36m = total(ln_retrf) if dist>0 & dist<=36 & max_dist>=36
replace ret_36m = exp(ret_36m)-1
by dealnumber: replace ret_36m = ret_36m[_n+1] if dist==0
replace ret_36m =. if dist !=0

drop ln_retrf
drop max_dist

/*
gen CAR_12m =.
gen CAR_24m =.
gen CAR_36m =.

egen reg_id = group(dealnumber)
egen max_dist = max(dist), by(reg_id)

sum reg_id
forval i = 1(1)`r(max)' {

qui capture reg retrf mktrf smb hml umd rmw cma if reg_id == `i'  & dist <=12 & dist > 0 & max_dist >= 12
if _rc != 0{
		continue
		}
qui replace CAR_12m = _b[_cons]*12 if dist == 0 & reg_id == `i'

qui capture reg retrf mktrf smb hml umd rmw cma if reg_id == `i'  & dist <=24 & dist > 0 & max_dist >= 24
if _rc != 0{
		continue
		}
qui replace CAR_24m = _b[_cons]*24 if dist == 0 & reg_id == `i'

qui capture reg retrf mktrf smb hml umd rmw cma if reg_id == `i'  & dist <=36 & dist > 0 & max_dist >= 36
if _rc != 0{
		continue
		}
qui replace CAR_36m = _b[_cons]*36 if dist == 0 & reg_id == `i'
}
*/

*********************** Long-run CAR as a function of changes in moments

*g sign_dummy =0 if dist==0
*replace sign_dummy=1 if disc_var_change_scaled <0 & dist ==0

*egen cf_lvl_scaled = mean(pre_cf_lvl_1q) if dist ==0, by(year)

est clear
eststo clear

eststo reg1: reghdfe pre_disc_lvl_1q ib3.error_cluster if dist ==0, noabsorb vce(cluster ccm_yq)
eststo reg2: reghdfe pre_disc_lvl_1q ib3.error_cluster ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, noabsorb vce(cluster ccm_yq)
eststo reg3: reghdfe pre_disc_lvl_1q ib3.error_cluster  ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)


eststo reg4: reghdfe disc_lvl_change_abs_1q_1y ib3.error_cluster if dist ==0,  noabsorb vce(cluster ccm_yq)
eststo reg5: reghdfe disc_lvl_change_abs_1q_1y ib3.error_cluster ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, noabsorb vce(cluster ccm_yq)
eststo reg6: reghdfe disc_lvl_change_abs_1q_1y ib3.error_cluster ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)


esttab reg1 reg2 reg3 reg4 reg5 reg6 using "~/Dropbox/Buybacks/Text/Tables_Figures/RKRV_discount_rate_news.tex", label replace booktabs width(1\hsize) title(Level of disocunt rate news and firm specific pricing error) ///
	mtitles("$\eta_{r_{i,t}}$" "$\eta_{r_{i,t}}$" "$\eta_{r_{i,t}}$" "$\Delta\eta_{r_{i,t}}$" "$\Delta\eta_{r_{i,t}}$" "$\Delta\eta_{r_{i,t}}$" ) b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) nonumber scalars("r2 Adj. R-squared") sfmt(%9.2f)



est clear
eststo clear

eststo reg1: reghdfe pre_cf_lvl_1q ib3.error_cluster if dist ==0, noabsorb vce(cluster ccm_yq)
eststo reg2: reghdfe pre_cf_lvl_1q ib3.error_cluster ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, noabsorb vce(cluster ccm_yq)
eststo reg3: reghdfe pre_cf_lvl_1q ib3.error_cluster  ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)


eststo reg4: reghdfe cf_lvl_change_abs_1q_1y ib3.error_cluster if dist ==0,  noabsorb vce(cluster ccm_yq)
eststo reg5: reghdfe cf_lvl_change_abs_1q_1y ib3.error_cluster ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, noabsorb vce(cluster ccm_yq)
eststo reg6: reghdfe cf_lvl_change_abs_1q_1y ib3.error_cluster ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)


esttab reg1 reg2 reg3 reg4 reg5 reg6 using "~/Dropbox/Buybacks/Text/Tables_Figures/RKRV_cashflow_rate_news.tex", label replace booktabs width(1\hsize) title(Level of cash flow news and firm specific pricing error) ///
	mtitles("$\eta_{cf_{i,t}}$" "$\eta_{cf_{i,t}}$" "$\eta_{cf_{i,t}}$" "$\Delta\eta_{cf_{i,t}}$" "$\Delta\eta_{cf_{i,t}}$" "$\Delta\eta_{cf_{i,t}}$" ) b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) nonumber scalars("r2 Adj. R-squared") sfmt(%9.2f)




corr RKRV_firm_error pre_disc_lvl_1q if dist ==0




reghdfe ret_12m error_cluster pre_disc_lvl_1q i.error_cluster#c.pre_disc_lvl_1q ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)








est clear
eststo clear

eststo car1: reghdfe ret_12m RKRV_firm_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_firm_error c.disc_var_change_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(disc_var_change_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

eststo car2: reghdfe ret_24m RKRV_firm_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_firm_error c.disc_var_change_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(disc_var_change_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

eststo car3: reghdfe ret_36m RKRV_firm_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_firm_error c.disc_var_change_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(disc_var_change_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

		esttab car1 car2 car3 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\lt_car_regressions_cont.tex", label replace booktabs width(1\hsize) title(Drivers of long-term CARs) ///
			mtitles("$\pm$ 12 months" "$\pm$ 24 months" "$\pm$ 36 months" ) b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) nonumber substitute("\caption{Drivers of long-term CARs}" "\caption{Drivers of long-term CARs} \label{table:lt_car_reg_cont} \caption*{\footnotesize This tables reports the estimation from the following regression: \begin{center} $CAR_{i,t,[\pm d]} = \alpha + \beta_1 RKRV_{i,t} + \beta_2 \Delta\mathbb{VAR}[\eta_{cf}]_{i,t} + \beta_3 RKRV_{i,t} \times \Delta\mathbb{VAR}[\eta_{cf}]_{i,t} + VarRatio_{i,t}  + \varepsilon_{i,t}$, \end{center} where $CAR_{i,t,[\pm d]}$ denotes the cumulative abnormal announcement return of stock $i$ over a event window of $d$ months subsequent the repurchase announcement. $RKRV_{i,t}$ measures the stocks mispricing at the time of the repurchase announcement using the method of \cite{Rhodes-Kropf2005} which we lay out in section III. $\Delta\mathbb{VAR}[\eta_{cf}]_{i,t}$ denotes the change in cash flow news variance scaled by the cross-sectional average cash flow variance prior the repurchase announcements. $\Delta\mathbb{VAR}[\eta_{r}]_{i,t}$ denotes the change in discount rate news variance scaled by the cross-sectional average discount rate variance prior the repurchase announcements.The ratio between the variance of discount rate news and the variance of cash flow news is represented by $ VarRatio_{i,t}$, measured as the quotient of the second moment of cash flow news divided by the second moment of cash flow news prior to the announcement. All cash flow and discount rate moments are estimated using the VAR method for all repurchasing firms using the method we outline in section III. We add year and industry fixed effects at the 2-digit industry level and standard errors are clustered at the repurchase-quarter level. t-stats are reported in parentheses.}") scalars("r2 Avg. R-squared") sfmt(%9.2f)

est clear
eststo clear
eststo car1: reghdfe ret_12m i.error_cluster cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#i.error_cluster c.disc_var_change_scaled#i.error_cluster pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)

eststo car2: reghdfe ret_24m i.error_cluster cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#i.error_cluster c.disc_var_change_scaled#i.error_cluster pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)

eststo car3: reghdfe ret_36m i.error_cluster cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#i.error_cluster c.disc_var_change_scaled#i.error_cluster pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)


	esttab car1 car2 car3 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\lt_car_regressions.tex", label replace booktabs width(1\hsize) title(Drivers of long-term CARs) ///
			mtitles("$\pm$ 1 day" "$\pm$ 3 days" "$\pm$ 5 days" ) b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) nonumber substitute("\caption{Drivers of long-term CARs}" "\caption{Drivers of long-term CARs} \label{table:lt_car_reg} \caption*{\footnotesize This tables reports the estimation from the following regression: \begin{center} $CAR_{i,t,[\pm d]} = \alpha + \beta_1 RKRV_{i,t} + \beta_2 \Delta\mathbb{VAR}[\eta_{cf}]_{i,t} + \beta_3 RKRV_{i,t} \times \Delta\mathbb{VAR}[\eta_{cf}]_{i,t} + VarRatio_{i,t}  + \varepsilon_{i,t},$\end{center} where $CAR_{i,t,[\pm d]}$ denotes the cumulative abnormal announcement return of stock $i$ in repurchase quarter $t$ over a event window of $\pm d$ days around the repurchase announcement. $RKRV_{i,t}$ measures the stocks mispricing at the time of the repurchase announcement using the method of \cite{Rhodes-Kropf2005} which we lay out in section III. $\Delta\mathbb{VAR}[\eta_{cf}]_{i,t}$ denotes the change in cash flow news variance scaled by the cross-sectional average cash flow variance prior the repurchase announcements. The ratio between the variance of discount rate news and the variance of cash flow news is represented by $ VarRatio_{i,t}$, measured as the quotient of the second moment of cash flow news divided by the second moment of cash flow news prior to the announcement. All cash flow and discount rate moments are estimated using the VAR method for all repurchasing firms using the method we outline in section III. We add year and industry fixed effects at the 2-digit industry level and standard errors are clustered at the repurchase-quarter level. t-stats are reported in parentheses.}") scalars("r2 Avg. R-squared") sfmt(%9.2f)



*** Pre SBB level of cash flow and discount rate variance

egen pre_cf_lvl_scaled = mean(pre_cf_lvl_1q) if dist ==0, by(year)
replace pre_cf_lvl_scaled = pre_cf_lvl_1q/pre_cf_lvl_scaled if dist ==0

egen pre_cf_var_scaled = mean(pre_cf_var) if dist ==0, by(year)
replace pre_cf_var_scaled = pre_cf_var/pre_cf_var_scaled if dist ==0

egen pre_disc_lvl_scaled = mean(pre_disc_lvl_1q) if dist ==0, by(year)
replace pre_disc_lvl_scaled = pre_disc_lvl_1q/pre_disc_lvl_scaled if dist ==0

egen pre_disc_var_scaled = mean(pre_disc_var) if dist ==0, by(year)
replace pre_disc_var_scaled = pre_disc_var/pre_disc_var_scaled if dist ==0





reghdfe ret_12m RKRV_firm_error pre_cf_lvl_scaled pre_cf_var_scaled pre_disc_var_scaled pre_disc_lvl_scaled c.pre_cf_lvl_scaled#c.RKRV_firm_error c.pre_disc_lvl_scaled#c.RKRV_firm_error c.pre_cf_var_scaled#c.RKRV_firm_error c.pre_disc_var_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(pre_disc_lvl_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish

reghdfe ret_12m RKRV_firm_error pre_cf_var_scaled pre_disc_var_scaled c.pre_cf_var_scaled#c.RKRV_firm_error c.pre_disc_var_scaled#c.RKRV_firm_error pre_var_ratio ln_mkvalt if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(pre_cf_var_scaled) at((p5) RKRV_firm_error) at((p95) RKRV_firm_error) vsquish



*** Industry mispricing
eststo car1: reghdfe ret_12m RKRV_sector_error cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.RKRV_sector_error c.disc_var_change_scaled#c.RKRV_sector_error pre_var_ratio ln_mkvalt ln_age debt_atl1 mtbl1 if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(disc_var_change_scaled) at((p5) RKRV_sector_error) at((p95) RKRV_sector_error) vsquish


*** MTB
eststo car1: reghdfe ret_12m mtbl1 cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.mtbl1 c.disc_var_change_scaled#c.mtbl1 pre_var_ratio ln_mkvalt ln_age debt_atl1 RKRV_firm_error if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(disc_var_change_scaled) at((p5) mtbl1) at((p95) mtbl1) vsquish


*** MTB
eststo car1: reghdfe ret_12m six_m_cum_ret cf_var_change_scaled disc_var_change_scaled c.cf_var_change_scaled#c.six_m_cum_ret c.disc_var_change_scaled#c.six_m_cum_ret pre_var_ratio ln_mkvalt ln_age debt_atl1 RKRV_firm_error if dist ==0, a(sic2 year) vce(cluster ccm_yq)
	margins, dydx(disc_var_change_scaled) at((p5) six_m_cum_ret) at((p95) six_m_cum_ret) vsquish



*** ALternative firts momments


preserve

	egen CAR_12m_cluster = xtile(CAR_12m) if dist == 0, nq(5) by(year)
	egen CAR_24m_cluster = xtile(CAR_24m) if dist == 0, nq(5) by(year)
	egen CAR_36m_cluster = xtile(CAR_36m) if dist == 0, nq(5) by(year)

	*CAR as a function of chnanges in moments

	* Scaled
	est clear
	eststo clear

	eststo CAR12: estpost tabstat cf_var_change_scaled  disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_12m_cluster) statistics(mean) columns(v)
	eststo CAR24: estpost tabstat cf_var_change_scaled  disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_24m_cluster) statistics(mean) columns(v)
	eststo CAR36: estpost tabstat cf_var_change_scaled  disc_var_change_scaled if year >=1996 & dist ==0, by(CAR_36m_cluster) statistics(mean) columns(v)

	esttab CAR12 CAR24 CAR36 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\CAR_moments.tex",  replace noobs nonumber cells("cf_var_change_scaled(fmt(%9.3f)label(Cash Flow)) disc_var_change_scaled(fmt(%9.3f)label(Discount rate))") label title(Changes in cash flow and discount rate variance conditional on long-term abnormal returns) substitute("&\multicolumn{2}{c}{}     &\multicolumn{2}{c}{}     &\multicolumn{2}{c}{}     \\" "&\multicolumn{2}{c}{12 months}     &\multicolumn{2}{c}{24 months}     &\multicolumn{2}{c}{36 months}     \\ \cmidrule(lr){2-3} \cmidrule(lr){4-5} \cmidrule(lr){6-7}" "\caption{Changes in cash flow and discount rate variance conditional on long-term abnormal returns}" "\caption{Changes in cash flow and discount rate variance conditional on long-term abnormal returns} \label{table:CAR_moments}")

	* Absolute
	est clear
	eststo clear
	eststo: estpost tabstat cf_lvl_change_abs cf_var_change_abs disc_lvl_change_abs disc_var_change_abs if year >=1996 & dist ==0, by(CAR_12m_cluster) statistics(mean) columns(v)
	eststo: estpost tabstat cf_lvl_change_abs cf_var_change_abs disc_lvl_change_abs disc_var_change_abs if year >=1996 & dist ==0, by(CAR_24m_cluster) statistics(mean) columns(v)
	eststo: estpost tabstat cf_lvl_change_abs cf_var_change_abs disc_lvl_change_abs disc_var_change_abs if year >=1996 & dist ==0, by(CAR_36m_cluster) statistics(mean) columns(v)

restore


********************************************************************************
*************************** Long run performance *******************************
********************************************************************************
preserve


	egen size_cluster = xtile(mkvalt) if dist == 0, nq(5) by(year)
	egen ivol_cluster = xtile(ivol) if dist == 0, nq(5) by(year)
	egen error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5) 				/* not yearly since we are interested in the absolute degree of misspricing */
	egen sector_error = xtile(RKRV_sector_error) if dist==0, nq(5)
	egen cf_var_cluster = xtile(cf_var_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_var_cluster = xtile(disc_var_change_scaled) if dist == 0, nq(5) by(year)
	egen cf_lvl_cluster = xtile(cf_lvl_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_lvl_cluster = xtile(disc_lvl_change_scaled) if dist == 0, nq(5) by(year)

sort dealnumber dist


	bysort dealnumber: carryforward size_cluster, replace
	bysort dealnumber: carryforward error_cluster, replace
	bysort dealnumber: carryforward sector_error, replace
	bysort dealnumber: carryforward cf_var_cluster, replace
	bysort dealnumber: carryforward disc_var_cluster, replace
	bysort dealnumber: carryforward disc_lvl_cluster, replace
	bysort dealnumber: carryforward cf_lvl_cluster, replace


	la var mktrf "MRP"
	la var smb "SMB"
	la var hml "HML"
	la var umd "Momentum"
	la var rmw "Profitability"
	la var cma "Investment"


	*** mispricing ***

	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & industry_error_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & error_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & error_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & industry_error_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & error_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & sector_error ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\firm_error_performance_analysis.tex", label replace booktabs width(1\hsize) title(Firm-specific pricing error and long-run CAR: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Firm-specific pricing error and long-run CAR: 1996-2019}" "\caption{Firm-specific pricing error and long-run CAR: 1996-2019} \label{table:firm_error_CAR}") scalars("r2 Avg. R-squared") sfmt(%9.2f)



	*** Cash Flow Variance ***

	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & cf_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & cf_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & cf_var_cluster ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\cf_var_change_performance_analysis.tex", label replace booktabs width(1\hsize) title(Change in cash flow variance and long-run CAR: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Change in cash flow variance and long-run CAR: 1996-2019}" "\caption{Change in cash flow variance and long-run CAR: 1996-2019} \label{table:cf_var_change_CAR}") scalars("r2 Avg. R-squared") sfmt(%9.2f)



	*** Discount Rate Variance ***
	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & disc_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & disc_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & disc_var_cluster ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\disc_var_change_performance_analysis.tex", label replace booktabs width(1\hsize) title(Change in discount rate variance and long-run CAR: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Change in discount rate variance and long-run CAR: 1996-2019}" "\caption{Change in discount rate variance and long-run CAR: 1996-2019} \label{table:disc_var_change_CAR}") scalars("r2 Avg. R-squared") sfmt(%9.2f)



restore

********************************************************************************


********************************************************************************
************* Long run performance conditional on mispricing *******************
********************************************************************************
preserve


	*** Undervalued ***

egen mispricing_indicator = xtile(RKRV_firm_error) if dist == 0, nq(3)
sort dealnumber dist
	bysort dealnumber: carryforward mispricing_indicator, replace


keep if mispricing_indicator == 1


	egen cf_var_cluster = xtile(cf_var_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_var_cluster = xtile(disc_var_change_scaled) if dist == 0, nq(5) by(year)
	egen cf_lvl_cluster = xtile(cf_lvl_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_lvl_cluster = xtile(disc_lvl_change_scaled) if dist == 0, nq(5) by(year)

sort dealnumber dist

	bysort dealnumber: carryforward cf_var_cluster, replace
	bysort dealnumber: carryforward disc_var_cluster, replace
	bysort dealnumber: carryforward disc_lvl_cluster, replace
	bysort dealnumber: carryforward cf_lvl_cluster, replace

	la var mktrf "MRP"
	la var smb "SMB"
	la var hml "HML"
	la var umd "Momentum"
	la var rmw "Profitability"
	la var cma "Investment"




	*** Cash Flow Variance ***

	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & cf_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & cf_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & cf_var_cluster ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\cf_var_change_performance_analysis_uval.tex", label replace booktabs width(1\hsize) title(Change in cash flow variance and long-run CAR for undervalued firms: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Change in cash flow variance and long-run CAR for undervalued firms: 1996-2019}" "\caption{Change in cash flow variance and long-run CAR for undervalued firms: 1996-2019} \label{table:cf_var_change_CAR_uval}") scalars("r2 Avg. R-squared") sfmt(%9.2f)



	*** Discount Rate Variance ***
	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & disc_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & disc_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & disc_var_cluster ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\disc_var_change_performance_analysis_uval.tex", label replace booktabs width(1\hsize) title(Change in discount rate variance and long-run CAR for undervalued firms: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Change in discount rate variance and long-run CAR for undervalued firms: 1996-2019}" "\caption{Change in discount rate variance and long-run CAR for undervalued firms: 1996-2019} \label{table:disc_var_change_CAR_uval}") scalars("r2 Avg. R-squared") sfmt(%9.2f)


restore


preserve


	*** Overvalued ***

egen mispricing_indicator = xtile(RKRV_firm_error) if dist == 0, nq(3)
sort dealnumber dist
	bysort dealnumber: carryforward mispricing_indicator, replace


keep if mispricing_indicator == 3

	egen entropy_cluster = xtile(entropy_H) if dist == 0, nq(5) by(year)
	egen binomial_entropy_cluster = xtile(entropy_H_binomial) if dist == 0, nq(5) by(year)
	egen information_cluster = xtile(entropy_I) if dist == 0, nq(5) by(year)
	egen size_cluster = xtile(mkvalt) if dist == 0, nq(5) by(year)
	egen ML_stock_entropy_cluster = xtile(ML_entropy) if dist == 0, nq(5) by(year)
	egen KT_stock_entropy_cluster = xtile(KT_Entropy) if dist == 0, nq(5) by(year)
	egen ivol_cluster = xtile(ivol) if dist == 0, nq(5) by(year)
	egen error_cluster = xtile(RKRV_firm_error) if dist == 0, nq(5) 				/* not yearly since we are interested in the absolute degree of misspricing */
	egen cf_var_cluster = xtile(cf_var_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_var_cluster = xtile(disc_var_change_scaled) if dist == 0, nq(5) by(year)
	egen cf_lvl_cluster = xtile(cf_lvl_change_scaled) if dist == 0, nq(5) by(year)
	egen disc_lvl_cluster = xtile(disc_lvl_change_scaled) if dist == 0, nq(5) by(year)

sort dealnumber dist
	bysort dealnumber: carryforward entropy_cluster, replace
	bysort dealnumber: carryforward binomial_entropy_cluster, replace
	bysort dealnumber: carryforward ML_stock_entropy_cluster, replace
	bysort dealnumber: carryforward KT_stock_entropy_cluster, replace
	bysort dealnumber: carryforward information_cluster, replace
	bysort dealnumber: carryforward size_cluster, replace
	bysort dealnumber: carryforward error_cluster, replace
	bysort dealnumber: carryforward cf_var_cluster, replace
	bysort dealnumber: carryforward disc_var_cluster, replace
	bysort dealnumber: carryforward disc_lvl_cluster, replace
	bysort dealnumber: carryforward cf_lvl_cluster, replace

	la var mktrf "MRP"
	la var smb "SMB"
	la var hml "HML"
	la var umd "Momentum"
	la var rmw "Profitability"
	la var cma "Investment"




	*** Cash Flow Variance ***

	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & cf_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & cf_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & cf_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & cf_var_cluster ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\cf_var_change_performance_analysis_oval.tex", label replace booktabs width(1\hsize) title(Change in cash flow variance and long-run CAR for overvalued firms: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Change in cash flow variance and long-run CAR for overvalued firms: 1996-2019}" "\caption{Change in cash flow variance and long-run CAR for overvalued firms: 1996-2019} \label{table:cf_var_change_CAR_oval}") scalars("r2 Avg. R-squared") sfmt(%9.2f)



	*** Discount Rate Variance ***
	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & disc_var_cluster ==1
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & disc_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & disc_var_cluster ==5
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & disc_var_cluster ==5

				esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Tables_Figures\disc_var_change_performance_analysis_oval.tex", label replace booktabs width(1\hsize) title(Change in discount rate variance and long-run CAR for overvalued firms: 1996-2019) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.1 ** 0.05 *** 0.01) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Change in discount rate variance and long-run CAR for overvalued firms: 1996-2019}" "\caption{Change in discount rate variance and long-run CAR for overvalued firms: 1996-2019} \label{table:disc_var_change_CAR_oval}") scalars("r2 Avg. R-squared") sfmt(%9.2f)


restore


********************************************************************************




































*** Old stuff **




tabstat RKRV_firm_error RKRV_sector_error RKRV_longrun_vb if year >=1996 & dist ==0, by(ivol_cluster) statistics(median) columns(variables) nototal

areg cf_var_change_rel RKRV_firm_error  KT_Entropy ivol top10instown_perc amihud_3M ln_mkvalt  si_mean i.year if dist==0 & , absorb(sic2)



reghdfe RKRV_firm_error ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & information_cluster ==1, absorb(sic2 year) vce(cluster permno)
reghdfe RKRV_firm_error ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & information_cluster ==5, absorb(sic2 year) vce(cluster permno)

reghdfe RKRV_longrun_vb ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & information_cluster ==1, absorb(sic2 year) vce(cluster permno)
reghdfe RKRV_longrun_vb ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & information_cluster ==5, absorb(sic2 year) vce(cluster permno)







est clear
eststo clear
eststo: reghdfe abs_error ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & entropy_cluster ==1, absorb(sic2 year) vce(cluster permno)
eststo: reghdfe abs_error ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & entropy_cluster ==5, absorb(sic2 year) vce(cluster permno)
eststo: reghdfe abs_error ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & information_cluster ==1, absorb(sic2 year) vce(cluster permno)
eststo: reghdfe abs_error ML_entropy ivol ret_sd instown_perc amihud_6M ln_mkvalt si_mean age if dist==0 & information_cluster ==5, absorb(sic2 year) vce(cluster permno)

esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\pricing_error_analysis.tex", label replace booktabs width(1\hsize) title(Absolute firm specific error analysis: Regression) ///
	mtitles("Low Entropy" "High Entropy" "Low Information" "High Information") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.05 ** 0.01 *** 0.001) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
	\end{tabular*} \end{tabular*}} "\caption{Absolute firm specific error analysis: Regression}" "\caption{Absolute firm specific error analysis: Regression}" "\toprule" "\toprule&\multicolumn{2}{c}{Entropy}&\multicolumn{2}{c}{Information Content}\\ \cmidrule{2-3} \cmidrule{4-5}") scalars("r2 Avg. R-squared") sfmt(%9.2f)


est clear
eststo clear
sort error_cluster
eststo: estpost tabstat RKRV_firm_error ivol instown_perc amihud_3M entropy_H entropy_I mkvalt six_m_cum_ret  si_mean, by(error_cluster) statistics(median) columns(variables) nototal
esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\error_overview_median.tex", replace ///
nomtitle nonumber cells("RKRV_firm_error(fmt(%9.2f) label(Firm error)) ivol(fmt(%9.2f) label(IVOL)) instown_perc(fmt(%9.2f)label(Inst. own.)) amihud_3M(fmt(%9.2f)label(Amihud)) entropy_H(fmt(%9.2f)label(H)) entropy_I(fmt(%9.2f)label(I)) mkvalt(fmt(%9.2f)label(Size)) six_m_cum_ret(fmt(%9.2f)label(Mom.))  si_mean(fmt(%9.2f)label(SI)) ") ///
noobs label nonumber title(Firm specific error overview: Median values)



est clear
eststo clear
sort error_cluster
eststo: estpost tabstat ML_entropy ML_entropy_1Y ML_entropy_2Y ML_entropy_3Y ML_entropy_4Y, by(error_cluster) statistics(mean) columns(variables) nototal

eststo: estpost tabstat KT_Entropy KT_Entropy_1Y KT_Entropy_2Y KT_Entropy_3Y KT_Entropy_4Y, by(error_cluster) statistics(mean) columns(variables) nototal
esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\error_efficiency.tex", replace ///
nomtitle nonumber cells("ML_entropy(fmt(%9.2f) label(pre-SBB)) ML_entropy_1Y(fmt(%9.2f) label(12 m.)) ML_entropy_2Y(fmt(%9.2f) label(24 m.)) ML_entropy_3Y(fmt(%9.2f) label(36 m.)) ML_entropy_4Y(fmt(%9.2f) label(48 m.)) KT_Entropy(fmt(%9.2f) label(pre-SBB)) KT_Entropy_1Y(fmt(%9.2f) label(12 m.)) KT_Entropy_2Y(fmt(%9.2f) label(24 m.)) KT_Entropy_3Y(fmt(%9.2f) label(36 m.)) KT_Entropy_4Y(fmt(%9.2f) label(48 m.))") ///
noobs label nonumber title(Firm specific error and stock price efficiency)  ///
substitute("\begin{tabular}{l*{2}{cccccccccc}}" "\begin{tabular}{c@{\hskip 0.25in}rrrrr@{\hskip 0.25in}rrrrr}" "\hline\hline" "\hline\hline&\multicolumn{5}{c}{\hspace{-0.0cm}Maximum Likelihood Estimator}&\multicolumn{5}{c}{Kontoyiannis Estimator}\\ \cmidrule{2-6} \cmidrule{7-11}" ///
"\caption{Firm specific error and stock price efficiency}" "\caption{Firm specific error and stock price efficiency} \label{table:errorEfficiency}" "&            &            &            &            &            &            &            &            &            &            &" "&" ///
"&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.\\" "&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.\\")

	est clear
	eststo clear
	sort year
	eststo: estpost tabstat RKRV_firm_errorl1 RKRV_sector_errorl1 RKRV_longrun_vbl1 if year >=1996 & dist ==0, by(entropy_cluster) statistics(mean median) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\ValueDecomp_yearly.tex", replace ///
	nomtitle nonumber cells("RKRV_firm_errorl1(fmt(%9.2f) label(firm-specific)) RKRV_sector_errorl1(fmt(%9.2f) label(sector))  RKRV_longrun_vbl1(fmt(%9.2f) label(value-to-book))") ///
	noobs label nonumber title(Entropy and Mispricing: 1996 - 2018)

	est clear
	eststo clear
	sort year
	eststo: estpost tabstat RKRV_firm_error RKRV_sector_error RKRV_longrun_vb if year >=1996 & dist ==0, by(entropy_cluster) statistics(mean median) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\ValueDecomp_monthly.tex", replace ///
	nomtitle nonumber cells("RKRV_firm_error(fmt(%9.2f) label(firm-specific)) RKRV_sector_error(fmt(%9.2f) label(sector))  RKRV_longrun_vb(fmt(%9.2f) label(value-to-book))") ///
	noobs label nonumber title(Entropy and Mispricing: 1996 - 2018, Monthly)



	est clear
	eststo clear
	sort year
	eststo: estpost tabstat entropy_H entropy_H_binomial if year >=1996, by(year) statistics(mean) columns(variables) nototal
	sort year
	eststo: estpost tabstat entropy_H entropy_H_binomial if mkvalt < 1000000 & year >=1996, by(year) statistics(mean) columns(variables) nototal
	sort year
	eststo: estpost tabstat entropy_H entropy_H_binomial if mkvalt > 5000000 & year >=1996, by(year) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\evolEntropy.tex", replace ///
	nomtitle nonumber cells("entropy_H(fmt(%9.2f) label(Binary)) entropy_H_binomial(fmt(%9.2f) label(Binomial))") ///
	noobs label nonumber title(Change in Entropy over time: 1996 - 2018) ///
	substitute("\hline\hline" "\hline\hline&\multicolumn{2}{c}{Full Sample}&\multicolumn{2}{c}{Small Firms}&\multicolumn{2}{c}{Large Firms}\\ \cmidrule{2-3} \cmidrule{4-5} \cmidrule{6-7}" ///
	"\caption{Change in Entropy over time: 1996 - 2018}" "\caption{Change in Entropy over time: 1996 - 2018} \label{table:EntropyChange}")

	est clear
	eststo clear
	sort binomial_entropy_cluster
	eststo: estpost tabstat ivol12m ivol24m ivol36m ivol48m, by(error_cluster) statistics(mean) columns(variables) nototal
	sort information_cluster
	eststo: estpost tabstat ivol12m ivol24m ivol36m ivol48m, by(information_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\IVol.tex", replace ///
	nomtitle nonumber cells("ivol12m(fmt(%9.2f) label(12 m.)) ivol24m(fmt(%9.2f) label(24 m.)) ivol36m(fmt(%9.2f) label(36 m.)) ivol48m(fmt(%9.2f) label(48 m.))") ///
	noobs label nonumber title(Change in Idiosyncratic Volatility subsequent to a repurchase announcement) ///
	substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{c@{\hskip 0.5in}rrrr@{\hskip 1in}rrrr}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{\hspace{-2cm}Entropy}&\multicolumn{4}{c}{Information Content}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
	"\caption{Change in Idiosyncratic Volatility subsequent to a repurchase announcement}" "\caption{Change in Idiosyncratic Volatility subsequent to a repurchase announcement} \label{table:IVOL}")

	est clear
	eststo clear
	sort binomial_entropy_cluster
	eststo: estpost tabstat ivol12m ivol24m ivol36m ivol48m if mkvalt <1000000, by(information_cluster) statistics(mean) columns(variables) nototal
	sort information_cluster
	eststo: estpost tabstat ivol12m ivol24m ivol36m ivol48m if mkvalt > 5000000, by(information_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\SIIVolSize.tex", replace ///
	nomtitle nonumber cells("ivol12m(fmt(%9.2f) label(12 m.)) ivol24m(fmt(%9.2f) label(24 m.)) ivol36m(fmt(%9.2f) label(36 m.)) ivol48m(fmt(%9.2f) label(48 m.))") ///
	noobs label nonumber title(Self-Information Content and Idiosyncratic Volatility) ///
	substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{c@{\hskip 0.5in}rrrr@{\hskip 1in}rrrr}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{\hspace{-2cm}Small Firms ($<\$1$bn.)}&\multicolumn{4}{c}{Large Firms ($>\$5$bn.)}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
	"\caption{Self-Information Content and Idiosyncratic Volatility}" "\caption{Self-Information Content and Idiosyncratic Volatility} \label{table:SIIVOLSize}")

	est clear
	eststo clear
	sort binomial_entropy_cluster
	eststo: estpost tabstat ivol12m ivol24m ivol36m ivol48m if mkvalt <1000000, by(entropy_cluster) statistics(mean) columns(variables) nototal
	sort information_cluster
	eststo: estpost tabstat ivol12m ivol24m ivol36m ivol48m if mkvalt > 5000000, by(entropy_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\HIVolSize.tex", replace ///
	nomtitle nonumber cells("ivol12m(fmt(%9.2f) label(12 m.)) ivol24m(fmt(%9.2f) label(24 m.)) ivol36m(fmt(%9.2f) label(36 m.)) ivol48m(fmt(%9.2f) label(48 m.))") ///
	noobs label nonumber title(Entropy and Idiosyncratic Volatility) ///
	substitute("\begin{tabular}{l*{2}{cccc}}" "\begin{tabular}{c@{\hskip 0.5in}rrrr@{\hskip 1in}rrrr}" "\hline\hline" "\hline\hline&\multicolumn{4}{c}{\hspace{-2cm}Small Firms ($<\$1$bn.)}&\multicolumn{4}{c}{Large Firms ($>\$5$bn.)}\\ \cmidrule{2-5} \cmidrule{6-9}" ///
	"\caption{Entropy and Idiosyncratic Volatility}" "\caption{Entropy and Idiosyncratic Volatility} \label{table:HIVOLSize}")

	est clear
	eststo clear
	sort ML_stock_entropy_cluster
	eststo: estpost tabstat ML_entropy ML_entropy_1Y ML_entropy_2Y ML_entropy_3Y ML_entropy_4Y, by(ML_stock_entropy_cluster) statistics(mean) columns(variables) nototal
	sort KT_stock_entropy_cluster
	eststo: estpost tabstat KT_Entropy KT_Entropy_1Y KT_Entropy_2Y KT_Entropy_3Y KT_Entropy_4Y, by(KT_stock_entropy_cluster) statistics(mean) columns(variables) nototal
	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\stockEntropy.tex", replace ///
	nomtitle nonumber cells("ML_entropy(fmt(%9.2f) label(pre-SBB)) ML_entropy_1Y(fmt(%9.2f) label(12 m.)) ML_entropy_2Y(fmt(%9.2f) label(24 m.)) ML_entropy_3Y(fmt(%9.2f) label(36 m.)) ML_entropy_4Y(fmt(%9.2f) label(48 m.)) KT_Entropy(fmt(%9.2f) label(pre-SBB)) KT_Entropy_1Y(fmt(%9.2f) label(12 m.)) KT_Entropy_2Y(fmt(%9.2f) label(24 m.)) KT_Entropy_3Y(fmt(%9.2f) label(36 m.)) KT_Entropy_4Y(fmt(%9.2f) label(48 m.))") ///
	noobs label nonumber title(Effect of repurchase announcement on stock price efficiency)  ///
	substitute("\begin{tabular}{l*{2}{cccccccccc}}" "\begin{tabular}{c@{\hskip 0.25in}rrrrr@{\hskip 0.25in}rrrrr}" "\hline\hline" "\hline\hline&\multicolumn{5}{c}{\hspace{-0.0cm}Maximum Likelihood Estimator}&\multicolumn{5}{c}{Kontoyiannis Estimator}\\ \cmidrule{2-6} \cmidrule{7-11}" ///
	"\caption{Effect of repurchase announcement on stock price efficiency}" "\caption{Effect of repurchase announcement on stock price efficiency} \label{table:stockEntropy}" "&            &            &            &            &            &            &            &            &            &            &" "&" ///
	"&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.\\" "&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.&     pre-SBB&       12 m.&       24 m.&       36 m.&       48 m.\\")










		esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\error_performance_analysis_small.tex", label replace booktabs width(1\hsize) title(Small firm specific errors and long-term performance: 1996-2018) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.05 ** 0.01 *** 0.001) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Small firm specific errors and long-term performance: 1996-2018}" "\caption{Small firm specific errors and long-term performance: 1996-2018} \label{table:error_lt_results_small}") scalars("r2 Avg. R-squared") sfmt(%9.2f)


	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & ML_stock_entropy_cluster ==1 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & ML_stock_entropy_cluster ==1 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & ML_stock_entropy_cluster ==1 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & ML_stock_entropy_cluster ==5 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & ML_stock_entropy_cluster ==5 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & ML_stock_entropy_cluster ==5 & mkvalt > 5000000

		esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\MLstock_entropy_analysis.tex", label replace booktabs width(1\hsize) title(Stock Price Efficiency - ML Estimator: Fama-MacBeth IRATS Approach 1995-2018) ///
			mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.05 ** 0.01 *** 0.001) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
			\end{tabular*} \end{tabular*}} "\caption{Stock Price Efficiency - ML Estimator: Fama-MacBeth IRATS Approach 1995-2018}" "\caption{Stock Price Efficiency - ML Estimator: Fama-MacBeth IRATS Approach 1995-2018} \label{table:MLStockEntropy_results}") scalars("r2 Avg. R-squared") sfmt(%9.2f)

		est clear
		eststo clear
			eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & KT_stock_entropy_cluster ==1
			eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & KT_stock_entropy_cluster ==1
			eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & KT_stock_entropy_cluster ==1
			eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & KT_stock_entropy_cluster ==5
			eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & KT_stock_entropy_cluster ==5
			eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & KT_stock_entropy_cluster ==5

			esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\KTstock_entropy_analysis.tex", label replace booktabs width(1\hsize) title(Stock Price Efficiency - KT Estimator: Fama-MacBeth IRATS Approach 1995-2018) ///
				mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.05 ** 0.01 *** 0.001) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
				\end{tabular*} \end{tabular*}} "\caption{Stock Price Efficiency - KT Estimator: Fama-MacBeth IRATS Approach 1995-2018}" "\caption{Stock Price Efficiency - KT Estimator: Fama-MacBeth IRATS Approach 1995-2018} \label{table:KTStockEntropy_results}") scalars("r2 Avg. R-squared") sfmt(%9.2f)


	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & entropy_cluster ==1 & mkvalt > 2000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & entropy_cluster ==1 & mkvalt > 2000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & entropy_cluster ==1 & mkvalt > 2000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & entropy_cluster ==5 & mkvalt > 2000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & entropy_cluster ==5 & mkvalt > 2000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & entropy_cluster ==5 & mkvalt > 2000000



	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\entropy_analysis.tex", label replace booktabs width(1\hsize) title(Binomial Entropy: Fama-MacBeth IRATS Approach 1995-2018) ///
		mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.05 ** 0.01 *** 0.001) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
		\end{tabular*} \end{tabular*}} "\caption{Entropy: Fama-MacBeth IRATS Approach 1995-2018}" "\caption{Entropy: Fama-MacBeth IRATS Approach 1995-2018} \label{table:Entropy_results}") scalars("r2 Avg. R-squared") sfmt(%9.2f)

	est clear
	eststo clear
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & information_cluster ==1 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & information_cluster ==1 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & information_cluster ==1 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & information_cluster ==5 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & information_cluster ==5 & mkvalt > 5000000
		eststo: xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & information_cluster ==5 & mkvalt > 5000000

	esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Entropy measure\sic_analysis.tex", label replace booktabs width(1\hsize) title(Self-Information Content: Fama-MacBeth IRATS Approach 1995-2018) ///
		mtitles("12M" "24M" "36M" "12M" "24M" "36M") b(%9.4f) t(%9.2f) eqlabels(none) star( * 0.05 ** 0.01 *** 0.001) substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ ///
		\end{tabular*} \end{tabular*}} "\caption{Self-Information Content: Fama-MacBeth IRATS Approach 1995-2018}" "\caption{Self-Information Content: Fama-MacBeth IRATS Approach 1995-2018} \label{table:Information_results}") scalars("r2 Avg. R-squared") sfmt(%9.2f)
