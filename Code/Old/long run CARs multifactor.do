*** Performance evaluation ***
	
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
	drop if dist < -24
	drop if year < 1991																	//Define time period, lower bound
	drop if year > 2007																	//Define time period, upper bound
	drop year
	*drop if n_obs <48
	
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
	
	
	* positive net governance score
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & net_hum_score > 0 /*& size_cluster ==2 *//*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_pos_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_pos_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_pos_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_pos_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_pos_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_pos_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_pos_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_pos_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_pos_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_pos_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_pos_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_pos_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		gen beta_rmw_pos_12m = _b[rmw] if dist==0 & n_obs >=12
		gen se_beta_rmw_pos_12m = _se[rmw] if dist==0 & n_obs >=12
		gen t_beta_rmw_pos_12m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=12
		
		gen beta_cma_pos_12m = _b[cma] if dist==0 & n_obs >=12
		gen se_beta_cma_pos_12m = _se[cma] if dist==0 & n_obs >=12
		gen t_beta_cma_pos_12m = _b[cma]/_se[cma] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & net_hum_score > 0 /*& size_cluster ==2*/ /*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		
		gen beta_mktrf_pos_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_pos_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_pos_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_pos_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_pos_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_pos_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_pos_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_pos_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_pos_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_pos_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_pos_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_pos_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		gen beta_rmw_pos_24m = _b[rmw] if dist==0 & n_obs >=24
		gen se_beta_rmw_pos_24m = _se[rmw] if dist==0 & n_obs >=24
		gen t_beta_rmw_pos_24m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=24
		
		gen beta_cma_pos_24m = _b[cma] if dist==0 & n_obs >=24
		gen se_beta_cma_pos_24m = _se[cma] if dist==0 & n_obs >=24
		gen t_beta_cma_pos_24m = _b[cma]/_se[cma] if dist==0 & n_obs >=24

		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & net_hum_score > 0 /*& size_cluster ==2 *//*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		
		gen beta_mktrf_pos_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_pos_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_pos_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_pos_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_pos_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_pos_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_pos_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_pos_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_pos_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_pos_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_pos_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_pos_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		gen beta_rmw_pos_36m = _b[rmw] if dist==0 & n_obs >=36
		gen se_beta_rmw_pos_36m = _se[rmw] if dist==0 & n_obs >=36
		gen t_beta_rmw_pos_36m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=36
		
		gen beta_cma_pos_36m = _b[cma] if dist==0 & n_obs >=36
		gen se_beta_cma_pos_36m = _se[cma] if dist==0 & n_obs >=36
		gen t_beta_cma_pos_36m = _b[cma]/_se[cma] if dist==0 & n_obs >=36
	
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 & net_hum_score > 0 /*& size_cluster ==2*/ /*, lag(3)*/			// change score type!!!
	
		gen alpha_pos_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		
		gen beta_mktrf_pos_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_pos_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_pos_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_pos_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_pos_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_pos_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_pos_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_pos_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_pos_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_pos_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_pos_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_pos_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48
		
		gen beta_rmw_pos_48m = _b[rmw] if dist==0 & n_obs >=48
		gen se_beta_rmw_pos_48m = _se[rmw] if dist==0 & n_obs >=48
		gen t_beta_rmw_pos_48m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=48
		
		gen beta_cma_pos_48m = _b[cma] if dist==0 & n_obs >=48
		gen se_beta_cma_pos_48m = _se[cma] if dist==0 & n_obs >=48
		gen t_beta_cma_pos_48m = _b[cma]/_se[cma] if dist==0 & n_obs >=48
	
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		gen se_alpha =.
		gen se_alpha_sq = .
		gen obs =.
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd rmw cma if dist == `i' & net_hum_score > 0 /*& size_cluster ==2	*/																						
				replace se_alpha = _se[_cons] if dist ==`i'
				replace obs = e(N) if dist ==`i'
				
						}
				 
		replace se_alpha_sq = se_alpha^2
		egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
		egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
		egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
		egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
		gen t_alpha_pos_12m = alpha_pos_12m/sqrt(sum_se_alpha_sq_12m)
		gen t_alpha_pos_24m = alpha_pos_24m/sqrt(sum_se_alpha_sq_24m)
		gen t_alpha_pos_36m = alpha_pos_36m/sqrt(sum_se_alpha_sq_36m)
		gen t_alpha_pos_48m = alpha_pos_48m/sqrt(sum_se_alpha_sq_48m)
				
		drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m
		
		egen sum_obs_pos_12m = total(obs) if dist <=12 & n_obs >= 12, by(group1)
		egen mean_obs_pos_12m = mean(obs) if dist <=12 & n_obs >= 12, by(group1)
		egen max_obs_pos_12m = max(obs) if dist <=12 & n_obs >= 12, by(group1)
		egen min_obs_pos_12m = min(obs) if dist <=12 & n_obs >= 12, by(group1)
		
		egen sum_obs_pos_24m = total(obs) if dist <=24 & n_obs >= 24, by(group1)
		egen mean_obs_pos_24m = mean(obs) if dist <=24 & n_obs >= 24, by(group1)
		egen max_obs_pos_24m = max(obs) if dist <=24 & n_obs >= 24, by(group1)
		egen min_obs_pos_24m = min(obs) if dist <=24 & n_obs >= 24, by(group1)
		
		egen sum_obs_pos_36m = total(obs) if dist <=36 & n_obs >= 36, by(group1)
		egen mean_obs_pos_36m = mean(obs) if dist <=36 & n_obs >= 36, by(group1)
		egen max_obs_pos_36m = max(obs) if dist <=36 & n_obs >= 36, by(group1)
		egen min_obs_pos_36m = min(obs) if dist <=36 & n_obs >= 36, by(group1)
		
		egen sum_obs_pos_48m = total(obs) if dist <=48 & n_obs >= 48, by(group1)
		egen mean_obs_pos_48m = mean(obs) if dist <=48 & n_obs >= 48, by(group1)
		egen max_obs_pos_48m = max(obs) if dist <=48 & n_obs >= 48, by(group1)
		egen min_obs_pos_48m = min(obs) if dist <=48 & n_obs >= 48, by(group1)

		
		
		
	* negative net governance score
	
		* 12 months
	
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=12 & dist > 0 & net_hum_score <= 0 /*& size_cluster ==1*//*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_neg_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_neg_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_neg_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_neg_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_neg_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_neg_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_neg_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_neg_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_neg_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_neg_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_neg_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_neg_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		gen beta_rmw_neg_12m = _b[rmw] if dist==0 & n_obs >=12
		gen se_beta_rmw_neg_12m = _se[rmw] if dist==0 & n_obs >=12
		gen t_beta_rmw_neg_12m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=12
		
		gen beta_cma_neg_12m = _b[cma] if dist==0 & n_obs >=12
		gen se_beta_cma_neg_12m = _se[cma] if dist==0 & n_obs >=12
		gen t_beta_cma_neg_12m = _b[cma]/_se[cma] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=24 & dist > 0 & net_hum_score <= 0 /*& size_cluster ==1 *//*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		
		gen beta_mktrf_neg_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_neg_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_neg_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_neg_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_neg_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_neg_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_neg_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_neg_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_neg_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_neg_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_neg_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_neg_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		gen beta_rmw_neg_24m = _b[rmw] if dist==0 & n_obs >=24
		gen se_beta_rmw_neg_24m = _se[rmw] if dist==0 & n_obs >=24
		gen t_beta_rmw_neg_24m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=24
		
		gen beta_cma_neg_24m = _b[cma] if dist==0 & n_obs >=24
		gen se_beta_cma_neg_24m = _se[cma] if dist==0 & n_obs >=24
		gen t_beta_cma_neg_24m = _b[cma]/_se[cma] if dist==0 & n_obs >=24

		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=36 & dist > 0 & net_hum_score <= 0 /* & size_cluster ==1*/ /*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		
		gen beta_mktrf_neg_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_neg_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_neg_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_neg_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_neg_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_neg_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_neg_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_neg_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_neg_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_neg_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_neg_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_neg_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		gen beta_rmw_neg_36m = _b[rmw] if dist==0 & n_obs >=36
		gen se_beta_rmw_neg_36m = _se[rmw] if dist==0 & n_obs >=36
		gen t_beta_rmw_neg_36m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=36
		
		gen beta_cma_neg_36m = _b[cma] if dist==0 & n_obs >=36
		gen se_beta_cma_neg_36m = _se[cma] if dist==0 & n_obs >=36
		gen t_beta_cma_neg_36m = _b[cma]/_se[cma] if dist==0 & n_obs >=36
	
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd rmw cma if dist <=48 & dist > 0 & net_hum_score <= 0 /*& size_cluster ==1 *//*, lag(3)*/			// change score type!!!
	
		gen alpha_neg_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		
		gen beta_mktrf_neg_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_neg_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_neg_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_neg_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_neg_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_neg_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_neg_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_neg_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_neg_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_neg_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_neg_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_neg_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48
		
		gen beta_rmw_neg_48m = _b[rmw] if dist==0 & n_obs >=48
		gen se_beta_rmw_neg_48m = _se[rmw] if dist==0 & n_obs >=48
		gen t_beta_rmw_neg_48m = _b[rmw]/_se[rmw] if dist==0 & n_obs >=48
		
		gen beta_cma_neg_48m = _b[cma] if dist==0 & n_obs >=48
		gen se_beta_cma_neg_48m = _se[cma] if dist==0 & n_obs >=48
		gen t_beta_cma_neg_48m = _b[cma]/_se[cma] if dist==0 & n_obs >=48
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		replace se_alpha =.
		replace se_alpha_sq = .
		replace obs = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd rmw cma if dist == `i' & net_hum_score <= 0 /*& size_cluster ==1	*/																						
				replace se_alpha = _se[_cons] if dist ==`i'
				replace obs = e(N) if dist ==`i'
						}
				 
		replace se_alpha_sq = se_alpha^2
		egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
		egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
		egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
		egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
		gen t_alpha_neg_12m = alpha_neg_12m/sqrt(sum_se_alpha_sq_12m)
		gen t_alpha_neg_24m = alpha_neg_24m/sqrt(sum_se_alpha_sq_24m)
		gen t_alpha_neg_36m = alpha_neg_36m/sqrt(sum_se_alpha_sq_36m)
		gen t_alpha_neg_48m = alpha_neg_48m/sqrt(sum_se_alpha_sq_48m)
				
		drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m
		
		egen sum_obs_neg_12m = total(obs) if dist <=12 & n_obs >= 12, by(group1)
		egen mean_obs_neg_12m = mean(obs) if dist <=12 & n_obs >= 12, by(group1)
		egen max_obs_neg_12m = max(obs) if dist <=12 & n_obs >= 12, by(group1)
		egen min_obs_neg_12m = min(obs) if dist <=12 & n_obs >= 12, by(group1)
		
		egen sum_obs_neg_24m = total(obs) if dist <=24 & n_obs >= 24, by(group1)
		egen mean_obs_neg_24m = mean(obs) if dist <=24 & n_obs >= 24, by(group1)
		egen max_obs_neg_24m = max(obs) if dist <=24 & n_obs >= 24, by(group1)
		egen min_obs_neg_24m = min(obs) if dist <=24 & n_obs >= 24, by(group1)
		
		egen sum_obs_neg_36m = total(obs) if dist <=36 & n_obs >= 36, by(group1)
		egen mean_obs_neg_36m = mean(obs) if dist <=36 & n_obs >= 36, by(group1)
		egen max_obs_neg_36m = max(obs) if dist <=36 & n_obs >= 36, by(group1)
		egen min_obs_neg_36m = min(obs) if dist <=36 & n_obs >= 36, by(group1)
		
		egen sum_obs_neg_48m = total(obs) if dist <=48 & n_obs >= 48, by(group1)
		egen mean_obs_neg_48m = mean(obs) if dist <=48 & n_obs >= 48, by(group1)
		egen max_obs_neg_48m = max(obs) if dist <=48 & n_obs >= 48, by(group1)
		egen min_obs_neg_48m = min(obs) if dist <=48 & n_obs >= 48, by(group1)
		
		
		
	*Delta CARs
		gen delta_pos_0_12 = alpha_pos_12m
		gen delta_pos_12_24 = alpha_pos_24m - alpha_pos_12m
		gen delta_pos_24_36 = alpha_pos_36m - alpha_pos_24m
		gen delta_pos_36_48 = alpha_pos_48m - alpha_pos_36m
		gen delta_neg_0_12 = alpha_neg_12m
		gen delta_neg_12_24 = alpha_neg_24m - alpha_neg_12m
		gen delta_neg_24_36 = alpha_neg_36m - alpha_neg_24m
		gen delta_neg_36_48 = alpha_neg_48m - alpha_neg_36m
		
	
	*Mean absolute performance 1-48 months after announcement
		gen cum_ret = ln(1+retrf) if dist > 0
		by group1: replace cum_ret = sum(cum_ret) if dist > 0
		replace cum_ret = exp(cum_ret)-1
			
			*positive score
		gen pos_mean_performance = .
		forvalues i = 1(1)48{
			sum cum_ret if dist == `i' & net_gov_score > 0
			replace pos_mean_performance = r(mean) if dist ==`i'
			}
			
			*negative score
		gen neg_mean_performance = .
		forvalues i = 1(1)48{
			sum cum_ret if dist == `i' & net_gov_score < 0
			replace neg_mean_performance = r(mean) if dist ==`i'
			}
			