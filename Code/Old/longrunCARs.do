	
	*** long run CARs
	
	use "J:\Sascha\Buyback Anomalies\long_run_ar.dta", clear
	
	sort firm_id dealnumber dist
	
	egen group1 = group(firm_id deal_month)
	egen n_obs = count(dist), by(group1)
	replace n_obs = n_obs-1
	
	
	gen year = yofd(deal_date)
	sort group1 dist
	by group1: carryforward year, replace
	drop if year < 1991																	//Define time period, lower bound
	drop if year > 2007																	//Define time period, upper bound
	drop year
	*drop if n_obs <48

	
	
	**** cross-sectional regression for all firms *
	
		
	gen alpha_cs = .
	gen beta_cs_mktrf = .
	gen beta_cs_smb = .
	gen beta_cs_hml = .
	gen beta_cs_umd = .
	
	forvalues i = 1(1)48{														
												

					reg retrf mktrf smb hml umd if dist == `i'  																							
					replace alpha_cs = _b[_cons] if dist ==`i'
					replace beta_cs_mktrf = _b[mktrf] if dist ==`i'
					replace beta_cs_smb = _b[smb] if dist ==`i'
					replace beta_cs_hml = _b[hml] if dist ==`i'
					replace beta_cs_umd = _b[umd] if dist ==`i'
					
					}
						
	sort group1 dist
	
	by group1: gen alpha_cs_sum = sum(alpha_cs)

	egen alpha_cs_12m = total(alpha_cs) if dist <=12 & n_obs >= 12, by(group1)
	egen alpha_cs_24m = total(alpha_cs) if dist <=24 & n_obs >= 24, by(group1)
	egen alpha_cs_36m = total(alpha_cs) if dist <=36 & n_obs >= 36, by(group1)	
	egen alpha_cs_48m = total(alpha_cs) if dist <=48 & n_obs >= 48, by(group1)	
	
	*keep if dist ==0
	
	
	*** Coss-sectional Quintile regressions ***
	
	by group1: carryforward six_m_sum_ret, replace
	by group1: carryforward six_m_cum_ret, replace
	
	gen winner_sum = 0
	replace winner_sum = 1 if six_m_sum_ret > 0										// Dummy variable for winner stocks subject to the sum of prior six month returns, positive sign denotes a winner
	
	gen winner_cum = 0
	replace winner_cum = 1 if six_m_cum_ret > 0										// Dummy variable for winner stocks subject to the prior six month cumulative return, positive sign denotes a winner
	
	xtile quint_sum = six_m_sum_ret if dist==0, nq(5)								// Quintiles subject to the sum of prior six months returns, top quintile denoted with 5
	xtile quint_cum = six_m_cum_ret if dist==0, nq(5)								// Quintiles subject to the prior six months cummualtive returns, top quintile denoted with 5
	
	by group1: carryforward quint_sum, replace
	by group1: carryforward quint_cum, replace
	
	sort group1 dist
	tsset group1 dist
	
	forvalues i = 1(1)5{																// Number of Quintiles
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd if dist <=12 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_12m = _b[_cons]*12 if dist==0 & n_obs >=12
		
		gen beta_mktrf_q`i'_12m = _b[mktrf] if dist==0 & n_obs >=12
		gen se_beta_mktrf_q`i'_12m = _se[mktrf] if dist==0 & n_obs >=12
		gen t_beta_mktrf_q`i'_12m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=12
		
		gen beta_smb_q`i'_12m = _b[smb] if dist==0 & n_obs >=12
		gen se_beta_smb_q`i'_12m = _se[smb] if dist==0 & n_obs >=12
		gen t_beta_smb_q`i'_12m = _b[smb]/_se[smb] if dist==0 & n_obs >=12
		
		gen beta_hml_q`i'_12m = _b[hml] if dist==0 & n_obs >=12
		gen se_beta_hml_q`i'_12m = _se[hml] if dist==0 & n_obs >=12
		gen t_beta_hml_q`i'_12m = _b[hml]/_se[hml] if dist==0 & n_obs >=12
		
		gen beta_umd_q`i'_12m = _b[umd] if dist==0 & n_obs >=12
		gen se_beta_umd_q`i'_12m = _se[umd] if dist==0 & n_obs >=12
		gen t_beta_umd_q`i'_12m = _b[umd]/_se[umd] if dist==0 & n_obs >=12
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd if dist <=24 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_24m = _b[_cons]*24 if dist==0 & n_obs >=24
		*gen se_alpha_q1_24m = 
		
		gen beta_mktrf_q`i'_24m = _b[mktrf] if dist==0 & n_obs >=24
		gen se_beta_mktrf_q`i'_24m = _se[mktrf] if dist==0 & n_obs >=24
		gen t_beta_mktrf_q`i'_24m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=24
		
		gen beta_smb_q`i'_24m = _b[smb] if dist==0 & n_obs >=24
		gen se_beta_smb_q`i'_24m = _se[smb] if dist==0 & n_obs >=24
		gen t_beta_smb_q`i'_24m = _b[smb]/_se[smb] if dist==0 & n_obs >=24
		
		gen beta_hml_q`i'_24m = _b[hml] if dist==0 & n_obs >=24
		gen se_beta_hml_q`i'_24m = _se[hml] if dist==0 & n_obs >=24
		gen t_beta_hml_q`i'_24m = _b[hml]/_se[hml] if dist==0 & n_obs >=24
		
		gen beta_umd_q`i'_24m = _b[umd] if dist==0 & n_obs >=24
		gen se_beta_umd_q`i'_24m = _se[umd] if dist==0 & n_obs >=24
		gen t_beta_umd_q`i'_24m = _b[umd]/_se[umd] if dist==0 & n_obs >=24
		
		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd if dist <=36 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_36m = _b[_cons]*36 if dist==0 & n_obs >=36
		*gen se_alpha_q1_36m = 
		
		gen beta_mktrf_q`i'_36m = _b[mktrf] if dist==0 & n_obs >=36
		gen se_beta_mktrf_q`i'_36m = _se[mktrf] if dist==0 & n_obs >=36
		gen t_beta_mktrf_q`i'_36m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=36
		
		gen beta_smb_q`i'_36m = _b[smb] if dist==0 & n_obs >=36
		gen se_beta_smb_q`i'_36m = _se[smb] if dist==0 & n_obs >=36
		gen t_beta_smb_q`i'_36m = _b[smb]/_se[smb] if dist==0 & n_obs >=36
		
		gen beta_hml_q`i'_36m = _b[hml] if dist==0 & n_obs >=36
		gen se_beta_hml_q`i'_36m = _se[hml] if dist==0 & n_obs >=36
		gen t_beta_hml_q`i'_36m = _b[hml]/_se[hml] if dist==0 & n_obs >=36
		
		gen beta_umd_q`i'_36m = _b[umd] if dist==0 & n_obs >=36
		gen se_beta_umd_q`i'_36m = _se[umd] if dist==0 & n_obs >=36
		gen t_beta_umd_q`i'_36m = _b[umd]/_se[umd] if dist==0 & n_obs >=36
		
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & quint_cum == `i'
	
		gen alpha_q`i'_48m = _b[_cons]*48 if dist==0 & n_obs >=48
		*gen se_alpha_q1_48m = 
		
		gen beta_mktrf_q`i'_48m = _b[mktrf] if dist==0 & n_obs >=48
		gen se_beta_mktrf_q`i'_48m = _se[mktrf] if dist==0 & n_obs >=48
		gen t_beta_mktrf_q`i'_48m = _b[mktrf]/_se[mktrf] if dist==0 & n_obs >=48
		
		gen beta_smb_q`i'_48m = _b[smb] if dist==0 & n_obs >=48
		gen se_beta_smb_q`i'_48m = _se[smb] if dist==0 & n_obs >=48
		gen t_beta_smb_q`i'_48m = _b[smb]/_se[smb] if dist==0 & n_obs >=48
		
		gen beta_hml_q`i'_48m = _b[hml] if dist==0 & n_obs >=48
		gen se_beta_hml_q`i'_48m = _se[hml] if dist==0 & n_obs >=48
		gen t_beta_hml_q`i'_48m = _b[hml]/_se[hml] if dist==0 & n_obs >=48
		
		gen beta_umd_q`i'_48m = _b[umd] if dist==0 & n_obs >=48
		gen se_beta_umd_q`i'_48m = _se[umd] if dist==0 & n_obs >=48
		gen t_beta_umd_q`i'_48m = _b[umd]/_se[umd] if dist==0 & n_obs >=48
		
		
		}
	
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
	gen se_alpha =.
	gen se_alpha_sq = .
	
	forvalues i = 1(1)5{
	
		forvalues j = 1(1)48{														
												

				reg retrf mktrf smb hml umd if dist == `j' & quint_cum == `i' 																							
				replace se_alpha = _se[_cons] if dist ==`j'
				
						}
				 
				replace se_alpha_sq = se_alpha^2
				egen sum_se_alpha_sq_12m = total(se_alpha_sq) if dist <=12 & n_obs >= 12, by(group1)
				egen sum_se_alpha_sq_24m = total(se_alpha_sq) if dist <=24 & n_obs >= 24, by(group1)
				egen sum_se_alpha_sq_36m = total(se_alpha_sq) if dist <=36 & n_obs >= 36, by(group1)
				egen sum_se_alpha_sq_48m = total(se_alpha_sq) if dist <=48 & n_obs >= 48, by(group1)
				
				gen t_alpha_q`i'_12m = alpha_q`i'_12m/sqrt(sum_se_alpha_sq_12m)
				gen t_alpha_q`i'_24m = alpha_q`i'_24m/sqrt(sum_se_alpha_sq_24m)
				gen t_alpha_q`i'_36m = alpha_q`i'_36m/sqrt(sum_se_alpha_sq_36m)
				gen t_alpha_q`i'_48m = alpha_q`i'_48m/sqrt(sum_se_alpha_sq_48m)
				
				drop sum_se_alpha_sq_12m sum_se_alpha_sq_24m sum_se_alpha_sq_36m sum_se_alpha_sq_48m	
			}
