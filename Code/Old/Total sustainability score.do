*** Total Social Responsbility Scores
	
	use "J:\Sascha\Buyback Anomalies\KLD.dta", clear
	drop if cusip == ""
	duplicates drop year cusip, force
	save "$temp\KLD_raw.dta", replace
	
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
	
	gen year = yofd(deal_date)
	merge m:1 cusip year using "$temp\KLD_raw.dta"
	sort cusip month
	drop if dealnumber ==.
	drop _merge companyid ticker companyname
	
	**Diversity																			// All indicators are constructed as done by Kecskes et al. 2016
	
	*strength: women and minorities
	gen wom_and_min = (div_str_a+ div_str_b+ div_str_c+ div_str_e- div_con_a- div_con_b)
	
	*strength: work-life balance
	gen wrklf_bal = div_str_d
	
	*strength: disabled people
	gen dis_ppl = div_str_f
	
	*Strength: gays and lesbians
	gen gay_and_les = div_str_g
	
	*other strengths minus other concerns
	gen other_str_div = div_str_x - div_con_x
	
	* toatal diversity score
	gen div_tot = wom_and_min + wrklf_bal + dis_ppl + gay_and_les + other_str_div 
	
	
	
	**Employee relations
	
	
	*strength: union relations
	gen union_rel = emp_str_a - emp_con_a
	
	*strength: employee profit sharing
	gen empl_prof_shr = emp_str_c + emp_str_d
	
	*strength: retirement benefits
	gen ret_benf = emp_str_f - emp_con_d
	
	*strength: health and safety
	gen hlth_and_sfty = emp_str_g - emp_con_b
	
	*other strengths minus other concerns
	gen other_str_emp = emp_str_x - emp_con_x
	
	*total employee score
	gen empl_tot = union_rel + empl_prof_shr + ret_benf + hlth_and_sfty + other_str_emp
	
	
	**Community
	
	*strength: charity
	gen charity = com_str_a + com_str_b + com_str_f + com_str_g
	
	*strength: support for housing
	gen sup_housing = com_str_c
	
	*strength: support for housing
	gen sup_edu = com_str_d
	
	*other strengths minus other concerns
	gen other_str_com = com_str_x - com_con_x
	
	*concern: negative economic impact
	gen neg_eco_imp = com_con_b
	
	*total community score
	gen com_tot = charity + sup_housing + sup_edu + other_str_com - neg_eco_imp
	
	
	** Environment
	
	*strenght: products and services
	gen prod_serv = env_str_a
	
	*strenght: pollution prevention
	gen pol_prev = env_str_b
	
	*strenght: recycling
	gen recycl = env_str_c
	
	*strenght: clean energy usage
	gen cln_ener = env_str_d
	
	*other strengths minus other concerns
	gen other_str_env = env_str_x - env_con_x
	
	*concern: legal and regulatory problems
	gen lgl_regl = env_con_a + env_con_b
	
	*concern: excessive pollution
	gen ex_pol = env_con_d
	
	*total environment score
	gen env_tot = prod_serv + pol_prev + recycl + cln_ener + other_str_env - lgl_regl - ex_pol
	
	
	
	*total score
	gen kld_total = div_tot + empl_tot + com_tot + env_tot
	
	sort group1 kld_total dist
	by group1: carryforward kld_total, replace
	
	
	sort group1 dist
	tsset group1 dist
	
	* positive total KLD score
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd if dist <=12 & dist > 0 & kld_total > 0
	
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
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd if dist <=24 & dist > 0 & kld_total > 0
	
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
		
		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd if dist <=36 & dist > 0 & kld_total > 0
	
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
		
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & kld_total > 0
	
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
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		gen se_alpha =.
		gen se_alpha_sq = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd if dist == `i' & kld_total > 0																							
				replace se_alpha = _se[_cons] if dist ==`i'
				
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
		
		
	* negative total KLD score
	
	* 12 months
	
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & kld_total < 0
	
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
		
		
	* 24 months
		
	xtfmb retrf mktrf smb hml umd if dist <=24 & dist > 0 & kld_total < 0
	
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
		
		
	* 36 months
		
	xtfmb retrf mktrf smb hml umd if dist <=36 & dist > 0 & kld_total < 0
	
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
		
		
		* 48 months
		
	xtfmb retrf mktrf smb hml umd if dist <=48 & dist > 0 & kld_total < 0
	
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
	
	* t-values for alpha																		// Cannot be computed with the Fama-MacBeth procedure, yields standard error for average alpha and not sum of alphas
	
		replace se_alpha =.
		replace se_alpha_sq = .
	
		forvalues i = 1(1)48{														
												

				reg retrf mktrf smb hml umd if dist == `i' & kld_total < 0																							
				replace se_alpha = _se[_cons] if dist ==`i'
				
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
		
		
		
		
	
	g year = year(deal_date)
	
	keep cusip deal_date year alpha_12m alpha_24m alpha_36m alpha_48m
	sort cusip year
	
	save "$data\repurchase_alphas.dta", replace	
	
*	use "$data\repurchase_cars.dta", clear
	
*	sort dealnumber
	
	
	
	use "$data\Compustat_sample.dta", replace
	
	sort cusip year
	merge 1:n cusip year using "$data\repurchase_cars.dta"
	keep if _merge == 3
	drop _merge
	
	sort cusip year
	merge n:n cusip year using "$data\repurchase_alphas.dta"
	keep if _merge == 3
	drop _merge
	
	tabstat car_11_m, by(year) stats(n sd med mean)
	tabstat car_11_ff, by(year) stats(n sd med mean)
	tabstat car_33_m, by(year) stats(n sd med mean)
	tabstat car_33_ff, by(year) stats(n sd med mean)
	
	reg car_11_m, vce(cl sic)	
	reg car_11_m, vce(cl year)	
	reg car_11_ff, vce(cl sic)	
	reg car_11_ff, vce(cl year)	
	
	reg car_11_m l_a mb cash_at div_d sr_index, cluster(gvkey)
	
	reg car_11_m l_a mb cash_at div_d cgov_str_numl1, cluster(gvkey) 
	
	g sr_pos = 1 if sr_index >=0
	replace sr_pos = 0 if sr_index <0
	replace sr_pos = . if sr_index == .
	
	g sr_neg = 1 if sr_index <0
	replace sr_neg = 0 if sr_index >=0
	replace sr_neg = . if sr_index == .		
	
	reg car_11_m l_a mb cash_at div_d sr_pos, cluster(gvkey)
	
	reg car_11_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic)
	reg car_33_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic)
	reg car_55_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic)
	
	areg car_11_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic) a(sic)
	areg car_33_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic) a(sic)
	areg car_55_ff l_a mb cash_at div_d i.cgov_str_num sr_pos i.year, cluster(sic) a(sic)
	
	areg car_11_ff l_al1 mbl1 cash_atl1 div_d i.cgov_str_numl1 i.year, cluster(sic) a(sic)
	areg car_33_ff l_al1 mbl1 cash_atl1 div_d i.cgov_str_numl1 i.year, cluster(sic) a(sic)
	areg car_55_ff l_al1 mbl1 cash_atl1 div_d i.cgov_str_numl1 i.year, cluster(sic) a(sic)	
