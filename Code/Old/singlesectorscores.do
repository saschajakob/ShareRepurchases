
	
	***** single sector scores
	
	use "J:\Sascha\Buyback Anomalies\KLD_clean.dta", clear
	duplicates drop year cusip, force
	egen firm_id = group(cusip)
	
		gen net_gov_score = cgov_str_num - cgov_con_num
		gen net_env_score = env_str_num - env_con_num
		gen net_hum_score = hum_str_num - hum_con_num
		gen net_emp_score = emp_str_num - emp_con_num
	
	sort firm_id year
	tsset firm_id year
	
		gen delta_net_gov_score = d.net_env_score
		gen lead_delta_net_gov_score = f.delta_net_gov_score
		gen two_lead_delta_net_gov_score = f2.delta_net_gov_score
		gen three_lead_delta_net_gov_score = f3.delta_net_gov_score
		gen four_lead_delta_net_gov_score = f4.delta_net_gov_score
		gen five_lead_delta_net_gov_score = f5.delta_net_gov_score
		
			gen y0_y4_delta_net_gov_score = lead_delta_net_gov_score + two_lead_delta_net_gov_score ///
			+ three_lead_delta_net_gov_score + four_lead_delta_net_gov_score + five_lead_delta_net_gov_score
			gen y0_y2_delta_net_gov_score = lead_delta_net_gov_score + two_lead_delta_net_gov_score + three_lead_delta_net_gov_score
			gen y3_y4_delta_net_gov_score = four_lead_delta_net_gov_score + five_lead_delta_net_gov_score
			
			drop lead_delta_net_gov_score two_lead_delta_net_gov_score ///
			three_lead_delta_net_gov_score four_lead_delta_net_gov_score ///
			five_lead_delta_net_gov_score
			
			
		gen delta_net_env_score = d.net_env_score
		gen lead_delta_net_env_score = f.delta_net_env_score
		gen two_lead_delta_net_env_score = f2.delta_net_env_score
		gen three_lead_delta_net_env_score = f3.delta_net_env_score
		gen four_lead_delta_net_env_score = f4.delta_net_env_score
		gen five_lead_delta_net_env_score = f5.delta_net_env_score
		
			gen y0_y4_delta_net_env_score = lead_delta_net_env_score + two_lead_delta_net_env_score ///
			+ three_lead_delta_net_env_score + four_lead_delta_net_env_score + five_lead_delta_net_env_score
			gen y0_y2_delta_net_env_score = lead_delta_net_env_score + two_lead_delta_net_env_score + three_lead_delta_net_env_score
			gen y3_y4_delta_net_env_score = four_lead_delta_net_env_score + five_lead_delta_net_env_score
			
			drop lead_delta_net_env_score two_lead_delta_net_env_score ///
			three_lead_delta_net_env_score four_lead_delta_net_env_score ///
			five_lead_delta_net_env_score
			
			
		gen delta_net_emp_score = d.net_env_score
		gen lead_delta_net_emp_score = f.delta_net_emp_score
		gen two_lead_delta_net_emp_score = f2.delta_net_emp_score
		gen three_lead_delta_net_emp_score = f3.delta_net_emp_score
		gen four_lead_delta_net_emp_score = f4.delta_net_emp_score
		gen five_lead_delta_net_emp_score = f5.delta_net_emp_score
		
			gen y0_y4_delta_net_emp_score = lead_delta_net_emp_score + two_lead_delta_net_emp_score ///
			+ three_lead_delta_net_emp_score + four_lead_delta_net_emp_score + five_lead_delta_net_emp_score
			gen y0_y2_delta_net_emp_score = lead_delta_net_emp_score + two_lead_delta_net_emp_score + three_lead_delta_net_emp_score
			gen y3_y4_delta_net_emp_score = four_lead_delta_net_emp_score + five_lead_delta_net_emp_score
			
			drop lead_delta_net_emp_score two_lead_delta_net_emp_score ///
			three_lead_delta_net_emp_score four_lead_delta_net_emp_score ///
			five_lead_delta_net_emp_score
			
			
		gen delta_net_hum_score = d.net_env_score
		gen lead_delta_net_hum_score = f.delta_net_hum_score
		gen two_lead_delta_net_hum_score = f2.delta_net_hum_score
		gen three_lead_delta_net_hum_score = f3.delta_net_hum_score
		gen four_lead_delta_net_hum_score = f4.delta_net_hum_score
		gen five_lead_delta_net_hum_score = f5.delta_net_hum_score
		
			gen y0_y4_delta_net_hum_score = lead_delta_net_hum_score + two_lead_delta_net_hum_score ///
			+ three_lead_delta_net_hum_score + four_lead_delta_net_hum_score + five_lead_delta_net_hum_score
			gen y0_y2_delta_net_hum_score = lead_delta_net_hum_score + two_lead_delta_net_hum_score + three_lead_delta_net_hum_score
			gen y3_y4_delta_net_hum_score = four_lead_delta_net_hum_score + five_lead_delta_net_hum_score
			
			drop lead_delta_net_hum_score two_lead_delta_net_hum_score ///
			three_lead_delta_net_hum_score four_lead_delta_net_hum_score ///
			five_lead_delta_net_hum_score
			
	
	replace year = year + 1
	sort firm_id year
	
	save "J:\Sascha\Buyback Anomalies\KLD_clean.dta", replace
