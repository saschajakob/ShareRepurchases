
set rmsg on
set more off

************************************************************************
*
*	JOB:		format_compustat.do
*	PROJECT:	The effects of permanent and transitory shocks on investment and cash
*	INPUT:		Compustat and other files
*	OUTPUT:		Sample
*
*	DESCRIPTION: This job creates a panel data set with compustat data
*				 at the annual frequency
*
*************************************************************************

set more 1
set matsize 800
set scrollbufsize 500000		
capture program drop _all
clear
clear matrix

**	0.i	Philip computer
	global 	codes	"C:\Users\valta\Dropbox\work\CFshocks_investment\codes"
	global 	data	"C:\Users\valta\Dropbox\work\CFshocks_investment\data"	
	global	comp    "C:\Users\valta\Dropbox\work\data\comp"
	global  temp	"C:/Users/valta/Desktop/temp"
	
	global  data2	"C:\Users\valta\Dropbox\work\SBB\Data"

	
**	0.i	Philip Laptop
	global 	codes	"C:\Users\Philip Valta\Dropbox\work\CFshocks_investment\codes"
	global 	data	"C:\Users\Philip Valta\Dropbox\work\CFshocks_investment\data"	
	global	comp    "C:\Users\Philip Valta\Dropbox\work\data\comp"
	global  temp	"C:/Users/Philip Valta/Desktop/temp"
	
	global  data2	"C:\Users\Philip Valta\Dropbox\work\SBB\Data"	
	
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

*************************
**** Begin: Analysis ****
*************************
  
**** Load data
	use "$data2\Compustat_sample.dta", clear
	
	tabstat net_rep_me net_rep2_me rep, stats(n mean sd min p50 max) c(s)
	su buy buy2, d
	
	gen soc_resp = div_str_num + div_con_num + emp_str_num + emp_con_num + com_str_num + com_con_num + env_str_num + env_con_num
	
	
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d, cluster(gvkey)
	
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, cluster(gvkey)
	reg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, cluster(gvkey)
	reg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, cluster(gvkey)
	reg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, cluster(gvkey)
	reg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, cluster(gvkey)
	
	areg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, a(gvkey) cluster(gvkey)
	areg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, a(gvkey) cluster(gvkey)
	areg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, a(gvkey) cluster(gvkey)
	areg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, a(gvkey) cluster(gvkey)
	areg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d eindexl1, a(gvkey) cluster(gvkey)
	
	* Use MSCI data
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, cluster(gvkey)
	reg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, cluster(gvkey)
	reg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, cluster(gvkey)
	reg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, cluster(gvkey)
	reg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, cluster(gvkey)	
	*
	areg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, a(gvkey) cluster(gvkey)
	areg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, a(gvkey) cluster(gvkey)
	areg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, a(gvkey) cluster(gvkey)
	areg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, a(gvkey) cluster(gvkey)
	areg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d cgov_str_numl1, a(gvkey) cluster(gvkey)		
	
	
	* Use SR Index
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)
	reg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)
	reg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)
	reg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)
	reg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)	
	*
	areg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)
	areg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)
	areg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)
	areg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)
	areg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)	
	*
	areg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)
	areg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)
	areg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)
	areg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)
	areg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)	
	
	
	g sr_pos = 1 if sr_index >=0
	replace sr_pos = 0 if sr_index <0
	replace sr_pos = . if sr_index == .
	
	g sr_neg = 1 if sr_index <0
	replace sr_neg = 0 if sr_index >=0
	replace sr_neg = . if sr_index == .	
	
	
	areg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	areg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	areg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	areg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	areg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)	
	
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	reg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	reg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	reg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	reg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)	
	
	
	areg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	areg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	areg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	areg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	areg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)	
	
	reg net_rep_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	reg net_rep2_me l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	reg rep l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	reg buy l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)
	reg buy2 l_al1 mbl1 cf3_atl1 cash_atl1 div_d sr_neg, a(gvkey) cluster(gvkey)	
	
	
	
	* Valuation of firms
	areg mb l_al1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)
	reg mb l_al1 cf3_atl1 cash_atl1 div_d sr_indexl1, cluster(gvkey)	
	
	areg mb l_al1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)
	reg mb l_al1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)	
	
	areg mb l_al1 cf3_atl1 cash_atl1 div_d cgov_str_num, a(gvkey) cluster(gvkey)
	reg mb l_al1 cf3_atl1 cash_atl1 div_d cgov_str_num, cluster(gvkey)	
	
	areg mb l_al1 cf3_atl1 cash_atl1 div_d sr_pos, a(gvkey) cluster(gvkey)
	reg mb l_al1 cf3_atl1 cash_atl1 div_d sr_pos, cluster(gvkey)	
	
	g l_mb = log(mb)
	areg l_mb l_al1 cf3_atl1 cash_atl1 div_d sr_index, a(gvkey) cluster(gvkey)
	reg l_mb l_al1 cf3_atl1 cash_atl1 div_d sr_index, cluster(gvkey)	
	
	areg l_mb l_al1 cf3_atl1 cash_atl1 div_d sr_indexl1, a(gvkey) cluster(gvkey)
	reg l_mb l_al1 cf3_atl1 cash_atl1 div_d sr_indexl1, cluster(gvkey)	

	
	
	
	
	areg mb l_al1 cf3_atl1 cash_atl1 div_d net_rep_me, a(gvkey) cluster(gvkey)
	areg mb l_al1 cf3_atl1 cash_atl1 div_d net_rep2_me, a(gvkey) cluster(gvkey)	
	areg mb l_al1 cf3_atl1 cash_atl1 div_d rep, a(gvkey) cluster(gvkey)	
	areg mb l_al1 cf3_atl1 cash_atl1 div_d buy, a(gvkey) cluster(gvkey)
	areg mb l_al1 cf3_atl1 cash_atl1 div_d buy2, a(gvkey) cluster(gvkey)	
	
	
	reg mb l_al1 cf3_atl1 cash_atl1 div_d net_rep_me, cluster(gvkey)
	reg mb l_al1 cf3_atl1 cash_atl1 div_d net_rep2_me, cluster(gvkey)	
	reg mb l_al1 cf3_atl1 cash_atl1 div_d rep, cluster(gvkey)	
	reg mb l_al1 cf3_atl1 cash_atl1 div_d buy, cluster(gvkey)
	reg mb l_al1 cf3_atl1 cash_atl1 div_d buy2, cluster(gvkey)		
	
	
	reg mb l_al1 cf3_atl1 cash_atl1 div_d sr_indexl1, cluster(gvkey)	
	
	