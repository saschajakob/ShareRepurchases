version 14.2
set rmsg on
set more off

********************************************************************************
*
*	JOB:			Computes the share repurchase entropy
*	PROJECT:		SBB
*	INPUT:			Share repurchase main data set
*	OUTPUT:			Share repurchase entropy measure
*
*	DESCRIPTION: 	This job yields the repruchase program specific entropy.
*
********************************************************************************

*******************************************************************************
*						COMPUSTAT Frequency
*
					scalar frequency= "annually"
*					scalar frequency= "quarterly"
*
*******************************************************************************
* Define paths

global temp	"C:\Users\\`=c(username)'\Desktop"
global data	"A:\Sascha\Buyback Anomalies"
cd "A:\Sascha\Buyback Anomalies"

clear
clear matrix

set matsize 11000
set scrollbufsize 500000
capture program drop _all
global F9 "browse;"

if frequency == "annually"{
	use "$data\sample_buyback_portfolio.dta", clear
	sort permno month
	xtset permno month
	g mkvaltl1 = l.mkvalt
	g vol_shares_meanl1 = l.vol_shares_mean
	egen buyback2 = max(buyback), by(permno year)
	drop if buyback2 ==1 & dealnumber ==.
	g mth = month(date)
	drop if buyback2 == 0 & mth !=fyr												// Keeping last month of fiscal year
	sort permno year
}

if frequency == "quarterly"{
	use "$data\sample_buyback_portfolio_q.dta", clear
	egen buyback2 = max(buyback), by(permno qdate)
	drop if buyback2 ==1 & dealnumber ==.
	drop if rdate != date & buyback2==0
	sort permno qdate
}

winsor2 cash_atl1, replace cuts(1 99)

g log_size = ln(atl1)
g log_mkvalt = ln(mkvaltl1)
g log_vol = ln(vol_shares_meanl1)
g log_age = ln(agel1)
g cash_interaction= ex_chel1*cash_atl1
g div_interaction = div_dl1*dvc_mkvaltl1
g growth_interaction = mbl1*ex_chel1
g lev_dev_i = lev_devl1*lev_dev_dl1

destring sic2, replace
destring sic3, replace

la var amihud_3M "3-Month Amihud Measure"
la var si_mean "Short Interest Level"
la var si_delta "Quaterly SHort Interest Change"
la var fcfl1 "FCF(t-1)"
la var ex_chel1 "Excess Cash(t-1)"
la var mbl1 "Tobins Q(t-1)"
la var six_m_cum_ret "Prior Return(6M)"
la var roal1 "Return on Assets(t-1)"
la var div_dl1 "Dividend Dummy(t-1)"
la var ivol "Idiosyncratic Volatility"
la var top10instown_perc "Institutional Holdings"
la var lev_devl1 "Industry Leverage Deviation(t-1)"
la var log_mkvalt "Market Cap(log)"
la var buyback "Share Buyback Indicator"
la var log_age "Age(log)"
la var cash_atl1 "Cash Holdings(t-1)"
la var log_size "Total Assets(t-1, log)"
la var log_vol "Trading Volume(log)"
la var cash_interaction "Cash Holdings x Excess Cash"
la var div_interaction " Dividend Dummy x Dividend Yield"
la var growth_interaction " Excess Cash x Tobins Q"
la var debt_mvl1 "Market Leverage"

est clear
eststo clear
estpost tabstat cash_atl1 ex_chel1 cash_interaction amihud_3M si_mean fcfl1  mbl1 growth_interaction six_m_cum_ret roal1 div_dl1 div_interaction ///
	ivol top10instown_perc debt_mvl1 lev_devl1 log_size log_age log_vol, stat(mean sd min p25 p50 p75 max n) col(stat)
esttab using "C:\Users\sajakob\Dropbox\Buybacks\Text\Entropy measure\sumstat_probit.tex", ///
	cells("mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) p25(fmt(%9.2f)) p50(fmt(%9.2f)) p75(fmt(%9.2f)) max(fmt(%9.2f)) count(fmt(%9.0f))") replace noobs label nonumber title(Summary statistics Repurchase Determinants) ///
	substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\footnotesize{ "\caption{Summary statistics Repurchase Determinants}" "\caption{Summary statistics Repurchase Determinants} \label{table:SummaryStats}" ///
	\end{tabular} \end{tabular}} \\ \vspace{0.1cm}\\)

est clear
eststo clear
eststo m1: probit buyback cash_atl1 ex_chel1 cash_interaction amihud_3M si_mean fcfl1  mbl1 growth_interaction six_m_cum_ret roal1 div_dl1 div_interaction ///
	ivol top10instown_perc debt_mvl1 lev_devl1 log_size log_age log_vol i.sic3
qui estadd fitstat

eststo m2: margins, dydx(cash_atl1 ex_chel1 cash_interaction amihud_3M si_mean fcfl1 mbl1 growth_interaction six_m_cum_ret roal1 div_dl1 div_interaction ///
	ivol top10instown_perc debt_mvl1 lev_devl1 log_size log_age log_vol _cons) post atmeans

esttab m1 m2 using "C:\Users\sajakob\Dropbox\Buybacks\Text\Entropy measure\entropy_probit.tex", scalars("r2_mf Mac-Fadden R2") sfmt(%9.2f) label replace booktabs width(1\hsize) title(Probit Model for Share Repurchases 1985 - 2018) ///
	keep(cash_atl1 ex_chel1 cash_interaction amihud_3M si_mean fcfl1 mbl1 growth_interaction six_m_cum_ret roal1 div_dl1 div_interaction ivol top10instown_perc debt_mvl1 lev_devl1 log_size log_age log_vol _cons) nonumb ///
	mtitles("Share Repurchase" "Marginal Effects at Means") star( * 0.10 ** 0.05 *** 0.01) b(%9.3f) t(%9.2f) eqlabels(none) interaction(" $\times$ ") ///
	substitute(\begin{table}[htbp]\centering \begin{table}[H]\centering\scriptsize{  \end{tabular*} \end{tabular*}} Observations "3-digit SIC&Yes&Yes\\Observations" Constant Intercept ///
	"\caption{Probit Model for Share Repurchases 1985 - 2018}" "\caption{Probit Model for Share Repurchases 1985 - 2018} \label{table:Probit}")

est clear
eststo clear

*** Rolling out of sample prediction ***

*define window length
scalar window_length = 15

gen p_SBB =.
forvalues i = 1995(1)2018 {
	qui probit buyback cash_atl1 /*ex_chel1 cash_interaction*/ amihud_3M /*si_mean*/ fcfl1 mbl1 /*growth_interaction*/ six_m_cum_ret roal1 div_dl1 div_interaction ///
	ivol top10instown_perc debt_mvl1 lev_devl1 log_size log_age log_vol i.sic3 if year < `i' & year >= (`i'- window_length)
	qui predict temp if year == `i', pr
	qui replace p_SBB = temp if year == `i'
	qui drop temp
	}

preserve
	drop if dealnumber ==.
	drop if p_SBB >0.99
	hist p_SBB, bin(50) xtitle(Repurchase Probability) graphregion(color(white)) bgcolor(white) color(ltblue%50)
	graph export "C:\Users\sajakob\Dropbox\Buybacks\Text\Entropy measure\SBBprob.png", replace
	g entropy_H = -(p_SBB*(ln(p_SBB)/ln(2)) + (1-p_SBB)*(ln(1-p_SBB)/ln(2)))
	g entropy_H_binomial = 0.5*(ln(2*c(pi)*exp(1)*age*p_SBB*(1-p_SBB))/ln(2))+(1/(24*ln(2)*age*p_SBB*(1-p_SBB)))
	g entropy_I = ln(1/p_SBB)/ln(2)
	winsor2 entropy_H_binomial, replace cut(0.25 99.75)
	hist entropy_H, saving(H, replace) bin(50) xtitle(Repurchase Entropy) graphregion(color(white)) bgcolor(white) color(ltblue%50)
	graph export "C:\Users\sajakob\Dropbox\Buybacks\Text\Entropy measure\entropy_H.png", replace
	hist entropy_H_binomial, saving(H2, replace) bin(50) xtitle(Repurchase Binomial Entropy) graphregion(color(white)) bgcolor(white) color(ltblue%50)
	graph export "C:\Users\sajakob\Dropbox\Buybacks\Text\Entropy measure\entropy_H_binomial.png", replace
	hist entropy_I if entropy_I <=10, saving(I, replace) bin(50) xtitle(Repurchase Self-Information Content) graphregion(color(white)) bgcolor(white) color(ltblue%50)
restore

g entropy_H = -(p_SBB*(ln(p_SBB)/ln(2)) + (1-p_SBB)*(ln(1-p_SBB)/ln(2))) if dealnumber != .
g entropy_H_binomial = 0.5*(ln(2*c(pi)*exp(1)*age*p_SBB*(1-p_SBB))/ln(2))+(1/(24*ln(2)*age*p_SBB*(1-p_SBB))) if dealnumber != .
g entropy_I = ln(1/p_SBB)/ln(2) if dealnumber !=.



sum p_SBB if dealnumber !=., d

la var entropy_I "Self-information content"
la var entropy_H "Binary Entropy"
la var entropy_H_binomial "Binomial Entropy"
keep if dealnumber !=.
keep permno gvkey entropy_H entropy_H_binomial entropy_I month dealnumber p_SBB
save "$data\buyback_entropy.dta", replace
