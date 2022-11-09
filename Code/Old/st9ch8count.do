//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 8 - Models for Count Outcomes
//  Long and Freese - 27Jul2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch8count, text replace

//  the poisson distribution (pg 246 RevEd)

clear
set obs 25
gen ya = .8
poisson ya, nolog
prcounts pya, plot max(20)
gen yb = 1.5
poisson yb, nolog
prcounts pyb, plot max(20)
gen yc = 2.9
poisson yc, nolog
prcounts pyc, plot max(20)
gen yd = 10.5
poisson yd, nolog
prcounts pyd, plot max(20)
label var pyapreq "mu=0.8"
label var pybpreq "mu=1.5"
label var pycpreq "mu=2.9"
label var pydpreq "mu=10.5"
label var pyaval "y=# of Events"
graph twoway connected pyapreq pybpreq pycpreq pydpreq pyaval, ///
    ytitle("Probability") ylabel(0(.1).5) xlabel(0(2)20)
graph  export 08poisson.emf, replace

//  fitting the poisson distribution with poisson (pg 246 RevEd)

sysuse  couart2, clear
describe
summarize
poisson art, nolog

//  plotting the poisson distribution with prcounts (pg 249 RevEd)

prcounts psn, plot max(9)
label var psnobeq "Observed Proportion"
label var psnpreq "Poisson Prediction"
label var psnval "# of Articles"
list psnval psnobeq psnpreq in 1/10
graph twoway connected psnobeq psnpreq psnval, ///
    ytitle("Probability") ylabel(0(.1).4) ///
    xlabel(0(1)9) ysize(2.7051) xsize(4.0421)
graph export 08psnpred.emf, replace

//  prm - estimation (pg 252 RevEd)

* non-integer outcomes - note warning!
sysuse  couart2, clear
gen artx = art + .3
poisson artx fem mar kid5 phd ment, nolog

* if and in conditions
poisson art mar kid5 phd ment if fem==1, nolog

//  prm - example (pg 253 RevEd)

sysuse  couart2, clear
poisson art fem mar kid5 phd ment, nolog

//  prm - interpretation using the rate mu (pg 255 RevEd)

* factor changes
listcoef fem ment, help

* percent change
listcoef fem ment, percent help

* marginal change with prchange
prchange phd ment, rest(mean)

* marginal change with mfx compute
mfx compute

* discrete change
prchange fem ment, rest(mean)
prchange kid5, uncentered x(kid5=1)
prchange kid5, uncentered x(kid5=1) delta(2)

* ci's for discrete change
qui prvalue, x(kid5=1) save
prvalue, x(kid5=3) dif

//  prm - interpretation using predicted probabilities (pg 259 RevEd)

* prvalue for single women without children
quietly prvalue, x(mar=0 fem=1 kid5=0) rest(mean) save
* compared to married women without children
prvalue, x(mar=1 fem=1 kid5=0) rest(mean) dif

* effects of number of children
prvalue, x(mar=1 fem=1 kid5=0) rest(mean) brief maxcnt(4)
prvalue, x(mar=1 fem=1 kid5=1) rest(mean) brief maxcnt(4)
prvalue, x(mar=1 fem=1 kid5=2) rest(mean) brief maxcnt(4)
prvalue, x(mar=1 fem=1 kid5=3) rest(mean) brief maxcnt(4)

* prgen
prgen kid5, x(fem=1 mar=1) rest(mean) from(0) to(3) gen(fprm) n(4)
prgen kid5, x(fem=0 mar=1) rest(mean) from(0) to(3) gen(mprm) n(4)
label var fprmp0 "Married Women"
label var mprmp0 "Married Men"
label var mprmx  "Number of Children"

* graph predictions
graph twoway connected fprmp0 mprmp0 mprmx, ///
    ylabel(0(.1).4) yline(.1 .2 .3) xlabel(0(1)3) ///
    ytitle("Probability of No Articles") ///
    ysize(2.5051) xsize(4.0421)
graph export 08prmprob0.emf, replace

*  list the predictions
list fprmp0 mprmp0 mprmx in 1/4

* prcounts for univariate poisson distribution
poisson art, nolog
prcounts psn, plot max(9)
label var psnpreq "Univariate Poisson Dist."
poisson art fem mar kid5 phd ment, nolog
prcounts prm, plot max(9)
label var prmpreq "PRM"
label var prmobeq "Observed"
graph twoway connected prmobeq psnpreq prmpreq prmval, ///
    ytitle("Probability of Count") ylabel(0(.1).4) ///
    xlabel(0(1)9) ysize(2.7051) xsize(4.0413)
graph export 08prmpred.emf, replace

//  prm - exposure time (pg 265 RevEd)

* simulate data to illustrate exposure
* construct random integers
set seed 11197411
gen profage = round((1+10*uniform()),1)
label var profage "Professional Age"
gen lnage = ln(profage)
label var lnage "Log of Professional Age"
* let total articles equal artiles*exposure time
gen totalarts = art*profage
label var totalarts "Total Articles during Career"

* PRM without including time
poisson totalarts fem mar kid5 phd ment, nolog

* PRM exposure time
poisson totalarts fem mar kid5 phd ment, nolog exposure(profage)

* PRM with exposure time as an independent variable with a constraint
constraint define 1 lnage=1
poisson totalarts fem mar kid5 phd ment lnage, nolog constraint(1)

* PRM with offset()
poisson totalarts fem mar kid5 phd ment, nolog offset(lnage)

//  nbrm - estimation (pg 268 RevEd)

sysuse  couart2, clear
nbreg art fem mar kid5 phd ment, nolog

* combining poisson and nbreg
poisson art fem mar kid5 phd ment, nolog
estimates store PRM
nbreg art fem mar kid5 phd ment, nolog
estimates store NBRM
estimates table PRM NBRM, b(%9.3f) t label varwidth(32) ///
    drop(lnalpha:_cons) stats(alpha N)

//  nbrm - interpretation using the rate mu (pg 270 RevEd)

nbreg art fem mar kid5 phd ment, nolog
listcoef fem ment, help
listcoef fem ment, help percent

//  nbrm - interpretation using predicted probabilities (pg 272 RevEd)

* prvalue for prm and nbrm
quietly poisson art fem mar kid5 phd ment
prvalue
quietly nbreg art fem mar kid5 phd ment
prvalue

* pr(y=0) for prm vs nbrm
quietly nbreg art fem mar kid5 phd ment, nolog
prgen ment, rest(mean) f(0) t(50) gen(nb) n(20)
quietly poisson art fem mar kid5 phd ment
prgen ment, rest(mean) f(0) t(50) gen(psn) n(20)
label var psnp0 "Pr(0) for PRM"
label var nbp0 "Pr(0) for NBRM"
graph twoway connected psnp0 nbp0 nbx, ///
    ylabel(0(.1).4) yline(.1 .2 .3) ///
    ytitle("Probability of a Zero Count") ///
    ysize(2.7051) xsize(4.0421)
graph export 08nbprm0.emf, replace

* pr(y=0) for prm vs nbrm with confidence intervals
sysuse  couart2, clear
quietly nbreg art fem mar kid5 phd ment, nolog
prgen ment, rest(mean) f(0) t(50) gen(nb) gap(5) ci
quietly poisson art fem mar kid5 phd ment
prgen ment, rest(mean) f(0) t(50) gen(psn) gap(5) ci
label var psnp0 "Pr(0) for PRM"
label var nbp0 "Pr(0) for NBRM"
graph twoway ///
    (rarea psnp0lb psnp0ub nbx, color(gs14)) ///
    (rarea nbp0lb nbp0ub nbx, color(gs14)) ///
    (connected psnp0 nbx, lpattern(dash) msize(zero)) ///
    (connected nbp0 nbx, lpattern(solid) msize(zero)), ///
    legend(on order(3 4)) ylabel(0(.1).4) yline(.1 .2 .3) ///
    ytitle("Probability of a Zero Count") ///
    ysize(2.7051) xsize(4.0421)
graph export 08nbprm0ci.emf, replace

//  fitting zero truncated models (pg  2Ed)

* load the data
sysuse  couart2, clear
drop if art==0 // artificially truncated the data

* estimate the zero truncated poisson
ztp art fem mar kid5 phd ment, nolog
est store ztp
* estimate the zero truncated negative binomial
ztnb art fem mar kid5 phd ment, nolog
est store ztnb
* compare the results
estimates table ztp ztnb, ///
    stats(N chi2 aic bic) eform b(%9.3f) t(%6.2f)

* interpretation using predicted probabilities and rates
ztnb art fem mar kid5 phd ment, nolog
prvalue, x(fem=1 mar=1 kid5=0)

* computing predicted rates and probabilities in the estimation sample
ztnb art fem mar kid5 phd ment, nolog
predict mztnbrateP, ir
predict mztnbCrateP, cm
desc mztnbCrate
prcounts mztnb, max(5)
sum  mztnb*
desc mztnb*

//  nbrm hurdle model (pg  2Ed)

sysuse  couart2, clear
gen iszero = (art==0)
label var iszero "Does scientists have 0 articles?"
label def iszero 1 NoArticles 0 HasArticles
label val iszero iszer

* binary logit for zero publications
logit iszero fem-ment, nolog or
est store hlogit

* zero truncated negative binomial
ztnb art fem-ment if art>0, nolog irr
est store hztnb

estout hlogit hztnb, eq(1) style(fixed) ///
    cells(b(star fmt(%9.3f)) se(par fmt(%9.3f))) ///
    collabels("") mlabels("h_logit" "h_ztnb") ///
    eqlabels(,none) stats(N ll, fmt(%9.0f %9.3f)) ///
    varlabels(lnalpha:_cons _alpha) legend

* predict prob of 0
estimates restore hlogit
predict Hlgt_pry0, p
label var Hlgt_pry0 "blm: Pr(y=0)=Pr(iszero=1)"
sum  Hlgt_pry0

* compute predictions from ztnb model
estimates restore hztnb
prcounts Hztnb_
desc Hztnb_C*
sum  Hztnb_C*
gen Hmu = (1-Hlgt_pry0) *  Hztnb_Crate
label var Hmu "Hurdle: mu"
sum Hmu

* probabilities from hurlde model
gen Hpr0 = Hlgt_pry0
label var Hpr0 "Hurdle: pr(y=0)"
foreach k of numlist  1(1)9  {
    gen Hpr`k' = (1-Hlgt_pry0) * Hztnb_Cpr`k'
    label var Hpr`k' "Hurdle: pr(y=`k')"
}
sum Hpr*

* predictions for user specified values
est restore hlogit
prvalue, x(fem=0 phd=1 ment=0) rest(mean)
matrix list pepred
scalar hrdlp0 = pepred[2,2]

est restore hztnb
prvalue, x(fem=0 phd=1 ment=0) rest(mean) all
forvalues i = 1(1)9 {
    scalar ztnbp`i' = peCpred[2,`i'+1]
    scalar hrdlp`i' = (1-hrdlp0) * ztnbp`i'
    display "Prob(art=`i'|X) = " hrdlp`i'
}
display "Prob(art=0|X) = " hrdlp0

//  zip/zinb - example of estimating the zip and zinb models (pg 278 RevEd)

zip art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) nolog
estimates store ZIP
zinb art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) nolog
estimates store ZINB
estimates table ZIP ZINB, b(%9.3f) t label varwidth(35)

//  zip/zinb - interpretation of coefficients (pg 279 RevEd)

zip art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) nolog
listcoef, help
zinb art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) nolog
listcoef, help

//  discrete change without confidence intervals for zinb
prvalue, x(fem=0 mar=1 kid5=3 phd=3 ment=10) save
prvalue, x(fem=1 mar=1 kid5=3 phd=1 ment= 2)  dif

//  confidence intervals for zinb
/* -- this will take about 30 minutes to run
set seed 14972132
set maxiter 50
prvalue, x(fem=0 mar=1 kid5=3 phd=3 ment=10) save boot dots
prvalue, x(fem=1 mar=1 kid5=3 phd=1 ment= 2)  dif boot dots
*/

//  zip/zinb - interpretation of predicted probabilities (pg 281 RevEd)

* prvalue
zinb art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) nolog
quietly prvalue, x(fem=0 mar=1 kid5=3 phd=3 ment=10) save
prvalue, x(fem=1 mar=1 kid5=3 phd=1 ment=0) dif

* prgen
prgen ment, rest(mean) f(0) t(20) gen(zinb) n(21)
gen zinbnb0 = zinbp0 - zinball0
label var zinbp0 "0's from Both Equations"
label var zinball0 "0's from Binary Equation"
label var zinbnb0  "0's from Count Equation"
label var zinbx"Mentor's Publications"
graph twoway connected zinball0 zinbnb0 zinbp0 zinbx, ///
    xlabel(0(5)20) ylabel(0(.1).7) ///
    ytitle(Probability of Zero) msymbol(Oh Sh O) ///
    ysize(2.6541) xsize(3.9678)
graph export 08zinbpr0.emf, replace

//  comparing mean probabilities (pg 283 RevEd)

* estimate various models and save predictions
sysuse  couart2, clear
quietly poisson art fem mar kid5 phd ment, nolog
prcounts prm, plot max(9)
label var prmpreq "Predicted: PRM"
label var prmobeq "Observed"
quietly nbreg art fem mar kid5 phd ment, nolog
prcounts nbrm, plot max(9)
label var nbrmpreq "Predicted: NBRM"
quietly zip art fem mar kid5 phd ment, ///
    inf(fem mar kid5 phd ment) vuong nolog
prcounts zip, plot max(9)
label var zippreq "Predicted: ZIP"
quietly zinb art fem mar kid5 phd ment, ///
    inf(fem mar kid5 phd ment) vuong nolog
prcounts zinb, plot max(9)
label var zinbpreq "Predicted: ZINB"

* create deviations
gen obs = prmobeq
gen dprm = obs - prmpreq
label var dprm "PRM"
gen dnbrm = obs - nbrmpreq
label var dnbrm "NBRM"
gen dzip = obs - zippreq
label var dzip "ZIP"
gen dzinb = obs - zinbpreq
label var dzinb "ZINB"
* plot deviations
graph twoway connected dprm dnbrm dzip dzinb prmval, ///
    ytitle(Observed-Predicted) ylabel(-.10(.05).10) ///
    xlabel(0(1)9) msymbol(Oh Sh O S) ysize(2.7051) xsize(4.0413)
graph export 08compare.emf, replace

//  tests to compare count models (pg 285 RevEd)

* LR test for zip and zinb
sysuse  couart2, clear
nbreg art fem mar kid5 phd ment, nolog
quietly zip art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) vuong nolog
scalar llzip = e(ll)
quietly zinb art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) vuong nolog
scalar llzinb = e(ll)
scalar lr = -2*(llzip-llzinb)
scalar pvalue = chiprob(1,lr)/2
scalar lnalpha = -.9763565
if (lnalpha < -20) {
    scalar pvalue= 1
}
di as text "Likelihood-ratio test comparing ZIP to ZINB: " as res %8.3f ///
    lr as text " Prob>=" as res %5.3f pvalue

* vuong test of non-nested models
zip art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) vuong nolog
listcoef, help
zinb art fem mar kid5 phd ment, inf(fem mar kid5 phd ment) vuong nolog
listcoef, help

//  countfit to compare count models (pg  2Ed)

sysuse  couart2, clear
countfit art fem mar kid5 phd ment, ///
    inf(ment fem) graphexport(08countfigeg.emf, replace)

log close
