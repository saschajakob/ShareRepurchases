//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 4 - Models for Binary Outcomes
//  Long and Freese - 27Jul2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch4binary, text replace

//  estimation using logit and probit (pg 115 RevEd)

sysuse binlfp2, clear
desc lfp k5 k618 age wc hc lwg inc
summarize lfp k5 k618 age wc hc lwg inc

logit lfp k5 k618 age wc hc lwg inc, nolog
estimates store logit

probit lfp k5 k618 age wc hc lwg inc, nolog
estimates store probit

estimates table logit probit, b(%9.3f) t label varwidth(30)

//  predicting perfectly (using artificial data) (pg 118 RevEd)

sysuse science2, clear
gen college = 1 - mmale
gen vote = pub1>10
tab vote college
logit vote college phd, nolog

//  testing individual coefficients (pg 119 RevEd)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog

* Wald test
test k5
di sqrt(55.14)
di sqrt(r(chi2)) // or, use returns to get the value of 55.14

* LR test
logit lfp k5 k618 age wc hc lwg inc, nolog
estimates store fmodel
logit lfp k618 age wc hc lwg inc, nolog
estimates store nmodel
lrtest fmodel nmodel
lrtest fmodel . // . refers to latest model

//  testing multiple coefficients (pg 122 RevEd)

logit lfp k5 k618 age wc hc lwg inc, nolog

* Wald tests
test hc wc
test hc=wc

* LR tests
logit lfp k5 k618 age wc hc lwg inc, nolog
estimates store fmodel
logit lfp k5 k618 age lwg inc, nolog
estimates store nmodel
lrtest fmodel nmodel

logit lfp, nolog
estimates store intercept_only
lrtest fmodel intercept_only

//  graph of residuals and influence used in text (pg 124 RevEd)

* create artificial data
clear
set seed 315
set obs 34
gen y = (uniform()*4) in 1/30
gen x = (4 - (y)) + (uniform()) in 1/30
* change one point
replace y = 9 in 31
sum x
sca meanx = r(mean)
replace x = meanx in 31
* compute the regression
reg y x
* change two points
replace x = 0 in 32
replace x = 5 in 33
* compute predictions
predict yhat in 32/33
predict res in 32/33, res
gen y3 = 9 in 31
set textsize 150
drop yhat res
predict yhat
predict res , res
local yout = y[31]
local xout = x[31]

* create artificial data
clear
set seed 315
set obs 34
gen y = (uniform()*4) in 1/30
gen x = (4 - (y)) + (uniform()) in 1/30
* change one point
replace y = 9 in 31
sum x
sca meanx = r(mean)
replace x = meanx in 31
* compute the regression
reg y x
* change two points
replace x = 0 in 32
replace x = 5 in 33
* compute predictions
predict yhat in 32/33
predict res in 32/33, res
gen y3 = 9 in 31
set textsize 150
drop yhat res
predict yhat
predict res , res
local yout = y[31]
local xout = x[31]

* graph outlier that is not influential
graph twoway (scatter y x, msymbol(dh) mcolor(black)) (line yhat x) ///
    (scatteri `yout' `xout' (3) "outlier"), ///
    title("Large outlier that is not influential") ///
    legend(pos(6) order( 2 ) label(2 "Regression line") ) ///
    ytitle("y") xlabel( 0 1 2 3 4 5 ) name(reg1, replace) ///
    ysize(3) xsize(4)

* change the data
replace y = 7 in 31
replace x = 29.5 in 31
replace x = 30 in 34
local yout = y[31]
local xout = x[31]
* compute the regression
reg y x
predict yhat2
replace yhat2 = . in 2/31

* graph the results
graph twoway ///
    (scatter y x, msymbol(dh) mcolor(black)) ///
    (line yhat2 x, sort clpattern(dash)) ///
    (line yhat x if x<25, sort) ///
    (scatteri `yout' `xout' (9) "influential observation  ", ///
        msymbol(S) mlabgap(*3)), ///
    ylabel(0 2 4 6 8 10) xlabel( 0 10 20 30) ///
    title("Smaller outlier that is influential") ///
    legend(pos(6) order( 2 3) label(2 "With influential observation") ///
    label(3 "Without influential observation") ) ytitle("y") ///
    name(reg2,replace) ysize(3) xsize(4)

*  combine the two graphs
graph combine reg1 reg2, col(1) ysize(6) xsize(4) iscale(*.9)
graph export 04residstata.emf, replace

//  residuals (pg 125 RevEd)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog
predict rstd, rs
label var rstd "Standardized Residual"
sort inc, stable
generate index = _n
label var index "Observation Number"

* index plot of std pearson residuals
graph twoway scatter rstd index, xlabel(0(200)800) ylabel(-4(2)4) ///
    xtitle("Observation Number") yline(0) msymbol(Oh) ///
    ysize(2.7051) xsize(4.0413)
graph export 04rstd.emf , replace

* index plot of std pearson residuals labeled with the index
graph twoway scatter rstd index, xlabel(0(200)800) ylabel(-4(2)4) ///
    xtitle("Observation Number") yline(0) ///
    msymbol(none) mlabel(index) mlabposition(0) ///
    ysize(2.7051) xsize(4.0413)
graph export 04rstdcase.emf , replace

* index plot of std pearson residuals labeled with value of k5
graph twoway scatter rstd index if (rstd>1.7) | (rstd<-1.7), ///
    msymbol(none) mlabel(k5) mlabposition(0) ///
    caption("Values indicate # of young children") ///
    xlabel(0(200)800) xtitle("Observation Number") ///
    ylabel(-4(2)4) yline(0) ysize(2.7051) xsize(4.0413)

* index plot of std pearson residuals for large values
graph twoway scatter rstd index if (rstd>1.7) | (rstd<-1.7), ///
    msymbol(none) mlabel(k5) mlabposition(0) ///
    caption("Values indicate # of young children") ///
    xlabel(0(200)800) xtitle("Observation Number") ///
    ylabel(-4(2)4) yline(0) ysize(2.7051) xsize(4.0413)
graph export 04rstdbig.emf , replace

* list a single point
list in 142, noobs

* list large outliers
list rstd index if rstd>2.5 | rstd<-2.5

//  influential cases (pg 128 RevEd)

predict cook,dbeta
label var cook "Cook's Statistic"
graph twoway scatter cook index, xlabel(0(200)800) ylabel(0(.1).3) ///
    xtitle("Observation Number") yline(.1 .2) ///
    msymbol(none) mlabel(index) mlabposition(0) ///
    ysize(2.7051) xsize(4.0413)
graph export 04cookcase.emf , replace

//  least likely cases (pg  2Ed)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog
leastlikely k5 k618 wc

//  scalar measures of fit (pg 129 RevEd)

quietly logit lfp k5 k618 age wc hc lwg inc, nolog
estimates store model1
quietly fitstat, save
gen agesq = age*age
logit lfp k5 age agesq wc inc, nolog
estimates store model2
estimates table model1 model2, b(%9.3f) t
fitstat, dif

//  hosmer-lemeshow test (pg  2Ed)

logit lfp k5 k618 age wc hc lwg inc, nolog
estat gof, group(10) table
estat gof

* lowess plot
predict p1
lowess lfp p1, ylabel(0(.2)1, grid) xlabel(0(.2)1, ///
    grid) ysize(5) xsize(5) addplot(function y = x, legend(off))
graph export 04hllowess.emf, replace

//  predicted probabilities with predict (pg 132 RevEd)

* predictions from logit
sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog
predict prlogit
summarize prlogit
label var prlogit "Logit: Pr(lfp)"

dotplot prlogit, ylabel(0(.2)1) ///
    ysize(2.7051) xsize(4.0413)
graph export 04dotpredict.emf, replace

* comparing logit and probit predictions
sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog
predict prlogit
label var prlogit "Logit: Pr(lfp)"
probit lfp k5 k618 age wc hc lwg inc, nolog
predict prprobit
label var prprobit "Probit: Pr(lfp)"
pwcorr prlogit prprobit

* graphing predicted probabilities from logit and probit
graph twoway scatter prlogit prprobit, ///
    xlabel(0(.25)1) ylabel(0(.25)1) ///
    xline(.25(.25)1) yline(.25(.25)1) ///
    plotregion(margin(zero)) msymbol(Oh) ///
    ysize(4.0413) xsize(4.0413)
graph export 04logitprobit.emf, replace

//  individual predicted probabilities with prvalue (pg 135 RevEd)

logit lfp k5 k618 age wc hc lwg inc, nolog

* young, low income, low education families with young children.
prvalue, x(age=35 k5=2 wc=0 hc=0 inc=15) rest(mean)

* highly educated families with no children at home.
prvalue, x(age=50 k5=0 k618=0 wc=1 hc=1) rest(mean)

* an average person
prvalue, rest(mean)

//  tables of predicted probabilities with prtab (pg 136 RevEd)

logit lfp k5 k618 age wc hc lwg inc, nolog
prtab k5 wc, rest(mean)

* using prvalue for same information
prvalue, x(k5=0 wc=0) rest(mean) brief
prvalue, x(k5=1 wc=0) rest(mean) brief
prvalue, x(k5=2 wc=0) rest(mean) brief
prvalue, x(k5=3 wc=0) rest(mean) brief

* compute the differences:
di 0.6069-0.7758
di 0.2633-0.4449
di 0.0764-0.1565
di 0.0188-0.0412

//  graphing predicted probabilities with -prgen- (pg 137 RevEd)

* compute predictions at age 30
prgen inc, from(0) to(100) generate(p30) x(age=30) rest(mean) n(11)
label var p30p1 "Age 30"
* compute predictions at age 40
prgen inc, from(0) to(100) generate(p40) x(age=40) rest(mean) n(11)
label var p40p1 "Age 40"
* compute predictions at age 50
prgen inc, from(0) to(100) generate(p50) x(age=50) rest(mean) n(11)
label var p50p1 "Age 50"
* compute predictions at age 60
prgen inc, from(0) to(100) generate(p60) x(age=60) rest(mean) n(11)
label var p60p1 "Age 60"

* list and graph predictions
list p30p1 p40p1 p50p1 p60p1 p60x in 1/11
graph twoway connected p30p1 p40p1 p50p1 p60p1 p60x, ///
    ytitle("Pr(In Labor Force)") ylabel(0(.25)1) ///
    xtitle("Income") ///
    ysize(2.7051) xsize(4.0413)
graph export 04ageincome.emf, replace

//  plotting confidence intervals (pg  2Ed)

prgen age, from(20) to(70) generate(prlfp) rest(mean) gap(2) ci
label var prlfpp1 "Predicted probability"
label var prlfpp1ub "95% upper limit"
label var prlfpp1lb "95% lower limit"
label var prlfpx "Age"

* using lines to show confidence interval
twoway ///
    (connected prlfpp1 prlfpx, ///
        clcolor(black) clpat(solid) clwidth(medthick) ///
        msymbol(i) mcolor(none)) ///
    (connected prlfpp1ub prlfpx, ///
        msymbol(i) mcolor(none) ///
     clcolor(black) clpat(dash) clwidth(thin)) ///
    (connected prlfpp1lb prlfpx, ///
         msymbol(i) mcolor(none) ///
         clcolor(black) clpat(dash) clwidth(thin)), ///
    ytitle("Probability of Being in Labor Force") yscale(range(0 .35)) ///
    ylabel(, grid glwidth(medium) glpattern(solid)) ///
    xscale(range(20 70)) xlabel(20(10)70) ///
    ysize(2.7051) xsize(4.0413)
graph export 04ageci.emf, replace

* using shading to show confidence interval
graph twoway ///
    (rarea prlfpp1lb prlfpp1ub prlfpx, color(gs14)) ///
    (connected prlfpp1 prlfpx, ///
        clcolor(black) clpat(solid) clwidth(medthick) ///
        msymbol(i) mcolor(none)), ///
    ytitle("Probability of Being in Labor Force") yscale(range(0 .35)) ///
    ylabel(, grid glwidth(medium) glpattern(solid)) ///
    xscale(range(20 70)) xlabel(20(10)70) ///
    ysize(2.7051) xsize(4.0413) ///
    legend(label (1 "95% confidence interval"))
graph export 04ageci2.emf, replace

//  prgen to compare those who do and do not attend college (not in book)

prgen age, from(30) to(60) generate(wc1) x(wc=1) rest(mean) n(13)
label var wc1p1 "Attended College"
prgen age, from(30) to(60) generate(wc0) x(wc=0) rest(mean) n(13)
label var wc0p1 "Did Not Attend College"
graph twoway connected wc1p1 wc0p1 wc1x, ///
    xtitle("Age") ytitle("Pr(In Labor Force)") ///
    ylabel(0(.25)1) xlabel(30(10)60)

//  changes in predicted probabilities (pg 139 RevEd)

* discrete change with prchange
prchange age, x(wc=1 age=40)
mfx compute, at(wc=1 age=40)
prchange, help
prchange k5 age wc lwg inc, fromto

* confidence intervals with prvalue
prchange wc, brief
qui prvalue, x(wc=0) rest(mean) save
    prvalue, x(wc=1) dif
mfx compute

* automating prvalue for discrete change for 0/1 changes
foreach v in k5 k618 wc {
    di _n "** Change from 0 to 1 in `v'"
    qui prvalue, x(`v'=0) rest(mean) save
        prvalue, x(`v'=1) dif brief
}

* automating prvalue for centered sd changes
foreach v in age lwg inc {
    qui sum `v'
    local start = r(mean) - (.5*r(sd))
    local end = r(mean) + (.5*r(sd))
    di _n "** Change from `start' to `end' in `v'"
    qui prvalue, x(`v'=`start') rest(mean) save
        prvalue, x(`v'=`end') dif brief
}

* discrete change using prvalue
prvalue, x(age=30) save brief
prvalue, x(age=40) dif brief

* discrete change using prchange with delta and uncentered options
prchange age, x(age=30) uncentered delta(10) rest(mean) brief

//  odds ratios using -listcoef- (pg 146 RevEd)

listcoef, help
listcoef, reverse
listcoef, percent

log close
