//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 5 - Models for Ordinal Outcomes
//  Long and Freese - 27Jul2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch5ordinal, text replace

//  a latent variable model (pg 154 RevEd)

* binary model with logit
sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog
estimates store logit

* binary model with ologit
ologit lfp k5 k618 age wc hc lwg inc, nolog
estimates store ologit
estimates table logit ologit, b(%9.3f) t label varwidth(30) equations(1:1)

//  estimation of ordered regression model (pg 155 RevEd)

sysuse ordwarm2, clear
ologit warm male white age ed prst if yr89==1, nolog
ologit warm male white age ed prst, table nolog
clear

//  attitudes toward working mothers (pg 157 RevEd)

sysuse ordwarm2
desc warm yr89 male white age ed prst
sum warm yr89 male white age ed prst
tab warm

* compare ologit and oprobit
ologit warm yr89 male white age ed prst, nolog
estimates store ologit
oprobit warm yr89 male white age ed prst, nolog
estimates store oprobit
estimates table ologit oprobit, b(%9.3f) t label varwidth(30)


//  predicting perfectly (with artifical data) (pg 160 RevEd)

* sort helps with convergence in nearly nonidentified model
sort age
gen dumprst = (prst<20 & warm==1)
tab dumprst warm, miss
ologit warm yr89 male white age ed dumprst, nolog

//  testing individual coefficients (pg 161 RevEd)

* Wald test
ologit warm male yr89 white age ed prst, nolog
test male
display "z*z=" -9.343*-9.343
return list
di "chi2=" r(chi2) "; sqrt(chi2)= " sqrt(r(chi2))

* LR test
ologit warm yr89 male white age ed prst, nolog
estimates store fmodel

* estimate constrained model
ologit warm yr89 white age ed prst, nolog
estimates store nmodel
lrtest fmodel nmodel

//  testing multiple coefficients (pg 162 RevEd)

ologit warm yr89 male white age ed prst, nolog

* Wald test
test age white male

* LR test
ologit warm yr89 male white age ed prst, nolog
estimates store fmodel
ologit warm yr89 ed prst, nolog
estimates store nmodel
lrtest fmodel nmodel

//  scalar measures of fit with fitstat (pg 163 RevEd)

ologit warm yr89 male white age ed prst, nolog
fitstat

//  converting to a different parameterization (pg 164 RevEd)

ologit warm yr89 male white age ed prst, nolog
* intercept
lincom 0 - _b[/cut1]
* cutpoint 2
lincom _b[/cut2] - _b[/cut1]
* cutpoint 3
lincom _b[/cut3] - _b[/cut1]

//  the parallel regression assumption (pg 167 RevEd)

* generate data to demonstrate the assumption
clear
set obs 41
gen n = 41-_n
replace n = (n-20)/2.5
gen p1 = exp((-1*n)-3.5)/(1+exp((-1*n)-3.5))
gen p2 = exp((-1*n))/(1+exp((-1*n)))
gen p3 = exp((-1*n)+2)/(1+exp((-1*n)+2))
gen x = (n+8)*(9.375)
label var p1 "Pr(y<=1 | x)"
label var p2 "Pr(y<=2 | x)"
label var p3 "Pr(y<=3 | x)"
graph twoway (line p1 p2 p3 x) , ylabel(0(.2)1) xlabel(0(50)150) ///
    ytitle("Pr(y<=m)") xtitle("x") ///
    ysize(2.6558) xsize(4.0413)
graph export 05parallel.emf, replace

* an approximate LR test (
* note: omodel is an add-on Stata command. Type: findit omodel
sysuse ordwarm2, clear
* omodel logit warm yr89 male white age ed prst

* a Wald test
ologit warm yr89 male white age ed prst, nolog
brant, detail

//  residuals and outliers with predict (pg  RevEd)

sysuse ordwarm2, clear
drop warmlt2 warmlt3 warmlt4
gen warmlt2 = (warm<2) if warm <.
gen warmlt3 = (warm<3) if warm <.
gen warmlt4 = (warm<4) if warm <.

* binary logit of warm < 2
logit warmlt2 yr89 male white age ed prst, nolog
predict rstd_lt2, rs

* binary logit of warm < 3
logit warmlt3 yr89 male white age ed prst, nolog
predict rstd_lt3, rs

* binary logit of warm < 4
logit warmlt4 yr89 male white age ed prst, nolog
predict rstd_lt4, rs

* binary logit of plot results for warm < 3
sort prst
gen index = _n
graph twoway scatter rstd_lt3 index, yline(0) ylabel(-4(2)4) ///
    xtitle("Observation Number")  xlabel(0(500)2293) ///
    ysize(2.6558) xsize(4.0413) msymbol(Oh)
graph export 05residplot.emf, replace

//  partial change in y* (pg  Re RevEd)

sysuse ordwarm2, clear
ologit warm yr89 male white age ed prst, nolog
listcoef, std help

//  in sample predicted probabilities with predict (pg 172 RevEd)

ologit warm yr89 male white age ed prst, nolog
predict SDwarm Dwarm Awarm SAwarm
label var SDwarm "Pr(SD)"
label var Dwarm "Pr(D)"
label var Awarm "Pr(A)"
label var SAwarm "Pr(SA)"
dotplot SDwarm Dwarm Awarm SAwarm, ylabel(0(.25).75) ///
    ysize(2.0124) xsize(3.039)
graph export 05pred.emf, replace

//  individual predicted probabilities with prvalue (pg 173 RevEd)

ologit warm yr89 male white age ed prst, nolog

* working class men in 1977 who are near retirement
prvalue, x(yr89=0 male=1 prst=20 age=64 ed=16) rest(mean)
* young, highly educated women with prestigious jobs
prvalue, x(yr89=1 male=0 prst=80 age=30 ed=24) rest(mean) brief
* an average individual in 1977
prvalue, x(yr89=0) rest(mean) brief
* an average individual in 1989
prvalue, x(yr89=1) rest(mean) brief

//  tables of predicted probabilities with prtab (pg 174 RevEd)

prtab yr89 male, novarlbl

* confidence intervals
prvalue, x(yr89=0 male=1)
prvalue, x(yr89=0 male=0)
prvalue, x(yr89=1 male=1)
prvalue, x(yr89=1 male=0)

//  confidence intervals for discrete change (pg  2Ed)

* men compared to women
qui prvalue , x(male=0 yr89=1) rest(mean) save
    prvalue , x(male=1 yr89=1) rest(mean) dif

* standard deviation change in age
di 44.935 - (.5*16.779)
di 44.935 + (.5*16.779)
qui prvalue, x(male=0 yr89=1 age=36.5455) rest(mean) save
    prvalue, x(male=0 yr89=1 age=53.3245) rest(mean) dif

//  graphing predicted probabilities with -prgen- (pg 177 RevEd)

prgen age, from(20) to(80) generate(w89) x(male=0 yr89=1) ncases(13)
desc w89*
label var w89p1 "SD"
label var w89p2 "D"
label var w89p3 "A"
label var w89p4 "SA"
label var w89s1 "SD"
label var w89s2 "SD or D"
label var w89s3 "SD, D or A"

* step 1: graph predicted probabilities
graph twoway connected w89p1 w89p2 w89p3 w89p4 w89x, ///
     title("Panel A: Predicted Probabilities") ///
     xtitle("Age") xlabel(20(10)80) ylabel(0(.25).50) ///
     yscale(noline) ylabel("") xline(44.93) ///
     ytitle("") name(tmp1, replace)

* step 2: graph cumulative probabilities
graph twoway connected w89s1 w89s2 w89s3 w89x, ///
    title("Panel B: Cumulative Probabilities") ///
    xtitle("Age") xlabel(20(10)80) ylabel(0(.25)1) xline(44.93) ///
    yscale(noline) ylabel("") name(tmp2, replace) ///
    ytitle("")

* step 3: combine graphs
graph combine tmp1 tmp2, col(1) iscale(*.9) imargin(small) ///
    ysize(4.31) xsize(3.287)

* step 4: save graph as eps
graph export 05prgen.emf, replace

//  changes in predicted probabilities with prchange (pg 178 RevEd)

* marginal change using prchange
prchange age, x(male=0 yr89=1) rest(mean)

* marginal change using mfx
mfx compute, at(male=0 yr89=1) predict(outcome(1))

* discrete change with prchange
prchange male age prst, x(male=0 yr89=1) rest(mean)

* using delta to compute effect of 10-year increase in age
ologit warm yr89 male white age ed prst, nolog
prchange age, x(male=0 yr89=1) rest(mean) delta(10)

//  odds ratios using listcoef (pg 183 RevEd)

ologit warm yr89 male white age ed prst, nolog
listcoef male age, help
listcoef male age, help percent
listcoef male, reverse

log close
