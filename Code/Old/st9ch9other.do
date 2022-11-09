//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 9 - Additional Topics
//  Long and Freese - 27Jul2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch9other, text replace

//  ordinal and nominal independent variables (pg 287 RevEd)

* create two variables that will be used later
sysuse couart2, clear
* create mentord from ment
gen mentord = ment
recode mentord 0=0 .9/3=1 3.5/9=2 9.5/20=3 20.1/1000=4
label var mentord "Ordinal measure of mentor's articles"
label def lment 0 None 1 Few 2 Some 3 Many 4 Lots
label val mentord lment
tab mentord, missing
* create binary outcome hasarts from art
gen hasarts = (art>0) if art<.
label var hasarts "Has publised: 1=yes 0=no"
label define lhasarts 0 NoArts 1 Arts
label values hasarts lhasarts
tab hasarts, m
sum hasarts art fem mar kid5 phd ment

//  coding categorical variables as dummy variables (pg 288 RevEd)

gen none = (mentord==0) if mentord<.
tab none mentord, missing
gen few  = (mentord==1) if mentord<.
gen some = (mentord==2) if mentord<.
gen many = (mentord==3) if mentord<.
gen lots = (mentord==4) if mentord<.

//  estimation and interpretation (pg 289 RevEd)

* estimate logit model without excluding any dummies
logit hasarts fem mar kid5 phd none few some many lots, nolog

* estimate logit with j-1 dummy variables, excluding none
logit hasarts fem mar kid5 phd few some many lots, nolog
listcoef

* comparing many to some by excluding some
logit hasarts fem mar kid5 phd none few many lots, nolog

//  tests with categorical independent variables (pg 291 RevEd)

* testing two non-reference categories
logit hasarts fem mar kid5 phd few some many lots, nolog
lincom many-some

* LR test that a categorical variable has no effect
logit hasarts fem mar kid5 phd few some many lots, nolog
estimates store fmodel
logit hasarts fem mar kid5 phd, nolog
lrtest fmodel

* Wald test that a categorical variable has no effect
logit hasarts fem mar kid5 phd few some many lots, nolog
test few some many lots

* LR test whether treating ordinal var as interval loses info
logit hasarts fem mar kid5 phd mentord some many lots, nolog
estimates store fmodel
logit hasarts fem mar kid5 phd mentord, nolog
lrtest fmodel

* LR test whether treating ordinal var as interval loses info
logit hasarts fem mar kid5 phd mentord some many lots, nolog
test some many lots

//  discrete change for categorical independent variables (pg 294 RevEd)

* computing discrete change with -prchange-
logit hasarts fem mar kid5 phd few some many lots, nolog
prchange few, x(some=0 many=0 lots=0)

* NOTE: the following does NOT work !!
prchange few

* -prvalue- with two non-reference categories
quietly prvalue, x(few=1 some=0 many=0 lots=0) save
prvalue, x(few=0 some=1 many=0 lots=0) dif

* NOTE: the following does NOT work !!
prchange some, x(few=1 some=0 many=0 lots=0)

//  interactions (pg 296 RevEd)

* estimate model with interaction
sysuse ordwarm2.dta, clear
gen maleXed = male*ed
ologit warm age prst yr89 white male ed maleXed, nolog

* LR test of interaction
quietly ologit warm age prst yr89 white male ed maleXed, nolog
estimates store fmodel
quietly ologit warm age prst yr89 white male ed, nolog
lrtest fmodel

//  computing gender differences in predictions
//  with interactions (pg 297 RevEd)


* predictions for women
ologit warm age prst yr89 white male ed maleXed, nolog
prvalue, x(male=0 maleXed=0) rest(mean) save

* predictions for men
sum ed
global meaned = r(mean)
prvalue, x(male=1 maleXed=$meaned) dif

* alternatively: predictions without using globals
prvalue, x(male=1 maleXed=12.21805) dif

//  computing gender differences in discrete change (pg 298 RevEd)

* for women
quietly prvalue, x(male=0 maleXed=0 ed=12) rest(mean) save
prvalue, x(male=0 maleXed=0 ed=16) rest(mean) dif
* for men
quietly prvalue, x(male=1 maleXed=12 ed=12) rest(mean) save
prvalue, x(male=1 maleXed=16 ed=16) rest(mean) dif

//  adding nonlinearities to linear predictors (pg 299 RevEd)

* construct graph from this section
graph twoway function y = 1 -.1*x + .1*x^2, range(0 100)
graph  export 08nonlinear.emf, replace

//  discrete change in nonlinear nonlinear models (pg 300 RevEd)

* discrete change without age2
sysuse binlfp2,clear
logit lfp k5 k618 wc hc lwg inc age, nolog
prchange age, x(age=30) delta(20) uncentered

* adding age2 to the model
gen age2 = age*age
logit lfp k5 k618 wc hc lwg inc age age2, nolog

* LR test for age and age2
quietly logit lfp k5 k618 wc hc lwg inc age age2, nolog
estimates store fmodel
quietly logit lfp k5 k618 wc hc lwg inc, nolog
lrtest fmodel

* discrete change with age2
logit lfp k5 k618 wc hc lwg inc age age2, nolog

* predictions at age 30
global age30 = 30
global age30sq = $age30*$age30
quietly prvalue, x(age=$age30 age2=$age30sq) rest(mean) save
* predictions at age 50
global age50 = 50
global age50sq = $age50*$age50
prvalue, x(age=$age50 age2=$age50sq) rest(mean) dif

//  example using age and age squared (pg 303 RevEd)

* use prgen in model with only age; plot results later
sysuse binlfp2,clear
quietly logit lfp k5 k618 age wc hc lwg inc
prgen age, from(20) to(60) gen(prage) ncases(9)
label var pragep1 "Pr(lpf | age)"

* estimate model with age squared
gen age2 = age*age
logit lfp k5 k618 age age2 wc hc lwg inc, nolog

* use praccum without globals or forvalues
quietly prvalue, x(age 20 age2 400) rest(mean)
* the first call uses saving()
praccum, saving(mat_age) xis(20)
quietly prvalue, x(age 25 age2 625) rest(mean)
* remaining calls add to mat_age with the using() option
praccum, using(mat_age) xis(25)
quietly prvalue, x(age 30 age2 900) rest(mean)
praccum, using(mat_age) xis(30)
quietly prvalue, x(age 35 age2 1225) rest(mean)
praccum, using(mat_age) xis(35)
quietly prvalue, x(age 40 age2 1600) rest(mean)
praccum, using(mat_age) xis(40)
quietly prvalue, x(age 45 age2 2025) rest(mean)
praccum, using(mat_age) xis(45)
quietly prvalue, x(age 50 age2 2500) rest(mean)
praccum, using(mat_age) xis(50)
quietly prvalue, x(age 55 age2 3025) rest(mean)
praccum, using(mat_age) xis(55)
quietly prvalue, x(age 60 age2 3600) rest(mean)
* the last call of praccum generates agesqp1 and agesqx variables
praccum, using(mat_age) xis(60) gen(agesq)

* take a look at what was created
list agesqx agesqp0 agesqp1 in 1/10

* graph the results
label var agesqp1 "Pr(lpf | age,age2)"
label var agesqx  "Age"
graph twoway connected pragep1 agesqp1 agesqx,  ///
    msymbol(Sh Dh) xlabel(20(5)60)       ///
    ytitle("Pr(Being in the Labor Force)")  ylabel(0(.2)1)        ///
    ysize(2.7051) xsize(4.0421)
graph export 08lfp.emf, replace

//  using forvalues with praccum (pg 305 RevEd)

   * demonstration of forvalues

forvalues count = 0(5)100 {
    display `count'
}

* alternative method using forvalues and locals *
capture matrix drop mage
forvalues count = 20(5)60 {
    local countsq = `count'^2
    prvalue, x(age `count' age2 `countsq') rest(mean) brief
    praccum, using(mage) xis(`count')
}
praccum, using(mage) gen(agsq)

//  using praccum for graphing a transformed variable (pg 306 RevEd)

* estimate model with log age
gen ageln = ln(age)
logit lfp k5 k618 ageln wc hc lwg inc, nolog

* using praccum and forvalues
capture matrix drop mat_ln
forvalues count = 20(5)60 {
    local countln = ln(`count')
    prvalue, x(ageln=`countln') rest(mean) brief
    praccum, using(mat_ln) xis(`count')
}
praccum, using(mat_ln) gen(ageln)

label var agelnp1 "Pr(lpf | log of age)"
graph twoway connected pragep1 agesqp1 agelnp1 agesqx,  ///
    xlabel(20(5)60) msymbol(Sh Dh Th)        ///
    ytitle("Pr(Being in the Labor Force)")    ///
    ylabel(0(.2)1) ysize(2.7051) xsize(4.0421)
graph export 08lfp2.emf, replace

//  using praccum to graph interactions (pg 307 RevEd)

* estimate the model
sysuse ordwarm2.dta, clear
gen maleXed = male*ed
ologit warm age prst yr89 white male ed maleXed, nolog

* compute and accumulate predicted values for women
forvalues count = 8(2)20 {
    quietly prvalue, x(male=0 ed=`count' maleXed=0) rest(mean)
    praccum, using(mat_f) xis(`count')
}
praccum, using(mat_f) gen(pfem)

* compute and accumulate predicted values for men
forvalues count = 8(2)20 {
    quietly prvalue, x(male=1 ed=`count' maleXed=`count') rest(mean)
    praccum, using(mat_m) xis(`count')
}
praccum, using(mat_m) gen(pmal)

* graph the results
label var pfemp4 "Pr(SA | female)"
label var pmalp4 "Pr(SA | male)"
label var pfemx "Education in Years"
graph twoway connected pfemp4 pmalp4 pfemx,   ///
    msymbol(Sh Dh) xlabel(8(2)20) ylabel(0(.1).4)    ///
    ytitle("Pr(Strongly Agreeing)")   ///
    ysize(2.7051) xsize(4.0421)
graph export 08warm.emf, replace

//  using forvalues with prvalue to create tables (pg  2Ed)

sysuse binlfp2,clear
logit lfp-inc, nolog

* prtab to compute predictions at multiple levels of other variables
prtab wc k5, novarlbl

* repeated uses of prvalue
prvalue, x(wc=0 k5=0)
prvalue, x(wc=0 k5=1)
prvalue, x(wc=0 k5=2)
prvalue, x(wc=0 k5=3)
prvalue, x(wc=1 k5=0)
prvalue, x(wc=1 k5=1)
prvalue, x(wc=1 k5=2)
prvalue, x(wc=1 k5=3)

* a simple loop with wc=0
forvalues k = 0/3 {
    prvalue, x(k5=`k' wc=0)
}

* a simple loop with wc=1
forvalues k = 0/3 {
    prvalue, x(k5=`k' wc=1)
}

* using two forvalue loops
forvalues w = 0/1 {
    forvalues k = 0/3 {
        prvalue, x(k5=`k' wc=`w')
    }
}

* matrices with results from prvalue
prvalue, x(wc=0 k5=0)
mat list pepred
mat list peupper
mat list pelower

* matrix to hold predictions and bounds
matrix prob = J(4,2,.)

* names of columns and rows of matrices
matrix rownames prob = "k5=0" "k5=1" "k5=2" "k5=3"
matrix colnames prob = "wc=0" "wc=1"
matrix LBprob = prob
matrix UBprob = prob

* loops to call prvalue and then saving results to matrices
forvalues w = 0/1 {
    forvalues k = 0/3 {
        qui prvalue, x(k5=`k' wc=`w')
        matrix prob[`k'+1,`w'+1] = pepred[2,2]
        matrix LBprob[`k'+1,`w'+1] = pelower[2,2]
        matrix UBprob[`k'+1,`w'+1] = peupper[2,2]
    }
}

matrix list prob, format(%6.3f)
matrix list LBprob, format(%6.3f)
matrix list UBprob, format(%6.3f)

//  a more advanced example (pg  2Ed)

matrix out = J(4,6,.)
local colnm ""
forvalues w = 0/1 {
    local colnm "`colnm' wc=`w' lb ub"
    local rownm ""
    forvalues k = 0/3 {
        local rownm "`rownm' k5=`k'"
        qui prvalue, x(k5=`k' wc=`w')
        scalar p = pepred[2,2] // get information from matrices
        scalar lb = pelower[2,2]
        scalar ub = peupper[2,2]
        local colnum = (`w'*3) + 1 // colnum=1 if wc=0, 4 if wc=1
        matrix out[`k'+1,`colnum'] = p
        local ++colnum  // same as local colnum = `colnum' + 1
        matrix out[`k'+1,`colnum'] = lb
        local ++colnum
        matrix out[`k'+1,`colnum'] = ub
    }
}

matrix colnames out = `colnm'
matrix rownames out = `rownm'
matrix list out, noheader format(%6.3f)

//  using forvalues to create tables with other commands (pg  2Ed)

sum age
return list

matrix mn = J(4,2,.) // matrix for means
matrix colnames mn = wc=0 wc=1
matrix rownames mn = k5=0 k5=1 k5=2 k5=3
matrix sd = mn // matrix for SDs
matrix n = mn // matrix for Ns

forvalues w = 0/1 {
    forvalues k = 0/3 {
        sum age if wc==`w' & k5==`k'
        matrix n[`k'+1,`w'+1] = r(N)
        matrix mn[`k'+1,`w'+1] = r(mean)
        matrix sd[`k'+1,`w'+1] = r(sd)
    }
}

matrix list n, format(%6.0f) noheader
matrix list mn, format(%6.3f) noheader
matrix list sd, format(%6.3f) noheader

log close
