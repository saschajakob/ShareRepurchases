//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 3 - Testing, estimationg, and fit
//  Long and Freese - 27Jul2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch3estimate, text replace

//  stata's output for ML estimation (pg 68 RevEd)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc

//  syntax of estimation commands (pg 71 RevEd)

sysuse binlfp2, clear
logit lfp k5 k618 age wc lwg
logit lfp k5 k618 age wc lwg if hc==1

*   hypothetical example of using weights:
*   logit lfp k5 k618 age wc lwg [pweight=wgtvar]

*   using if and level options
logit lfp k5 k618 age wc lwg if hc==1, level(90)

*   dealing with (artifically created) missing data
sysuse binlfp2, clear
replace k5 = . in 1/5
replace age = . in 20/30
replace k618 = . in 3/12
logit lfp k5 k618 age wc hc lwg inc, nolog
logit lfp k5 age wc hc lwg inc, nolog

//  mark & markmiss to delete cases with missing data (pg 72 RevEd)

mark nomiss
markout nomiss lfp k5 k618 age wc hc lwg inc
tab nomiss
logit lfp k5 k618 age wc hc lwg inc if nomiss==1, nolog
logit lfp k5 age wc hc lwg inc if nomiss==1, nolog

//  using misschk (pg  2Ed)

sysuse gsskidvalue2.dta, clear
misschk age anykids black degree female kidvalue ///
    othrrace year income91 income, help gen(m_) dummy

*   logit on whether income is missing
logit m_income female black othrrace age, nolog

//  reading the output (pg 75 RevEd)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog

//  reformatting output with estimates table (pg 77 RevEd)

logit lfp k5 k618 age wc hc lwg, nolog
estimates table, b(%9.3f) t label varwidth(30)

//  reformatting output with estout (pg  2Ed)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc

*   single column table
estout using table1.txt, replace style(fixed) ///
    prehead("Model of women's labor force participation") ///
    posthead("---------------------------------------------") ///
    collabels("Coef") ///
    cells( b(fmt(%9.3f)) se(par fmt(%9.2f)) ) ///
    label varwidth(30) varlabels(_cons "Constant") ///
    prefoot("---------------------------------------------") ///
    stats(N ll, fmt(%9.0g)) ///
    postfoot("Note: Standard errors in parentheses")

*   two column table
estout using table2.txt, replace style(fixed) ///
    prehead("Model of women's labor force participation") ///
    posthead("------------------------------------------------------------") ///
    cells("b(fmt(%9.3f) star label(Coef)) se(fmt(%9.3f) label(Std Err))") ///
    label varwidth(30) varlabels(_cons "Constant") ///
    prefoot("------------------------------------------------------------") ///
    stats(N ll, fmt(%9.0g))

//  alternative output with listcoef (pg 82 RevEd)

sysuse science2, clear
regress job female phd mcit3 fellow pub1 cit1
listcoef female cit1, help

//  Wald tests (pg 85 RevEd)

sysuse binlfp2, clear
logit lfp k5 k618 age wc hc lwg inc, nolog
test k5
test k5 k618
test k5 k618 age wc hc lwg inc
test k5=k618

*   accumulate option
test k5=k618
test wc=hc, accumulate

//  LR tests (pg 86 RevEd)

logit lfp k5 k618 age wc hc lwg inc, nolog
estimates store fmodel
logit lfp age wc hc lwg inc, nolog
estimates store nmodel
lrtest fmodel nmodel

//  estat summarize (pg  2Ed)

logit lfp k5 k618 age wc hc lwg inc, nolog
estat sum

//  measures of fit (pg 89 RevEd)

*   model 1
logit lfp k5 k618 age wc hc lwg inc, nolog
fitstat
quietly fitstat, saving(mod1)

*   model 2
generate agesq = age*age
logit lfp k5 age agesq wc inc, nolog

*   compare model 1 and model 1
fitstat, using(mod1)
logit lfp k5 k618 age wc hc lwg inc, nolog
lstat

//  predictions using predict (pg 99 RevEd)

logit lfp k5 k618 age wc hc lwg inc, nolog
predict pr1
sum pr1

log close
