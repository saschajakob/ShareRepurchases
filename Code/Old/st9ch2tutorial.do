//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 2 - Stata tutorial
//  Long and Freese - 27Jul2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch2tutorial, replace text

//  loading the data

sysuse science2, clear

//  examining the data set

describe

//  examining individual variables

sum work
tab work, missing
codebook work

//  graphing variables

dotplot work

//  saving graphs

graph export 02dotplot.emf, replace

//  creating a dummy variable

gen isfac = (work==1) if work<.

//  checking transformations

tab isfac work, missing

//  labeling variables and values

label variable isfac "1=Faculty in University"
label define isfac 0 "NotFac" 1 "Faculty"
label values isfac isfac
tab isfac

//  creating an ordinal variable

tab job, missing
gen jobprst = job
recode jobprst .=. 1/1.99=1 2/2.99=2 3/3.99=3 4/5=4
label variable jobprst "Rankings of University Job"
label define prstlbl 1 "Adeq" 2 "Good" 3 "Strong" 4 "Dist"
label values jobprst prstlbl
tab jobprst, missing

//  combining variables

gen pubsum = pub3 + pub6 + pub9
label variable pubsum "Total Pubs in 9 Yrs post-Ph.D."
sum pub3 pub6 pub9 pubsum
graph matrix pub3 pub6 pub9 pubsum, ///
    half msymbol(smcircle_hollow) ysize(5) xsize(5)
graph export 02matrix.emf, replace

//  saving the new data

note: temporary data set for st9ch2tutorial.do
save sciwork, replace

//  closing the log

log close
