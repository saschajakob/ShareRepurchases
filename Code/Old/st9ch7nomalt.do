clear
* set memory 100m // removed for Stata 11
capture log close
version 9
set more off
estimates clear
clear

log using st9ch7nomalt, text replace

//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 7 - Models for Nominal Outcomes for Alternative-based Data
//  Long and Freese - 12Oct2005

//  Data arrangement for models with alternative-specific variables (pg  2Ed)

* data arranged by alternative
spex travel2, clear
list id mode choice train bus time invc in 1/6, nolabel sepby(id)

* data arranged by case
spex travel2case, clear
list id time1 time2 time3 invc1 invc2 invc3 choice in 1/2, nolabel

* convert from case-based to alternative-based
case2alt, alt(time invc) case(id) choice(choice) gen(choice2) altnum(mode)
list id choice2 train bus time invc in 1/6, nolabel sepby(id)

* example not in text
spex travel2case, clear
case2alt, alt(time invc gc invt) case(id) choice(choice) replace casevars(hinc psize)

//  Fitting the conditional logit model

spex travel2, clear
clogit choice train bus time invc, group(id) nolog

* interpreting odds ratios from clogit
listcoef, help

* interpreting probabilities from clogit - using predict
predict prob
list id prob choice train bus time invc in 1/6, nolabel sepby(id)
list id prob choice train bus time invc in 16/18, nolabel sepby(id)

* interpreting probabilities from clogit - using asprvalue
asprvalue, cat(train bus) base(car)

* verify that predictions do not depend on levels of time or invc
asprvalue, x(time=300 invc=20) cat(train bus) base(car)
asprvalue, rest(asmean) cat(train bus) base(car)

* compute predictions with average travel times
quietly asprvalue, x(time=643.4 674.6 578.3) rest(asmean) ///
    cat(train bus) base(car) save

* change time for train by 10 (643.4 become 653.4)
asprvalue, x(time=653.4 674.6 578.3) rest(asmean) ///
    cat(train bus) base(car) dif

* change time for bus by 10 (674.6 become 684.6)
asprvalue, x(time=643.4 684.6 578.3) rest(asmean) ///
    cat(train bus) base(car) dif

* change time for car by 10 (578.3 become 588.3)
asprvalue, x(time=643.4 674.6 588.3) rest(asmean) ///
    cat(train bus) base(car) dif

//  Fitting the multinomial logit model using clogit

* setting up the data with case2alt
spex nomocc2, clear
tab occ
desc occ
label define occlbl 1 "m" 2 "b" 3 "c" 4 "w" 5 "p", modify
tab occ
case2alt, casevars(ed exper white) choice(occ) gen(choice)
list _id choice m b c w p in 1/10, sepby(_id)
list _id m b c w mXed bXed cXed wXed in 1/10, sepby(_id)

/* ================================================================
*  not in text: using official stata to convert from case- to alternative- data
spex nomocc2, clear
* _id for each case
gen _id = _n
* make one record for each outcome
expand 5
sort _id
* define indicators for outcomes and choice
gen alt = mod(_n, 5)
replace alt = 5 if alt == 0
gen m = (alt==1)
gen b = (alt==2)
gen c = (alt==3)
gen w = (alt==4)
gen choice = (occ==alt)
list _id m b c w choice in 1/10, sepby(_id)
* interactions between outcomes and variables
gen mXwhite = white*m
gen bXwhite = white*b
gen cXwhite = white*c
gen wXwhite = white*w
gen mXed = ed*m
gen bXed = ed*b
gen cXed = ed*c
gen wXed = ed*w
gen mXexper = exper*m
gen bXexper = exper*b
gen cXexper = exper*c
gen wXexper = exper*w
list m b c w mXed bXed cXed wXed in 1/5
* to test if we did this right
mlogit occ white ed exper if alt==1, nolog
================================================================ */

* fitting multinomial logit with clogit
clogit choice mXwhite mXed mXexper m bXwhite bXed bXexper b  ///
              cXwhite cXed cXexper c wXwhite wXed wXexper w, ///
              group(_id) nolog

//  Using clogit to fit models with case- and alternative-specific variables

spex travel2, clear
gen busXhinc = bus*hinc
gen trainXhinc = train*hinc
gen busXpsize = bus*psize
gen trainXpsize = train*psize
clogit choice busXhinc busXpsize bus trainXhinc trainXpsize train ///
    time invc, group(id) nolog

* interpretation of odds ratios
listcoef busXhinc trainXhinc busXpsize trainXpsize, percent help
listcoef time invc bus train, help

* interpretation of predicted probabilities using asprvalue
asprvalue, base(car)
asprvalue, rest(asmean) base(car)

* discrete change in psize
quietly asprvalue, x(psize=1) rest(asmean) base(car) save
asprvalue, x(psize=2) rest(asmean) base(car) dif

* discrete change in time
quietly asprvalue, x(time=675 643 578) rest(asmean) base(car) save
asprvalue, x(time=540 643 578) rest(asmean) base(car) dif

//  Allowing the effects of alternative-specific variables to
//  vary over the alternatives

gen busXtime = bus*time
gen trainXtime = train*time
clogit choice busXhinc busXpsize busXtime bus trainXhinc trainXpsize ///
    trainXtime train time invc, group(id) nolog

//  Example of estimation by simulation using uniform random numbers

clear
set seed 11020
set obs 100
gen r_norm = invnormal(uniform())
gen rgt196 = r_norm>1.96
sum rgt196

clear
set obs 1000
gen r_norm = invnormal(uniform())
gen rgt196 = r_norm>1.96
sum rgt196

clear
set obs 1000000
gen r_norm = invnormal(uniform())
gen rgt196 = r_norm>1.96
sum rgt196

/*

//  Halton sequences

* requires halton mata routines from Rich Gates at StataCorp
clear
set obs 100
gen h_norm = .
mata
h_matrix = halton(100,1)
st_store(.,"h_norm",h_matrix)
end
replace h_norm = invnormal(h)
gen hgt196 = h_norm>1.96
sum hgt196

clear
set obs 1000
gen h_norm = .
mata
h_matrix = halton(1000,1)
st_store(.,"h_norm",h_matrix)
end
replace h_norm = invnormal(h)
gen hgt196 = h_norm>1.96
sum hgt196

clear
set obs 1000000
gen h_norm = .
mata
h_matrix = halton(1000000,1)
st_store(.,"h_norm",h_matrix)
end
replace h_norm = invnormal(h)
gen hgt196 = h_norm>1.96
sum hgt196

* plotting random numbers and halton sequences
clear
set seed 13209
set obs 2000
gen pseudo_random_1 = uniform()
gen pseudo_random_2 = uniform()
gen halton_sequence_1 = .
gen halton_sequence_2 = .

mata
a = halton(2000,2)
st_store(.,(3,4),a)
end

twoway (scatter pseudo_random_1 pseudo_random_2, sort msymbol(circle_hollow) msize(small)), ysize(4) xsize(4)
graph export haltonpseudo.emf, replace

twoway (scatter halton_sequence_2 halton_sequence_1, sort msymbol(circle_hollow) msize(small)), ysize(4) xsize(4)
graph export haltonhalton.emf, replace

*/

//  clogit and asmprobit with uncorrelated errors

* clogit model
spex travel2, clear
clogit choice time invc train bus, group(id) nolog
predict clmpr

* asmprobit model with uncorrelated homoskedastistic errors
asmprobit choice time invc, case(id) alternatives(mode) basealternative(Car) ///
    correlation(independent) stddev(homoskedastic) nolog
estat covariance
predict mnppr

* compare clogit and asmprobit predictions
pwcorr clmpr mnppr

//  Alternative-based data with correlated errors

spex travel2, clear
list id mode choice time invc in 1/6, nolabel sepby(id)
asmprobit choice time invc, case(id) alternatives(mode) nolog
estat covariance
estat correlation
estimates store asmp_correlated

//  The structural covariance matrix

* base 1 and scale 2
asmprobit choice time invc, nolog case(id) alternatives(mode) ///
    base(1) scale(2) structural
estat cov
estat cor
asprvalue

* base and scale at defaults
asmprobit choice time invc, nolog case(id) alternatives(mode) ///
    structural
estat cov
estat cor
asprvalue

* base 1 and scale 3
asmprobit choice time invc, nolog case(id) alternatives(mode) ///
    base(1) scale(3) structural
estat cov
estat cor
asprvalue

//  Interpretations using probabilities

estimates restore asmp_correlated

* predictions using predict
predict prob
list id prob choice train bus time invc in 1/6, nolabel sepby(id)
list id prob choice train bus time invc in 16/18, nolabel sepby(id)

* predictions using asprvalue
asprvalue
asprvalue, rest(asmean)
asprvalue, x(time=300 invc=20)

* computing discrete change
quietly asprvalue, x(time=643.4 674.6 578.3) rest(asmean) save
* increase the time for train by 10
asprvalue, x(time=653.4 674.6 578.3) rest(asmean) dif
* increase the time for bus by 10
asprvalue, x(time=643.4 684.6 578.3) rest(asmean) dif
* increase the time for car by 10
asprvalue, x(time=643.4 674.6 588.3) rest(asmean) dif

//  identification, probabilities and marginal/discrete change

* default base and scale
asmprobit choice time invc, case(id) alternatives(mode)
estat covariance

* base is Train, scale is Bus
asmprobit choice time invc, case(id) alternatives(mode) base(Train) scale(Bus)
    estimates store train_bus

* base is Bus, scale is Car
asmprobit choice time invc, case(id) alternatives(mode) base(Bus) scale(Car)
    estimates store bus_car

* compare models with different base and scale values
estimates table train_bus bus_car, stats(N ll) b(%9.4f) t(%6.2f)

* compare predicted probabilities
estimates restore train_bus
asprvalue
estimates restore bus_car
asprvalue

* discrete change and partial change
quietly asprvalue, x(time=643.4 674.6 578.3) rest(asmean) save
asprvalue, x(time=653.4 674.6 578.3) rest(asmean) dif
estat mfx

* discrete and partial change for models with other base and scale alternatives
estimates restore train_bus
quietly asprvalue, x(time=643.4 674.6 578.3) rest(asmean) save
asprvalue, x(time=653.4 674.6 578.3) rest(asmean) dif
estat mfx

* discrete and partial change for models with other base and scale alternatives
estimates restore bus_car
quietly asprvalue, x(time=643.4 674.6 578.3) rest(asmean) save
asprvalue, x(time=653.4 674.6 578.3) rest(asmean) dif
estat mfx

//  Compare LR tests of coefficient for time

* base(Train) scale(Bus)
asmprobit choice invc, case(id) alternatives(mode) base(Train) scale(Bus)
    estimates store train_bus_notime
lrtest train_bus train_bus_notime

*   base(Bus) scale(Car)
asmprobit choice invc, case(id) alternatives(mode) base(Bus) scale(Car)
    estimates store bus_car_notime
lrtest bus_car_notime bus_car

//  Testing for IIA with an LR test

* model with uncorrelated, heteroskedastistic errors
asmprobit choice time invc, case(id) alternatives(mode) ///
    correlation(independent)
estat covariance
estimates store asmp_uncorrelated

* compare to model with correlated errors
lrtest asmp_correlated asmp_uncorrelated

//  Adding case specific data

list id mode choice time invc hinc psize in 1/6, sepby(id)
asmprobit choice time invc, case(id) alternatives(mode) ///
    casevars(hinc psize)
asprvalue

//  Rank-ordered logistic regression model

spex wlsrnk, clear
label variable value1 "est"
label variable value2 "var"
label variable value3 "aut"
label variable value4 "sec"

* convert data to alternative-based form
case2alt, casevars(fem hn) rank(value) case(id) alt(hashi haslo) gen(rank)

* estimate model
rologit rank estXfem estXhn est varXfem varXhn var ///
    autXfem autXhn aut hashi haslo, group(id) reverse nolog

* interpretation using odds ratios
listcoef, percent help

* interpretation using predicted probabilities
asprvalue, x(fem=1 hashi=0 haslo=0) base(sec) save
asprvalue, x(fem=0 hashi=0 haslo=0) base(sec) dif
quietly asprvalue, x(fem=1 hashi=0 0 0 0 haslo=0 0 1 0) base(sec) save
asprvalue, x(fem=1 hashi=0 0 1 0 haslo=0 0 0 0) base(sec) dif

log close
