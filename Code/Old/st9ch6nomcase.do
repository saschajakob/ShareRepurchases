//  Regression Models for Categorical Dependent Variables - 2nd Edition
//  Chapter 6 - Models for Nominal Outcomes for Case-based Data
//  Long and Freese - 11Aug2005

version 9
set scheme s2manual
set more off

capture log close
log using st9ch6nomcase, text replace

//  the multinomial logit model (pg 190 RevEd)

* construct new outcome variables with 2 or 3 categories
sysuse nomocc2, clear
tab occ
gen occ3 = occ
recode occ3 1/3=1 4=2 5=3
label def locc3 1 Manual 2 WhiteCol 3 Prof
label val occ3 locc3
tab occ occ3,miss
gen wc_man= occ3==2 if occ3~=3
gen prof_man = occ3==3 if occ3~=2
gen prof_wc = occ3==3 if occ3~=1
label def lwm 1 WhiteCol 0 Manual
label def lpm 1 Prof 0 Manual
label def lpw 1 Prof 0 WhiteCol
label val wc_man lwm
label val prof_man lpm
label val prof_wc lpw
tab occ3 wc_man, miss
tab occ3 prof_man, miss
tab occ3 prof_wc, miss
tab prof_man, miss

* binary logits
tab prof_man, miss
logit prof_man ed, nolog

* estimate the corresponding multinomial logit
tab occ3, miss
mlogit occ3 ed, nolog

* effect of different category values for outcome
gen occ999 = occ3
recode occ999 1=-3 2=0 3=999
mlogit occ3 ed, nolog b(3)
mlogit occ999 ed, nolog b(999)

//  example of occupational attainment (pg 195 RevEd)

desc white ed exper
sum white ed exper
tab occ
mlogit occ white ed exper, baseoutcome(5) nolog

//  using different base categories (pg 197 RevEd)

listcoef white, help
listcoef, pvalue(.05)
listcoef ed, pvalue(.05) gt nolabel

//  predicting perfectly (example not in book) (pg 198 RevEd)

sysuse ordwarm2, clear
gen byte dprst = (prst<20 & warm==1)
tab dprst warm, miss
mlogit warm yr89 male white age ed dprst, nolog
drop if dprst==1
mlogit warm yr89 male white age ed, nolog

//  testing the effects of the independent variables (pg 200 RevEd)

* LR tests of single coefficient using lrtest
sysuse nomocc2, clear
mlogit occ white ed exper, baseoutcome(5) nolog
estimates store fmodel
* test of white
quietly mlogit occ ed exper, baseoutcome(5) nolog
estimates store nmodel_white
lrtest fmodel nmodel_white
* test of ed
quietly mlogit occ white exper, baseoutcome(5) nolog
estimates store nmodel_ed
lrtest fmodel nmodel_ed
* test of exper
quietly mlogit occ white ed, baseoutcome(5) nolog
estimates store nmodel_exper
lrtest fmodel nmodel_exper
* tests using mlogtest
quietly mlogit occ white ed exper, baseoutcome(5) nolog
mlogtest, lr

* Wald tests of single variable using test
quietly mlogit occ white ed exper, baseoutcome(5) nolog
test white
test ed
test exper
* tests using mlogtest
mlogtest, wald

* LR test of multiple variables using lrtest
mlogit occ white ed exper, baseoutcome(5) nolog
estimates store fmodel
mlogit occ white, baseoutcome(5) nolog
estimates store nmodel
lrtest fmodel nmodel
* test using mlogtest
mlogit occ white ed exper, baseoutcome(5) nolog
mlogtest, lr set(ed exper)

* Wald test of multiple variables with test
mlogit occ white ed exper, baseoutcome(5) nolog
test ed exper
* test using mlogtest
mlogit occ white ed exper, baseoutcome(5) nolog
mlogtest, wald set(ed exper)

//  tests for combining dependent categories (pg 203 RevEd)

* Wald test of combining categories using test
mlogit occ white ed exper, baseoutcome(5) nolog
mlogtest, combine
test [Menial]
test [Menial=Craft]

* LR test of combining categories using mlogtest
mlogit occ white ed exper, baseoutcome(5) nolog
mlogtest, lrcomb

* Example of the constraint imposed by mlogtest
mlogit occ white ed exper, nolog
estimates store fmodel
constraint define 999 [Menial]
mlogit occ exper ed white, base(2) constraint(999) nolog
estimates store nmodel
lrtest fmodel nmodel

//  dependence of irrelevant alternatives (pg 208 RevEd)

* Hausman test of iia
mlogit occ white ed exper, baseoutcome(5) nolog
mlogtest, hausman base

* Hausman using alternative outcomes (not in book)
quietly mlogit occ white ed exper, baseoutcome(1) nolog
mlogtest, hausman base
quietly mlogit occ white ed exper, baseoutcome(2) nolog
mlogtest, hausman base
quietly mlogit occ white ed exper, baseoutcome(3) nolog
mlogtest, hausman base
quietly mlogit occ white ed exper, baseoutcome(4) nolog
mlogtest, hausman base

* Small-Hsiao test of iia
quietly mlogit occ white ed exper, baseoutcome(5) nolog
set seed 339487731
mlogtest, smhsiao
set seed 8675309
mlogtest, smhsiao

//  in sample predicted probabilities with predict (pg 211 RevEd)

* estimate model and compute predictions
mlogit occ white ed exper, baseoutcome(5) nolog
predict ProbM ProbB ProbC ProbW ProbP
desc Prob*
sum Prob*

* using predict to compare mlogit and ologit
sysuse ordwarm2,clear
ologit warm yr89 male white age ed prst, nolog
predict SDologit Dologit Aologit SAologit
label var Dologit "ologit-D"
mlogit warm yr89 male white age ed prst, nolog
predict SDmlogit Dmlogit Amlogit SAmlogit
label var Dmlogit "mlogit-D"
dotplot Dologit Dmlogit, ylabel(0(.25).75)      ///
     ysize(2.0254) xsize(3.0381)
graph export 06omlogdot.emf, replace

* another interesting way to compare the predictions (not in book)
corr SDologit SDmlogit
corr Dologit Dmlogit
corr Aologit Amlogit
corr SAologit SAmlogit
graph twoway scatter Dologit Dmlogit,           ///
    xtitle("mlogit: Pr(y=Disagree)")        ///
    ytitle("ologit: Pr(y=Disagree)")        ///
    ylabel(0(.25).75) xlabel(0(.25).75)  sort
graph export 06omloggraph.emf, replace

//  individual predicted probabilities with prvalue (pg 212 RevEd)

sysuse nomocc2, clear
mlogit occ white ed exper, baseoutcome(5) nolog
quietly prvalue, x(white=0) rest(mean) save
prvalue, x(white=1) rest(mean) dif

//  tables of predicted probabilities with prtab (pg 213 RevEd)

label def lwhite 0 NonWhite 1 White
label val white lwhite
prtab ed white, novarlbl outcome(1)

//  graphing predicted probabilities with prgen (pg 214 RevEd)

* plotting probabilities to compare two groups
quietly mlogit occ white ed exper, baseoutcome(5)
prgen ed, x(white=1) from(6) to(20) generate(wht) ncases(15)
desc wht*
prgen ed, x(white=0) from(6) to(20) generate(nwht) ncases(15)
desc nwht*
label var whtp1 "Whites"
label var nwhtp1 "Nonwhites"

graph twoway connected whtp1 nwhtp1 nwhtx,  ///
    xtitle("Years of Education")            ///
    ytitle("Pr(Menial Job)")                ///
    ylabel(0(.25).50) xlabel(6 8 12 16 20)  ///
    ysize(2.7051) xsize(4.0421)
graph export 06prmenial.emf, replace

* plotting probabilities for all outcomes for one group
label var whts1 "Menial"
label var whts2 "Blue Collar"
label var whts3 "Craft"
label var whts4 "White Collar"
graph twoway connected whts1 whts2 whts3 whts4 whtx,    ///
    xtitle("Whites: Years of Education")                ///
    ytitle("Summed Probability")                        ///
    xlabel(6(2)20) ylabel(0(.25)1) ysize(2.6195) xsize(4.0421)
graph export 06prsum.emf, replace

gen zero = 0
gen one  = 1
graph twoway (rarea zero whts1 whtx, bc(gs1))           ///
    (rarea whts1 whts2 whtx, bc(gs4))                   ///
    (rarea whts2 whts3 whtx, bc(gs8))                   ///
    (rarea whts3 whts4 whtx, bc(gs11))                  ///
    (rarea whts4 one whtx, bc(gs14)),                   ///
    ytitle("Summed Probability")                        ///
    legend( order( 1 2 3 4 5)                           ///
    label( 1 "Menial")                                  ///
    label( 2 "Blue Collar") label( 3 "Craft")           ///
    label(4 "White Collar") label(5 "Professional"))    ///
    xtitle("Whites: Years of Education")                ///
    xlabel(6 8 12 16 20) ylabel(0(.25)1)                ///
    ysize(2.6195) xsize(4.0421)                         ///
    plotregion(margin(zero))
graph export 06prsumrevised.emf, replace

//  changes in predicted probabilities (pg 219 RevEd)

mlogit occ white ed exper, baseoutcome(5) nolog
* prchange
prchange
* mfx compute: patience, this can take a long time!
mfx compute, predict(outcome(1))

//  plotting discrete changes with mlogview (pg 221 RevEd)

   * Examples do not use mlogview since mlogview cannot automatically
   * reproduce graphs in do files; instead we use mlogplot

mlogplot, std(0uu) p(.1) dc ntics(9)
mlogplot, std(0ss) p(.1) min(-.2) max(.4) dc ntics(7)

//  odds ratios using listcoef and mlogview (pg 224 RevEd)

* plot hypothetical values: relative to A
matrix mnlbeta = (-.693, .693, .347 \ .347, -.347, .693 )
matrix mnlsd = (1, 2, 4)
global mnlname = "x1 x2 x3"
global mnlcatnm = "B C A"
global mnldepnm "depvar"
mlogplot, matrix std(uuu) vars(x1 x2 x3) packed
graph export 06orploteg.emf, replace

* plot hypothetical values: relative to B
matrix mnlbeta = (-.693, .693, .347 \ .347, -.347, .693 )
matrix mnlsd = (1, 2, 4)
global mnlname = "x1 x2 x3"
global mnlcatnm = "B C A"
global mnldepnm "depvar"
* note: use base(1) since B is the first letter in mnlcatnm
mlogplot, matrix std(uuu) vars(x1 x2 x3) packed base(1)
graph export 06orplotegb.emf, replace

* plot odds ratios from occupational outcomes example
mlogit occ white ed exper,baseoutcome(5) nolog
listcoef white, help
* or plot without significance levels
mlogplot white ed exper, std(0ss) b(5) p(1) min(-2.75) max(.55) or  ///
    packed ntics(7)
graph export 06orplotocc1.emf,replace

* or plot with significance levels
mlogplot white ed exper, prob(.1) std(0ss) b(5) min(-2.75) max(.55) ///
    or ntics(7)
graph export 06orplotocc2.emf, replace
* or plot with significance levels and discrete change
prchange
mlogplot white ed exper, prob(.1) std(0ss) b(5) min(-2.75) max(.55) ///
    dc or ntics(7)
graph export 06orplotocc3.emf, replace

//  using mlogplot (pg 230 RevEd)

* or plot using mlogplot
mlogit occ white ed exper, baseoutcome(5) nolog
prchange
mlogplot  white ed exper, std(0ss) p(.1) min(-2.75) max(.55) or ntics(7)

* other examples using mlogplot (not in book)
mlogplot white ed exper, std(0ss) p(.1) min(-2.75) max(.55) or dc ntics(7)
mlogplot white ed exper, dc std(0ss) min(-.5) max(.5)
mlogplot white ed exper, or std(0ss) min(-2.5) max(.5)
mlogplot white ed exper, or dc std(0ss) min(-.5) max(.5)
mlogplot white ed exper, dc or std(0ss) min(-2.5) max(.5) p(.1)

//   plotting estimates from matrices with mlogplot (pg 231 RevEd)

* plot example data
matrix mnlbeta = (-.693, .693, .347 \ .347, -.347, .693 )
matrix mnlsd   = (1, 2, 4)
global mnlname = "x1 x2 x3"
global mnlcatnm = "B C A"
global mnldepnm "depvar"
mlogplot, matrix std(uuu) vars(x1 x2 x3) packed
graph export 06matrix1.emf, replace

//  combining coefficients from two groups

* estimate two sets of coefficients
sysuse nomocc2, clear
mlogit occ ed exper if white==1, base(5) nolog
mlogit occ ed exper if white==0, base(5) nolog
* save coefficients for ed to mnlbeta
matrix mnlbeta =                    ///
    (-.8307514, -.9225522, -.6876114, -.4196403 \   ///
    -.7012628, -.560695 , -.882502 , -.5311514 )
* transpose to make columns correspond to variables
matrix mnlbeta = mnlbeta'
* compute sd of ed and add to matrix
sum ed
matrix mnlsd = (2.946427,2.946427)
global mnlname = "White NonWhite"
global mnlcatnm = "Menial BlueCol Craft WhiteCol Prof"
mlogplot, vars(White NonWhite) packed               ///
    or matrix std(ss)                   ///
    note("Racial Differences in Effects of Education")
graph export 06matrix2.emf, replace

//  multinomial probit (pg  2Ed)

* compare mprobit coefficients to probit coefficients
sysuse binlfp2, clear
probit lfp k5 k618 age wc hc lwg inc, nolog
mprobit lfp k5 k618 age wc hc lwg inc, nolog base(0)
mprobit lfp k5 k618 age wc hc lwg inc, nolog base(0) probitparam

* compare mprobit and probit predictions
sysuse binlfp2, clear
probit lfp k5 k618 age wc hc lwg inc, nolog
predict p_probit1
mprobit lfp k5 k618 age wc hc lwg inc, nolog base(0)
predict p_mprobit0 p_mprobit1
mprobit lfp k5 k618 age wc hc lwg inc, nolog base(0) probitparam
predict p_mprobit0p p_mprobit1p
pwcorr p_probit1 p_mprobit1 p_mprobit0 p_mprobit1p p_mprobit0p

* compare mprobit and mlogit
sysuse nomocc2, clear
mprobit occ white ed exper, base(1)
predict mpp1 mpp2 mpp3 mpp4 mpp5
* with probit normalization
mprobit occ white ed exper, base(1) probitparam
mlogit occ white ed exper, base(1)
predict mlp1 mlp2 mlp3 mlp4 mlp5
corr *p1
gen prdif1 = abs(mpp1-mlp1)
gen prdif2 = abs(mpp2-mlp2)
gen prdif3 = abs(mpp3-mlp3)
gen prdif4 = abs(mpp4-mlp4)
gen prdif5 = abs(mpp5-mlp5)
sum prdif*

//  stereotype logit model (pg  2Ed)

sysuse ordwarm2, replace
tab warm
tab warm, nolabel

* mlogit for attitudes toward working mothers
mlogit warm yr89 male white age ed prst, nolog base(4)

* slm for attitudes toward working mothers
slogit warm yr89 male white age ed prst, nolog

* predictions for the estimation sample
predict slpr1 slpr2 slpr3 slpr4
list warm slpr1-slpr4 in 1/4
mlogit warm yr89 male white age ed prst, nolog base(4)
predict mlpr1 mlpr2 mlpr3 mlpr4
list warm slpr1 mlpr1 slpr2 mlpr2 slpr3 mlpr3 in 1/5
pwcorr slpr1 mlpr1
pwcorr slpr2 mlpr2
pwcorr slpr3 mlpr3
pwcorr slpr4 mlpr4

* using pr* commands
prvalue , x(male=0) rest(mean) save
prvalue , x(male=1) rest(mean) dif
prchange male, rest(mean)

* odds ratios
listcoef, expand

* distinguishability and the phi parameters
slogit warm yr89 male white age ed prst, nolog
est store slbase
* wald test that phi1=phi2
nlcom ([phi1_1]_b[_cons] - [phi1_2]_b[_cons])
* phi1=phi2
constraint define 1 [phi1_1]_cons=[phi1_2]_cons
slogit warm yr89 male white age ed prst, nolog constraint(1)
lrtest slbase .
* phi2=phi3
constraint define 2 [phi1_2]_cons=[phi1_3]_cons
slogit warm yr89 male white age ed prst, nolog constraint(2)
lrtest slbase .
* phi3=phi4
constraint define 2 [phi1_3]_cons=0
slogit warm yr89 male white age ed prst, nolog constraint(2)
lrtest slbase .
* phi1=phi2 and phi3=phi4
slogit warm yr89 male white age ed prst, nolog constraint(1 2)
lrtest slbase .

* ordinality and the 1-dimensional slogit model
gen warm2 = warm
recode warm2 1=1 2=3 3=2 4=4
slogit warm2 yr89 male white age ed prst, nolog
est store warm2
slogit warm yr89 male white age ed prst, nolog
est store warm
estimates table warm warm2, b(%9.3f) t(%6.2f)

* dimensions in slogit
slogit warm yr89 male white age ed prst, nolog
slogit warm yr89 male white age ed prst, nolog dim(2)

log close

