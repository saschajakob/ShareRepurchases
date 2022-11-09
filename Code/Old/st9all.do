//  all spost 9 do files 19Jul2009 (update from 27Jul2005)

capture log close master
log using st9all, name(master) replace text

//  program:    st9all.do
//  task:       All test programs for spost9_ado
//  project:    SPost
//  author:     jsl \ 2008-06-27

do st9ch2tutorial.do
do st9ch3estimate.do
do st9ch4binary.do
do st9ch5ordinal.do
do st9ch6nomcase.do
do st9ch7nomalt.do
do st9ch8count.do
do st9ch9other.do

log close master
exit
