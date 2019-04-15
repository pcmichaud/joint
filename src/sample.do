clear all
capture log close
set more off
capture cd "~/cedia/Projets/joint"
capture cd "~/dropbox/joint"
log using tex/tables/sample.txt, text replace

global scn "ref"
use "data/hrs_final_$scn.dta", clear

global vars "age_mod lim college likejob wage liv75r hlth62"


preserve 
keep r* 
renpfix r
gen male = 1
save temp/husbands.dta, replace
restore

preserve 
keep s* 
renpfix s
gen male = 0
save temp/wives.dta, replace
use temp/husbands.dta, clear
append using temp/wives.dta

label var age_mod "age"
label var lim "any health limitations"
label var college "college educated"
label var likejob "job satisfaction"
label var wage "hourly wage"
label var liv75r "prov lives to 75 (relative to life-table)"
label var hlth62 "probability has health limitations at age 62"

eststo wives: quietly estpost sum $vars if male == 0
eststo husbands: quietly estpost summarize ///
    $vars if male==1
esttab wives husbands using tex/tables/sample.tex, replace ///
mtitles( "wives" "husbands") nonum ///
cells("mean(pattern(1 1) fmt(2)) sd(pattern(1 1))") ///
label
restore


* stats for text around age difference
gen agediff = rage_mod - sage_mod
sum agediff, d

sum wageratio, d


capture log close


