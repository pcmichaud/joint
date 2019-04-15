clear all
capture log close
set more off

capture cd ~/cedia/Projets/joint

log using runtime/output_simulation.txt, replace text

matrix result = J(7,3,.)
local j = 1
*"ref" "discount" "nouh" "unitary" "nocorr" "nocomp" "nosurv"
foreach scn in "discount" {

infile hhidpn insim jprob rexpret_sim sexpret_sim leisure_m leisure_f using "data/outcomes_`scn'.dat", clear
format hhidpn %9.0f
foreach var of varlist jprob rexpret_sim sexpret_sim {
	qui recode `var' (-999=.)
	qui replace `var' = . if insim==0
}

label var insim "used in simulations"
label var jprob "probability of joint retirement"
label var rexpret_sim "expected retirement age of husband"
label var sexpret_sim "expected retirement age of wife"
label var leisure_m "unobserved heterogeneity husband"
label var leisure_f "unobserved heterogeneity wife"

sort hhidpn

save "temp/outcomes_`scn'.dta", replace

use "data/hrs_final_ref.dta", clear
format hhidpn %9.0f
sort hhidpn
qui merge 1:1 hhidpn using "temp/outcomes_`scn'.dta"
qui keep if _merge==3
keep if insim==1
drop _merge

save "data/hrs_final_`scn'_withsims.dta", replace


qui sum jprob 
matrix result[`j',1] = r(mean)

sum rexpret_sim
matrix result[`j',2] = r(mean)

sum sexpret_sim
matrix result[`j',3] = r(mean)

gen error = ((rexpret_sim < rage_mod) + (sexpret_sim < sage_mod))>0

gen diff = rexpret_sim - sexpret_sim

tab diff

gen joint = diff == (rage_mod - sage_mod)

sum joint

tab sexpret_sim

qui sum leisure_m leisure_f
qui pwcorr leisure_m leisure_f
if ("`scn'"=="discount") {
qui tab rexpret_sim sexpret_sim, nofreq cell
}
local j = `j' + 1
}

matrix rownames result = "ref" "discount" "nouh" "unitary" "nocorr" "nocomp" "nosurv"
matrix colnames result = "joint" "husband" "wife"

matrix list result

capture log close

