

capture log close
set more off


cd ~/cedia/Projets/joint

import delimited data/survival.csv, clear 

rename v1 sx_male 
rename v2 sx_female

gen mx_male = -(sx_male[_n+1] - sx_male[_n])/sx_male[_n]
gen mx_female = -(sx_female[_n+1] - sx_female[_n])/sx_female[_n]

gen logmx_male = log(mx_male)
gen logmx_female = log(mx_female)

gen age = _n

reg logmx_male age
matrix bm = e(b)'
gen pmx_male = exp(bm[2,1])*exp(bm[1,1]*age)
gen psx_male = exp(-exp(bm[2,1])/bm[1,1]*(exp(bm[1,1]*age)-1))


predict plogmx_male
reg logmx_female age
matrix bf = e(b)'
predict plogmx_female
gen pmx_female = exp(bf[2,1])*exp(bf[1,1]*age)
gen psx_female = exp(-exp(bf[2,1])/bf[1,1]*(exp(bf[1,1]*age)- 1))

matrix result = (bm,bf)
matrix result[2,1] = exp(result[2,1])
matrix result[2,2] = exp(result[2,2])

list age mx_male pmx_male mx_female pmx_female

list age sx_male psx_male sx_female psx_female

matrix list result
exit

svmat result
keep result1 result2
keep if result1!=.
outsheet using "params/gompertz_ref.csv", comma replace  nolabel nonames

