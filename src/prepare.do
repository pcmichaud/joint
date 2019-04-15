clear all
capture log close
set mem 500m
set more off
set maxvar 25000, perm
capture cd "~/cedia/Projets/joint"
capture cd "~/dropbox/joint"

log using tex/tables/prepare.txt, text replace

global scn "ref"
use raw/NET11_R.dta, clear

	foreach var of varlist * {
		capture	rename `var' `=lower("`var'")'
	}


* 705 respondents with non missing answer
di _N
keep if j003_11!=.
di _N
**** create hhidpn from hhid and pn
gen hhidpn = hhid + pn
gen shhidpn = hhidpn
destring hhidpn, replace

sort hhid
by hhid: gen nobs = _N


**** there are only 6 ratings with answers , (1 to 6)
	rename j003_11 rrate_scn1
	rename j007_11 rrate_scn2
	rename j011_11 rrate_scn3
	rename j015_11 rrate_scn4
	rename j019_11 rrate_scn5
	rename j023_11 rrate_scn6

	rename j004_11 srate_scn1
	rename j008_11 srate_scn2
	rename j012_11 srate_scn3
	rename j016_11 srate_scn4
	rename j020_11 srate_scn5
	rename j024_11 srate_scn6

	sum rrate_* srate_*

**** choice question 2 was dropped, so we have 3 choice questions (1,3,4)

	* scn 1
	rename j028_11 rchoice_scn1_r
	rename j029_11 rchoice_scn1_s
	rename j030_11 rchoice_scn1_h

	* scn 3 (now 2 since skip 2)
	rename j038_11 rchoice_scn2_r
	rename j039_11 rchoice_scn2_s
	rename j040_11 rchoice_scn2_h

	* scn 4 (now 3 since skip 2)
	rename j043_11 rchoice_scn3_r
	rename j044_11 rchoice_scn3_s
	rename j045_11 rchoice_scn3_h

sum rchoice*

**** data on age used in study
	rename ca_rs_age rage_mod
	rename ca_sp_age sage_mod
	rename da_11 dage_mod
sum ?age_mod

*** Hours data ****

	* current work hours
	rename a010_11 rwork
	rename a011_11 rhours
	rename a012_11 swork
	rename a013_11 shours
	replace rhours = min(rhours,80) if rhours!=.
	replace shours = min(shours,80) if shours!=.
	sum ?hours


	* hours in rating scenarios (they all work)

		* scn 1
		* r works until 65, then retires
		gen rh_scn1_a62 = rhours
		gen rh_scn1_a65 = 0
		gen rh_scn1_a68 = 0

		* s works until 68, then retires (unless 68+)
		gen sh_scn1_a62 = shours if da62<68
		replace sh_scn1_a62 = 0 if da62>=68

		gen sh_scn1_a65 = shours if da65<68
		replace sh_scn1_a65 = 0 if da65>=68

		gen sh_scn1_a68 = shours if da68<62
		replace sh_scn1_a68 = 0 if da68>=62

		* scn 2
		* r retires at 62, then retires
		gen rh_scn2_a62 = 0
		gen rh_scn2_a65 = 0
		gen rh_scn2_a68 = 0

		* s works until 68, then retires (unless 68+)
		gen sh_scn2_a62 = shours if da62<62
		replace sh_scn2_a62 = 0 if da62>=62

		gen sh_scn2_a65 = shours if da65<62
		replace sh_scn2_a65 = 0 if da65>=62

		gen sh_scn2_a68 = shours if da68<62
		replace sh_scn2_a68 = 0 if da68>=62

		* scn 3
		* r works until 68, then retires
		gen rh_scn3_a62 = rhours
		gen rh_scn3_a65 = rhours
		gen rh_scn3_a68 = 0

		* s works until 68, then retires (unless 68+)
		gen sh_scn3_a62 = shours if da62<65
		replace sh_scn3_a62 = 0 if da62>=65

		gen sh_scn3_a65 = shours if da65<65
		replace sh_scn3_a65 = 0 if da65>=65

		gen sh_scn3_a68 = shours if da68<62
		replace sh_scn3_a68 = 0 if da68>=62

		* scn 4
		* r gradually retires at 62 and fully retires at 65
		gen rh_scn4_a62 = rhours if rhours<=35
		replace rh_scn4_a62 = 0.6*rhours if rhours>35
		gen rh_scn4_a65 = 0
		gen rh_scn4_a68 = 0

		* s works until 65, then retires
		gen sh_scn4_a62 = shours if da62<62
		replace sh_scn4_a62 = 0 if da62>=62

		gen sh_scn4_a65 = shours if da65<62
		replace sh_scn4_a65 = 0 if da65>=62

		gen sh_scn4_a68 = shours if da68<62
		replace sh_scn4_a68 = 0 if da68>=62

		* scn 5
		* r works until 68, then retires
		gen rh_scn5_a62 = rhours
		gen rh_scn5_a65 = rhours
		gen rh_scn5_a68 = 0

		* s works until 65, then retires
		gen sh_scn5_a62 = shours if da62<65
		replace sh_scn5_a62 = 0.6*shours if da62>=65&da62<=67&shours>=35
		replace sh_scn5_a62 = shours if da62>=65&da62<=67&shours<35
		replace sh_scn5_a62 = 0 if sh_scn5_a62==.

		gen sh_scn5_a65 = shours if da65<65
		replace sh_scn5_a65 = 0.6*shours if da65>=65&da65<=67&shours>=35
		replace sh_scn5_a65 = shours if da65>=65&da65<=67&shours<35
		replace sh_scn5_a65 = 0 if sh_scn5_a65==.

		gen sh_scn5_a68 = shours if da68<62
		replace sh_scn5_a68 = 0 if da68>=62


		* scn 6
		gen rh_scn6_a62 = rhours
		gen rh_scn6_a65 = 0
		gen rh_scn6_a68 = 0

		gen sh_scn6_a62 = shours if da62<65
		replace sh_scn6_a62 = 0 if da62>=65

		gen sh_scn6_a65 = shours if da65<65
		replace sh_scn6_a65 = 0 if da65>=65

		gen sh_scn6_a68 = shours if da68<62
		replace sh_scn6_a68 = 0 if da68>=62


	    * hours in choice scenarios

		* choice 1, scn 2 against 4
		gen rh_scn6a_a62 = rh_scn2_a62
		gen rh_scn6a_a65 = rh_scn2_a65
		gen rh_scn6a_a68 = rh_scn2_a68

		gen sh_scn6a_a62 = sh_scn2_a62
		gen sh_scn6a_a65 = sh_scn2_a65
		gen sh_scn6a_a68 = sh_scn2_a68

		gen rh_scn6b_a62 = rh_scn4_a62
		gen rh_scn6b_a65 = rh_scn4_a65
		gen rh_scn6b_a68 = rh_scn4_a68

		gen sh_scn6b_a62 = sh_scn4_a62
		gen sh_scn6b_a65 = sh_scn4_a65
		gen sh_scn6b_a68 = sh_scn4_a68


		* choice 3, scn 5 against 2
		gen rh_scn7a_a62 = rh_scn5_a62
		gen rh_scn7a_a65 = rh_scn5_a65
		gen rh_scn7a_a68 = rh_scn5_a68

		gen sh_scn7a_a62 = sh_scn5_a62
		gen sh_scn7a_a65 = sh_scn5_a65
		gen sh_scn7a_a68 = sh_scn5_a68

		gen rh_scn7b_a62 = rh_scn2_a62
		gen rh_scn7b_a65 = rh_scn2_a65
		gen rh_scn7b_a68 = rh_scn2_a68

		gen sh_scn7b_a62 = sh_scn2_a62
		gen sh_scn7b_a65 = sh_scn2_a65
		gen sh_scn7b_a68 = sh_scn2_a68


		* choice 4, scn 3 against 6
		gen rh_scn8a_a62 = rh_scn3_a62
		gen rh_scn8a_a65 = rh_scn3_a65
		gen rh_scn8a_a68 = rh_scn3_a68

		gen sh_scn8a_a62 = sh_scn3_a62
		gen sh_scn8a_a65 = sh_scn3_a65
		gen sh_scn8a_a68 = sh_scn3_a68

		gen rh_scn8b_a62 = rh_scn6_a62
		gen rh_scn8b_a65 = rh_scn6_a65
		gen rh_scn8b_a68 = rh_scn6_a68

		gen sh_scn8b_a62 = sh_scn6_a62
		gen sh_scn8b_a65 = sh_scn6_a65
		gen sh_scn8b_a68 = sh_scn6_a68

sum rh_scn* sh_scn*

*** Income data ***

	* income in rating scenario (replacement rate)

		* scn 1
		* r works until 65, then retires
		gen ri_scn1_a62 = 100
		gen ri_scn1_a65 = 50 + 5*(pr_randomyou1-1)
		gen ri_scn1_a68 = 50 + 5*(pr_randomyou1-1)

		* s works until 68, then retires (unless 68+)
		gen si_scn1_a62 = 100 if da62<68
		replace si_scn1_a62 = 60 + 5*(pr_randomspp1-1) if da62>=68

		gen si_scn1_a65 =100 if da65<68
		replace si_scn1_a65 = 60 + 5*(pr_randomspp1-1) if da65>=68

		gen si_scn1_a68 = 100 if da68<62
		replace si_scn1_a68 = 40 + 5*(pr_randomspp1-1) if da68>=62 & da68<=64
		replace si_scn1_a68 = 50 + 5*(pr_randomspp1-1) if da68>=65 & da68<=67
		replace si_scn1_a68 = 60 + 5*(pr_randomspp1-1) if da68>=68

		* scn 2
		* r retires at 62, then retires
		gen ri_scn2_a62 = 40 + 5*(pr_randomyou2-1)
		gen ri_scn2_a65 = 40 + 5*(pr_randomyou2-1)
		gen ri_scn2_a68 = 40 + 5*(pr_randomyou2-1)

		* s works until 68, then retires (unless 68+)
		gen si_scn2_a62 = 100 if da62<62
		replace si_scn2_a62 = 40 + 5*(pr_randomspp2-1) if da62>=62

		gen si_scn2_a65 = 100 if da65<62
		replace si_scn2_a65 = 40 + 5*(pr_randomspp2-1) if da65>=62

		gen si_scn2_a68 = 100 if da68<62
		replace si_scn2_a68 = 40 + 5*(pr_randomspp2-1) if da68>=62

		* scn 3
		* r works until 68, then retires
		gen ri_scn3_a62 = 100
		gen ri_scn3_a65 = 100
		gen ri_scn3_a68 = 60 + 5*(pr_randomyou3-1)

		* s works until 68, then retires (unless 68+)
		gen si_scn3_a62 = 100 if da62<65
		replace si_scn3_a62 = 50 + 5*(pr_randomspp3-1) if da62>=65

		gen si_scn3_a65 = 100 if da65<65
		replace si_scn3_a65 = 50 + 5*(pr_randomspp3-1) if da65>=65

		gen si_scn3_a68 = 100 if da68<62
		replace si_scn3_a68 = 40 + 5*(pr_randomspp3-1) if da68>=62 & da68<=64
		replace si_scn3_a68 = 50 + 5*(pr_randomspp3-1) if da68>=65

		* scn 4
		* r gradually retires at 62 and fully retires at 65
		gen ri_scn4_a62 = 100 if rhours<=35
		replace ri_scn4_a62 = 100 if rhours>35
		gen ri_scn4_a65 = 50 + 5*(pr_randomyou4-1)
		gen ri_scn4_a68 = 50 + 5*(pr_randomyou4-1)

		* s works until 65, then retires
		gen si_scn4_a62 = 100 if da62<62
		replace si_scn4_a62 = 40 + 5*(pr_randomspp4-1) if da62>=62

		gen si_scn4_a65 = 100 if da65<62
		replace si_scn4_a65 = 40 + 5*(pr_randomspp4-1) if da65>=62

		gen si_scn4_a68 = 100 if da68<62
		replace si_scn4_a68 = 40 + 5*(pr_randomspp4-1) if da68>=62

		* scn 5
		* r works until 68, then retires
		gen ri_scn5_a62 = 100
		gen ri_scn5_a65 = 100
		gen ri_scn5_a68 = 70 + 5*(pr_randomyou5-1)

		* s works until 65, then retires
		gen si_scn5_a62 = 100 if da62<65
		replace si_scn5_a62 = 100 if da62>=65&da62<=67&shours>=35
		replace si_scn5_a62 = 100 if da62>=65&da62<=67&shours<35
		replace si_scn5_a62 = 60 + 5*(pr_randomspp5-1) if si_scn5_a62==.

		gen si_scn5_a65 = 100 if da65<65
		replace si_scn5_a65 = 100 if da65>=65&da65<=67&shours>=35
		replace si_scn5_a65 = 100 if da65>=65&da65<=67&shours<35
		replace si_scn5_a65 = 60 + 5*(pr_randomspp5-1) if si_scn5_a65==.

		gen si_scn5_a68 = 100 if da68<62
		replace si_scn5_a68 = 40 + 5*(pr_randomspp5-1) if da68>=62&da68<=64
		replace si_scn5_a68 = 50 + 5*(pr_randomspp5-1) if da68>=65&da68<=67
		replace si_scn5_a68 = 60 + 5*(pr_randomspp5-1) if da68>=68


		* scn 6
		gen ri_scn6_a62 = 100
		gen ri_scn6_a65 = 50 + 5*(pr_randomyou6-1)
		gen ri_scn6_a68 = 50 + 5*(pr_randomyou6-1)

		* s works until 68, then retires (unless 68+)
		gen si_scn6_a62 = 100 if da62<65
		replace si_scn6_a62 = 50 + 5*(pr_randomspp6-1) if da62>=65

		gen si_scn6_a65 =100 if da65<65
		replace si_scn6_a65 = 50 + 5*(pr_randomspp6-1) if da65>=65

		gen si_scn6_a68 = 100 if da68<62
		replace si_scn6_a68 = 40 + 5*(pr_randomspp6-1) if da68>=62 & da68<=64
		replace si_scn6_a68 = 50 + 5*(pr_randomspp6-1) if da68>=65


	* income in choice scenarios
		* choice 1, scn 2 against 4
		gen ri_scn61_a62 = ri_scn2_a62
		gen ri_scn61_a65 = ri_scn2_a65
		gen ri_scn61_a68 = ri_scn2_a68

		gen si_scn61_a62 = si_scn2_a62
		gen si_scn61_a65 = si_scn2_a65
		gen si_scn61_a68 = si_scn2_a68

		gen ri_scn62_a62 = ri_scn4_a62
		gen ri_scn62_a65 = ri_scn4_a65
		gen ri_scn62_a68 = ri_scn4_a68

		gen si_scn62_a62 = si_scn4_a62
		gen si_scn62_a65 = si_scn4_a65
		gen si_scn62_a68 = si_scn4_a68


		* choice 3, scn 5 against 2
		gen ri_scn71_a62 = ri_scn5_a62
		gen ri_scn71_a65 = ri_scn5_a65
		gen ri_scn71_a68 = ri_scn5_a68

		gen si_scn71_a62 = si_scn5_a62
		gen si_scn71_a65 = si_scn5_a65
		gen si_scn71_a68 = si_scn5_a68

		gen ri_scn72_a62 = ri_scn2_a62
		gen ri_scn72_a65 = ri_scn2_a65
		gen ri_scn72_a68 = ri_scn2_a68

		gen si_scn72_a62 = si_scn2_a62
		gen si_scn72_a65 = si_scn2_a65
		gen si_scn72_a68 = si_scn2_a68


		* choice 4, scn 3 against 6
		gen ri_scn81_a62 = ri_scn3_a62
		gen ri_scn81_a65 = ri_scn3_a65
		gen ri_scn81_a68 = ri_scn3_a68

		gen si_scn81_a62 = si_scn3_a62
		gen si_scn81_a65 = si_scn3_a65
		gen si_scn81_a68 = si_scn3_a68

		gen ri_scn82_a62 = ri_scn6_a62
		gen ri_scn82_a65 = ri_scn6_a65
		gen ri_scn82_a68 = ri_scn6_a68

		gen si_scn82_a62 = si_scn6_a62
		gen si_scn82_a65 = si_scn6_a65
		gen si_scn82_a68 = si_scn6_a68


sum ri_scn* si_scn*


**** other questions from internet survey ***

	* job satisfaction
	rename j046_11 rjobsat
	rename j047_11 sjobsat


**** merge to RAND HRS 2010 (interviews for half the new respondents in 2010) ****
	merge hhidpn using raw/rndhrs_n.dta, nokeep sort

	foreach var of varlist * {
		capture	rename `var' `=lower("`var'")'
	}

	tab _merge



	* get wage and earnings of both spouses
	egen rearn = rowmean(r7iearn r8iearn r9iearn r10iearn)
	egen searn = rowmean(r7iearn r8iearn r9iearn r10iearn)

	* get wages from various waves
	egen rwage = rowmean(r7wgihr r8wgihr r9wgihr r10wgihr)
	egen swage = rowmean(s7wgihr s8wgihr s9wgihr s10wgihr)


*	keep if r10work==1 & s10work==1

	* sex
	recode ragender (1=1) (2=0), gen(rsex)
	gen hwho = rsex==1

	* get education status
	rename raeduc reduc
	gen rcollege = reduc==5 
	rename s10educ seduc
	gen scollege = seduc==5 

* j045 j046 job satisfaction (1 to 5, 5 very  disatisfied)
	recode rjobsat (1/2=1) (else=0), gen(rlikejob)
	recode sjobsat (1/2=1) (else=0), gen(slikejob)

* J052 J053 health limits work after 62
	recode j052_11 (999=.), gen(rhlth62)
	recode j054_11 (999=.), gen(shlth62)
	replace rhlth62 = rhlth62/100.0
	replace shlth62 = shlth62/100.0

	rename r10hlthlm rlim
	rename s10hlthlm slim 

	* get their current health status
	rename r10shlt rshlt
	gen rbad = inlist(rshlt,4,5) 
	rename s10shlt sshlt
	gen sbad = inlist(sshlt,4,5)
	* get their current wealth (non housing), (not used right now)
	rename h10atotn hwealth

	* get their other income (not used right now)
	rename h10itot otherinc
	replace otherinc = otherinc - rearn - searn

	* get retirement expectations
	rename  r10rplnya  rexpret
	rename  s10rplnya  sexpret
	gen hexpret = rexpret - sexpret
	recode hexpret (min/-5=-5) (-4/-2=-3) (-1/1=0) (2/4=3) (5/max=5), gen(hexpret_g)
	replace hexpret_g = .n if rexpret==.n | sexpret==.n

	reg rwage rage_mod rcollege rbad rearn h10atota
	predict prwage
	replace rwage = prwage if missing(rwage)
	drop prwage
	reg swage sage_mod scollege sbad searn h10atota
	predict pswage
	replace swage = pswage if missing(swage)
	drop pswage
	drop if missing(rwage)
	drop if missing(swage)
	sum rwage swage, d
	gen wageratio = log(max((min(rwage/swage,5)),0.1))

* j048, j050 survival 75
	rename r10liv75r rliv75r
	rename s10liv75r sliv75r
	foreach var of varlist rliv75r sliv75r {
		qui sum `var' if `var'>0
		replace `var' = r(p1) if `var'==0
		reg `var' rage_mod sage_mod rbad sbad rcollege scollege if missing(`var')==0
		predict p`var' 
		replace `var' = p`var' if missing(`var')==1
		drop if missing(`var')==1
	}



*** create groups of variables
drop if rage_mod>62
drop if sage_mod>62
gen rage = 0
gen sage = 0
gen hwho_m = 1-hwho
gen hwho_f = hwho
format hhidpn %12.0f
global id "hhidpn"
global taste_m "rage rlim  rcollege  hwho_m rlikejob"
global taste_f "sage slim  scollege hwho_f slikejob"
global weight "wageratio"
global ratings "rrate_scn1 srate_scn1 rrate_scn2 srate_scn2 rrate_scn3 srate_scn3 rrate_scn4 srate_scn4 rrate_scn5 srate_scn5 rrate_scn6 srate_scn6"
renpfix rchoice hchoice
global choice  "hchoice_scn1_? hchoice_scn2_? hchoice_scn3_?"
global age "rage_mod sage_mod"
global who "hwho"
global hours_m "rh_scn?_a6* rh_scn6a_a6* rh_scn6b_a6* rh_scn7a_a6* rh_scn7b_a6* rh_scn8*_a6*"
global hours_f "sh_scn?_a6* sh_scn6a_a6* sh_scn6b_a6* sh_scn7a_a6* sh_scn7b_a6* sh_scn8*_a6*"
global income_m "ri_scn?_a6* ri_scn61_a6* ri_scn62_a6* ri_scn7*_a6* ri_scn8*_a6*"
global income_f "si_scn?_a6* si_scn61_a6* si_scn62_a6* si_scn7*_a6* si_scn8*_a6*"
global expect "rexpret sexpret"
global leavebehind "rwage swage rhours shours "
global prob "rhlth62 shlth62 rliv75r sliv75r"
**** rescale the income vars for scenarios (can add other income and wealth later)
replace rrate_scn6 = 0
replace srate_scn6 = 0

foreach var of varlist ri_* {
	qui replace `var' = rwage*rhours*`var'/100
}
foreach var of varlist si_* {
	qui replace `var' = swage*shours*`var'/100
}

di _N

foreach j of numlist 1/6 61 62 71 72 81 82 {
	foreach a of numlist 62 65 68 {
		gen hi_scn`j'_a`a' = ri_scn`j'_a`a' + si_scn`j'_a`a'
		drop if hi_scn`j'_a`a'<1
		label var hi_scn`j'_a`a' "household income in scenario `j' when r is `a'"
	}
}

global income_h "hi_scn1_a62-hi_scn6_a68 hi_scn61_a62-hi_scn82_a68"

di _N

keep $id $taste_m $taste_f $weight $ratings $choice $age $who $hours_m $hours_f $income_h rsex $leavebehind $prob $expect
order $id $taste_m $taste_f $weight $ratings $choice $age $who $hours_m $hours_f $income_h rsex $leavebehind $prob $expect

foreach var of varlist $id $taste_m $taste_f $weight $ratings $choice $age $who $hours_m $hours_f $income_h $prob $leavebehind  {
	drop if missing(`var')
}

*drop if rage_mod > 61

*drop if rage_mod < 40


save temp/hrs_unordered.dta, replace

di _N

* reorder so r is always husband and s always wife

* map r to husband, s to wife
keep if rsex==1
gen ssex=0
sort $id
order $id $taste_m $taste_f $weight $ratings $choice $age $who $hours_m $hours_f $income_h rsex ssex $prob $leavebehind $expect
save "temp/hrs_husband.dta", replace

use "temp/hrs_unordered.dta", clear
keep if rsex==0
gen ssex=1
renpfix s k
renpfix r s
renpfix k r
sort $id
order $id $taste_m $taste_f $weight $ratings $choice $age $who $hours_m $hours_f $income_h rsex ssex $prob $leavebehind $expect
save "temp/hrs_wives.dta", replace

capture drop _merge
append using "temp/hrs_husband.dta"
save "temp/hrs_ordered.dta", replace

di _N

drop rsex ssex
label var hhidpn "unique id of record"
label var rcol "husband has college education (binary)"
label var scol "wife has college education (binary)"
label var wageratio "ratio of husband's wage to wife's wage, min(w,5)"
label var rage_mod "age of husband in years from module"
label var sage_mod "age of wife in years from module"
label var hwho "who answered question 1=husband, 0=wife"
label var rhours "hours worked husband"
label var shours "hours worked wife"
label var rwage "wage husband"
label var swage "wage wife"
label var rliv75r "relative survival to age 75 (husband)"
label var sliv75r "relative survival to age 75 (wife)"
label var rhlth62 "probability health limits work after age 62 (husband)"
label var shlth62 "probability health limits work after age 62 (wife)"
label var rlim "health limits work of husband"
label var slim "health limits work of wife"
forvalues j = 1/3 {
	gen  same_`j' = (hchoice_scn`j'_r==hchoice_scn`j'_s&hchoice_scn`j'_s==hchoice_scn`j'_h)
}
egen countsame = anycount(same_*), values(1)
*drop if countsame==3
drop same_* countsame*

* drop rating 6
drop *scn6_* rrate_scn6 srate_scn6
replace hhidpn = _n

save "data/hrs_final_$scn.dta", replace

drop $expect

* dimensions for fortran

global nobs _N
global nquestions 8
global nratings 5
global nchoice 3
global k_taste_m = wordcount("$taste_m")
global k_taste_f = wordcount("$taste_f")
global k_weight = wordcount("$weight")
global nvar c(k)


capture file close info
file open info using "data/info_$scn.dat", write text replace
file write info "Information on sp HRS dataset produced $S_DATE $S_TIME for scenario $scn" _n
file write info ($nquestions) "	" ($nratings) "	" ($nchoice) " " ($nobs) " " ($k_taste_m) "	" ($k_taste_f) " " ($k_weight) " " ($nvar) _n
file close info


global vars "age hlim college male job"  

file open vars using "data/varlist_$scn.dat", write text replace
file write vars  "List of variables used in model $S_DATE $S_TIME for scenario $scn" _n
foreach var in $vars {
    file write vars ("`var'")  _n
}
file close vars

* same file for output
outsheet using "data/hrs_final_$scn.csv", comma replace  nolabel nonames

capture erase temp/hrs_ordered.dta
capture erase temp/hrs_unordered.dta
capture erase temp/hrs_husband.dta
capture erase temp/hrs_wives.dta

capture log close

exit
