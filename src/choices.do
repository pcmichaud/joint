clear all
capture log close
set more off
capture cd "~/cedia/Projets/joint"
capture cd "~/dropbox/joint"
log using tex/tables/choices.txt, text replace

global scn "ref"
use "data/hrs_final_$scn.dta", clear

matrix tab = J(9,6,.)
local n = _N
local nq = 3
local ns = 3
qui sum hwho_m if hwho_m==1
local nm = r(N)
qui sum hwho_f if hwho_f==1
local nf = r(N)
di "number of husbands who are respondents: ", `nm'
di "number of wives who are respondents: ", `nf'
di "total number of respondents: ", `n'
forvalues q = 1/`nq' {
	local ss = 1
	foreach s in "r" "s" "h" {
		tab hchoice_scn`q'_`s' if hwho_m==1, matcell(qs)
		matrix qs = qs'/`nm'
		matrix tab[(`q'-1)*`ns' + `ss',1] = qs
		tab hchoice_scn`q'_`s' if hwho_f==1, matcell(qs)
		matrix qs = qs'/`nf'
		matrix tab[(`q'-1)*`ns' + `ss',4] = qs
		local ss = `ss' + 1
	}
}

matrix rownames tab = "husband - 2vs4" "wife - 2vs4" "both - 2vs4" "husband - 5vs2" "wife - 5vs2" "both - 5vs2" "husband - 3vs6" "wife - 3vs6" "both - 3vs6"
matrix colnames tab = "1st (husband)" "2nd (husband)" "Skipped (husband)" "1st (wives)" "2nd (wives)" "Skipped (wives)"



global vlist "age male income"
global labnames "Age Gender "Household Income""

local i = 1
file open table using "tex/tables/choices.tex", write replace text
file write table "\begin{tabular}{lrrrrrr} " _n
file write table "\hline \hline " _n
file write table " & \multicolumn{3}{c}{Males} & \multicolumn{3}{c}{Females} \\" _n
file write table "\hline" _n
file write table "Made for & Scenario 2 & Scenario 4 & Skipped & Scenario 2 & Scenario 4 & Skipped \\" _n


global labnames = "Males Females Both"
local i = 1
forvalues j = 1/3 {
local lab : word `i' of $labnames
#d ;
file write table " `lab' & " %7.3f (tab[`j',1]) " & "  %7.3f (tab[`j',2]) " & " %7.3f (tab[`j',3])  
		" & " %7.3f (tab[`j',4]) " & "  %7.3f (tab[`j',5]) " & " %7.3f (tab[`j',6]) " \\ " _n;
#d cr
local i = `i'+1
}
file write table " & Scenario 5 & Scenario 2 & Skipped & Scenario 5 & Scenario 2 & Skipped \\" _n
local i = 1
forvalues j = 4/6 {
local lab : word `i' of $labnames
#d ;
file write table " `lab' & " %7.3f (tab[`j',1]) " & "  %7.3f (tab[`j',2]) " & " %7.3f (tab[`j',3])  
		" & " %7.3f (tab[`j',4]) " & "  %7.3f (tab[`j',5]) " & " %7.3f (tab[`j',6]) " \\ " _n;
#d cr
local i = `i'+1
}
file write table " & Scenario 3 & Scenario 6 & Skipped & Scenario 3 & Scenario 6 & Skipped \\" _n
local i = 1
forvalues j = 7/9 {
local lab : word `i' of $labnames
#d ;
file write table " `lab' & " %7.3f (tab[`j',1]) " & "  %7.3f (tab[`j',2]) " & " %7.3f (tab[`j',3])  
		" & " %7.3f (tab[`j',4]) " & "  %7.3f (tab[`j',5]) " & " %7.3f (tab[`j',6]) " \\ " _n;
#d cr
local i = `i'+1
}


// foreach var of varlist $vlist {
// local lab : word `i' of $labnames
// sum `var'
// di "`lab'"
// #d ;
// file write table "`lab'"  " & " %7.3f (r(mean)) " & " %7.3f  (r(sd)) " & " 
// %7.3f (r(min)) " & " %7.3f (r(max)) " \\"  _n ;
// #d cr

//local ++i
//}
file write table "\hline \hline " _n
file write table "\end{tabular}" _n
file close table



matrix list tab

capture log close


