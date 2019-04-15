
capture log close
set more off

capture cd "~/cedia/Projets/joint"
capture cd "~/dropbox/joint"

log using tex/tables/main.txt, text replace

cd runtime
! ./estimates.sh 
cd ..

capture log close
