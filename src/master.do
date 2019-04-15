capture log close
set more off

capture cd "~/cedia/Projets/joint"
capture cd "~/dropbox/joint"

* prepare data
do src/prepare.do
* sample statistics
do src/sample.do
* choice table
do src/choices.do
* main estimates
do src/main.do
* simulations
do src/simulation.do