*********************************************************************
**** Eileen Xia
**** Ec20
*********************************************************************

cd /Users/eileenxia/Desktop/Ec20 // tell Stata where your master folder is
clear // empty the working memory
use "./Data/s19_final_data_XIA.dta" // pull in the dataset name
save "./Work/final.dta", replace

*********************************************************************
**** Specifications
*********************************************************************

//categorical variable period to indicate pre, during, and post treatment years
gen period = 0
replace period = 1 if year <= 2006 //before treatment
replace period = 2 if year >= 2009 //during treatment
replace period = 3 if year == 2007 | year == 2008 //after treatment


*********************************************************************
**** Data
*********************************************************************

//Initial summary of the untouched data
estpost summarize id year program educ county age employ heart er
esttab using "./Output/summ_initial.tex", replace ///
	cells("count mean Var sd min max") nomtitle nonumber
	
//60,372 observations. Missing values for educ, age, employ, heart

//if missing eduction, replace with correct value using surrounding data
gen educfull = educ
replace educfull = educ[_n-1] if educ == . & year != 2004
replace educfull = educfull[_n-1] if educ == . & year != 2004
replace educfull = educ[_n+1] if educ == . & year == 2004
summ educfull

//if missing age, replace with correct value using surrounding data
gen agefull = age
replace agefull = age[_n+3]-3 if agefull == . & year == 2004 & age[_n+3]!= .
replace agefull = age[_n+1]-1 if agefull == . & year == 2004 & age[_n+1]!= .
replace agefull = age[_n-1]+1 if agefull == . & year != 2004 & age[_n-1] != .
replace agefull = agefull[_n-1]+1 if agefull == . & year != 2004 & agefull[_n-1] != .
summ agefull

//replace heart values of 9999 as missing
replace heart = . if heart == 9999

//Summary of fixed data
estpost summarize educfull agefull employ heart er 
esttab using "./Output/summ_final.tex", replace ///
	cells("count mean Var sd min max") nomtitle nonumber
	
	
*********************************************************************
**** Heart Regressions
*********************************************************************

//baseline specification
quiet reg heart i.program##i.period, robust
estimates store heartreg1

//T-tests, two-sided significance tests to obtain p-values of coefficients of interest
test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

//controlling for age
quiet reg heart i.program##i.period i.agefull, robust
estimates store heartreg2

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

//F test for age control to compare full and restricted models; how much does age explain
testparm i.agefull

//controlling for age and education
quiet reg heart i.program##i.period i.agefull i.educfull, robust 
estimates store heartreg3

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

//F test for age and education controls to compare full and restricted models
testparm i.agefull i.educfull

//controlling for age, education, and county
quiet reg heart i.program##i.period i.agefull i.educfull i.county, robust 
estimates store heartreg4

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

//F test for all controls to compare full and restricted models; how much explanatory power do we lose
testparm i.agefull i.educfull i.county

esttab heartreg1 heartreg2 heartreg3 heartreg4 ///
	using "./Output/heart_reg.tex", replace /// writes a file you can open in Latex
	keep(1.program 2.period 3.period 1.program#2.period 1.program#3.period) ///
	coeflabels(1.program "Program dummy" 2.period "During treatment" 3.period "After treatment" 1.program#2.period "Program dummy*During treat" 1.program#3.period "Program dummy*After treat") ///
	indicate("Controls for age = *agefull*" "Controls for education = *educfull*" "Controls for county = *county*") ///
	star(* 0.10 ** 0.05 *** 0.01) ///set the significance stars
	se(4) b(4) /// show standard errors, not t-statistics
	title("Dependent variable: Heart") /// put name of dependent variable above table//
	scalars("tpvalue1 p-value 1.program=0" "tpvalue2 p-value 2.period=0" "tpvalue3 p-value 3.period=0" "tpvalue4 p-value 1.program#2.period=0" "tpvalue5 p-value 1.program#2.period=0") ///
	r2 ar2 /// add the R-squared and adjusted R-squared to the table
	interaction(" $\times$ ") /// this is just for when using Latex
	
	
	
*********************************************************************
**** ER Regressions
*********************************************************************


quiet reg er i.program##i.period, robust
estimates store erreg1

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

quiet reg er i.program##i.period i.agefull, robust //age controls 
estimates store erreg2

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

testparm i.agefull

quiet reg er i.program##i.period i.agefull i.educfull, robust //age and education controls
estimates store erreg3

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

testparm i.agefull i.educfull

quiet reg er i.program##i.period i.agefull i.educfull i.county, robust //age, education, and county controls
estimates store erreg4

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

testparm i.agefull i.educfull i.county

esttab erreg1 erreg2 erreg3 erreg4 ///
	using "./Output/er_reg.tex", replace /// writes a file you can open in Latex
	keep(1.program 2.period 3.period 1.program#2.period 1.program#3.period) ///
	coeflabels(1.program "Program dummy" 2.period "During treatment" 3.period "After treatment" 1.program#2.period "Program dummy*During treat" 1.program#3.period "Program dummy*After treat") ///
	indicate("Controls for age = *agefull*" "Controls for education = *educfull*" "Controls for county = *county*") ///
	star(* 0.10 ** 0.05 *** 0.01) ///set the significance stars
	se(4) b(4) /// show standard errors, not t-statistics
	title("Dependent variable: Er") /// put name of dependent variable above table//
	scalars("tpvalue1 p-value 1.program=0" "tpvalue2 p-value 2.period=0" "tpvalue3 p-value 3.period=0" "tpvalue4 p-value 1.program#2.period=0" "tpvalue5 p-value 1.program#2.period=0") ///
	r2 ar2 /// add the R-squared and adjusted R-squared to the table
	interaction(" $\times$ ") /// this is just for when using Latex
	
	
	
*********************************************************************
**** Employ Regressions
*********************************************************************


quiet reg employ i.program##i.period, robust
estimates store employreg1

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

quiet reg employ i.program##i.period i.agefull, robust //age controls 
estimates store employreg2

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

testparm i.agefull

quiet reg employ i.program##i.period i.agefull i.educfull, robust //age and education controls
estimates store employreg3

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

testparm i.agefull i.educfull


quiet reg employ i.program##i.period i.agefull i.educfull i.county, robust //age, education, and county controls
estimates store employreg4

test 1.program = 0
estadd scalar tpvalue1 = r(p) // save the p-value from the simple t-test
test 2.period = 0
estadd scalar tpvalue2 = r(p) // save the p-value from the simple t-test
test 3.period = 0
estadd scalar tpvalue3 = r(p) // save the p-value from the simple t-test
test 1.program#2.period = 0
estadd scalar tpvalue4 = r(p) // save the p-value from the simple t-test
test 1.program#3.period = 0
estadd scalar tpvalue5 = r(p) // save the p-value from the simple t-test

testparm i.agefull i.educfull i.county

esttab employreg1 employreg2 employreg3 employreg4 ///
	using "./Output/employ_reg.tex", replace /// writes a file you can open in Latex
	keep(1.program 2.period 3.period 1.program#2.period 1.program#3.period) ///
	coeflabels(1.program "Program dummy" 2.period "During treatment" 3.period "After treatment" 1.program#2.period "Program dummy*During treat" 1.program#3.period "Program dummy*After treat") ///
	indicate("Controls for age = *agefull*" "Controls for education = *educfull*" "Controls for county = *county*") ///
	star(* 0.10 ** 0.05 *** 0.01) ///set the significance stars
	se(4) b(4) /// show standard errors, not t-statistics
	title("Dependent variable: Employ") /// put name of dependent variable above table//
	scalars("tpvalue1 p-value 1.program=0" "tpvalue2 p-value 2.period=0" "tpvalue3 p-value 3.period=0" "tpvalue4 p-value 1.program#2.period=0" "tpvalue5 p-value 1.program#2.period=0") ///
	r2 ar2 /// add the R-squared and adjusted R-squared to the table
	interaction(" $\times$ ") /// this is just for when using Latex
	
*********************************************************************
**** Year by Year Regressions Plotted
*********************************************************************
//make year a categorical variable
gen yearfixed = year
replace yearfixed =1 if year==2004
replace yearfixed =2 if year==2005
replace yearfixed =3 if year==2006
replace yearfixed =4 if year==2007
replace yearfixed =5 if year==2008
replace yearfixed =6 if year==2009
replace yearfixed =7 if year==2010
replace yearfixed =8 if year==2011
replace yearfixed =9 if year==2012

**** Heart ****
//Run a regression and plot just the estimated fixed effects for year
quiet reg heart i.program##i.yearfixed i.agefull i.educfull i.county
estimates store reg1
coefplot reg1, keep(?.program#?.yearfixed) ///
	graphregion(color(white)) ///
	vertical ///
	title("Effect on Heart Health") ///
	ytitle("Heart Health") ///
	xtitle("Program Dummy x Year") ///
	rename(^1.program#([0-9]+).yearfixed$ = \1, regex) ///
	mlabel format(%9.2f) mlabposition(12) mlabsize(medium) ///
	xlabel(0 "2004" 1 "2005" 2 "2006" 3 "2007" 4 "2008" 5 "2009" 6 "2010" 7 "2011" 8 "2012") //
graph export "./Output/fig_year_heart.png", replace 


**** ER ****
//Run a regression and plot just the estimated fixed effects for year
quiet reg er i.program##i.yearfixed i.agefull i.educfull i.county
estimates store reg2
coefplot reg2, keep(?.program#?.yearfixed) ///
	graphregion(color(white)) ///
	vertical ///
	title("Effect on Trips to the ER") ///
	ytitle("Trips to the ER") ///
	xtitle("Program Dummy x Year") ///
	rename(^1.program#([0-9]+).yearfixed$ = \1, regex) ///
	mlabel format(%9.2f) mlabposition(12) mlabsize(medium) ///	
	xlabel(0 "2004" 1 "2005" 2 "2006" 3 "2007" 4 "2008" 5 "2009" 6 "2010" 7 "2011" 8 "2012") //
graph export "./Output/fig_year_er.png", replace

**** Employ ****
//Run a regression and plot just the estimated fixed effects for year
quiet reg employ i.program##i.yearfixed i.agefull i.educfull i.county
estimates store reg3
coefplot reg3, keep(?.program#?.yearfixed) ///
	graphregion(color(white)) ///
	vertical ///
	title("Effect on Weeks of Employment") ///
	ytitle("Weeks of Employment") ///
	xtitle("Program Dummy x Year") ///
	rename(^1.program#([0-9]+).yearfixed$ = \1, regex) ///
	mlabel format(%9.2f) mlabposition(12) mlabsize(medium) ///
	xlabel(0 "2004" 1 "2005" 2 "2006" 3 "2007" 4 "2008" 5 "2009" 6 "2010" 7 "2011" 8 "2012") //
graph export "./Output/fig_year_employ.png", replace

