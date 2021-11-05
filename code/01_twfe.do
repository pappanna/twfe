**************************************************************
** TWFE / DiD Literature Exploration
** Based on Goodman-Bacon (2021), Sun and Abraham (2020), Callaway and Sant'Anna (2021), Borusyak et al. (2021), Wooldridge (2021)
** Other coding sources: 
**** https://lost-stats.github.io/Model_Estimation/Research_Design/event_study.html
**** https://www.stata.com/meeting/us21/slides/US21_SantAnna.pdf
**** https://github.com/borusyak/did_imputation/blob/main/five_estimators_example.do
**************************************************************
** ap3907@columbia.edu - November 2021
** Last modified: 11/02/21
**************************************************************

************* 00 - SETUP *************************************

cd "/Users/annapapp/Desktop/twfe/"

* scheme
set scheme plotplain

* need to have the following packages installed:
/*
ssc install reghdfe
ssc install did_imputation 
ssc install event_plot
ssc install csdid 
ssc install eventstudyinteract 
ssc install bacondecomp 
*/

* select data to run: 
*** 0 = Wooldridge Simulation
*** 1 = Stevenson and Wolfers 2006
*** 2 = Cheng and Hoekstra 2013
*** 3 = other (define variables)
local datatorun = 1

****** set file name and label for graphs *****
if `datatorun' == 0{
	* names for Wooldridge Simulation data
	local filename wooldridgeexample
	local filenametext "Wooldridge Simulation Data"
}
if `datatorun' == 1{
	* names for Stevenson and Wolfers 2006
	local filename stevensonwolfers2006
	local filenametext "Stevenson and Wolfers (2006)"
}
if `datatorun' == 2{
	* names for Cheng and Hoekstra 2013
	local filename chenghoekstra2013
	local filenametext "Cheng and Hoekstra (2013)"
}
if `datatorun' == 3{
	* names for other data
	local filename 
	local filenametext ""
}

***** set variable names *****

if `datatorun' == 0{
	* core variables 
	local outcomevar logy
	local postvar w
	local timevar year
	local areavar id
	local timetreatvar effyear
	local othercontrolvars 
	* weights
	local weightvarind 0
	local weightvar 
	if `weightvarind' == 1 {
		local weights = "[aweight = `weightvar']"
	}
	if `weightvarind' == 0 {
		local weights 
	}
}
if `datatorun' == 1{
	* core variables 
	local outcomevar asmrs
	local postvar post
	local timevar year
	local areavar stfips 
	local timetreatvar _nfd
	local othercontrolvars 
	* weights
	local weightvarind 0
	local weightvar weight
	if `weightvarind' == 1 {
		local weights = "[aweight = `weightvar']"
	}
	if `weightvarind' == 0 {
		local weights 
	}
}
if `datatorun' == 2{
	* core variables 
	local outcomevar l_homicide
	local postvar post
	local timevar year
	local areavar sid
	local timetreatvar effyear
	local othercontrolvars 
	* weights
	local weightvarind 1
	local weightvar popwt
	if `weightvarind' == 1 {
		local weights = "[aweight = `weightvar']"
		local weightscs = "[weight = `weightvar']"
	}
	if `weightvarind' == 0 {
		local weights 
		local weightscs 
	}
}
if `datatorun' == 3{
	* core variables 
	local outcomevar
	local postvar 
	local timevar 
	local areavar 
	local timetreatvar 
	local othercontrolvars 
	* weights
	local weightvarind 0
	local weightvar 
	if `weightvarind' == 1 {
		local weights = "[aweight = `weightvar']"
		local weightscs = "[weight = `weightvar']"
	}
	if `weightvarind' == 0 {
		local weights 
		local weightscs 
	}
}

***** graph formating *****
if `datatorun' == 0{
	* x-axis 
	local pretrendmin -3
	local posttrendmax 3
	local xaxisstep 1
	local pretrendminnonneg = - `pretrendmin'
	* y-axis 
	local ymin -0.3
	local ymax 0.3
	local yaxisstep 0.05
}
if `datatorun' == 1{
	* x-axis 
	local pretrendmin -8
	local posttrendmax 17
	local xaxisstep 4
	local pretrendminnonneg = - `pretrendmin'
	* y-axis 
	local ymin -20
	local ymax 10
	local yaxisstep 5
}
if `datatorun' == 2{
	* x-axis 
	local pretrendmin -9
	local posttrendmax 5
	local xaxisstep 2
	local pretrendminnonneg = - `pretrendmin'
	* y-axis 
	local ymin -0.5
	local ymax 0.5
	local yaxisstep 0.1
}
if `datatorun' == 3{
	* x-axis 
	local pretrendmin 
	local posttrendmax 
	local xaxisstep 
	local pretrendminnonneg = - `pretrendmin'
	* y-axis 
	local ymin 
	local ymax 
	local yaxisstep 
}

**************************************************************
************* 01 - TWFE **************************************

use "data/`filename'.dta", clear

* run fixed effects regression, method 1
eststo clear
quietly{ 
	eststo: reg `outcomevar' `postvar' i.`timevar' i.`areavar' `othercontrolvars' `weights', r cluster(`areavar')
}
esttab, cells(b(star fmt(3) label(Coef.)) se(par label(Std. err.) fmt(3)) ) stats(r2 N, labels(R-squared "Observations") fmt(3 0)) label title("`filenametext': TWFE Example") drop(_cons *.`timevar' *.`areavar') legend replace

* save estimates 
local twfe_b = _b[`postvar']
local twfe_se = _se[`postvar']

* set panel 
xtset `areavar' `timevar' 

* run fixed effects regression, method 2
reghdfe `outcomevar' `postvar' `othercontrolvars' `weights', a(`areavar' `timevar') vce(cluster `areavar')

**************************************************************
************* 02 - BACON DECOMPOSITION ***********************

use "data/`filename'.dta", clear

* set panel 
xtset `areavar' `timevar' 

* run fixed effects regression, method 2
reghdfe `outcomevar' `postvar' `othercontrolvars' `weights', a(`areavar' `timevar') vce(cluster `areavar')

* decomposition, note it doesn't work with no control variables and weights
bacondecomp `outcomevar' `postvar', ddetail legend(pos(6)) title("`filenametext': Bacon Decomposition")
graph export "output/`filename'_bacondecomp.png", replace

* save separate betas and weights 
local baconestimates e l u a 
foreach baconestimate of local baconestimates{
	local bacon_b_`baconestimate' = e(dd_avg_`baconestimate')
	local bacon_wt_`baconestimate' = e(wt_sum_`baconestimate')
	display `bacon_b_`baconestimate''
	display `bacon_wt_`baconestimate''
}

**************************************************************
************* 03 - EVENT STUDY *******************************

use "data/`filename'.dta", clear

* generate time to treat variables 
gen time = `timevar' - `timetreatvar'
replace time = 0 if missing(`timetreatvar')
gen treat = !missing(`timetreatvar')

* shift due to negative values 
summ time
gen shift = time - r(min)
summ shift if time == -1
local negative1 = r(mean)

* event study regression 
reghdfe `outcomevar' ib`negative1'.shift `weights', a(`areavar' `timevar') vce(cluster `areavar')

* get coefficients and standard errors 
gen coef = .
gen se = .
levelsof shift, l(times)
foreach t in `times' {
	replace coef = _b[`t'.shift] if shift == `t'
	replace se = _se[`t'.shift] if shift == `t'
}

* calculate confidence intervals
g ci_top = coef+1.96*se
g ci_bottom = coef - 1.96*se

* keep one observation for each time period 
keep time coef se ci_*
duplicates drop
sort time

* keep only relevant time periods here 
keep if time >= `pretrendmin' & time < `posttrendmax'

* plot results 
twoway (sc coef time, mcolor(black) msymbol(i) msize(1) lcolor(black)  connect(direct)) ///
	(sc ci_top time, mcolor(gray) msymbol(i) msize(1) lcolor(gray) lpattern(dash) connect(direct)) ///
	(sc ci_bottom time, mcolor(gray) msymbol(i) msize(1) lcolor(gray) lpattern(dash) connect(direct)), ///
	xlabel(`pretrendmin'(`xaxisstep')`posttrendmax') ylabel(`ymin'(`yaxisstep')`ymax') title("`filenametext': Event-Study Estimates") xtitle("Time to Treatment") ytitle("Outcome") yline(0, lpattern(dot)) yline(`twfe_b', lcolor(red) lpattern(dot)) xline(-1, lpattern(dot)) legend(off)
graph export "output/`filename'_eventstudy.png", replace

* calculate average post-treatment effect 
keep if time >= 0
sum coef
local eventstudyavg = r(mean)
display `eventstudyavg'

**************************************************************
************* 04 - SUN AND ABRAHAM (2020) ********************
***** something is not working here with this implementation? not sure 

use "data/`filename'.dta", clear

* generate time to treat variables 
gen time = `timevar' - `timetreatvar'
replace time = 0 if missing(`timetreatvar')
gen treat = !missing(`timetreatvar')
gen nevertreated = missing(`timetreatvar')

* create relative time indicators 

forvalues t = `pretrendmin'(1)`posttrendmax'{
	if `t' < -1 {
		local tname = abs(`t')
		gen var_neg`tname' = time == `t'
	}
	else if `t' >= 0 {
		gen var_pos`t' = time == `t'
	}
}

* run event study estimation 
eventstudyinteract `outcomevar' var_* `weights', cohort(`timetreatvar') control_cohort(nevertreated) absorb(i.`areavar' i.`timevar') vce(cluster `areavar')

* get coefficients and standard errors from matrix
matrix T = r(table)
g coef = 0 if time == -1
g se = 0 if time == -1
forvalues t = `pretrendmin'(1)`posttrendmax' {
	if `t' < -1 {
		local tname = abs(`t')
		replace coef = T[1,colnumb(T,"var_neg`tname'")] if time == `t'
		replace se = T[2,colnumb(T,"var_neg`tname'")] if time == `t'
	}
	else if `t' >= 0 {
		replace coef =  T[1,colnumb(T,"var_pos`t'")] if time == `t'
		replace se = T[2,colnumb(T,"var_pos`t'")] if time == `t'
	}
}

* make confidence intervals
g ci_top = coef+1.96*se
g ci_bottom = coef - 1.96*se

* keep relevant 
keep time coef se ci_*
duplicates drop
sort time 

* keep only relevant time periods here 
keep if time >= `pretrendmin' & time < `posttrendmax'

* plot results 
twoway (sc coef time, mcolor(black) msymbol(i) msize(1) lcolor(black)  connect(direct)) ///
	(sc ci_top time, mcolor(gray) msymbol(i) msize(1) lcolor(gray) lpattern(dash) connect(direct)) ///
	(sc ci_bottom time, mcolor(gray) msymbol(i) msize(1) lcolor(gray) lpattern(dash) connect(direct)), ///
	xlabel(`pretrendmin'(`xaxisstep')`posttrendmax') ylabel(`ymin'(`yaxisstep')`ymax') title("`filenametext': Sun and Abraham (2020) Event-Study Estimates") xtitle("Time to Treatment") ytitle("Outcome") yline(0, lpattern(dot)) yline(`twfe_b', lcolor(red) lpattern(dot)) xline(-1, lpattern(dot)) legend(off)
graph export "output/`filename'_eventstudy_sunabraham.png", replace

**************************************************************
************* 05 - CALLAWAY AND SANT'ANNA (2020) *************

use "data/`filename'.dta", clear

* set panel 
xtset `areavar' `timevar' 

* generate group variable, should be 0 where not treated
gen group = `timetreatvar'
replace group = 0 if `timetreatvar' == .

* using never-treated as the control group 
csdid `outcomevar' `weightscs', ivar(`areavar') time(`timevar') gvar(group) method(drimp) wboot
estat simple, estore(simple)
matrix bmatrix = r(b)
matrix Vmatrix = r(V)
local cs_never_b = bmatrix[1,1]
display `cs_never_b'
local cs_never_se = sqrt(Vmatrix[1,1])
display `cs_never_se'

* using not-yet-treated as the control group 
csdid `outcomevar' `weightscs', ivar(`areavar') time(`timevar') gvar(group) method(drimp) wboot notyet
estat simple, estore(simplenotyet)
matrix bmatrix = r(b)
matrix Vmatrix = r(V)
local cs_notyet_b = bmatrix[1,1]
display `cs_notyet_b'
local cs_notyet_se = sqrt(Vmatrix[1,1])
display `cs_notyet_se'

* plot 
csdid `outcomevar' `weightscs', ivar(`areavar') time(`timevar') gvar(group) method(drimp) wboot notyet
estat event, estore(cs)
event_plot cs, default_look graph_opt(xtitle("Periods Since the Event") ytitle("Average Causal Effect") xlabel(`pretrendmin'(`xaxisstep')`posttrendmax') ylabel(`ymin'(`yaxisstep')`ymax') ///
	title("`filenametext': Callaway and Sant'Anna (2020)")) stub_lag(T+#) stub_lead(T-#) together trimlag(`posttrendmax') trimlead(`pretrendminnonneg')
graph export "output/`filename'_eventstudy_cs.png", replace

**************************************************************
************* 06 - BORUSYAK ET AL (2021) *********************

use "data/`filename'.dta", clear

did_imputation `outcomevar' `areavar' `timevar' `timetreatvar' `weights'
local borusyak_b = _b[tau]
display `borusyak_b'
local borusyak_se = _se[tau]
display `borusyak_se'

did_imputation `outcomevar' `areavar' `timevar' `timetreatvar' `weights', allhorizons pretrend(`pretrendminnonneg') minn(0)
event_plot, default_look graph_opt(xtitle("Periods Since the Event") ytitle("Average Causal Effect") ///
	title("`filenametext': Borusyak et al. (2021) Imputation Estimator") xlabel(`pretrendmin'(`xaxisstep')`posttrendmax') ylabel(`ymin'(`yaxisstep')`ymax')) trimlag(`posttrendmax') trimlead(`pretrendminnonneg')
graph export "output/`filename'_eventstudy_borusyak.png", replace


**************************************************************
************* 07 - WOOLDRIDGE (2021) *************************

use "data/`filename'.dta", clear

xtset `areavar' `timevar' 

* drop those who were treated for entire time period??
drop if `timetreatvar' == . & `postvar' == 1

sum `timevar'
local timemin = r(min)
local timemax = r(max)
local timediff = `timemax' - `timemin' + 1

* generate dummy variables for each year, f
forvalues t = `timemin'(1)`timemax'{
	gen f`t' = `timevar' == `t'
}

* generate treatment timing indicators, d
* these are the treatment cohorts 
egen wsum = sum(`postvar'), by(`areavar')
levelsof `timetreatvar', l(dgroups)
foreach d in `dgroups'{
	gen d`d' = `timemax' - wsum + 1 == `d'
}

* create list of variables we need for the regression
local regressionlist = ""
levelsof `timetreatvar', l(dgroups)
foreach d in `dgroups'{
	
	forvalues t = `timemin'(1)`timemax'{
		
		* here we could restrict more, e.g. only look x years after, etc.
		if `t' >= `d' {
			local regressionlist `" `regressionlist' c.d`d'#c.f`t' "'
		}
		
	}
	
}
* double check regression list
display "`regressionlist'"

* run regression 
eststo clear
eststo: xtreg `outcomevar' `regressionlist' i.`timevar', fe vce(cluster `areavar')
esttab using "output/`filename'_wooldridge.txt", cells(b(star fmt(3) label(Coef.)) se(par label(Std. err.) fmt(3)) ) stats(r2 N, labels(R-squared "Observations") fmt(3 0)) label title("`filenametext': Wooldridge ETWFE Regression") drop(_cons *.`timevar') legend replace
* get matrix of all the relevant coefficients 
matrix M = e(b)
local columns = `=colsof(M)' - `timediff' - 1
matrix MX = M[1..1,1..`columns']
* take average of all group-time coefficients 
* note: this is NOT the right way, just very quick summary for now 
mata
column = strtoreal(st_local("columns"))
M = st_matrix("MX")
MT = M'
x = colsum(MT) :/ colnonmissing(MT)
st_local("wooldridge_b", strofreal(x))
end


**************************************************************
************* 08 - SUMMARIZE AND SAVE ************************

* this section saves all the estimates from above to a csv file for a quick comparison 
local localnames twfe_b twfe_se bacon_b_e bacon_wt_e bacon_b_l bacon_wt_l bacon_b_u bacon_wt_u bacon_b_a bacon_wt_a eventstudyavg cs_never_b cs_never_se cs_notyet_b cs_notyet_se borusyak_b borusyak_se wooldridge_b

foreach localname of local localnames{
	gen `localname' = ``localname''
}

keep if _n == 1
keep `localnames'

expand 4

gen TWFE = . 
replace TWFE = twfe_b[1] if _n == 1
replace TWFE = twfe_se[1] if _n == 2

gen BACON = . 
replace BACON = bacon_b_e[1] if _n == 1
replace BACON = bacon_b_l[1] if _n == 2
replace BACON = bacon_b_u[1] if _n == 3
replace BACON = bacon_b_a[1] if _n == 4

gen BACONWT = . 
replace BACONWT = bacon_wt_e[1] if _n == 1
replace BACONWT = bacon_wt_l[1] if _n == 2
replace BACONWT = bacon_wt_u[1] if _n == 3
replace BACONWT = bacon_wt_a[1] if _n == 4

gen EVENTSTUDY = . 
replace EVENTSTUDY = eventstudyavg[1] if _n == 1

gen CSNEVER = . 
replace CSNEVER = cs_never_b[1] if _n == 1
replace CSNEVER = cs_never_se[1] if _n == 2

gen CSNOTYET = . 
replace CSNOTYET = cs_notyet_b[1] if _n == 1
replace CSNOTYET = cs_notyet_se[1] if _n == 2

gen BORUSYAK = . 
replace BORUSYAK = borusyak_b[1] if _n == 1
replace BORUSYAK = borusyak_se[1] if _n == 2

gen WOOLDRIDGE = . 
replace WOOLDRIDGE = wooldridge_b[1] if _n == 1

keep TWFE - WOOLDRIDGE

* save 
export delimited "output/`filename'_results.csv", replace
