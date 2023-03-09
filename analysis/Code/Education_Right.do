/* ==================================================
// education regressions
// andrewjkao@gmail.com
// Updated: 10/16/2021
================================================== */ 

* quicker spatial


clear all
set maxvar  30000
set matsize 11000 
set more off
cap log close

if regexm(c(hostname), "ak") == 1 | c(username) == "AndrewKao" {
 global dir = "~/Documents/College/All/thesis/explore/Data/education"
} 

cd ${dir}

import delim "dda.csv", clear

gen inter = tv * eth

gen primary = (sch_grade_g01 == "Yes")
gen middle = (sch_grade_g06 == "Yes")
gen high = (sch_grade_g09 == "Yes")

destring sch_satact sch_mathenr_calc sch_appass_oneormore , force replace
encode leaid, gen(id)

gen ihs_satact = asinh(sch_satact)
gen ihs_calc = asinh(sch_mathenr_calc)
gen ihs_app = asinh(sch_appass_oneormore)

local outcomes "satact calc app"

*** spec guide 
* 1: baseline
* 2: placebo regs
* 3: placebo plot

local spec = 3

if `spec' == 1 {
foreach out in `outcomes' {
	acreg ihs_`out' inter tv eth hisp_students asian_students, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) 
	est sto a1
	
	acreg ihs_`out' inter tv eth hisp_students asian_students total_students sch_teachers_curr_tot, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) 
	est sto a2
	
	acreg ihs_`out' inter tv eth hisp_students asian_students total_students sch_teachers_curr_tot primary middle high, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) 
	est sto a3
	
	estout a* using "../../Output/Regs/edu_dda_`out'OLSIHS_spatial.tex", ///
replace style(tex) keep(inter) ///
order(inter) ///
 varlabels(inter "Hispanic dummy $\times$ TV")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label

acreg ihs_`out' inter tv eth hisp_students asian_students, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) bartlett
	est sto a1
	
	acreg ihs_`out' inter tv eth hisp_students asian_students total_students sch_teachers_curr_tot, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) bartlett
	est sto a2
	
	acreg ihs_`out' inter tv eth hisp_students asian_students total_students sch_teachers_curr_tot primary middle high, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) bartlett
	est sto a3
	
	estout a* using "../../Output/Regs/edu_dda_`out'OLSIHS_spatialbart.tex", ///
replace style(tex) keep(inter) ///
order(inter) ///
 varlabels(inter "Hispanic dummy $\times$ TV")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label
	
}


	
}
else if `spec' == 2 {


tempname fh 
file open `fh' using "../../../Analysis/Output/graphs/placebo_coeffs.csv", write text replace

	
forv i= 1/100 {
	di "`i'"
	cap drop placebo*
	replace tv = 0
	forv j = 1/200 {
		qui gen placebo_x_`j' = runiform(-124.848974,-66.885444) if _n == 1
		qui gen placebo_y_`j' = runiform(24.396308,49.384358) if _n == 1
		egen placebo_xc_`j' = mean(placebo_x_`j')
		egen placebo_yc_`j' = mean(placebo_y_`j')
		qui replace tv = 1 if abs(x - placebo_xc_`j') < 2 & abs(y - placebo_yc_`j') < 2
	}
	
	replace inter = tv * eth
	
	foreach out in `outcomes' {
		acreg ihs_`out' inter tv eth hisp_students asian_students, latitude(y) longitude(x) pfe1(id) spatial distcutoff(100) 
		local b = _b[inter]
		local se = _se[inter]
		file write `fh' "`out',`b',`se'" _newline
	}	
}	
file close `fh'
	
}
else if `spec' == 3 {
	
import delim "../../../Analysis/Output/graphs/placebo_coeffs.csv", clear	

ren (v1 v2 v3) (outcome b se)	

local os "satact calc app"

foreach o in `os' {

preserve 
keep if outcome == "`o'"

sort b
gen nn = _n

if "`o'" == "satact" {
	local ntext = "(0.1598), 100th"
}
else if "`o'" == "calc" {
	local ntext = "(0.2718), 77th"
}
else if "`o'" == "app" {
	local ntext = "(0.0964), 61st"
}

gen top = b + se*1.97
gen bot = b - se*1.97

graph twoway (scatter b nn if _n > 5 & _n < 95,color(black)) ///
		(scatter b nn if _n <= 5 | _n >= 95,color(red)) ///
		(rcap top bot nn) ///
	, ytitle("") xtitle(" ") ///
					graphregion(fcolor(white) ilcolor(white) lcolor(white)) /// 
					legend(label(1 "Coefficient") label(2 "10% tail") label(3 "95% SE")) ///
					title("Effect by countour") ///
					note("Rank of main coefficient `ntext' ")

	graph export "../../../Analysis/Output/graphs/placebo_contour_coeffs_`o'.png", replace

restore
}
}
