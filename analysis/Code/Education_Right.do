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
* 2: placebo

local spec = 2

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


import delim "../../../Analysis/Output/graphs/placebo_coeffs.csv", clear	

ren (v1 v2 v3) (outcome b se)	

sort b
drop nn
gen nn = _n


local max5 = _N - 5	
su b if nn == 5
local b5 = string(r(mean),"%9.3f")
su b if nn == 50
local b50 = string(r(mean),"%9.3f")
su b if nn == `max5'
local b95 = string(r(mean),"%9.3f")
graph twoway (scatter b nn ,color(black)) ///
	, ytitle("") xtitle(" ") xline(5 50 `max5') yline(1.889, lcolor(grey)) ///
					graphregion(fcolor(white) ilcolor(white) lcolor(white)) /// 
					legend(label(1 "Coefficient")) ///
					title("by countour") ///
					note("5%: `b5', 50%: `b50' , 95%: `b95', rank of main coefficient (0.375) = 87th ")
					
	graph export "../../../Analysis/Output/graphs/placebo_contour_coeffs.png", replace
	
}
