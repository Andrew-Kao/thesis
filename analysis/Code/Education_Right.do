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


