/* ==================================================
// safegraph regressions
// andrewjkao@gmail.com
// Updated: 10/16/2021
================================================== */ 

* because R won't let me have robust FEs


clear all
set maxvar  30000
set matsize 11000 
set more off
cap log close

if regexm(c(hostname), "ak") == 1 | c(username) == "AndrewKao" {
 global dir = "~/Documents/College/All/thesis/explore/Data/safegraph"
} 

cd ${dir}

import delim "POI/POI_pattern_clean.csv", clear


* data cleaning
gen ihs_v = asinh(visitors)
destring origpchisp origloginc origlogpop, force replace

gen hisp_inside = hispanic * inside
gen hisp_hfood = hispanic * hispanic_food
gen inside_hfood = inside * hispanic_food
gen hisp_inside_hfood = hisp_inside * hispanic_food


ivreghdfe ihs_v hispanic inside hispanic_food hisp_inside hisp_hfood inside_hfood hisp_inside_hfood origpchisp if sector == 72,  robust
est sto a1

ivreghdfe ihs_v hispanic inside hispanic_food hisp_inside hisp_hfood inside_hfood hisp_inside_hfood origpchisp origloginc if sector == 72,  robust
est sto a2

ivreghdfe ihs_v hispanic inside hispanic_food hisp_inside hisp_hfood inside_hfood hisp_inside_hfood origpchisp origloginc origlogpop if sector == 72, robust
est sto a3

ivreghdfe ihs_v hispanic inside hispanic_food hisp_inside hisp_hfood inside_hfood hisp_inside_hfood origpchisp origloginc origlogpop if sector == 72, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hispfood.tex", ///
replace style(tex) keep(hispanic inside hispanic_food hisp_inside hisp_hfood inside_hfood hisp_inside_hfood) ///
order(hisp_inside_hfood hisp_inside hisp_hfood inside_hfood hispanic inside hispanic_food) ///
 varlabels(hisp_inside_hfood "Hispanic $\times$ TV $\times$ Hispanic food" hisp_inside "Hispanic $\times$ TV" hisp_hfood "Hispanic $\times$ Hispanic food" inside_hfood "TV $\times$ Hispanic food" hispanic "Hispanic" inside "TV dummy" hispanic_food "Hispanic food")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label


gen hisp_hloc = hispanic * hispanic_loc
gen inside_hloc = inside * hispanic_loc
gen hisp_inside_hloc = hisp_inside * hispanic_loc


ivreghdfe ihs_v hispanic inside hispanic_loc hisp_inside hisp_hloc inside_hloc hisp_inside_hloc origpchisp  if sector == 71, robust
est sto a1

ivreghdfe ihs_v hispanic inside hispanic_loc hisp_inside hisp_hloc inside_hloc hisp_inside_hloc origpchisp origloginc if sector == 71, robust
est sto a2

ivreghdfe ihs_v hispanic inside hispanic_loc hisp_inside hisp_hloc inside_hloc hisp_inside_hloc origpchisp origloginc origlogpop if sector == 71, robust
est sto a3

ivreghdfe ihs_v hispanic inside hispanic_loc hisp_inside hisp_hloc inside_hloc hisp_inside_hloc origpchisp origloginc origlogpop if sector == 71, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hisploc71.tex", ///
replace style(tex) keep(hispanic inside hispanic_loc hisp_inside hisp_hloc inside_hloc hisp_inside_hloc) ///
order(hisp_inside_hloc hisp_inside hisp_hloc inside_hloc hispanic inside hispanic_loc) ///
 varlabels(hisp_inside_hloc "Hispanic $\times$ TV $\times$ Hispanic brand" hisp_inside "Hispanic $\times$ TV" hisp_hloc "Hispanic $\times$ Hispanic brand" inside_hloc "TV $\times$ Hispanic brand" hispanic "Hispanic" inside "TV dummy" hispanic_loc "Hispanic brand")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label






