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

gen kosher_food = regexm(category_tags,"Kosher Food") == 1
gen greek_food = regexm(category_tags,"Greek Food") == 1
gen brazil_food = regexm(category_tags,"Brazilian Food") == 1
gen chinese_food = regexm(category_tags,"Chinese Food") == 1
gen japanese_food = regexm(category_tags,"Japanese Food") == 1
// gen italian_food = regexm(category_tags,"Italian Food") == 1

gen hisp_inside = hispanic * inside
gen hisp_hfood = hispanic * hispanic_food
gen inside_hfood = inside * hispanic_food
gen hisp_inside_hfood = hisp_inside * hispanic_food


** main regression
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

* greek placebo
gen greek_3 = greek_food * hispanic * inside

ivreghdfe ihs_v greek_3 hispanic##inside##greek_food origpchisp if sector == 72,  robust
est sto a1

ivreghdfe ihs_v greek_3 hispanic##inside##greek_food origpchisp origloginc if sector == 72,  robust
est sto a2

ivreghdfe ihs_v  greek_3 hispanic##inside##greek_food origpchisp origloginc origlogpop if sector == 72, robust
est sto a3

ivreghdfe ihs_v  greek_3 hispanic##inside##greek_food origpchisp origloginc origlogpop if sector == 72, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hispfood_greek.tex", ///
replace style(tex) keep(greek_3) ///
order(greek_3) ///
 varlabels(greek_3 "Hispanic $\times$ TV $\times$ Greek food" )  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label

* japanese placebo
gen japan_3 = japanese_food * hispanic * inside

ivreghdfe ihs_v japan_3 hispanic##inside##japanese_food origpchisp if sector == 72,  robust
est sto a1

ivreghdfe ihs_v japan_3 hispanic##inside##japanese_food origpchisp origloginc if sector == 72,  robust
est sto a2

ivreghdfe ihs_v  japan_3 hispanic##inside##japanese_food origpchisp origloginc origlogpop if sector == 72, robust
est sto a3

ivreghdfe ihs_v  japan_3 hispanic##inside##japanese_food origpchisp origloginc origlogpop if sector == 72, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hispfood_japan.tex", ///
replace style(tex) keep(japan_3) ///
order(japan_3) ///
 varlabels(japan_3 "Hispanic $\times$ TV $\times$ Japanese food" )  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label

* brazilian placebo
gen brazil_3 = brazil_food * hispanic * inside

ivreghdfe ihs_v brazil_3 hispanic##inside##brazil_food origpchisp if sector == 72,  robust
est sto a1

ivreghdfe ihs_v brazil_3 hispanic##inside##brazil_food origpchisp origloginc if sector == 72,  robust
est sto a2

ivreghdfe ihs_v  brazil_3 hispanic##inside##brazil_food origpchisp origloginc origlogpop if sector == 72, robust
est sto a3

ivreghdfe ihs_v  brazil_3 hispanic##inside##brazil_food origpchisp origloginc origlogpop if sector == 72, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hispfood_brazil.tex", ///
replace style(tex) keep(brazil_3) ///
order(brazil_3) ///
 varlabels(brazil_3 "Hispanic $\times$ TV $\times$ Brazilian food" )  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label


********* recreation

gen brazil_loc = regexm(location_name, "Brazil") | regexm(location_name, "brazil") 

// | regexm(location_name, " seu ") | regexm(location_name, " ele ") | regexm(location_name, " foi ") | regexm(location_name, " em ") | regexm(location_name, " são ") | regexm(location_name, " com ") | regexm(location_name, " uma ") | regexm(location_name, " tem ") | regexm(location_name, " quente ") | regexm(location_name, " palavra ") | regexm(location_name, " você ") | regexm(location_name, " teve ") 

gen greek_loc = regexm(location_name, "Greek") | regexm(location_name, "greek") | regexm(location_name, "Greece") | regexm(location_name, "greece")
gen japan_loc = regexm(location_name, "Japan") | regexm(location_name, "japan")



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


* greek placebo
gen greek_3l = greek_loc * hispanic * inside

ivreghdfe ihs_v greek_3l hispanic##inside##greek_loc origpchisp  if sector == 71, robust
est sto a1

ivreghdfe ihs_v greek_3l hispanic##inside##greek_loc origpchisp origloginc if sector == 71, robust
est sto a2

ivreghdfe ihs_v greek_3l hispanic##inside##greek_loc origpchisp origloginc origlogpop if sector == 71, robust
est sto a3

ivreghdfe ihs_v greek_3l hispanic##inside##greek_loc origpchisp origloginc origlogpop if sector == 71, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hisploc71_greek.tex", ///
replace style(tex) keep(greek_3l) ///
order(greek_3l hisp_inside hisp_hloc inside_hloc hispanic inside hispanic_loc) ///
 varlabels(greek_3l "Hispanic $\times$ TV $\times$ Greek brand" hisp_inside "Hispanic $\times$ TV" hisp_hloc "Hispanic $\times$ Hispanic brand" inside_hloc "TV $\times$ Hispanic brand" hispanic "Hispanic" inside "TV dummy" hispanic_loc "Hispanic brand")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label

* japan placebo
gen japan_3l = japan_loc * hispanic * inside

ivreghdfe ihs_v japan_3l hispanic##inside##japan_loc origpchisp  if sector == 71, robust
est sto a1

ivreghdfe ihs_v japan_3l hispanic##inside##japan_loc origpchisp origloginc if sector == 71, robust
est sto a2

ivreghdfe ihs_v japan_3l hispanic##inside##japan_loc origpchisp origloginc origlogpop if sector == 71, robust
est sto a3

ivreghdfe ihs_v japan_3l hispanic##inside##japan_loc origpchisp origloginc origlogpop if sector == 71, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hisploc71_japan.tex", ///
replace style(tex) keep(japan_3l) ///
order(japan_3l hisp_inside hisp_hloc inside_hloc hispanic inside hispanic_loc) ///
 varlabels(japan_3l "Hispanic $\times$ TV $\times$ Japanese brand" hisp_inside "Hispanic $\times$ TV" hisp_hloc "Hispanic $\times$ Hispanic brand" inside_hloc "TV $\times$ Hispanic brand" hispanic "Hispanic" inside "TV dummy" hispanic_loc "Hispanic brand")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label

* brazil placebo
gen brazil_3l = brazil_loc * hispanic * inside

ivreghdfe ihs_v brazil_3l hispanic##inside##brazil_loc origpchisp  if sector == 71, robust
est sto a1

ivreghdfe ihs_v brazil_3l hispanic##inside##brazil_loc origpchisp origloginc if sector == 71, robust
est sto a2

ivreghdfe ihs_v brazil_3l hispanic##inside##brazil_loc origpchisp origloginc origlogpop if sector == 71, robust
est sto a3

ivreghdfe ihs_v brazil_3l hispanic##inside##brazil_loc origpchisp origloginc origlogpop if sector == 71, absorb(statecounty naics_code) robust
est sto a4

estout a* using "../../Output/Regs/sg_3d_hisploc71_brazil.tex", ///
replace style(tex) keep(brazil_3l) ///
order(brazil_3l hisp_inside hisp_hloc inside_hloc hispanic inside hispanic_loc) ///
 varlabels(brazil_3l "Hispanic $\times$ TV $\times$ Brazilian brand" hisp_inside "Hispanic $\times$ TV" hisp_hloc "Hispanic $\times$ Hispanic brand" inside_hloc "TV $\times$ Hispanic brand" hispanic "Hispanic" inside "TV dummy" hispanic_loc "Hispanic brand")  ///
 ml(, none) collabels(, none) stats(N, fmt(0))  ///
cells(b(star fmt(%9.3f)) se(par)) ///
starlevels(* 0.10 ** 0.05 *** 0.01) label



