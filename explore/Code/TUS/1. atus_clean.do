clear all
set more off
if regexm(c(hostname), "ak") == 1 {
 global wkdir = "~/Documents/College/All/thesis/explore/Data/TUS/2015"
}

cd ${wkdir}

set more off


use "atus_hierarchy.dta", clear


* indiv data
preserve

keep if rectype == 2
keep caseid pernum wt06 age sex race hispan yrimmig citizen bpl
tempfile indiv
save `indiv'

restore

preserve
* activity data
keep if activity == 120303

* median 90 min, mean 114

collapse (sum) duration_ext duration, by(caseid)

tempfile tv
save `tv'
restore

* household data
keep if rectype == 1
keep caseid county

merge 1:m caseid using `tv', keep(1 3) nogen
merge m:1 caseid using `indiv', keep(1 3) nogen

replace duration_ext = 0 if missing(duration_ext)
replace duration = 0 if missing(duration)

// save "atus_indiv_tv.dta", replace
export delim "atus_indiv_tv.csv", replace


* we can get more data from more years if need more power
