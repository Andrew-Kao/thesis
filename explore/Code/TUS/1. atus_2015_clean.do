clear all
set more off
if regexm(c(hostname), "ak") == 1 | c(username) == "AndrewKao"  {
 global wkdir = "~/Documents/College/All/thesis/explore/Data/TUS"
}

* spec guide
* 1: 2015
* 2: all years
* 3: individual, all years

local spec = 2

if `spec' == 1 {
cd "${wkdir}/2015"

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
}
else if `spec' == 2 {
cd "${wkdir}"
set more off


use "atus_hierarchy_all.dta", clear


* indiv data
preserve

keep if rectype == 2
keep caseid pernum wt06 age sex race hispan yrimmig citizen bpl

collapse (count) cases=pernum (mean) wt06 (lastnm) pernum age sex race hispan yrimmig citizen bpl ,by(caseid)

label values sex sex_lbl
label values race race_lbl
label values hispan hispan_lbl
label values yrimmig yrimmig_lbl
label values citizen citizen_lbl
label values bpl bpl_lbl
tempfile indiv
save `indiv'

restore

* activity data
* TV
preserve
keep if activity == 120303 | (rectype==4 & activity[_n - 1] == 120303)
replace relatewu = relatewu[_n + 1]

keep if rectype == 3
decode relatewu, gen(rw)
drop relatewu
ren rw relatewu
gen alone = (relatewu == "Alone") 
gen family = (relatewu == "Spouse" | relatewu == " Unmarried partner" | relatewu == "Own household child" | relatewu == "Grandchild" | regexm(relatewu,"Parent") | relatewu == "Brother sister" | relatewu == "Other related person" | relatewu == "Foster child" | regexm(relatewu, "family"))  
gen social = (relatewu != "Alone" & relatewu != "Refused" & relatewu != "Don't know" & relatewu != "Blank")   
gen parent = (regexm(relatewu,"Parent"))
gen child = (relatewu == "Own household child" | relatewu == "Grandchild" | relatewu == "Foster child" | regexm(relatewu, "under"))

gen duration_alone = duration * alone
gen duration_family = duration * family
gen duration_social = duration * social
gen duration_parent = duration * parent
gen duration_child = duration * child

* median 90 min, mean 114

collapse (sum) duration_ext duration duration_alone duration_family duration_social duration_parent duration_child, by(caseid)

tempfile tv
save `tv'
restore


* non-edu child
preserve
keep if (activity >= 30102 & activity <= 30106) | (rectype==4 & activity[_n - 1] == (activity >= 30102 & activity <= 30106)) 
replace relatewu = relatewu[_n + 1]

keep if rectype == 3
decode relatewu, gen(rw)
drop relatewu
ren rw relatewu
gen child = (relatewu == "Own household child" | relatewu == "Grandchild" | relatewu == "Foster child" | regexm(relatewu, "under"))

gen nonedu_child = duration * child
gen nonedu = duration

collapse (sum) nonedu nonedu_child, by(caseid)

tempfile nonedu
save `nonedu'
restore


* edu child
preserve
keep if (activity >= 30201 & activity <= 30299) | (rectype==4 & activity[_n - 1] == (activity >= 30201 & activity <= 30299)) 
replace relatewu = relatewu[_n + 1]

keep if rectype == 3
decode relatewu, gen(rw)
drop relatewu
ren rw relatewu
gen child = (relatewu == "Own household child" | relatewu == "Grandchild" | relatewu == "Foster child" | regexm(relatewu, "under"))

gen edu_child = duration * child
gen edu = duration

collapse (sum) edu edu_child, by(caseid)

tempfile edu
save `edu'
restore

* household data
keep if rectype == 1
keep caseid county

merge 1:m caseid using `tv', keep(1 3) nogen
merge 1:m caseid using `nonedu', keep(1 3) nogen
merge 1:m caseid using `edu', keep(1 3) nogen
merge m:1 caseid using `indiv', keep(1 3) nogen

replace duration_ext = 0 if missing(duration_ext)
replace duration = 0 if missing(duration)
replace duration_alone = 0 if missing(duration_alone)
replace duration_family = 0 if missing(duration_family)
replace duration_social = 0 if missing(duration_social)
replace duration_parent = 0 if missing(duration_parent)
replace duration_parent = duration_parent/cases

replace nonedu = 0 if missing(nonedu)
replace nonedu_child = 0 if missing(nonedu_child)
replace edu = 0 if missing(edu)
replace edu_child = 0 if missing(edu_child)


export delim "atus_indiv_tv_all.csv", replace
	
}
else if `spec' == 3 {
cd "${wkdir}"
set more off


use "atus_hierarchy_all.dta", clear


* household data
preserve
keep if rectype == 1

collapse (lastnm) caseid, by(county)

tempfile hh
save `hh'

restore



preserve
* activity data
keep if activity == 120303 | (rectype==4 & activity[_n - 1] == 120303)
replace relatewu = relatewu[_n + 1]

keep if rectype == 3
decode relatewu, gen(rw)
drop relatewu
ren rw relatewu
gen alone = (relatewu == "Alone") 
gen family = (relatewu == "Spouse" | relatewu == " Unmarried partner" | relatewu == "Own household child" | relatewu == "Grandchild" | regexm(relatewu,"Parent") | relatewu == "Brother sister" | relatewu == "Other related person" | relatewu == "Foster child" | regexm(relatewu, "family"))  
gen social = (relatewu != "Alone" & relatewu != "Refused" & relatewu != "Don't know" & relatewu != "Blank")   
gen parent = (regexm(relatewu,"Parent"))
gen child = (relatewu == "Own household child" | relatewu == "Grandchild" | relatewu == "Foster child" | regexm(relatewu, "under"))

gen duration_alone = duration * alone
gen duration_family = duration * family
gen duration_social = duration * social
gen duration_parent = duration * parent
gen duration_child = duration * child

* median 90 min, mean 114

collapse (sum) duration_ext duration duration_alone duration_family duration_social duration_parent duration_child, by(caseid)

tempfile tv
save `tv'
restore


* indiv data

keep if rectype == 2 & pernum == 1
keep caseid pernum wt06 age sex race hispan yrimmig citizen bpl


merge 1:m caseid using `tv', keep(1 3) nogen
merge m:1 caseid using `hh', keep(1 3) nogen

replace duration_ext = 0 if missing(duration_ext)
replace duration = 0 if missing(duration)
replace duration_alone = 0 if missing(duration_alone)
replace duration_family = 0 if missing(duration_family)
replace duration_social = 0 if missing(duration_social)
replace duration_parent = 0 if missing(duration_parent)
// replace duration_parent = duration_parent/cases


export delim "atus_indiv_tv_ind.csv", replace
	
}

* we can get more data from more years if need more power
