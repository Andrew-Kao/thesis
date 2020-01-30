//===========================================================================
// Clinton Donations
// andrewjkao@gmail.com
// Updated: 01/30/2020
//===========================================================================

** Data sourced from: https://www.fec.gov/data/receipts/?data_type=processed&committee_id=C00431569&committee_id=C00575795&two_year_transaction_period=2016&min_date=11%2F01%2F2016&max_date=12%2F01%2F2016

clear all
set more off
if regexm(c(hostname), "ak") == 1 {
 global wkdir = "~/Documents/College/All/thesis/explore/Data/politics/clinton_donations"
} 
else if regexm(c(hostname), "midway") == 1 {
 global wkdir = "/project2/bursztyn/"
}
cd "${wkdir}"

tempfile don_all
local i = 1
local files : dir "${wkdir}" files "schedule_*"
foreach f of local files {
	qui import delim using "${wkdir}/`f'", clear
	qui if `i' > 1 append using `don_all', force
	qui save `don_all', replace
	local ++i
}

* Make dates
gen date_string = substr(contribution_receipt_date, 1, 10)
gen donation_datevar = date(date_string,"YMD")
gen year = year(donation_datevar)
gen month = month(donation_datevar)

gen unid = contributor_name + contributor_street_1 + contributor_city
* Check sum within a year == aggregate ytd, 517,179/1,090,262 fail

tostring contributor_zip, replace
gen postcode = substr(contributor_zip, 1, 5)
destring postcode, replace

merge m:1 postcode using "postcode.dta", keep(3) nogen
// 12,465 in master don't match, 13,435 in using don't match

gen CountyFips = statecode*1000+countycode

drop committee_id committee_name v9 entity_type_desc unused_contbr_id contributor_id contribution_receipt_date candidate* conduit* ///
	national_committee_nonfederal_ac national_committee_nonfederal_ac load_date original_sub_id back* filing_form link_id memo_text ///
	two_year_transaction_period increased_limit sub_id pdf_url
	
export delim "ClintonDonations", replace
