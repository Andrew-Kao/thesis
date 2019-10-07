* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

if regexm(c(hostname), "ak") == 1 {
 global wkdir = "~/Documents/College/All/thesis/explore/Data/counties/nhgis0002_fixed"
} 

cd ${wkdir}

set more off

clear
quietly infix                 ///
  str     year       1-4      ///
  str     anrca      5-6      ///
  str     aianhha    7-10     ///
  str     res_onlya  11-14    ///
  str     trusta     15-18    ///
  str     res_trsta  19-19    ///
  str     blocka     20-23    ///
  str     blck_grpa  24-24    ///
  str     tracta     25-30    ///
  str     cda        31-32    ///
  str     c_citya    33-37    ///
  str     county     38-94    ///
  str     countya    95-97    ///
  str     cty_suba   98-102   ///
  str     divisiona  103-103  ///
  str     msa_cmsaa  104-107  ///
  str     placea     108-112  ///
  str     pmsaa      113-116  ///
  str     regiona    117-117  ///
  str     state      118-141  ///
  str     statea     142-143  ///
  str     urbrurala  144-144  ///
  str     urb_areaa  145-148  ///
  str     cd103a     149-150  ///
  str     anpsadpi   151-216  ///
  double  eu1001     217-225  ///
  double  eu1002     226-234  ///
  double  eu1003     235-243  ///
  double  eu1004     244-252  ///
  double  eu1005     253-261  ///
  using `"nhgis0002_ds120_1990_county.dat"'


format eu1001    %9.0f
format eu1002    %9.0f
format eu1003    %9.0f
format eu1004    %9.0f
format eu1005    %9.0f

label var year      `"Data File Year"'
label var anrca     `"Alaska Native Regional Corporation Code"'
label var aianhha   `"American Indian Area/Alaska Native Area/Hawaiian Home Land Code"'
label var res_onlya `"American Indian Reservation [excluding trust lands] Code"'
label var trusta    `"American Indian Reservation [trust lands only] Code"'
label var res_trsta `"Reservation/Trust Lands Code"'
label var blocka    `"Block Code"'
label var blck_grpa `"Block Group Code"'
label var tracta    `"Census Tract Code"'
label var cda       `"Congressional District (101st) Code"'
label var c_citya   `"Consolidated City Code"'
label var county    `"County Name"'
label var countya   `"County Code"'
label var cty_suba  `"County Subdivision Code"'
label var divisiona `"Division Code"'
label var msa_cmsaa `"Metropolitan Statistical Area/Consolidated Metropolitan Statistical Area Code"'
label var placea    `"Place Code"'
label var pmsaa     `"Primary Metropolitan Statistical Area Code"'
label var regiona   `"Region Code"'
label var state     `"State Name"'
label var statea    `"State Code"'
label var urbrurala `"Urban/Rural Code"'
label var urb_areaa `"Urban Area Code"'
label var cd103a    `"Congressional District (103rd) Code"'
label var anpsadpi  `"Area Name/PSAD Term/Part Indicator"'
label var eu1001    `"Not of Hispanic origin"'
label var eu1002    `"Hispanic origin: Mexican"'
label var eu1003    `"Hispanic origin: Puerto Rican"'
label var eu1004    `"Hispanic origin: Cuban"'
label var eu1005    `"Hispanic origin: Other Hispanic"'


