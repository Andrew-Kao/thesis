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
  str     blck_grpa  20-20    ///
  str     tracta     21-26    ///
  str     cda        27-28    ///
  str     c_citya    29-33    ///
  str     county     34-90    ///
  str     countya    91-93    ///
  str     cty_suba   94-98    ///
  str     divisiona  99-99    ///
  str     msa_cmsaa  100-103  ///
  str     placea     104-108  ///
  str     pmsaa      109-112  ///
  str     regiona    113-113  ///
  str     state      114-137  ///
  str     statea     138-139  ///
  str     urbrurala  140-140  ///
  str     urb_areaa  141-144  ///
  str     zipa       145-149  ///
  str     cd103a     150-151  ///
  str     anpsadpi   152-217  ///
  double  e03001     218-232  ///
  double  e03002     233-247  ///
  double  e03003     248-262  ///
  double  e03004     263-277  ///
  double  e03005     278-292  ///
  double  e05001     293-307  ///
  using `"nhgis0002_ds123_1990_county.dat"'


format e03001    %15.0f
format e03002    %15.0f
format e03003    %15.0f
format e03004    %15.0f
format e03005    %15.0f
format e05001    %15.0f

label var year      `"Data File Year"'
label var anrca     `"Alaska Native Regional Corporation Code"'
label var aianhha   `"American Indian Area/Alaska Native Area/Hawaiian Home Land Code"'
label var res_onlya `"American Indian Reservation [excluding trust lands] Code"'
label var trusta    `"American Indian Reservation [trust lands only] Code"'
label var res_trsta `"Reservation/Trust Lands Code"'
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
label var zipa      `"5-Digit ZIP Code Code"'
label var cd103a    `"Congressional District (103rd) Code"'
label var anpsadpi  `"Area Name/PSAD Term/Part Indicator"'
label var e03001    `"Aggregate income in 1989 >> White"'
label var e03002    `"Aggregate income in 1989 >> Black"'
label var e03003    `"Aggregate income in 1989 >> American Indian, Eskimo, or Aleut"'
label var e03004    `"Aggregate income in 1989 >> Asian or Pacific Islander"'
label var e03005    `"Aggregate income in 1989 >> Other race"'
label var e05001    `"Aggregate income in 1989"'


keep e* statea countya

rename (e05001 statea countya) (income_hisp state county)
gen income = e03001 + e03002 + e03003 + e03004
drop e*

save "census_income", replace
