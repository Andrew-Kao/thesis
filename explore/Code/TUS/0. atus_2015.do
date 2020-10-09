
* Custom ATUS Pull

clear all
set more off
if regexm(c(hostname), "ak") == 1 {
 global wkdir = "~/Documents/College/All/thesis/explore/Data/TUS/2015"
}

cd ${wkdir}

set more off

clear
quietly infix                  ///
  byte    rectype       1-1    ///
  long    year          2-6    ///
  double  caseid        7-20   ///
  long    county        21-25  ///
  using `"atus_00002.dat"'
gen  _line_num = _n
drop if rectype != 1
sort _line_num
save __temp_ipums_hier_1.dta

clear
quietly infix                  ///
  byte    rectype       1-1    ///
  long    year          2-6    ///
  double  caseid        7-20   ///
  byte    pernum        21-22  ///
  int     lineno        23-25  ///
  double  wt06          26-42  ///
  int     age           43-45  ///
  byte    sex           46-47  ///
  int     race          48-51  ///
  int     hispan        52-55  ///
  int     yrimmig       56-58  ///
  int     citizen       59-61  ///
  long    bpl           62-67  ///
  int     relate        68-70  ///
  using `"atus_00002.dat"'
gen  _line_num = _n
drop if rectype != 2
sort _line_num
save __temp_ipums_hier_2.dta

clear
quietly infix                  ///
  byte    rectype       1-1    ///
  long    year          2-6    ///
  double  caseid        7-20   ///
  byte    actline       21-22  ///
  long    activity      23-28  ///
  int     where         29-32  ///
  int     duration_ext  33-36  ///
  int     duration      37-40  ///
  str     start         41-48  ///
  str     stop          49-56  ///
  using `"atus_00002.dat"'
gen  _line_num = _n
drop if rectype != 3
sort _line_num
save __temp_ipums_hier_3.dta

clear
quietly infix                  ///
  byte    rectype       1-1    ///
  long    year          2-6    ///
  double  caseid        7-20   ///
  byte    actlinew      21-22  ///
  int     linenow       23-25  ///
  byte    wholine       26-27  ///
  int     relatewu      28-31  ///
  using `"atus_00002.dat"'
gen  _line_num = _n
drop if rectype != 4
sort _line_num
save __temp_ipums_hier_4.dta

clear
use __temp_ipums_hier_1.dta
append using __temp_ipums_hier_2.dta
append using __temp_ipums_hier_3.dta
append using __temp_ipums_hier_4.dta
sort _line_num
drop _line_num
erase __temp_ipums_hier_1.dta
erase __temp_ipums_hier_2.dta
erase __temp_ipums_hier_3.dta
erase __temp_ipums_hier_4.dta


format caseid       %14.0f
format caseid       %14.0f
format wt06         %17.0g
format caseid       %14.0f
format caseid       %14.0f

label var rectype      `"Record Type"'
label var year         `"Survey year"'
label var caseid       `"ATUS Case ID"'
label var county       `"FIPS County code"'
label var pernum       `"Person number (general)"'
label var lineno       `"Person line number"'
label var wt06         `"Person weight, 2006 methodology"'
label var age          `"Age"'
label var sex          `"Sex"'
label var race         `"Race"'
label var hispan       `"Hispanic origin"'
label var yrimmig      `"Year of immigration"'
label var citizen      `"Citizenship status"'
label var bpl          `"Birthplace"'
label var relate       `"Relationship to ATUS respondent"'
label var actline      `"Activity line number"'
label var activity     `"Activity"'
label var where        `"Location of activity"'
label var duration_ext `"Duration of activity (extended version)"'
label var duration     `"Duration of activity"'
label var start        `"Activity start time"'
label var stop         `"Activity stop time"'
label var actlinew     `"Activity line number"'
label var linenow      `"Person line number"'
label var wholine      `"Number of the who record for this episode"'
label var relatewu     `"Relationship of person with whom activity was done (uncollapsed version)"'

label define rectype_lbl 1 `"Household"'
label define rectype_lbl 2 `"Person"', add
label define rectype_lbl 3 `"Activity"', add
label define rectype_lbl 4 `"Who"', add
label define rectype_lbl 5 `"Eldercare"', add
label values rectype rectype_lbl

label define pernum_lbl 01 `"1"'
label define pernum_lbl 02 `"2"', add
label define pernum_lbl 03 `"3"', add
label define pernum_lbl 04 `"4"', add
label define pernum_lbl 05 `"5"', add
label define pernum_lbl 06 `"6"', add
label define pernum_lbl 07 `"7"', add
label define pernum_lbl 08 `"8"', add
label define pernum_lbl 09 `"9"', add
label define pernum_lbl 10 `"10"', add
label define pernum_lbl 11 `"11"', add
label define pernum_lbl 12 `"12"', add
label define pernum_lbl 13 `"13"', add
label define pernum_lbl 14 `"14"', add
label define pernum_lbl 15 `"15"', add
label define pernum_lbl 16 `"16"', add
label values pernum pernum_lbl

label define lineno_lbl 001 `"1"'
label define lineno_lbl 002 `"2"', add
label define lineno_lbl 003 `"3"', add
label define lineno_lbl 004 `"4"', add
label define lineno_lbl 005 `"5"', add
label define lineno_lbl 006 `"6"', add
label define lineno_lbl 007 `"7"', add
label define lineno_lbl 008 `"8"', add
label define lineno_lbl 009 `"9"', add
label define lineno_lbl 010 `"10"', add
label define lineno_lbl 011 `"11"', add
label define lineno_lbl 012 `"12"', add
label define lineno_lbl 013 `"13"', add
label define lineno_lbl 014 `"14"', add
label define lineno_lbl 015 `"15"', add
label define lineno_lbl 016 `"16"', add
label define lineno_lbl 017 `"17"', add
label define lineno_lbl 018 `"18"', add
label define lineno_lbl 019 `"19"', add
label define lineno_lbl 999 `"NIU (Not in universe)"', add
label values lineno lineno_lbl

label define sex_lbl 01 `"Male"'
label define sex_lbl 02 `"Female"', add
label define sex_lbl 99 `"NIU (Not in universe)"', add
label values sex sex_lbl

label define race_lbl 0100 `"White only"'
label define race_lbl 0110 `"Black only"', add
label define race_lbl 0120 `"American Indian, Alaskan Native"', add
label define race_lbl 0130 `"Asian or Pacific Islander"', add
label define race_lbl 0131 `"Asian only"', add
label define race_lbl 0132 `"Hawaiian Pacific Islander only"', add
label define race_lbl 0200 `"White-Black"', add
label define race_lbl 0201 `"White-American Indian"', add
label define race_lbl 0202 `"White-Asian"', add
label define race_lbl 0203 `"White-Hawaiian"', add
label define race_lbl 0210 `"Black-American Indian"', add
label define race_lbl 0211 `"Black-Asian"', add
label define race_lbl 0212 `"Black-Hawaiian"', add
label define race_lbl 0220 `"American Indian-Asian"', add
label define race_lbl 0221 `"American Indian-Hawaiian"', add
label define race_lbl 0230 `"Asian-Hawaiian"', add
label define race_lbl 0300 `"White-Black-American Indian"', add
label define race_lbl 0301 `"White-Black-Asian"', add
label define race_lbl 0302 `"White-Black-Hawaiian"', add
label define race_lbl 0310 `"White-American Indian-Asian"', add
label define race_lbl 0311 `"White-American Indian-Hawaiian"', add
label define race_lbl 0320 `"White-Asian-Hawaiian"', add
label define race_lbl 0330 `"Black-American Indian-Asian"', add
label define race_lbl 0331 `"Black-American Indian-Hawaiian"', add
label define race_lbl 0340 `"Black-Asian-Hawaiian"', add
label define race_lbl 0350 `"American Indian-Asian-Hawaiian"', add
label define race_lbl 0398 `"Other 3 race combinations"', add
label define race_lbl 0399 `"2 or 3 races, unspecified"', add
label define race_lbl 0400 `"White-Black-American Indian-Asian"', add
label define race_lbl 0401 `"White-Black-American Indian-Hawaiian"', add
label define race_lbl 0402 `"White-Black-Asian-Hawaiian"', add
label define race_lbl 0403 `"Black-American Indian-Asian-Hawaiian"', add
label define race_lbl 0404 `"White-American Indian-Asian-Hawaiian"', add
label define race_lbl 0500 `"White-Black-American Indian-Asian-Hawaiian"', add
label define race_lbl 0599 `"4 or 5 races, unspecified"', add
label define race_lbl 9999 `"NIU (Not in universe)"', add
label values race race_lbl

label define hispan_lbl 0100 `"Not Hispanic"'
label define hispan_lbl 0210 `"Mexican"', add
label define hispan_lbl 0211 `"Mexican American"', add
label define hispan_lbl 0212 `"Chicano"', add
label define hispan_lbl 0213 `"Mexican (Mexicano)"', add
label define hispan_lbl 0220 `"Puerto Rican"', add
label define hispan_lbl 0230 `"Cuban"', add
label define hispan_lbl 0240 `"Central-South American"', add
label define hispan_lbl 0241 `"Dominican"', add
label define hispan_lbl 0242 `"Salvadoran"', add
label define hispan_lbl 0243 `"Other Central American"', add
label define hispan_lbl 0244 `"South American"', add
label define hispan_lbl 0250 `"Other Spanish"', add
label define hispan_lbl 9999 `"NIU (Not in universe)"', add
label values hispan hispan_lbl

label define yrimmig_lbl 000 `"Not foreign born"'
label define yrimmig_lbl 001 `"Before 1950"', add
label define yrimmig_lbl 002 `"1950-1959"', add
label define yrimmig_lbl 003 `"1960-1964"', add
label define yrimmig_lbl 004 `"1965-1969"', add
label define yrimmig_lbl 005 `"1970-1974"', add
label define yrimmig_lbl 006 `"1975-1979"', add
label define yrimmig_lbl 007 `"1980-1981"', add
label define yrimmig_lbl 008 `"1982-1983"', add
label define yrimmig_lbl 009 `"1984-1985"', add
label define yrimmig_lbl 010 `"1986-1987"', add
label define yrimmig_lbl 011 `"1988-1989"', add
label define yrimmig_lbl 012 `"1990-1991"', add
label define yrimmig_lbl 013 `"1992-1993"', add
label define yrimmig_lbl 014 `"1994-1995"', add
label define yrimmig_lbl 015 `"1996-1997"', add
label define yrimmig_lbl 016 `"1998-1999"', add
label define yrimmig_lbl 017 `"2000-2001"', add
label define yrimmig_lbl 018 `"2002-2003"', add
label define yrimmig_lbl 019 `"2002-2004"', add
label define yrimmig_lbl 020 `"2002-2005"', add
label define yrimmig_lbl 021 `"2004-2005"', add
label define yrimmig_lbl 022 `"2004-2006"', add
label define yrimmig_lbl 023 `"2004-2007"', add
label define yrimmig_lbl 024 `"2006-2007"', add
label define yrimmig_lbl 025 `"2006-2008"', add
label define yrimmig_lbl 026 `"2006-2009"', add
label define yrimmig_lbl 027 `"2008-2009"', add
label define yrimmig_lbl 028 `"2008-2010"', add
label define yrimmig_lbl 029 `"2008-2011"', add
label define yrimmig_lbl 030 `"2010-2011"', add
label define yrimmig_lbl 031 `"2010-2012"', add
label define yrimmig_lbl 032 `"2010-2013"', add
label define yrimmig_lbl 033 `"2012-2013"', add
label define yrimmig_lbl 034 `"2012-2014"', add
label define yrimmig_lbl 035 `"2012-2015"', add
label define yrimmig_lbl 036 `"2014-2015"', add
label define yrimmig_lbl 037 `"2014-2016"', add
label define yrimmig_lbl 038 `"2014-2017"', add
label define yrimmig_lbl 039 `"2016-2018"', add
label define yrimmig_lbl 040 `"2016-2019"', add
label define yrimmig_lbl 998 `"Blank"', add
label define yrimmig_lbl 999 `"NIU (Not in universe)"', add
label values yrimmig yrimmig_lbl

label define citizen_lbl 001 `"Native, born in United States"'
label define citizen_lbl 002 `"Native, born in Puerto Rico or U.S. Outlying Area"', add
label define citizen_lbl 003 `"Native, born abroad of American parent or parents"', add
label define citizen_lbl 004 `"Foreign born, U.S. citizen by naturalization"', add
label define citizen_lbl 005 `"Foreign born, not a U.S. citizen"', add
label define citizen_lbl 999 `"NIU (Not in universe)"', add
label values citizen citizen_lbl

label define bpl_lbl 009900 `"U.S., n.s."'
label define bpl_lbl 010000 `"American Samoa"', add
label define bpl_lbl 010500 `"Guam"', add
label define bpl_lbl 010750 `"Nothern Marianas"', add
label define bpl_lbl 011000 `"Puerto Rico"', add
label define bpl_lbl 011500 `"U.S. Virgin Islands"', add
label define bpl_lbl 012090 `"U.S. outlying areas, n.s."', add
label define bpl_lbl 015000 `"Canada"', add
label define bpl_lbl 016010 `"Bermuda"', add
label define bpl_lbl 019900 `"North America, n.s."', add
label define bpl_lbl 020000 `"Mexico"', add
label define bpl_lbl 021010 `"Belize/British Honduras"', add
label define bpl_lbl 021020 `"Costa Rica"', add
label define bpl_lbl 021030 `"El Salvador"', add
label define bpl_lbl 021040 `"Guatemala"', add
label define bpl_lbl 021050 `"Honduras"', add
label define bpl_lbl 021060 `"Nicaragua"', add
label define bpl_lbl 021070 `"Panama"', add
label define bpl_lbl 021090 `"Central America, n.s."', add
label define bpl_lbl 025000 `"Cuba"', add
label define bpl_lbl 026000 `"West Indies"', add
label define bpl_lbl 026010 `"Dominican Republic"', add
label define bpl_lbl 026020 `"Haiti"', add
label define bpl_lbl 026030 `"Jamaica"', add
label define bpl_lbl 026043 `"Bahamas"', add
label define bpl_lbl 026044 `"Barbados"', add
label define bpl_lbl 026054 `"Dominica"', add
label define bpl_lbl 026055 `"Grenada"', add
label define bpl_lbl 026060 `"Trinidad and Tobago"', add
label define bpl_lbl 026065 `"Antigua and Barbuda"', add
label define bpl_lbl 026070 `"St. Kitts--Nevis"', add
label define bpl_lbl 026075 `"St. Lucia"', add
label define bpl_lbl 026080 `"St. Vincent and the Grenadi"', add
label define bpl_lbl 026091 `"Caribbean, n.s."', add
label define bpl_lbl 030005 `"Argentina"', add
label define bpl_lbl 030010 `"Bolivia"', add
label define bpl_lbl 030015 `"Brazil"', add
label define bpl_lbl 030020 `"Chile"', add
label define bpl_lbl 030025 `"Colombia"', add
label define bpl_lbl 030030 `"Ecuador"', add
label define bpl_lbl 030040 `"Guyana/British Guiana"', add
label define bpl_lbl 030050 `"Peru"', add
label define bpl_lbl 030060 `"Uruguay"', add
label define bpl_lbl 030065 `"Venezuala"', add
label define bpl_lbl 030070 `"Paraguay"', add
label define bpl_lbl 030090 `"South America, n.s."', add
label define bpl_lbl 031000 `"Americas, n.s."', add
label define bpl_lbl 040000 `"Denmark"', add
label define bpl_lbl 040100 `"Finland"', add
label define bpl_lbl 040200 `"Iceland"', add
label define bpl_lbl 040400 `"Norway"', add
label define bpl_lbl 040500 `"Sweden"', add
label define bpl_lbl 041000 `"England"', add
label define bpl_lbl 041100 `"Scotland"', add
label define bpl_lbl 041200 `"Wales"', add
label define bpl_lbl 041300 `"United Kingdom, n.s."', add
label define bpl_lbl 041400 `"Ireland"', add
label define bpl_lbl 041410 `"Northern Ireland"', add
label define bpl_lbl 042000 `"Belgium"', add
label define bpl_lbl 042100 `"France"', add
label define bpl_lbl 042500 `"Netherlands"', add
label define bpl_lbl 042600 `"Switzerland"', add
label define bpl_lbl 043300 `"Greece"', add
label define bpl_lbl 043400 `"Italy"', add
label define bpl_lbl 043600 `"Portugal"', add
label define bpl_lbl 043610 `"Azores"', add
label define bpl_lbl 043800 `"Spain"', add
label define bpl_lbl 045000 `"Austria"', add
label define bpl_lbl 045200 `"Czechoslavakia"', add
label define bpl_lbl 045212 `"Slovakia"', add
label define bpl_lbl 045213 `"Czech Republic"', add
label define bpl_lbl 045300 `"Germany"', add
label define bpl_lbl 045400 `"Hungary"', add
label define bpl_lbl 045500 `"Poland"', add
label define bpl_lbl 045600 `"Romania"', add
label define bpl_lbl 045650 `"Bulgaria"', add
label define bpl_lbl 045675 `"Albania"', add
label define bpl_lbl 045700 `"Yugoslavia"', add
label define bpl_lbl 045720 `"Bosnia and Herzegovina"', add
label define bpl_lbl 045730 `"Croatia"', add
label define bpl_lbl 045740 `"Macedonia"', add
label define bpl_lbl 045750 `"Serbia"', add
label define bpl_lbl 045760 `"Kosovo"', add
label define bpl_lbl 045770 `"Montenegro"', add
label define bpl_lbl 046100 `"Estonia"', add
label define bpl_lbl 046200 `"Latvia"', add
label define bpl_lbl 046300 `"Lithuania"', add
label define bpl_lbl 046500 `"Other USSR/Russia"', add
label define bpl_lbl 046530 `"Ukraine"', add
label define bpl_lbl 046535 `"Belarus"', add
label define bpl_lbl 046540 `"Moldova"', add
label define bpl_lbl 046590 `"USSR, n.s."', add
label define bpl_lbl 049900 `"Europe, n.s."', add
label define bpl_lbl 050000 `"China"', add
label define bpl_lbl 050010 `"Hong Kong"', add
label define bpl_lbl 050040 `"Taiwan"', add
label define bpl_lbl 050100 `"Japan"', add
label define bpl_lbl 050200 `"Korea"', add
label define bpl_lbl 050220 `"South Korea"', add
label define bpl_lbl 050300 `"Mongolia"', add
label define bpl_lbl 051100 `"Cambodia"', add
label define bpl_lbl 051200 `"Indonesia"', add
label define bpl_lbl 051300 `"Laos"', add
label define bpl_lbl 051400 `"Malaysia"', add
label define bpl_lbl 051500 `"Philippines"', add
label define bpl_lbl 051600 `"Singapore"', add
label define bpl_lbl 051700 `"Thailand"', add
label define bpl_lbl 051800 `"Vietnam"', add
label define bpl_lbl 052000 `"Afghanistan"', add
label define bpl_lbl 052100 `"India"', add
label define bpl_lbl 052110 `"Bangladesh"', add
label define bpl_lbl 052120 `"Bhutan"', add
label define bpl_lbl 052130 `"Burma (Myanmar)"', add
label define bpl_lbl 052140 `"Pakistan"', add
label define bpl_lbl 052150 `"Sri Lanka"', add
label define bpl_lbl 052200 `"Nepal"', add
label define bpl_lbl 055100 `"Armenia"', add
label define bpl_lbl 055200 `"Azerbaijan"', add
label define bpl_lbl 055300 `"Georgia"', add
label define bpl_lbl 055400 `"Uzbekistan"', add
label define bpl_lbl 055500 `"Kazakhstan"', add
label define bpl_lbl 053000 `"Iran"', add
label define bpl_lbl 053200 `"Iraq"', add
label define bpl_lbl 053400 `"Isreal/Palestine"', add
label define bpl_lbl 053420 `"Palestine"', add
label define bpl_lbl 053500 `"Jordan"', add
label define bpl_lbl 053700 `"Lebanon"', add
label define bpl_lbl 054000 `"Saudi Arabia"', add
label define bpl_lbl 054100 `"Syria"', add
label define bpl_lbl 054200 `"Turkey"', add
label define bpl_lbl 054300 `"Cyprus"', add
label define bpl_lbl 054350 `"Kuwait"', add
label define bpl_lbl 054400 `"Yemen"', add
label define bpl_lbl 054500 `"United Arab Emirates"', add
label define bpl_lbl 054700 `"Middle East, n.s."', add
label define bpl_lbl 059900 `"Asia, n.e.c. or n.s."', add
label define bpl_lbl 060010 `"Northern Africa"', add
label define bpl_lbl 060012 `"Egypt/United Arab Rep."', add
label define bpl_lbl 060014 `"Morocco"', add
label define bpl_lbl 060016 `"Algeria"', add
label define bpl_lbl 060018 `"Sudan"', add
label define bpl_lbl 060019 `"Libya"', add
label define bpl_lbl 060023 `"Ghana"', add
label define bpl_lbl 060031 `"Nigeria"', add
label define bpl_lbl 060032 `"Cameroon"', add
label define bpl_lbl 060033 `"Cape Verde"', add
label define bpl_lbl 060034 `"Liberia"', add
label define bpl_lbl 060035 `"Senegal"', add
label define bpl_lbl 060036 `"Sierra Leone"', add
label define bpl_lbl 060037 `"Guinea"', add
label define bpl_lbl 060038 `"Ivory Coast"', add
label define bpl_lbl 060039 `"Togo"', add
label define bpl_lbl 060040 `"Eritrea"', add
label define bpl_lbl 060044 `"Ethiopia"', add
label define bpl_lbl 060045 `"Kenya"', add
label define bpl_lbl 060050 `"Somalia"', add
label define bpl_lbl 060060 `"Tanzania"', add
label define bpl_lbl 060065 `"Uganda"', add
label define bpl_lbl 060070 `"Zimbabwe"', add
label define bpl_lbl 060094 `"South Africa (Union of)"', add
label define bpl_lbl 060095 `"Zaire"', add
label define bpl_lbl 060096 `"Congo"', add
label define bpl_lbl 060097 `"Zambia"', add
label define bpl_lbl 060099 `"Africa, n.s."', add
label define bpl_lbl 070010 `"Australia"', add
label define bpl_lbl 070020 `"New Zealand"', add
label define bpl_lbl 071000 `"Pacific Islands"', add
label define bpl_lbl 071021 `"Fiji"', add
label define bpl_lbl 071022 `"Tonga"', add
label define bpl_lbl 071023 `"Samoa"', add
label define bpl_lbl 071024 `"Marshall Islands"', add
label define bpl_lbl 072000 `"Micronesia"', add
label define bpl_lbl 071090 `"Oceania, n.s."', add
label define bpl_lbl 096000 `"Other, n.e.c. and unknown"', add
label define bpl_lbl 999999 `"NIU (Not in universe)"', add
label values bpl bpl_lbl

label define relate_lbl 010 `"Self"'
label define relate_lbl 020 `"Spouse"', add
label define relate_lbl 021 `"Unmarried Partner"', add
label define relate_lbl 022 `"Own household child"', add
label define relate_lbl 023 `"Grandchild"', add
label define relate_lbl 024 `"Parent"', add
label define relate_lbl 025 `"Brother/Sister"', add
label define relate_lbl 026 `"Other relative"', add
label define relate_lbl 027 `"Foster child"', add
label define relate_lbl 028 `"Housemate/roommate"', add
label define relate_lbl 029 `"Roomer/boarder"', add
label define relate_lbl 030 `"Other nonrelative"', add
label define relate_lbl 040 `"Own non-household child lt 18"', add
label define relate_lbl 996 `"Refused"', add
label define relate_lbl 997 `"Don't know"', add
label define relate_lbl 999 `"NIU (Not in universe)"', add
label values relate relate_lbl

label define activity_lbl 010000 `"Personal Care"'
label define activity_lbl 010100 `"Sleeping"', add
label define activity_lbl 010101 `"Sleeping"', add
label define activity_lbl 010102 `"Sleeplessness"', add
label define activity_lbl 010199 `"Sleeping, n.e.c."', add
label define activity_lbl 010200 `"Grooming"', add
label define activity_lbl 010201 `"Washing, dressing and grooming oneself"', add
label define activity_lbl 010299 `"Grooming, n.e.c."', add
label define activity_lbl 010300 `"Health-Related Self Care"', add
label define activity_lbl 010301 `"Health-related self care"', add
label define activity_lbl 010399 `"Self care, n.e.c."', add
label define activity_lbl 010400 `"Personal Activities"', add
label define activity_lbl 010401 `"Personal/Private activities"', add
label define activity_lbl 010499 `"Personal activities, n.e.c."', add
label define activity_lbl 010500 `"Personal Care Emergencies"', add
label define activity_lbl 010501 `"Personal emergencies"', add
label define activity_lbl 010599 `"Personal care emergencies, n.e.c."', add
label define activity_lbl 019900 `"Personal Care, n.e.c."', add
label define activity_lbl 019999 `"Personal care, n.e.c."', add
label define activity_lbl 020000 `"Household Activities"', add
label define activity_lbl 020100 `"Housework"', add
label define activity_lbl 020101 `"Interior cleaning"', add
label define activity_lbl 020102 `"Laundry"', add
label define activity_lbl 020103 `"Sewing, repairing, and maintaining textiles"', add
label define activity_lbl 020104 `"Storing interior hh items, inc. food"', add
label define activity_lbl 020199 `"Housework, n.e.c."', add
label define activity_lbl 020200 `"Food and Drink Preparation, Presentation, and Clean-up"', add
label define activity_lbl 020201 `"Food and drink preparation"', add
label define activity_lbl 020202 `"Food presentation"', add
label define activity_lbl 020203 `"Kitchen and food clean-up"', add
label define activity_lbl 020299 `"Food and drink prep, serving and clean-up, n.e.c."', add
label define activity_lbl 020300 `"Interior Maintenance, Repair, and Decoration"', add
label define activity_lbl 020301 `"Interior arrangement, decoration, and repairs"', add
label define activity_lbl 020302 `"Building and repairing furniture"', add
label define activity_lbl 020303 `"Heating and cooling"', add
label define activity_lbl 020399 `"Interior maintenance, repair, and decoration, n.e.c."', add
label define activity_lbl 020400 `"Exterior Maintenance, Repair, and Decoration"', add
label define activity_lbl 020401 `"Exterior cleaning"', add
label define activity_lbl 020402 `"Exterior repair, improvements, and decoration"', add
label define activity_lbl 020499 `"Exterior maintenance, repair and decoration, n.e.c."', add
label define activity_lbl 020500 `"Lawn, Garden, and Houseplants"', add
label define activity_lbl 020501 `"Lawn, garden, and houseplant care"', add
label define activity_lbl 020502 `"Ponds, pools, and hot tubs"', add
label define activity_lbl 020599 `"Lawn and garden, n.e.c."', add
label define activity_lbl 020600 `"Animals and Pets"', add
label define activity_lbl 020601 `"Care for animals and pets (not veterinary care)"', add
label define activity_lbl 020602 `"Care for animals and pets (not veterinary care) (2008+)"', add
label define activity_lbl 020603 `"Walking, exercising, playing with animals (2008+)"', add
label define activity_lbl 020699 `"Pet and animal care, n.e.c."', add
label define activity_lbl 020700 `"Vehicles"', add
label define activity_lbl 020701 `"Vehicle repair and maintenance (by self)"', add
label define activity_lbl 020799 `"Vehicles, n.e.c."', add
label define activity_lbl 020800 `"Appliances, Tools, and Toys"', add
label define activity_lbl 020801 `"App, tool, toy set-up, repair, and maint (by self)"', add
label define activity_lbl 020899 `"Appliances and tools, n.e.c."', add
label define activity_lbl 020900 `"Household Management"', add
label define activity_lbl 020901 `"Financial management"', add
label define activity_lbl 020902 `"Household and personal organization and planning"', add
label define activity_lbl 020903 `"HH and personal mail and messages (except e-mail)"', add
label define activity_lbl 020904 `"HH and personal e-mail and messages"', add
label define activity_lbl 020905 `"Home security"', add
label define activity_lbl 020999 `"Household management, n.e.c."', add
label define activity_lbl 029900 `"Household Activities, n.e.c."', add
label define activity_lbl 029999 `"Household activities, n.e.c."', add
label define activity_lbl 030000 `"Caring for and Helping Household Members"', add
label define activity_lbl 030100 `"Caring for and Helping Household Children"', add
label define activity_lbl 030101 `"Physical care for hh children"', add
label define activity_lbl 030102 `"Reading to/with hh children"', add
label define activity_lbl 030103 `"Playing with hh children, not sports"', add
label define activity_lbl 030104 `"Arts and crafts with hh children"', add
label define activity_lbl 030105 `"Playing sports with hh children"', add
label define activity_lbl 030106 `"Talking with/listening to hh children"', add
label define activity_lbl 030107 `"Helping or teaching hh children"', add
label define activity_lbl 030108 `"Organization and planning for hh children"', add
label define activity_lbl 030109 `"Looking after hh children (as a primary activity)"', add
label define activity_lbl 030110 `"Attending hh children's events"', add
label define activity_lbl 030111 `"Waiting for/with hh children"', add
label define activity_lbl 030112 `"Picking up/dropping off hh children"', add
label define activity_lbl 030199 `"Caring for and helping hh children, n.e.c."', add
label define activity_lbl 030200 `"Activities Related to Household Children's Education"', add
label define activity_lbl 030201 `"Homework (hh children)"', add
label define activity_lbl 030202 `"Meetings and school conferences (hh children)"', add
label define activity_lbl 030203 `"Home schooling of hh children"', add
label define activity_lbl 030204 `"Waiting associated with hh children's education"', add
label define activity_lbl 030299 `"Activities related to hh child's education, n.e.c."', add
label define activity_lbl 030300 `"Activities Related to Household Children's Health"', add
label define activity_lbl 030301 `"Providing medical care to hh children"', add
label define activity_lbl 030302 `"Obtaining medical care for hh children"', add
label define activity_lbl 030303 `"Waiting associated with hh children's health"', add
label define activity_lbl 030399 `"Activities related to hh child's health, n.e.c."', add
label define activity_lbl 030400 `"Caring for Household Adults"', add
label define activity_lbl 030401 `"Physical care for hh adults"', add
label define activity_lbl 030402 `"Looking after hh adult (as a primary activity)"', add
label define activity_lbl 030403 `"Providing medical care to hh adult"', add
label define activity_lbl 030404 `"Obtaining medical and care services for hh adult"', add
label define activity_lbl 030405 `"Waiting associated with caring for hh adults"', add
label define activity_lbl 030499 `"Caring for household adults, n.e.c."', add
label define activity_lbl 030500 `"Helping Household Adults"', add
label define activity_lbl 030501 `"Helping hh adults"', add
label define activity_lbl 030502 `"Organization and planning for hh adults"', add
label define activity_lbl 030503 `"Picking up/dropping off hh adult"', add
label define activity_lbl 030504 `"Waiting associated with helping hh adults"', add
label define activity_lbl 030599 `"Helping household adults, n.e.c."', add
label define activity_lbl 039900 `"Caring for and Helping Household Members, n.e.c."', add
label define activity_lbl 039999 `"Caring for and helping hh members, n.e.c."', add
label define activity_lbl 040000 `"Caring for and Helping Non-Household Members"', add
label define activity_lbl 040100 `"Caring for and Helping Non-Household Children"', add
label define activity_lbl 040101 `"Physical care for nonhh children"', add
label define activity_lbl 040102 `"Reading to/with nonhh children"', add
label define activity_lbl 040103 `"Playing with nonhh children, not sports"', add
label define activity_lbl 040104 `"Arts and crafts with nonhh children"', add
label define activity_lbl 040105 `"Playing sports with nonhh children"', add
label define activity_lbl 040106 `"Talking with/listening to nonhh children"', add
label define activity_lbl 040107 `"Helping or teaching nonhh children"', add
label define activity_lbl 040108 `"Organization and planning for nonhh children"', add
label define activity_lbl 040109 `"Looking after nonhh children (as primary activity)"', add
label define activity_lbl 040110 `"Attending nonhh children's events"', add
label define activity_lbl 040111 `"Waiting for/with nonhh children"', add
label define activity_lbl 040112 `"Dropping off/picking up nonhh children"', add
label define activity_lbl 040199 `"Caring for and helping nonhh children, n.e.c."', add
label define activity_lbl 040200 `"Activities Related to Non-Household Children's Education"', add
label define activity_lbl 040201 `"Homework (nonhh children)"', add
label define activity_lbl 040202 `"Mtgs and school conferences (nonhh children)"', add
label define activity_lbl 040203 `"Home schooling of nonhh children"', add
label define activity_lbl 040204 `"Waiting assoc w/ nonhh children's education"', add
label define activity_lbl 040299 `"Activities related to nonhh child's educ., n.e.c."', add
label define activity_lbl 040300 `"Activities Related to Non-Household Children's Health"', add
label define activity_lbl 040301 `"Providing medical care to nonhh children"', add
label define activity_lbl 040302 `"Obtaining medical care for nonhh children"', add
label define activity_lbl 040303 `"Waiting associated with nonhh children's health"', add
label define activity_lbl 040399 `"Activities related to nonhh child's health, n.e.c."', add
label define activity_lbl 040400 `"Caring for Non-Household Adults"', add
label define activity_lbl 040401 `"Physical care for nonhh adults"', add
label define activity_lbl 040402 `"Looking after nonhh adult (as a primary activity)"', add
label define activity_lbl 040403 `"Providing medical care to nonhh adult"', add
label define activity_lbl 040404 `"Obtaining medical and care svcs for nonhh adult"', add
label define activity_lbl 040405 `"Waiting associated with caring for nonhh adults"', add
label define activity_lbl 040499 `"Caring for nonhh adults, n.e.c."', add
label define activity_lbl 040500 `"Helping Non-Household Adults"', add
label define activity_lbl 040501 `"Hswrk, cooking, and shopping asst, nonhh adults"', add
label define activity_lbl 040502 `"House and lawn maint and repair asst, nonhh adults"', add
label define activity_lbl 040503 `"Animal and pet care assistance for nonhh adults"', add
label define activity_lbl 040504 `"Vehicle/appliance maint/repair asst, nonhh adults"', add
label define activity_lbl 040505 `"Financial mgmt asst for nonhh adults"', add
label define activity_lbl 040506 `"HH mgmt and paperwork asst, nonhh adults"', add
label define activity_lbl 040507 `"Picking up/dropping off nonhh adult"', add
label define activity_lbl 040508 `"Waiting associated with helping nonhh adults"', add
label define activity_lbl 040599 `"Helping nonhh adults, n.e.c."', add
label define activity_lbl 049900 `"Caring for and Helping Non-Household Members, n.e.c."', add
label define activity_lbl 049999 `"Caring for and helping nonhh members, n.e.c."', add
label define activity_lbl 050000 `"Work and Work-Related Activities"', add
label define activity_lbl 050100 `"Working"', add
label define activity_lbl 050101 `"Work, main job"', add
label define activity_lbl 050102 `"Work, other job(s)"', add
label define activity_lbl 050103 `"Security procedures related to work"', add
label define activity_lbl 050104 `"Waiting associated with working"', add
label define activity_lbl 050199 `"Working, n.e.c."', add
label define activity_lbl 050200 `"Work-Related Activities"', add
label define activity_lbl 050201 `"Socializing, relaxing, and leisure as part of job"', add
label define activity_lbl 050202 `"Eating and drinking as part of job"', add
label define activity_lbl 050203 `"Sports and exercise as part of job"', add
label define activity_lbl 050204 `"Security procedures as part of job"', add
label define activity_lbl 050205 `"Waiting associated with work-related activities"', add
label define activity_lbl 050299 `"Work-related activities, n.e.c."', add
label define activity_lbl 050300 `"Other Income-Generating Activities"', add
label define activity_lbl 050301 `"Income-generating hobbies, crafts, and food"', add
label define activity_lbl 050302 `"Income-generating performances"', add
label define activity_lbl 050303 `"Income-generating services"', add
label define activity_lbl 050304 `"Income-generating rental property activities"', add
label define activity_lbl 050305 `"Waiting assoc w/other income-generating acts"', add
label define activity_lbl 050399 `"Other income-generating activities, n.e.c."', add
label define activity_lbl 050400 `"Job Search and Interviewing"', add
label define activity_lbl 050401 `"Job search activities"', add
label define activity_lbl 050403 `"Job interviewing"', add
label define activity_lbl 050404 `"Waiting associated with job search or interview"', add
label define activity_lbl 050405 `"Sec. procedures rel. to job search/interviewing"', add
label define activity_lbl 050499 `"Job search and Interviewing, n.e.c."', add
label define activity_lbl 059900 `"Work and Work-Related Activities, n.e.c."', add
label define activity_lbl 059999 `"Work and work-related activities, n.e.c."', add
label define activity_lbl 060000 `"Education"', add
label define activity_lbl 060100 `"Taking Class"', add
label define activity_lbl 060101 `"Taking class for degree, certification, or lic"', add
label define activity_lbl 060102 `"Taking class for personal interest"', add
label define activity_lbl 060103 `"Waiting associated with taking classes"', add
label define activity_lbl 060104 `"Security procedures rel. to taking classes"', add
label define activity_lbl 060199 `"Taking class, n.e.c."', add
label define activity_lbl 060200 `"Extracurricular School Activities (except sports)"', add
label define activity_lbl 060201 `"Extracurricular club activities"', add
label define activity_lbl 060202 `"Extracurricular music and performance activities"', add
label define activity_lbl 060203 `"Extracurricular student government activities"', add
label define activity_lbl 060204 `"Waiting associated with extracurricular activities"', add
label define activity_lbl 060299 `"Education-related extracurricular activities, n.e.c."', add
label define activity_lbl 060300 `"Research or Homework"', add
label define activity_lbl 060301 `"Rsrch/HW for class for degree, cert, or lic"', add
label define activity_lbl 060302 `"Research/homework for class for pers. interest"', add
label define activity_lbl 060303 `"Waiting associated with research/homework"', add
label define activity_lbl 060399 `"Research/homework n.e.c."', add
label define activity_lbl 060400 `"Registration or Administrative Activities"', add
label define activity_lbl 060401 `"Admin activities: class for degree, cert, or lic"', add
label define activity_lbl 060402 `"Admin activities: class for personal interest"', add
label define activity_lbl 060403 `"Waiting assoc w/admin. activities (education)"', add
label define activity_lbl 060499 `"Administrative for education, n.e.c."', add
label define activity_lbl 069900 `"Education, n.e.c."', add
label define activity_lbl 069999 `"Education, n.e.c."', add
label define activity_lbl 070000 `"Consumer Purchases"', add
label define activity_lbl 070100 `"Shopping (store, telephone, internet)"', add
label define activity_lbl 070101 `"Grocery shopping"', add
label define activity_lbl 070102 `"Purchasing gas"', add
label define activity_lbl 070103 `"Purchasing food (not groceries)"', add
label define activity_lbl 070104 `"Shopping, except groceries, food and gas"', add
label define activity_lbl 070105 `"Waiting associated with shopping"', add
label define activity_lbl 070199 `"Shopping, n.e.c."', add
label define activity_lbl 070200 `"Researching Purchases"', add
label define activity_lbl 070201 `"Comparison shopping"', add
label define activity_lbl 070299 `"Researching purchases, n.e.c."', add
label define activity_lbl 070300 `"Security Procedures Related to Consumer Purchases"', add
label define activity_lbl 070301 `"Security procedures rel. to consumer purchases"', add
label define activity_lbl 070399 `"Sec procedures rel. to cons purchases, n.e.c."', add
label define activity_lbl 079900 `"Consumer Purchases, n.e.c."', add
label define activity_lbl 079999 `"Consumer purchases, n.e.c."', add
label define activity_lbl 080000 `"Professional and Personal Care Services"', add
label define activity_lbl 080100 `"Childcare Services"', add
label define activity_lbl 080101 `"Using paid childcare services"', add
label define activity_lbl 080102 `"Waiting associated w/purchasing childcare svcs"', add
label define activity_lbl 080199 `"Using paid childcare services, n.e.c."', add
label define activity_lbl 080200 `"Financial Services and Banking"', add
label define activity_lbl 080201 `"Banking"', add
label define activity_lbl 080202 `"Using other financial services"', add
label define activity_lbl 080203 `"Waiting associated w/banking/financial services"', add
label define activity_lbl 080299 `"Using financial services and banking, n.e.c."', add
label define activity_lbl 080300 `"Legal Services"', add
label define activity_lbl 080301 `"Using legal services"', add
label define activity_lbl 080302 `"Waiting associated with legal services"', add
label define activity_lbl 080399 `"Using legal services, n.e.c."', add
label define activity_lbl 080400 `"Medical and Care Services"', add
label define activity_lbl 080401 `"Using health and care services outside the home"', add
label define activity_lbl 080402 `"Using in-home health and care services"', add
label define activity_lbl 080403 `"Waiting associated with medical services"', add
label define activity_lbl 080499 `"Using medical services, n.e.c."', add
label define activity_lbl 080500 `"Personal Care Services"', add
label define activity_lbl 080501 `"Using personal care services"', add
label define activity_lbl 080502 `"Waiting associated w/personal care services"', add
label define activity_lbl 080599 `"Using personal care services, n.e.c."', add
label define activity_lbl 080600 `"Real Estate"', add
label define activity_lbl 080601 `"Activities rel. to purchasing/selling real estate"', add
label define activity_lbl 080602 `"Waiting assoc w/purchasing/selling real estate"', add
label define activity_lbl 080699 `"Using real estate services, n.e.c."', add
label define activity_lbl 080700 `"Veterinary Services (excluding grooming)"', add
label define activity_lbl 080701 `"Using veterinary services"', add
label define activity_lbl 080702 `"Waiting associated with veterinary services"', add
label define activity_lbl 080799 `"Using veterinary services, n.e.c."', add
label define activity_lbl 080800 `"Security Procedures Related to Professional or Personal Services"', add
label define activity_lbl 080801 `"Security procedures rel. to prof/personal svcs."', add
label define activity_lbl 080899 `"Sec procedures rel. to prof/personal svcs n.e.c."', add
label define activity_lbl 089900 `"Professional and Personal Services, n.e.c."', add
label define activity_lbl 089999 `"Professional and personal services, n.e.c."', add
label define activity_lbl 090000 `"Household Services"', add
label define activity_lbl 090100 `"Household Services (not done by self)"', add
label define activity_lbl 090101 `"Using interior cleaning services"', add
label define activity_lbl 090102 `"Using meal preparation services"', add
label define activity_lbl 090103 `"Using clothing repair and cleaning services"', add
label define activity_lbl 090104 `"Waiting associated with using household services"', add
label define activity_lbl 090199 `"Using household services, n.e.c."', add
label define activity_lbl 090200 `"Home Maintenance, Repair, Decoration, and Construction (not done by self)"', add
label define activity_lbl 090201 `"Using home maint/repair/décor/construction svcs"', add
label define activity_lbl 090202 `"Wtg assoc w/ home main/repair/décor/constr"', add
label define activity_lbl 090299 `"Using home maint/repair/décor/constr svcs n.e.c."', add
label define activity_lbl 090300 `"Pet Services (not done by self and not veterinary care)"', add
label define activity_lbl 090301 `"Using pet services"', add
label define activity_lbl 090302 `"Waiting associated with pet services"', add
label define activity_lbl 090399 `"Using pet services, n.e.c."', add
label define activity_lbl 090400 `"Lawn and Garden Services (not done by self)"', add
label define activity_lbl 090401 `"Using lawn and garden services"', add
label define activity_lbl 090402 `"Waiting assoc with using lawn and garden svcs"', add
label define activity_lbl 090499 `"Using lawn and garden services, n.e.c."', add
label define activity_lbl 090500 `"Vehicle Maintenance and Repair Services (not done by self)"', add
label define activity_lbl 090501 `"Using vehicle maintenance or repair services"', add
label define activity_lbl 090502 `"Waiting assoc with vehicle main. or repair svcs"', add
label define activity_lbl 090599 `"Using vehicle maint. and repair svcs, n.e.c."', add
label define activity_lbl 099900 `"Household Services, n.e.c."', add
label define activity_lbl 099999 `"Using household services, n.e.c."', add
label define activity_lbl 100000 `"Government Services and Civic Obligations"', add
label define activity_lbl 100100 `"Using Government Services"', add
label define activity_lbl 100101 `"Using police and fire services"', add
label define activity_lbl 100102 `"Using social services"', add
label define activity_lbl 100103 `"Obtaining licenses and paying fines, fees, taxes"', add
label define activity_lbl 100199 `"Using government services, n.e.c."', add
label define activity_lbl 100200 `"Civic Obligations and Participation"', add
label define activity_lbl 100201 `"Civic obligations and participation"', add
label define activity_lbl 100299 `"Civic obligations and participation, n.e.c."', add
label define activity_lbl 100300 `"Waiting Associated with Government Services or Civic Obligations"', add
label define activity_lbl 100303 `"Waiting assoc w/civic obligations and participation"', add
label define activity_lbl 100304 `"Waiting associated with using government services"', add
label define activity_lbl 100399 `"Wtg assoc w/govt svcs or civic obligations, n.e.c."', add
label define activity_lbl 100400 `"Security Procedures Related to Government Services or Civic Obligations"', add
label define activity_lbl 100401 `"Sec procedures rel. to govt svcs/civic obligations"', add
label define activity_lbl 100499 `"Sec procs rel. to govt svcs/civic obligations, n.e.c."', add
label define activity_lbl 109900 `"Government Services, n.e.c."', add
label define activity_lbl 109999 `"Government services, n.e.c."', add
label define activity_lbl 110000 `"Eating and Drinking"', add
label define activity_lbl 110100 `"Eating and Drinking"', add
label define activity_lbl 110101 `"Eating and drinking"', add
label define activity_lbl 110199 `"Eating and drinking, n.e.c."', add
label define activity_lbl 110200 `"Waiting Associated with Eating and Drinking"', add
label define activity_lbl 110201 `"Waiting associated w/eating and drinking"', add
label define activity_lbl 110299 `"Waiting associated with eating and drinking, n.e.c."', add
label define activity_lbl 119900 `"Eating and Drinking, n.e.c."', add
label define activity_lbl 119999 `"Eating and drinking, n.e.c."', add
label define activity_lbl 120000 `"Socializing, Relaxing, and Leisure"', add
label define activity_lbl 120100 `"Socializing and Communicating"', add
label define activity_lbl 120101 `"Socializing and communicating with others"', add
label define activity_lbl 120199 `"Socializing and communicating, n.e.c."', add
label define activity_lbl 120200 `"Attending or Hosting Social Events"', add
label define activity_lbl 120201 `"Attending or hosting parties/receptions/ceremonies"', add
label define activity_lbl 120202 `"Attending meetings for personal interest (not volunteering)"', add
label define activity_lbl 120299 `"Attending/hosting social events, n.e.c."', add
label define activity_lbl 120300 `"Relaxing and Leisure"', add
label define activity_lbl 120301 `"Relaxing, thinking"', add
label define activity_lbl 120302 `"Tobacco and drug use"', add
label define activity_lbl 120303 `"Television and movies (not religious)"', add
label define activity_lbl 120304 `"Television (religious)"', add
label define activity_lbl 120305 `"Listening to the radio"', add
label define activity_lbl 120306 `"Listening to/playing music (not radio)"', add
label define activity_lbl 120307 `"Playing games"', add
label define activity_lbl 120308 `"Computer use for leisure (exc. Games)"', add
label define activity_lbl 120309 `"Arts and crafts as a hobby"', add
label define activity_lbl 120310 `"Collecting as a hobby"', add
label define activity_lbl 120311 `"Hobbies, except arts and crafts and collecting"', add
label define activity_lbl 120312 `"Reading for personal interest"', add
label define activity_lbl 120313 `"Writing for personal interest"', add
label define activity_lbl 120399 `"Relaxing and leisure, n.e.c."', add
label define activity_lbl 120400 `"Arts and Entertainment (other than sports)"', add
label define activity_lbl 120401 `"Attending performing arts"', add
label define activity_lbl 120402 `"Attending museums"', add
label define activity_lbl 120403 `"Attending movies/film"', add
label define activity_lbl 120404 `"Attending gambling establishments"', add
label define activity_lbl 120405 `"Security procedures rel. to arts and entertainment"', add
label define activity_lbl 120499 `"Arts and entertainment, n.e.c."', add
label define activity_lbl 120500 `"Waiting Associated with Socializing, Relaxing, and Leisure"', add
label define activity_lbl 120501 `"Waiting assoc. w/socializing and communicating"', add
label define activity_lbl 120502 `"Waiting assoc. w/attending/hosting social events"', add
label define activity_lbl 120503 `"Waiting associated with relaxing/leisure"', add
label define activity_lbl 120504 `"Waiting associated with arts and entertainment"', add
label define activity_lbl 120599 `"Waiting associated with socializing, n.e.c."', add
label define activity_lbl 129900 `"Socializing, Relaxing, and Leisure, n.e.c."', add
label define activity_lbl 129999 `"Socializing, relaxing, and leisure, n.e.c."', add
label define activity_lbl 130000 `"Sports, Exercise, and Recreation"', add
label define activity_lbl 130100 `"Participating in Sports, Exercise, or Recreation"', add
label define activity_lbl 130101 `"Doing aerobics"', add
label define activity_lbl 130102 `"Playing baseball"', add
label define activity_lbl 130103 `"Playing basketball"', add
label define activity_lbl 130104 `"Biking"', add
label define activity_lbl 130105 `"Playing billiards"', add
label define activity_lbl 130106 `"Boating"', add
label define activity_lbl 130107 `"Bowling"', add
label define activity_lbl 130108 `"Climbing, spelunking, caving"', add
label define activity_lbl 130109 `"Dancing"', add
label define activity_lbl 130110 `"Participating in equestrian sports"', add
label define activity_lbl 130111 `"Fencing"', add
label define activity_lbl 130112 `"Fishing"', add
label define activity_lbl 130113 `"Playing football"', add
label define activity_lbl 130114 `"Golfing"', add
label define activity_lbl 130115 `"Doing gymnastics"', add
label define activity_lbl 130116 `"Hiking"', add
label define activity_lbl 130117 `"Playing hockey"', add
label define activity_lbl 130118 `"Hunting"', add
label define activity_lbl 130119 `"Participating in martial arts"', add
label define activity_lbl 130120 `"Playing racquet sports"', add
label define activity_lbl 130121 `"Participating in rodeo competitions"', add
label define activity_lbl 130122 `"Rollerblading"', add
label define activity_lbl 130123 `"Playing rugby"', add
label define activity_lbl 130124 `"Running"', add
label define activity_lbl 130125 `"Skiing, ice skating, snowboarding"', add
label define activity_lbl 130126 `"Playing soccer"', add
label define activity_lbl 130127 `"Playing softball"', add
label define activity_lbl 130128 `"Using cardiovascular equipment"', add
label define activity_lbl 130129 `"Vehicle touring/racing"', add
label define activity_lbl 130130 `"Playing volleyball"', add
label define activity_lbl 130131 `"Walking"', add
label define activity_lbl 130132 `"Participating in water sports"', add
label define activity_lbl 130133 `"Weightlifting/strength training"', add
label define activity_lbl 130134 `"Working out, unspecified"', add
label define activity_lbl 130135 `"Wrestling"', add
label define activity_lbl 130136 `"Doing yoga"', add
label define activity_lbl 130199 `"Playing sports n.e.c."', add
label define activity_lbl 130200 `"Attending Sports or Recreational Events"', add
label define activity_lbl 130201 `"Watching aerobics"', add
label define activity_lbl 130202 `"Watching baseball"', add
label define activity_lbl 130203 `"Watching basketball"', add
label define activity_lbl 130204 `"Watching biking"', add
label define activity_lbl 130205 `"Watching billiards"', add
label define activity_lbl 130206 `"Watching boating"', add
label define activity_lbl 130207 `"Watching bowling"', add
label define activity_lbl 130208 `"Watching climbing, spelunking, caving"', add
label define activity_lbl 130209 `"Watching dancing"', add
label define activity_lbl 130210 `"Watching equestrian sports"', add
label define activity_lbl 130211 `"Watching fencing"', add
label define activity_lbl 130212 `"Watching fishing"', add
label define activity_lbl 130213 `"Watching football"', add
label define activity_lbl 130214 `"Watching golfing"', add
label define activity_lbl 130215 `"Watching gymnastics"', add
label define activity_lbl 130216 `"Watching hockey"', add
label define activity_lbl 130217 `"Watching martial arts"', add
label define activity_lbl 130218 `"Watching racquet sports"', add
label define activity_lbl 130219 `"Watching rodeo competitions"', add
label define activity_lbl 130220 `"Watching rollerblading"', add
label define activity_lbl 130221 `"Watching rugby"', add
label define activity_lbl 130222 `"Watching running"', add
label define activity_lbl 130223 `"Watching skiing, ice skating, snowboarding"', add
label define activity_lbl 130224 `"Watching soccer"', add
label define activity_lbl 130225 `"Watching softball"', add
label define activity_lbl 130226 `"Watching vehicle touring/racing"', add
label define activity_lbl 130227 `"Watching volleyball"', add
label define activity_lbl 130228 `"Watching walking"', add
label define activity_lbl 130229 `"Watching water sports"', add
label define activity_lbl 130230 `"Watching weightlifting/strength training"', add
label define activity_lbl 130231 `"Watching people working out, unspecified"', add
label define activity_lbl 130232 `"Watching wrestling"', add
label define activity_lbl 130299 `"Attending sporting events, n.e.c."', add
label define activity_lbl 130300 `"Waiting Associated with Sports, Exercise, and Recreation"', add
label define activity_lbl 130301 `"Waiting related to playing sports or exercising"', add
label define activity_lbl 130302 `"Waiting related to attending sporting events"', add
label define activity_lbl 130399 `"Wtg assoc with sports, exercise, and rec, n.e.c."', add
label define activity_lbl 130400 `"Security Procedures Related to Sports, Exercise, and Recreation"', add
label define activity_lbl 130401 `"Security related to playing sports or exercising"', add
label define activity_lbl 130402 `"Security related to attending sporting events"', add
label define activity_lbl 130499 `"Sec related to sports, exercise, and rec, n.e.c."', add
label define activity_lbl 139900 `"Sports, Exercise, and Recreation, n.e.c."', add
label define activity_lbl 139999 `"Sports, exercise, and recreation, n.e.c."', add
label define activity_lbl 140000 `"Religious and Spiritual Activities"', add
label define activity_lbl 140100 `"Religious or Spiritual Practices"', add
label define activity_lbl 140101 `"Attending religious services"', add
label define activity_lbl 140102 `"Participation in religious practices"', add
label define activity_lbl 140103 `"Waiting assoc w/religious and spiritual activities"', add
label define activity_lbl 140104 `"Sec procedures rel. to relig and spiritual activities"', add
label define activity_lbl 140105 `"Religious education activities (2007+)"', add
label define activity_lbl 149900 `"Religious and Spiritual Activities, n.e.c."', add
label define activity_lbl 149999 `"Religious and spiritual activities, n.e.c."', add
label define activity_lbl 150000 `"Volunteer Activities"', add
label define activity_lbl 150100 `"Administrative and Support Activities"', add
label define activity_lbl 150101 `"Computer use"', add
label define activity_lbl 150102 `"Organizing and preparing"', add
label define activity_lbl 150103 `"Reading"', add
label define activity_lbl 150104 `"Telephone calls (except hotline counseling)"', add
label define activity_lbl 150105 `"Writing"', add
label define activity_lbl 150106 `"Fundraising"', add
label define activity_lbl 150199 `"Administrative and support activities, n.e.c."', add
label define activity_lbl 150200 `"Social Service and Care Activities (except medical)"', add
label define activity_lbl 150201 `"Food preparation, presentation, clean-up"', add
label define activity_lbl 150202 `"Collecting and delivering clothing and other goods"', add
label define activity_lbl 150203 `"Providing care"', add
label define activity_lbl 150204 `"Teaching, leading, counseling, mentoring"', add
label define activity_lbl 150299 `"Social service and care activities, n.e.c."', add
label define activity_lbl 150300 `"Indoor and Outdoor Maintenance, Building, and Clean-Up Activities"', add
label define activity_lbl 150301 `"Building houses, wildlife sites, and other structures"', add
label define activity_lbl 150302 `"Indoor and outdoor maintenance, repair, and clean-up"', add
label define activity_lbl 150399 `"Indoor and outdoor maint, bldg and clean-up, n.e.c."', add
label define activity_lbl 150400 `"Participating in Performance and Cultural Activities"', add
label define activity_lbl 150401 `"Performing"', add
label define activity_lbl 150402 `"Serving at volunteer events and cultural activities"', add
label define activity_lbl 150499 `"Participating performance, cultural act., n.e.c."', add
label define activity_lbl 150500 `"Attending Meetings, Conferences, and Training"', add
label define activity_lbl 150501 `"Attending meetings, conferences, and training"', add
label define activity_lbl 150599 `"Attending mtgs conferences, and training, n.e.c."', add
label define activity_lbl 150600 `"Public Health and Safety Activities"', add
label define activity_lbl 150601 `"Public health activities"', add
label define activity_lbl 150602 `"Public safety activities"', add
label define activity_lbl 150699 `"Public health and safety activities, n.e.c."', add
label define activity_lbl 150700 `"Waiting Associated with Volunteer Activities"', add
label define activity_lbl 150701 `"Waiting associated with volunteer activities"', add
label define activity_lbl 150799 `"Waiting assoc with volunteer activities, n.e.c."', add
label define activity_lbl 150800 `"Security Procedures Related to Volunteer Activities"', add
label define activity_lbl 150801 `"Security procedures related to volunteer activities (2007+)"', add
label define activity_lbl 150899 `"Security proecdures related to voluteer activities, n.e.c. (2007+)"', add
label define activity_lbl 159900 `"Volunteer Activities, n.e.c."', add
label define activity_lbl 159999 `"Volunteer activities, n.e.c."', add
label define activity_lbl 160000 `"Telephone Calls"', add
label define activity_lbl 160100 `"Telephone Calls (to or from)"', add
label define activity_lbl 160101 `"Telephone calls to/from family members"', add
label define activity_lbl 160102 `"Phone calls to/from friends, neighbors"', add
label define activity_lbl 160103 `"Telephone calls to/from education svcs providers"', add
label define activity_lbl 160104 `"Telephone calls to/from salespeople"', add
label define activity_lbl 160105 `"Phone calls to/from prof, personal svcs providers"', add
label define activity_lbl 160106 `"Phone calls to/from household services providers"', add
label define activity_lbl 160107 `"Phone calls to/from child or adult care providers"', add
label define activity_lbl 160108 `"Telephone calls to/from government officials"', add
label define activity_lbl 160199 `"Telephone calls (to or from), n.e.c."', add
label define activity_lbl 160200 `"Waiting Associated with Telephone Calls"', add
label define activity_lbl 160201 `"Waiting associated with telephone calls"', add
label define activity_lbl 160299 `"Waiting associated with telephone calls, n.e.c."', add
label define activity_lbl 169900 `"Telephone Calls, n.e.c."', add
label define activity_lbl 169999 `"Telephone calls, n.e.c."', add
label define activity_lbl 180000 `"Traveling"', add
label define activity_lbl 180100 `"Travel Related to Personal Care"', add
label define activity_lbl 180101 `"Travel related to personal care"', add
label define activity_lbl 180199 `"Travel related to personal care, n.e.c."', add
label define activity_lbl 180200 `"Travel Related to Household Activities"', add
label define activity_lbl 180201 `"Travel related to housework"', add
label define activity_lbl 180202 `"Travel related to food and drink prep"', add
label define activity_lbl 180203 `"Travel related to int. maint, repair, and decoration"', add
label define activity_lbl 180204 `"Travel related to ext. maint, repair, and decoration"', add
label define activity_lbl 180205 `"Travel related to lawn, garden, and houseplants"', add
label define activity_lbl 180206 `"Travel related to care for animals (not vet care)"', add
label define activity_lbl 180207 `"Travel related to vehicle care and maint (by self)"', add
label define activity_lbl 180208 `"Trvl rel to app, tool, toy set-up, repair, and maint"', add
label define activity_lbl 180209 `"Travel related to household management"', add
label define activity_lbl 180299 `"Travel related to household activities, n.e.c."', add
label define activity_lbl 180300 `"Travel Related to Caring for and Helping Household Members"', add
label define activity_lbl 180301 `"Travel related to caring for and helping HH children"', add
label define activity_lbl 180302 `"Travel related to caring for and helping household children (2005+)"', add
label define activity_lbl 180303 `"Travel related to hh children's education"', add
label define activity_lbl 180304 `"Travel related to hh children's health"', add
label define activity_lbl 180305 `"Travel related to caring for hh adults"', add
label define activity_lbl 180306 `"Travel related to helping hh adults"', add
label define activity_lbl 180307 `"Travel related to caring for and helping HH adults"', add
label define activity_lbl 180399 `"Trvl rel. to caring for, helping HH members, n.e.c."', add
label define activity_lbl 180400 `"Travel Related to Caring for and Helping Non-Household Members"', add
label define activity_lbl 180401 `"Trvl rel to caring for and helping nonHH kids, inclusive"', add
label define activity_lbl 180402 `"Trvl rel to caring for and helping nonHH kids"', add
label define activity_lbl 180403 `"Travel related to nonhh children's education"', add
label define activity_lbl 180404 `"Travel related to nonhh children's health"', add
label define activity_lbl 180405 `"Travel related to caring for nonhh adults"', add
label define activity_lbl 180406 `"Travel related to helping nonhh adults"', add
label define activity_lbl 180407 `"Travel related to caring for, helping NonHH adults"', add
label define activity_lbl 180499 `"Trvl rel. to caring for, helping NonHH,  n.e.c."', add
label define activity_lbl 180500 `"Travel Related to Work"', add
label define activity_lbl 180501 `"Travel related to working"', add
label define activity_lbl 180502 `"Travel related to work-related activities"', add
label define activity_lbl 180503 `"Travel related to income-generating activities"', add
label define activity_lbl 180504 `"Travel related to job search and interviewing"', add
label define activity_lbl 180599 `"Travel related to work, n.e.c."', add
label define activity_lbl 180600 `"Travel Related to Education"', add
label define activity_lbl 180601 `"Travel related to taking class"', add
label define activity_lbl 180602 `"Trvl rel to extracurricular activities (ex. Sports)"', add
label define activity_lbl 180603 `"Travel related to research/homework"', add
label define activity_lbl 180604 `"Travel related to registration/admin activities"', add
label define activity_lbl 180605 `"Education-related travel, not commuting"', add
label define activity_lbl 180699 `"Education travel, n.e.c."', add
label define activity_lbl 180700 `"Travel Related to Consumer Purchases"', add
label define activity_lbl 180701 `"Traveling to/from the grocery store"', add
label define activity_lbl 180702 `"Travel related to other shopping"', add
label define activity_lbl 180703 `"Travel related to purchasing food (not groceries)"', add
label define activity_lbl 180704 `"Travel related to shopping, ex groc, food, gas"', add
label define activity_lbl 180705 `"Traveling to/from gas station"', add
label define activity_lbl 180799 `"Travel related to consumer purchases, n.e.c."', add
label define activity_lbl 180800 `"Travel Related to Using Professional and Personal Care Services"', add
label define activity_lbl 180801 `"Travel related to using childcare services"', add
label define activity_lbl 180802 `"Travel related to using financial svcs and banking"', add
label define activity_lbl 180803 `"Travel related to using legal services"', add
label define activity_lbl 180804 `"Travel related to using medical services"', add
label define activity_lbl 180805 `"Travel related to using personal care services"', add
label define activity_lbl 180806 `"Travel related to using real estate services"', add
label define activity_lbl 180807 `"Travel related to using veterinary services"', add
label define activity_lbl 180899 `"Travel rel. to using prof, personal care svcs n.e.c."', add
label define activity_lbl 180900 `"Travel Related to Using Household Services"', add
label define activity_lbl 180901 `"Travel related to using household services"', add
label define activity_lbl 180902 `"Trvl rel to using home maint etc svcs"', add
label define activity_lbl 180903 `"Travel related to using pet services (not vet)"', add
label define activity_lbl 180904 `"Travel related to using lawn and garden services"', add
label define activity_lbl 180905 `"Trvl rel to using vehicle maint and repair services"', add
label define activity_lbl 180999 `"Travel related to using household services, n.e.c."', add
label define activity_lbl 181000 `"Travel Related to Using Government Services and Civic Obligations"', add
label define activity_lbl 181001 `"Travel related to using government services"', add
label define activity_lbl 181002 `"Travel related to civic obligations and participation"', add
label define activity_lbl 181099 `"Travel rel. to govt svcs and civic obligations, n.e.c."', add
label define activity_lbl 181100 `"Travel Related to Eating and Drinking"', add
label define activity_lbl 181101 `"Travel related to eating and drinking"', add
label define activity_lbl 181199 `"Travel related to eating and drinking, n.e.c."', add
label define activity_lbl 181200 `"Travel Related to Socializing, Relaxing, and Leisure"', add
label define activity_lbl 181201 `"Travel related to socializing and communicating"', add
label define activity_lbl 181202 `"Trvl related to attending or hosting social events"', add
label define activity_lbl 181203 `"Travel related to relaxing and leisure (2003, 2004)"', add
label define activity_lbl 181204 `"Travel related to arts and entertainment"', add
label define activity_lbl 181205 `"Travel as a form of entertainment"', add
label define activity_lbl 181206 `"Travel related to relaxing and leisure (2005+)"', add
label define activity_lbl 181299 `"Travel rel. to socializing, relaxing, leisure, n.e.c."', add
label define activity_lbl 181300 `"Travel Related to Sports, Exercise, and Recreation"', add
label define activity_lbl 181301 `"Trvl rel to doing sports/exercise/recreation"', add
label define activity_lbl 181302 `"Trvl rel to attending sporting/recreational events"', add
label define activity_lbl 181399 `"Travel rel to sports, exercise, recreation, n.e.c."', add
label define activity_lbl 181400 `"Travel Related to Religious or Spiritual Activities"', add
label define activity_lbl 181401 `"Travel related to religious/spiritual practices"', add
label define activity_lbl 181499 `"Travel rel. to religious/spiritual activities, n.e.c."', add
label define activity_lbl 181500 `"Travel Related to Volunteering"', add
label define activity_lbl 181501 `"Travel related to volunteering"', add
label define activity_lbl 181599 `"Travel related to volunteer activities, n.e.c."', add
label define activity_lbl 181600 `"Travel Related to Phone Calls"', add
label define activity_lbl 181601 `"Travel related to phone calls"', add
label define activity_lbl 181699 `"Travel rel. to phone calls, n.e.c."', add
label define activity_lbl 181800 `"Security Procedures Related to Traveling"', add
label define activity_lbl 181801 `"Security procedures related to traveling"', add
label define activity_lbl 181899 `"Security procedures related to traveling, n.e.c."', add
label define activity_lbl 189900 `"Traveling, n.e.c."', add
label define activity_lbl 189999 `"Traveling, n.e.c."', add
label define activity_lbl 500000 `"Data Codes"', add
label define activity_lbl 500100 `"Unable to Code"', add
label define activity_lbl 500101 `"Insufficient detail in verbatim"', add
label define activity_lbl 500102 `"Recorded activity using incorrect words"', add
label define activity_lbl 500103 `"Missing travel or destination"', add
label define activity_lbl 500104 `"Recorded simultaneous activities incorrectly"', add
label define activity_lbl 500105 `"Respondent refused to provide information"', add
label define activity_lbl 500106 `"Gap/can't remember"', add
label define activity_lbl 500107 `"Unable to code activity at 1st tier"', add
label define activity_lbl 509900 `"Data codes, n.e.c."', add
label define activity_lbl 509999 `"Data codes, n.e.c."', add
label values activity activity_lbl

label define where_lbl 0100 `"Place"'
label define where_lbl 0101 `"R's home or yard"', add
label define where_lbl 0102 `"R's workplace"', add
label define where_lbl 0103 `"Someone else's home"', add
label define where_lbl 0104 `"Restaurant or bar"', add
label define where_lbl 0105 `"Place of worship"', add
label define where_lbl 0106 `"Grocery store"', add
label define where_lbl 0107 `"Other store/mall"', add
label define where_lbl 0108 `"School"', add
label define where_lbl 0109 `"Outdoors--not at home"', add
label define where_lbl 0110 `"Library"', add
label define where_lbl 0111 `"Bank"', add
label define where_lbl 0112 `"Gym/health club"', add
label define where_lbl 0113 `"Post Office"', add
label define where_lbl 0114 `"Other place"', add
label define where_lbl 0115 `"Unspecified place"', add
label define where_lbl 0200 `"Mode of Transportation"', add
label define where_lbl 0230 `"Driver of car, truck, or motorcycle"', add
label define where_lbl 0231 `"Passenger of car, truck, or motorcycle"', add
label define where_lbl 0232 `"Walking"', add
label define where_lbl 0233 `"Bus"', add
label define where_lbl 0234 `"Subway/train"', add
label define where_lbl 0235 `"Bicycle"', add
label define where_lbl 0236 `"Boat/ferry"', add
label define where_lbl 0237 `"Taxi/limousine service"', add
label define where_lbl 0238 `"Airplane"', add
label define where_lbl 0239 `"Other mode of transportation"', add
label define where_lbl 0240 `"Unspecified mode of transportation"', add
label define where_lbl 9997 `"Don't know"', add
label define where_lbl 9998 `"Refused"', add
label define where_lbl 9999 `"NIU (Not in universe)"', add
label values where where_lbl

label define relatewu_lbl 0100 `"Alone"'
label define relatewu_lbl 0200 `"Spouse"', add
label define relatewu_lbl 0201 `"Unmarried partner"', add
label define relatewu_lbl 0202 `"Own household child"', add
label define relatewu_lbl 0203 `"Grandchild"', add
label define relatewu_lbl 0204 `"Parent"', add
label define relatewu_lbl 0205 `"Brother sister"', add
label define relatewu_lbl 0206 `"Other related person"', add
label define relatewu_lbl 0207 `"Foster child"', add
label define relatewu_lbl 0208 `"Housemate, roommate"', add
label define relatewu_lbl 0209 `"Roomer, boarder"', add
label define relatewu_lbl 0210 `"Other nonrelative"', add
label define relatewu_lbl 0300 `"Own non-household child under 18"', add
label define relatewu_lbl 0400 `"Parents (not living in household)"', add
label define relatewu_lbl 0401 `"Other non-household family members under 18"', add
label define relatewu_lbl 0402 `"Other non-household family members 18 and older (including parents-in-law)"', add
label define relatewu_lbl 0403 `"Friends"', add
label define relatewu_lbl 0404 `"Co-workers, colleagues, clients (non-work activities only)"', add
label define relatewu_lbl 0405 `"Neighbors, acquaintances"', add
label define relatewu_lbl 0406 `"Other non-household children under 18"', add
label define relatewu_lbl 0407 `"Other non-household adults 18 and older"', add
label define relatewu_lbl 0408 `"Boss or manager"', add
label define relatewu_lbl 0409 `"People whom I supervise"', add
label define relatewu_lbl 0410 `"Co-workers"', add
label define relatewu_lbl 0411 `"Customers"', add
label define relatewu_lbl 9996 `"Refused"', add
label define relatewu_lbl 9997 `"Don't know"', add
label define relatewu_lbl 9998 `"Blank"', add
label values relatewu relatewu_lbl

save "atus_hierarchy", replace


