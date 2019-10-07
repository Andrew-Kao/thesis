* NOTE: You need to edit the `cd` command to specify the path to the directory
* where the data file is located. For example: "C:\ipums_directory".
* .

cd ".".

data list file = "nhgis0002_ds120_1990_county.dat" /
  YEAR       1-4 (a)
  ANRCA      5-6 (a)
  AIANHHA    7-10 (a)
  RES_ONLYA  11-14 (a)
  TRUSTA     15-18 (a)
  RES_TRSTA  19-19 (a)
  BLOCKA     20-23 (a)
  BLCK_GRPA  24-24 (a)
  TRACTA     25-30 (a)
  CDA        31-32 (a)
  C_CITYA    33-37 (a)
  COUNTY     38-94 (a)
  COUNTYA    95-97 (a)
  CTY_SUBA   98-102 (a)
  DIVISIONA  103-103 (a)
  MSA_CMSAA  104-107 (a)
  PLACEA     108-112 (a)
  PMSAA      113-116 (a)
  REGIONA    117-117 (a)
  STATE      118-141 (a)
  STATEA     142-143 (a)
  URBRURALA  144-144 (a)
  URB_AREAA  145-148 (a)
  CD103A     149-150 (a)
  ANPSADPI   151-216 (a)
  EU1001     217-225
  EU1002     226-234
  EU1003     235-243
  EU1004     244-252
  EU1005     253-261
.

variable labels
  YEAR         "Data File Year"
  ANRCA        "Alaska Native Regional Corporation Code"
  AIANHHA      "American Indian Area/Alaska Native Area/Hawaiian Home Land Code"
  RES_ONLYA    "American Indian Reservation [excluding trust lands] Code"
  TRUSTA       "American Indian Reservation [trust lands only] Code"
  RES_TRSTA    "Reservation/Trust Lands Code"
  BLOCKA       "Block Code"
  BLCK_GRPA    "Block Group Code"
  TRACTA       "Census Tract Code"
  CDA          "Congressional District (101st) Code"
  C_CITYA      "Consolidated City Code"
  COUNTY       "County Name"
  COUNTYA      "County Code"
  CTY_SUBA     "County Subdivision Code"
  DIVISIONA    "Division Code"
  MSA_CMSAA    "Metropolitan Statistical Area/Consolidated Metropolitan Statistical Area Code"
  PLACEA       "Place Code"
  PMSAA        "Primary Metropolitan Statistical Area Code"
  REGIONA      "Region Code"
  STATE        "State Name"
  STATEA       "State Code"
  URBRURALA    "Urban/Rural Code"
  URB_AREAA    "Urban Area Code"
  CD103A       "Congressional District (103rd) Code"
  ANPSADPI     "Area Name/PSAD Term/Part Indicator"
  EU1001       "Not of Hispanic origin"
  EU1002       "Hispanic origin: Mexican"
  EU1003       "Hispanic origin: Puerto Rican"
  EU1004       "Hispanic origin: Cuban"
  EU1005       "Hispanic origin: Other Hispanic"
.

execute.

