* NOTE: You need to edit the `cd` command to specify the path to the directory
* where the data file is located. For example: "C:\ipums_directory".
* .

cd ".".

data list file = "nhgis0002_ds123_1990_county.dat" /
  YEAR       1-4 (a)
  ANRCA      5-6 (a)
  AIANHHA    7-10 (a)
  RES_ONLYA  11-14 (a)
  TRUSTA     15-18 (a)
  RES_TRSTA  19-19 (a)
  BLCK_GRPA  20-20 (a)
  TRACTA     21-26 (a)
  CDA        27-28 (a)
  C_CITYA    29-33 (a)
  COUNTY     34-90 (a)
  COUNTYA    91-93 (a)
  CTY_SUBA   94-98 (a)
  DIVISIONA  99-99 (a)
  MSA_CMSAA  100-103 (a)
  PLACEA     104-108 (a)
  PMSAA      109-112 (a)
  REGIONA    113-113 (a)
  STATE      114-137 (a)
  STATEA     138-139 (a)
  URBRURALA  140-140 (a)
  URB_AREAA  141-144 (a)
  ZIPA       145-149 (a)
  CD103A     150-151 (a)
  ANPSADPI   152-217 (a)
  E03001     218-232
  E03002     233-247
  E03003     248-262
  E03004     263-277
  E03005     278-292
  E05001     293-307
.

variable labels
  YEAR         "Data File Year"
  ANRCA        "Alaska Native Regional Corporation Code"
  AIANHHA      "American Indian Area/Alaska Native Area/Hawaiian Home Land Code"
  RES_ONLYA    "American Indian Reservation [excluding trust lands] Code"
  TRUSTA       "American Indian Reservation [trust lands only] Code"
  RES_TRSTA    "Reservation/Trust Lands Code"
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
  ZIPA         "5-Digit ZIP Code Code"
  CD103A       "Congressional District (103rd) Code"
  ANPSADPI     "Area Name/PSAD Term/Part Indicator"
  E03001       "Aggregate income in 1989 >> White"
  E03002       "Aggregate income in 1989 >> Black"
  E03003       "Aggregate income in 1989 >> American Indian, Eskimo, or Aleut"
  E03004       "Aggregate income in 1989 >> Asian or Pacific Islander"
  E03005       "Aggregate income in 1989 >> Other race"
  E05001       "Aggregate income in 1989"
.

execute.

