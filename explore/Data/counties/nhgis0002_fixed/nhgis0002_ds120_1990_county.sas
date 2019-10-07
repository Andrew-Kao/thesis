/*
   NOTE: You need to edit the `libname` command to specify the path to the directory
   where the data file is located. For example: "C:\ipums_directory".
   Edit the `filename` command similarly to include the full path (the directory and the data file name).
*/

libname IPUMS ".";
filename ASCIIDAT "nhgis0002_ds120_1990_county.dat";

data IPUMS.nhgis0002_ds120_1990_county;
infile ASCIIDAT pad missover lrecl=261;

input
  YEAR      $ 1-4
  ANRCA     $ 5-6
  AIANHHA   $ 7-10
  RES_ONLYA $ 11-14
  TRUSTA    $ 15-18
  RES_TRSTA $ 19-19
  BLOCKA    $ 20-23
  BLCK_GRPA $ 24-24
  TRACTA    $ 25-30
  CDA       $ 31-32
  C_CITYA   $ 33-37
  COUNTY    $ 38-94
  COUNTYA   $ 95-97
  CTY_SUBA  $ 98-102
  DIVISIONA $ 103-103
  MSA_CMSAA $ 104-107
  PLACEA    $ 108-112
  PMSAA     $ 113-116
  REGIONA   $ 117-117
  STATE     $ 118-141
  STATEA    $ 142-143
  URBRURALA $ 144-144
  URB_AREAA $ 145-148
  CD103A    $ 149-150
  ANPSADPI  $ 151-216
  EU1001      217-225
  EU1002      226-234
  EU1003      235-243
  EU1004      244-252
  EU1005      253-261
;

label
  YEAR      = "Data File Year"
  ANRCA     = "Alaska Native Regional Corporation Code"
  AIANHHA   = "American Indian Area/Alaska Native Area/Hawaiian Home Land Code"
  RES_ONLYA = "American Indian Reservation [excluding trust lands] Code"
  TRUSTA    = "American Indian Reservation [trust lands only] Code"
  RES_TRSTA = "Reservation/Trust Lands Code"
  BLOCKA    = "Block Code"
  BLCK_GRPA = "Block Group Code"
  TRACTA    = "Census Tract Code"
  CDA       = "Congressional District (101st) Code"
  C_CITYA   = "Consolidated City Code"
  COUNTY    = "County Name"
  COUNTYA   = "County Code"
  CTY_SUBA  = "County Subdivision Code"
  DIVISIONA = "Division Code"
  MSA_CMSAA = "Metropolitan Statistical Area/Consolidated Metropolitan Statistical Area Code"
  PLACEA    = "Place Code"
  PMSAA     = "Primary Metropolitan Statistical Area Code"
  REGIONA   = "Region Code"
  STATE     = "State Name"
  STATEA    = "State Code"
  URBRURALA = "Urban/Rural Code"
  URB_AREAA = "Urban Area Code"
  CD103A    = "Congressional District (103rd) Code"
  ANPSADPI  = "Area Name/PSAD Term/Part Indicator"
  EU1001    = "Not of Hispanic origin"
  EU1002    = "Hispanic origin: Mexican"
  EU1003    = "Hispanic origin: Puerto Rican"
  EU1004    = "Hispanic origin: Cuban"
  EU1005    = "Hispanic origin: Other Hispanic"
;

format
  EU1001     9.
  EU1002     9.
  EU1003     9.
  EU1004     9.
  EU1005     9.
;

run;

