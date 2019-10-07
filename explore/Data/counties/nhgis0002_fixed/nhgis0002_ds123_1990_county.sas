/*
   NOTE: You need to edit the `libname` command to specify the path to the directory
   where the data file is located. For example: "C:\ipums_directory".
   Edit the `filename` command similarly to include the full path (the directory and the data file name).
*/

libname IPUMS ".";
filename ASCIIDAT "nhgis0002_ds123_1990_county.dat";

data IPUMS.nhgis0002_ds123_1990_county;
infile ASCIIDAT pad missover lrecl=307;

input
  YEAR      $ 1-4
  ANRCA     $ 5-6
  AIANHHA   $ 7-10
  RES_ONLYA $ 11-14
  TRUSTA    $ 15-18
  RES_TRSTA $ 19-19
  BLCK_GRPA $ 20-20
  TRACTA    $ 21-26
  CDA       $ 27-28
  C_CITYA   $ 29-33
  COUNTY    $ 34-90
  COUNTYA   $ 91-93
  CTY_SUBA  $ 94-98
  DIVISIONA $ 99-99
  MSA_CMSAA $ 100-103
  PLACEA    $ 104-108
  PMSAA     $ 109-112
  REGIONA   $ 113-113
  STATE     $ 114-137
  STATEA    $ 138-139
  URBRURALA $ 140-140
  URB_AREAA $ 141-144
  ZIPA      $ 145-149
  CD103A    $ 150-151
  ANPSADPI  $ 152-217
  E03001      218-232
  E03002      233-247
  E03003      248-262
  E03004      263-277
  E03005      278-292
  E05001      293-307
;

label
  YEAR      = "Data File Year"
  ANRCA     = "Alaska Native Regional Corporation Code"
  AIANHHA   = "American Indian Area/Alaska Native Area/Hawaiian Home Land Code"
  RES_ONLYA = "American Indian Reservation [excluding trust lands] Code"
  TRUSTA    = "American Indian Reservation [trust lands only] Code"
  RES_TRSTA = "Reservation/Trust Lands Code"
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
  ZIPA      = "5-Digit ZIP Code Code"
  CD103A    = "Congressional District (103rd) Code"
  ANPSADPI  = "Area Name/PSAD Term/Part Indicator"
  E03001    = "Aggregate income in 1989 >> White"
  E03002    = "Aggregate income in 1989 >> Black"
  E03003    = "Aggregate income in 1989 >> American Indian, Eskimo, or Aleut"
  E03004    = "Aggregate income in 1989 >> Asian or Pacific Islander"
  E03005    = "Aggregate income in 1989 >> Other race"
  E05001    = "Aggregate income in 1989"
;

format
  E03001     15.
  E03002     15.
  E03003     15.
  E03004     15.
  E03005     15.
  E05001     15.
;

run;

