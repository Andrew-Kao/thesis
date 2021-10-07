/*
   NOTE: You need to edit the `libname` command to specify the path to the directory
   where the data file is located. For example: "C:\ipums_directory".
   Edit the `filename` command similarly to include the full path (the directory and the data file name).
*/

libname IPUMS ".";
filename ASCIIDAT "nhgis0005_ds176_20105_2010_sd_uni.dat";

data IPUMS.nhgis0005_ds176_20105_2010_sd_uni;
infile ASCIIDAT pad missover lrecl=1097;

input
  YEAR      $ 1-9
  REGIONA   $ 10-10
  DIVISIONA $ 11-11
  STATE     $ 12-35
  STATEA    $ 36-37
  COUNTYA   $ 38-40
  COUSUBA   $ 41-45
  PLACEA    $ 46-50
  TRACTA    $ 51-56
  BLKGRPA   $ 57-57
  CONCITA   $ 58-62
  AIANHHA   $ 63-66
  RES_ONLYA $ 67-70
  TRUSTA    $ 71-74
  AITSCEA   $ 75-77
  ANRCA     $ 78-82
  CBSAA     $ 83-87
  CSAA      $ 88-90
  METDIVA   $ 91-95
  NECTAA    $ 96-100
  CNECTAA   $ 101-103
  NECTADIVA $ 104-108
  UAA       $ 109-113
  CDCURRA   $ 114-115
  SLDUA     $ 116-118
  SLDLA     $ 119-121
  SUBMCDA   $ 122-126
  SDELMA    $ 127-131
  SDSECA    $ 132-136
  SDUNI     $ 137-218
  SDUNIA    $ 219-223
  PUMA5A    $ 224-228
  BTTRA     $ 229-234
  BTBGA     $ 235-235
  NAME_E    $ 236-435
  JMKE001     436-444
  JMKE002     445-453
  JMKE003     454-462
  JN8E001     463-471
  JN8E002     472-480
  JN8E003     481-489
  JN8E004     490-498
  JN8E005     499-507
  JN8E006     508-516
  JN8E007     517-525
  JORE001     526-534
  JO1E001     535-549
  JREE001     550-558
  JREE002     559-567
  JREE003     568-576
  JREE004     577-585
  JREE005     586-594
  JREE006     595-603
  JREE007     604-612
  JREE008     613-621
  JREE009     622-630
  JREE010     631-639
  JREE011     640-648
  JREE012     649-657
  JREE013     658-666
  NAME_M    $ 667-866
  JMKM001     867-875
  JMKM002     876-884
  JMKM003     885-893
  JN8M001     894-902
  JN8M002     903-911
  JN8M003     912-920
  JN8M004     921-929
  JN8M005     930-938
  JN8M006     939-947
  JN8M007     948-956
  JORM001     957-965
  JO1M001     966-980
  JREM001     981-989
  JREM002     990-998
  JREM003     999-1007
  JREM004     1008-1016
  JREM005     1017-1025
  JREM006     1026-1034
  JREM007     1035-1043
  JREM008     1044-1052
  JREM009     1053-1061
  JREM010     1062-1070
  JREM011     1071-1079
  JREM012     1080-1088
  JREM013     1089-1097
;

label
  YEAR      = "Data File Year"
  REGIONA   = "Region Code"
  DIVISIONA = "Division Code"
  STATE     = "State Name"
  STATEA    = "State Code"
  COUNTYA   = "County Code"
  COUSUBA   = "County Subdivision Code"
  PLACEA    = "Place Code"
  TRACTA    = "Census Tract Code"
  BLKGRPA   = "Block Group Code"
  CONCITA   = "Consolidated City Code"
  AIANHHA   = "American Indian Area/Alaska Native Area/Hawaiian Home Land Code"
  RES_ONLYA = "American Indian Area/Alaska Native Area (Reservation or Statistical Entity Only) Code"
  TRUSTA    = "American Indian Reservation with Trust Lands; trust lands only Code"
  AITSCEA   = "Tribal Subdivision/Remainder Code"
  ANRCA     = "Alaska Native Regional Corporation Code"
  CBSAA     = "Metropolitan Statistical Area/Micropolitan Statistical Area Code"
  CSAA      = "Combined Statistical Area Code"
  METDIVA   = "Metropolitan Division Code"
  NECTAA    = "New England City and Town Area Code"
  CNECTAA   = "Combined New England City and Town Area Code"
  NECTADIVA = "New England City and Town Area Division Code"
  UAA       = "Urban Area Code"
  CDCURRA   = "Congressional District (111th Congress) Code"
  SLDUA     = "State Legislative District (Upper Chamber) Code"
  SLDLA     = "State Legislative District (Lower Chamber) Code"
  SUBMCDA   = "Subminor Civil Division Code"
  SDELMA    = "School District (Elementary)/Remainder Code"
  SDSECA    = "School District (Secondary)/Remainder Code"
  SDUNI     = "School District (Unified)/Remainder Name"
  SDUNIA    = "School District (Unified)/Remainder Code"
  PUMA5A    = "Public Use Microdata Sample Area (PUMA) Code"
  BTTRA     = "Tribal Census Tract Code"
  BTBGA     = "Tribal Block Group Code"
  NAME_E    = "Estimates: Area Name"
  JMKE001   = "Estimates: Total"
  JMKE002   = "Estimates: Not Hispanic or Latino"
  JMKE003   = "Estimates: Hispanic or Latino"
  JN8E001   = "Estimates: Total"
  JN8E002   = "Estimates: Enrolled in school"
  JN8E003   = "Estimates: Enrolled in school: Enrolled in nursery school, preschool, kindergarten"
  JN8E004   = "Estimates: Enrolled in school: Enrolled in grade 1 to grade 8"
  JN8E005   = "Estimates: Enrolled in school: Enrolled in grade 9 to grade 12"
  JN8E006   = "Estimates: Enrolled in school: Enrolled in college or graduate school"
  JN8E007   = "Estimates: Not enrolled in school"
  JORE001   = "Estimates: Median household income in the past 12 months (in 2010 inflation-adjusted dollars)"
  JO1E001   = "Estimates: Aggregate household income in the past 12 months (in 2010 inflation-adjusted dollars)"
  JREE001   = "Estimates: Total"
  JREE002   = "Estimates: Male"
  JREE003   = "Estimates: Male: Management, business, science, and arts occupations"
  JREE004   = "Estimates: Male: Service occupations"
  JREE005   = "Estimates: Male: Sales and office occupations"
  JREE006   = "Estimates: Male: Natural resources, construction, and maintenance occupations"
  JREE007   = "Estimates: Male: Production, transportation, and material moving occupations"
  JREE008   = "Estimates: Female"
  JREE009   = "Estimates: Female: Management, business, science, and arts occupations"
  JREE010   = "Estimates: Female: Service occupations"
  JREE011   = "Estimates: Female: Sales and office occupations"
  JREE012   = "Estimates: Female: Natural resources, construction, and maintenance occupations"
  JREE013   = "Estimates: Female: Production, transportation, and material moving occupations"
  NAME_M    = "Margins of error: Area Name"
  JMKM001   = "Margins of error: Total"
  JMKM002   = "Margins of error: Not Hispanic or Latino"
  JMKM003   = "Margins of error: Hispanic or Latino"
  JN8M001   = "Margins of error: Total"
  JN8M002   = "Margins of error: Enrolled in school"
  JN8M003   = "Margins of error: Enrolled in school: Enrolled in nursery school, preschool, kindergarten"
  JN8M004   = "Margins of error: Enrolled in school: Enrolled in grade 1 to grade 8"
  JN8M005   = "Margins of error: Enrolled in school: Enrolled in grade 9 to grade 12"
  JN8M006   = "Margins of error: Enrolled in school: Enrolled in college or graduate school"
  JN8M007   = "Margins of error: Not enrolled in school"
  JORM001   = "Margins of error: Median household income in the past 12 months (in 2010 inflation-adjusted dollars)"
  JO1M001   = "Margins of error: Aggregate household income in the past 12 months (in 2010 inflation-adjusted dolla"
              "rs)"
  JREM001   = "Margins of error: Total"
  JREM002   = "Margins of error: Male"
  JREM003   = "Margins of error: Male: Management, business, science, and arts occupations"
  JREM004   = "Margins of error: Male: Service occupations"
  JREM005   = "Margins of error: Male: Sales and office occupations"
  JREM006   = "Margins of error: Male: Natural resources, construction, and maintenance occupations"
  JREM007   = "Margins of error: Male: Production, transportation, and material moving occupations"
  JREM008   = "Margins of error: Female"
  JREM009   = "Margins of error: Female: Management, business, science, and arts occupations"
  JREM010   = "Margins of error: Female: Service occupations"
  JREM011   = "Margins of error: Female: Sales and office occupations"
  JREM012   = "Margins of error: Female: Natural resources, construction, and maintenance occupations"
  JREM013   = "Margins of error: Female: Production, transportation, and material moving occupations"
;

format
  JMKE001    9.
  JMKE002    9.
  JMKE003    9.
  JN8E001    9.
  JN8E002    9.
  JN8E003    9.
  JN8E004    9.
  JN8E005    9.
  JN8E006    9.
  JN8E007    9.
  JORE001    9.
  JO1E001    15.
  JREE001    9.
  JREE002    9.
  JREE003    9.
  JREE004    9.
  JREE005    9.
  JREE006    9.
  JREE007    9.
  JREE008    9.
  JREE009    9.
  JREE010    9.
  JREE011    9.
  JREE012    9.
  JREE013    9.
  JMKM001    9.
  JMKM002    9.
  JMKM003    9.
  JN8M001    9.
  JN8M002    9.
  JN8M003    9.
  JN8M004    9.
  JN8M005    9.
  JN8M006    9.
  JN8M007    9.
  JORM001    9.
  JO1M001    15.
  JREM001    9.
  JREM002    9.
  JREM003    9.
  JREM004    9.
  JREM005    9.
  JREM006    9.
  JREM007    9.
  JREM008    9.
  JREM009    9.
  JREM010    9.
  JREM011    9.
  JREM012    9.
  JREM013    9.
;

run;

