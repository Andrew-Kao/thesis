* NOTE: You need to edit the `cd` command to specify the path to the directory
* where the data file is located. For example: "C:\ipums_directory".
* .

cd ".".

data list file = "nhgis0005_ds176_20105_2010_sd_uni.dat" /
  YEAR       1-9 (a)
  REGIONA    10-10 (a)
  DIVISIONA  11-11 (a)
  STATE      12-35 (a)
  STATEA     36-37 (a)
  COUNTYA    38-40 (a)
  COUSUBA    41-45 (a)
  PLACEA     46-50 (a)
  TRACTA     51-56 (a)
  BLKGRPA    57-57 (a)
  CONCITA    58-62 (a)
  AIANHHA    63-66 (a)
  RES_ONLYA  67-70 (a)
  TRUSTA     71-74 (a)
  AITSCEA    75-77 (a)
  ANRCA      78-82 (a)
  CBSAA      83-87 (a)
  CSAA       88-90 (a)
  METDIVA    91-95 (a)
  NECTAA     96-100 (a)
  CNECTAA    101-103 (a)
  NECTADIVA  104-108 (a)
  UAA        109-113 (a)
  CDCURRA    114-115 (a)
  SLDUA      116-118 (a)
  SLDLA      119-121 (a)
  SUBMCDA    122-126 (a)
  SDELMA     127-131 (a)
  SDSECA     132-136 (a)
  SDUNI      137-218 (a)
  SDUNIA     219-223 (a)
  PUMA5A     224-228 (a)
  BTTRA      229-234 (a)
  BTBGA      235-235 (a)
  NAME_E     236-435 (a)
  JMKE001    436-444
  JMKE002    445-453
  JMKE003    454-462
  JN8E001    463-471
  JN8E002    472-480
  JN8E003    481-489
  JN8E004    490-498
  JN8E005    499-507
  JN8E006    508-516
  JN8E007    517-525
  JORE001    526-534
  JO1E001    535-549
  JREE001    550-558
  JREE002    559-567
  JREE003    568-576
  JREE004    577-585
  JREE005    586-594
  JREE006    595-603
  JREE007    604-612
  JREE008    613-621
  JREE009    622-630
  JREE010    631-639
  JREE011    640-648
  JREE012    649-657
  JREE013    658-666
  NAME_M     667-866 (a)
  JMKM001    867-875
  JMKM002    876-884
  JMKM003    885-893
  JN8M001    894-902
  JN8M002    903-911
  JN8M003    912-920
  JN8M004    921-929
  JN8M005    930-938
  JN8M006    939-947
  JN8M007    948-956
  JORM001    957-965
  JO1M001    966-980
  JREM001    981-989
  JREM002    990-998
  JREM003    999-1007
  JREM004    1008-1016
  JREM005    1017-1025
  JREM006    1026-1034
  JREM007    1035-1043
  JREM008    1044-1052
  JREM009    1053-1061
  JREM010    1062-1070
  JREM011    1071-1079
  JREM012    1080-1088
  JREM013    1089-1097
.

variable labels
  YEAR         "Data File Year"
  REGIONA      "Region Code"
  DIVISIONA    "Division Code"
  STATE        "State Name"
  STATEA       "State Code"
  COUNTYA      "County Code"
  COUSUBA      "County Subdivision Code"
  PLACEA       "Place Code"
  TRACTA       "Census Tract Code"
  BLKGRPA      "Block Group Code"
  CONCITA      "Consolidated City Code"
  AIANHHA      "American Indian Area/Alaska Native Area/Hawaiian Home Land Code"
  RES_ONLYA    "American Indian Area/Alaska Native Area (Reservation or Statistical Entity Only) Code"
  TRUSTA       "American Indian Reservation with Trust Lands; trust lands only Code"
  AITSCEA      "Tribal Subdivision/Remainder Code"
  ANRCA        "Alaska Native Regional Corporation Code"
  CBSAA        "Metropolitan Statistical Area/Micropolitan Statistical Area Code"
  CSAA         "Combined Statistical Area Code"
  METDIVA      "Metropolitan Division Code"
  NECTAA       "New England City and Town Area Code"
  CNECTAA      "Combined New England City and Town Area Code"
  NECTADIVA    "New England City and Town Area Division Code"
  UAA          "Urban Area Code"
  CDCURRA      "Congressional District (111th Congress) Code"
  SLDUA        "State Legislative District (Upper Chamber) Code"
  SLDLA        "State Legislative District (Lower Chamber) Code"
  SUBMCDA      "Subminor Civil Division Code"
  SDELMA       "School District (Elementary)/Remainder Code"
  SDSECA       "School District (Secondary)/Remainder Code"
  SDUNI        "School District (Unified)/Remainder Name"
  SDUNIA       "School District (Unified)/Remainder Code"
  PUMA5A       "Public Use Microdata Sample Area (PUMA) Code"
  BTTRA        "Tribal Census Tract Code"
  BTBGA        "Tribal Block Group Code"
  NAME_E       "Estimates: Area Name"
  JMKE001      "Estimates: Total"
  JMKE002      "Estimates: Not Hispanic or Latino"
  JMKE003      "Estimates: Hispanic or Latino"
  JN8E001      "Estimates: Total"
  JN8E002      "Estimates: Enrolled in school"
  JN8E003      "Estimates: Enrolled in school: Enrolled in nursery school, preschool, kindergarten"
  JN8E004      "Estimates: Enrolled in school: Enrolled in grade 1 to grade 8"
  JN8E005      "Estimates: Enrolled in school: Enrolled in grade 9 to grade 12"
  JN8E006      "Estimates: Enrolled in school: Enrolled in college or graduate school"
  JN8E007      "Estimates: Not enrolled in school"
  JORE001      "Estimates: Median household income in the past 12 months (in 2010 inflation-adjusted dollars)"
  JO1E001      "Estimates: Aggregate household income in the past 12 months (in 2010 inflation-adjusted dollars)"
  JREE001      "Estimates: Total"
  JREE002      "Estimates: Male"
  JREE003      "Estimates: Male: Management, business, science, and arts occupations"
  JREE004      "Estimates: Male: Service occupations"
  JREE005      "Estimates: Male: Sales and office occupations"
  JREE006      "Estimates: Male: Natural resources, construction, and maintenance occupations"
  JREE007      "Estimates: Male: Production, transportation, and material moving occupations"
  JREE008      "Estimates: Female"
  JREE009      "Estimates: Female: Management, business, science, and arts occupations"
  JREE010      "Estimates: Female: Service occupations"
  JREE011      "Estimates: Female: Sales and office occupations"
  JREE012      "Estimates: Female: Natural resources, construction, and maintenance occupations"
  JREE013      "Estimates: Female: Production, transportation, and material moving occupations"
  NAME_M       "Margins of error: Area Name"
  JMKM001      "Margins of error: Total"
  JMKM002      "Margins of error: Not Hispanic or Latino"
  JMKM003      "Margins of error: Hispanic or Latino"
  JN8M001      "Margins of error: Total"
  JN8M002      "Margins of error: Enrolled in school"
  JN8M003      "Margins of error: Enrolled in school: Enrolled in nursery school, preschool, kindergarten"
  JN8M004      "Margins of error: Enrolled in school: Enrolled in grade 1 to grade 8"
  JN8M005      "Margins of error: Enrolled in school: Enrolled in grade 9 to grade 12"
  JN8M006      "Margins of error: Enrolled in school: Enrolled in college or graduate school"
  JN8M007      "Margins of error: Not enrolled in school"
  JORM001      "Margins of error: Median household income in the past 12 months (in 2010 inflation-adjusted dollars)"
  JO1M001      "Margins of error: Aggregate household income in the past 12 months (in 2010 inflation-adjusted dolla"
             + "rs)"
  JREM001      "Margins of error: Total"
  JREM002      "Margins of error: Male"
  JREM003      "Margins of error: Male: Management, business, science, and arts occupations"
  JREM004      "Margins of error: Male: Service occupations"
  JREM005      "Margins of error: Male: Sales and office occupations"
  JREM006      "Margins of error: Male: Natural resources, construction, and maintenance occupations"
  JREM007      "Margins of error: Male: Production, transportation, and material moving occupations"
  JREM008      "Margins of error: Female"
  JREM009      "Margins of error: Female: Management, business, science, and arts occupations"
  JREM010      "Margins of error: Female: Service occupations"
  JREM011      "Margins of error: Female: Sales and office occupations"
  JREM012      "Margins of error: Female: Natural resources, construction, and maintenance occupations"
  JREM013      "Margins of error: Female: Production, transportation, and material moving occupations"
.

execute.

