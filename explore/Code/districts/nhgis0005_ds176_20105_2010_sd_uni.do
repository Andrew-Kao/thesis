* NOTE: You need to set the Stata working directory to the path
* where the data file is located.

set more off

clear
quietly infix                   ///
  str     year       1-9        ///
  str     regiona    10-10      ///
  str     divisiona  11-11      ///
  str     state      12-35      ///
  str     statea     36-37      ///
  str     countya    38-40      ///
  str     cousuba    41-45      ///
  str     placea     46-50      ///
  str     tracta     51-56      ///
  str     blkgrpa    57-57      ///
  str     concita    58-62      ///
  str     aianhha    63-66      ///
  str     res_onlya  67-70      ///
  str     trusta     71-74      ///
  str     aitscea    75-77      ///
  str     anrca      78-82      ///
  str     cbsaa      83-87      ///
  str     csaa       88-90      ///
  str     metdiva    91-95      ///
  str     nectaa     96-100     ///
  str     cnectaa    101-103    ///
  str     nectadiva  104-108    ///
  str     uaa        109-113    ///
  str     cdcurra    114-115    ///
  str     sldua      116-118    ///
  str     sldla      119-121    ///
  str     submcda    122-126    ///
  str     sdelma     127-131    ///
  str     sdseca     132-136    ///
  str     sduni      137-218    ///
  str     sdunia     219-223    ///
  str     puma5a     224-228    ///
  str     bttra      229-234    ///
  str     btbga      235-235    ///
  str     name_e     236-435    ///
  double  jmke001    436-444    ///
  double  jmke002    445-453    ///
  double  jmke003    454-462    ///
  double  jn8e001    463-471    ///
  double  jn8e002    472-480    ///
  double  jn8e003    481-489    ///
  double  jn8e004    490-498    ///
  double  jn8e005    499-507    ///
  double  jn8e006    508-516    ///
  double  jn8e007    517-525    ///
  double  jore001    526-534    ///
  double  jo1e001    535-549    ///
  double  jree001    550-558    ///
  double  jree002    559-567    ///
  double  jree003    568-576    ///
  double  jree004    577-585    ///
  double  jree005    586-594    ///
  double  jree006    595-603    ///
  double  jree007    604-612    ///
  double  jree008    613-621    ///
  double  jree009    622-630    ///
  double  jree010    631-639    ///
  double  jree011    640-648    ///
  double  jree012    649-657    ///
  double  jree013    658-666    ///
  str     name_m     667-866    ///
  double  jmkm001    867-875    ///
  double  jmkm002    876-884    ///
  double  jmkm003    885-893    ///
  double  jn8m001    894-902    ///
  double  jn8m002    903-911    ///
  double  jn8m003    912-920    ///
  double  jn8m004    921-929    ///
  double  jn8m005    930-938    ///
  double  jn8m006    939-947    ///
  double  jn8m007    948-956    ///
  double  jorm001    957-965    ///
  double  jo1m001    966-980    ///
  double  jrem001    981-989    ///
  double  jrem002    990-998    ///
  double  jrem003    999-1007   ///
  double  jrem004    1008-1016  ///
  double  jrem005    1017-1025  ///
  double  jrem006    1026-1034  ///
  double  jrem007    1035-1043  ///
  double  jrem008    1044-1052  ///
  double  jrem009    1053-1061  ///
  double  jrem010    1062-1070  ///
  double  jrem011    1071-1079  ///
  double  jrem012    1080-1088  ///
  double  jrem013    1089-1097  ///
  using `"nhgis0005_ds176_20105_2010_sd_uni.dat"'


format jmke001   %9.0f
format jmke002   %9.0f
format jmke003   %9.0f
format jn8e001   %9.0f
format jn8e002   %9.0f
format jn8e003   %9.0f
format jn8e004   %9.0f
format jn8e005   %9.0f
format jn8e006   %9.0f
format jn8e007   %9.0f
format jore001   %9.0f
format jo1e001   %15.0f
format jree001   %9.0f
format jree002   %9.0f
format jree003   %9.0f
format jree004   %9.0f
format jree005   %9.0f
format jree006   %9.0f
format jree007   %9.0f
format jree008   %9.0f
format jree009   %9.0f
format jree010   %9.0f
format jree011   %9.0f
format jree012   %9.0f
format jree013   %9.0f
format jmkm001   %9.0f
format jmkm002   %9.0f
format jmkm003   %9.0f
format jn8m001   %9.0f
format jn8m002   %9.0f
format jn8m003   %9.0f
format jn8m004   %9.0f
format jn8m005   %9.0f
format jn8m006   %9.0f
format jn8m007   %9.0f
format jorm001   %9.0f
format jo1m001   %15.0f
format jrem001   %9.0f
format jrem002   %9.0f
format jrem003   %9.0f
format jrem004   %9.0f
format jrem005   %9.0f
format jrem006   %9.0f
format jrem007   %9.0f
format jrem008   %9.0f
format jrem009   %9.0f
format jrem010   %9.0f
format jrem011   %9.0f
format jrem012   %9.0f
format jrem013   %9.0f

label var year      `"Data File Year"'
label var regiona   `"Region Code"'
label var divisiona `"Division Code"'
label var state     `"State Name"'
label var statea    `"State Code"'
label var countya   `"County Code"'
label var cousuba   `"County Subdivision Code"'
label var placea    `"Place Code"'
label var tracta    `"Census Tract Code"'
label var blkgrpa   `"Block Group Code"'
label var concita   `"Consolidated City Code"'
label var aianhha   `"American Indian Area/Alaska Native Area/Hawaiian Home Land Code"'
label var res_onlya `"American Indian Area/Alaska Native Area (Reservation or Statistical Entity Only)"'
label var trusta    `"American Indian Reservation with Trust Lands; trust lands only Code"'
label var aitscea   `"Tribal Subdivision/Remainder Code"'
label var anrca     `"Alaska Native Regional Corporation Code"'
label var cbsaa     `"Metropolitan Statistical Area/Micropolitan Statistical Area Code"'
label var csaa      `"Combined Statistical Area Code"'
label var metdiva   `"Metropolitan Division Code"'
label var nectaa    `"New England City and Town Area Code"'
label var cnectaa   `"Combined New England City and Town Area Code"'
label var nectadiva `"New England City and Town Area Division Code"'
label var uaa       `"Urban Area Code"'
label var cdcurra   `"Congressional District (111th Congress) Code"'
label var sldua     `"State Legislative District (Upper Chamber) Code"'
label var sldla     `"State Legislative District (Lower Chamber) Code"'
label var submcda   `"Subminor Civil Division Code"'
label var sdelma    `"School District (Elementary)/Remainder Code"'
label var sdseca    `"School District (Secondary)/Remainder Code"'
label var sduni     `"School District (Unified)/Remainder Name"'
label var sdunia    `"School District (Unified)/Remainder Code"'
label var puma5a    `"Public Use Microdata Sample Area (PUMA) Code"'
label var bttra     `"Tribal Census Tract Code"'
label var btbga     `"Tribal Block Group Code"'
label var name_e    `"Estimates: Area Name"'
label var jmke001   `"Estimates: Total"'
label var jmke002   `"Estimates: Not Hispanic or Latino"'
label var jmke003   `"Estimates: Hispanic or Latino"'
label var jn8e001   `"Estimates: Total"'
label var jn8e002   `"Estimates: Enrolled in school"'
label var jn8e003   `"Estimates: Enrolled in school: Enrolled in nursery school, preschool, kindergart"'
label var jn8e004   `"Estimates: Enrolled in school: Enrolled in grade 1 to grade 8"'
label var jn8e005   `"Estimates: Enrolled in school: Enrolled in grade 9 to grade 12"'
label var jn8e006   `"Estimates: Enrolled in school: Enrolled in college or graduate school"'
label var jn8e007   `"Estimates: Not enrolled in school"'
label var jore001   `"Estimates: Median household income in the past 12 months (in 2010 inflation-adju"'
label var jo1e001   `"Estimates: Aggregate household income in the past 12 months (in 2010 inflation-a"'
label var jree001   `"Estimates: Total"'
label var jree002   `"Estimates: Male"'
label var jree003   `"Estimates: Male: Management, business, science, and arts occupations"'
label var jree004   `"Estimates: Male: Service occupations"'
label var jree005   `"Estimates: Male: Sales and office occupations"'
label var jree006   `"Estimates: Male: Natural resources, construction, and maintenance occupations"'
label var jree007   `"Estimates: Male: Production, transportation, and material moving occupations"'
label var jree008   `"Estimates: Female"'
label var jree009   `"Estimates: Female: Management, business, science, and arts occupations"'
label var jree010   `"Estimates: Female: Service occupations"'
label var jree011   `"Estimates: Female: Sales and office occupations"'
label var jree012   `"Estimates: Female: Natural resources, construction, and maintenance occupations"'
label var jree013   `"Estimates: Female: Production, transportation, and material moving occupations"'
label var name_m    `"Margins of error: Area Name"'
label var jmkm001   `"Margins of error: Total"'
label var jmkm002   `"Margins of error: Not Hispanic or Latino"'
label var jmkm003   `"Margins of error: Hispanic or Latino"'
label var jn8m001   `"Margins of error: Total"'
label var jn8m002   `"Margins of error: Enrolled in school"'
label var jn8m003   `"Margins of error: Enrolled in school: Enrolled in nursery school, preschool, kin"'
label var jn8m004   `"Margins of error: Enrolled in school: Enrolled in grade 1 to grade 8"'
label var jn8m005   `"Margins of error: Enrolled in school: Enrolled in grade 9 to grade 12"'
label var jn8m006   `"Margins of error: Enrolled in school: Enrolled in college or graduate school"'
label var jn8m007   `"Margins of error: Not enrolled in school"'
label var jorm001   `"Margins of error: Median household income in the past 12 months (in 2010 inflati"'
label var jo1m001   `"Margins of error: Aggregate household income in the past 12 months (in 2010 infl"'
label var jrem001   `"Margins of error: Total"'
label var jrem002   `"Margins of error: Male"'
label var jrem003   `"Margins of error: Male: Management, business, science, and arts occupations"'
label var jrem004   `"Margins of error: Male: Service occupations"'
label var jrem005   `"Margins of error: Male: Sales and office occupations"'
label var jrem006   `"Margins of error: Male: Natural resources, construction, and maintenance occupat"'
label var jrem007   `"Margins of error: Male: Production, transportation, and material moving occupati"'
label var jrem008   `"Margins of error: Female"'
label var jrem009   `"Margins of error: Female: Management, business, science, and arts occupations"'
label var jrem010   `"Margins of error: Female: Service occupations"'
label var jrem011   `"Margins of error: Female: Sales and office occupations"'
label var jrem012   `"Margins of error: Female: Natural resources, construction, and maintenance occup"'
label var jrem013   `"Margins of error: Female: Production, transportation, and material moving occupa"'


