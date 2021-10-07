* NOTE: You need to edit the `cd` command to specify the path to the directory
* where the data file is located. For example: "C:\ipums_directory".
* .

cd ".".

data list file = "nhgis0005_ds177_20105_2010_sd_uni.dat" /
  YEAR       1-9 (a)
  REGIONA    10-10 (a)
  DIVISIONA  11-11 (a)
  STATE      12-35 (a)
  STATEA     36-37 (a)
  COUNTYA    38-40 (a)
  COUSUBA    41-45 (a)
  PLACEA     46-50 (a)
  TRACTA     51-56 (a)
  CONCITA    57-61 (a)
  AIANHHA    62-65 (a)
  RES_ONLYA  66-69 (a)
  TRUSTA     70-73 (a)
  AITSCEA    74-76 (a)
  ANRCA      77-81 (a)
  CBSAA      82-86 (a)
  CSAA       87-89 (a)
  METDIVA    90-94 (a)
  NECTAA     95-99 (a)
  CNECTAA    100-102 (a)
  NECTADIVA  103-107 (a)
  UAA        108-112 (a)
  CDCURRA    113-114 (a)
  SLDUA      115-117 (a)
  SLDLA      118-120 (a)
  SUBMCDA    121-125 (a)
  SDELMA     126-130 (a)
  SDSECA     131-135 (a)
  SDUNI      136-217 (a)
  SDUNIA     218-222 (a)
  PUMA5A     223-227 (a)
  BTTRA      228-233 (a)
  NAME_E     234-433 (a)
  JXHE001    434-442
  JXHE002    443-451
  JXHE003    452-460
  JXHE004    461-469
  JXHE005    470-478
  JYHE001    479-487
  JYHE002    488-496
  JYHE003    497-505
  JYHE004    506-514
  JYHE005    515-523
  JYHE006    524-532
  JZGE001    533-541
  JZGE002    542-550
  JZGE003    551-559
  JZGE004    560-568
  JZGE005    569-577
  JZGE006    578-586
  JZGE007    587-595
  J0HE001    596-604
  J0HE002    605-613
  J0HE003    614-622
  J0HE004    623-631
  J0HE005    632-640
  J0HE006    641-649
  J0HE007    650-658
  J05E001    659-667
  J05E002    668-676
  J05E003    677-685
  J05E004    686-694
  J05E005    695-703
  J05E006    704-712
  J05E007    713-721
  J05E008    722-730
  J05E009    731-739
  J05E010    740-748
  J05E011    749-757
  J05E012    758-766
  J05E013    767-775
  J1SE001    776-784
  J1SE002    785-793
  J1SE003    794-802
  J1SE004    803-811
  J1SE005    812-820
  J1SE006    821-829
  J1SE007    830-838
  J2DE001    839-847
  J2DE002    848-856
  J2DE003    857-865
  J2DE004    866-874
  J2DE005    875-883
  J2DE006    884-892
  J2DE007    893-901
  J2DE008    902-910
  J2DE009    911-919
  J2DE010    920-928
  J2DE011    929-937
  J2PE001    938-946
  J2PE002    947-955
  J2PE003    956-964
  J2PE004    965-973
  J2PE005    974-982
  J2PE006    983-991
  J2PE007    992-1000
  J2PE008    1001-1009
  J2PE009    1010-1018
  J2PE010    1019-1027
  J2PE011    1028-1036
  J2QE001    1037-1045
  J2QE002    1046-1054
  J2QE003    1055-1063
  J2QE004    1064-1072
  J2QE005    1073-1081
  J2QE006    1082-1090
  J2QE007    1091-1099
  J2QE008    1100-1108
  J24E001    1109-1117
  J24E002    1118-1126
  J24E003    1127-1135
  J24E004    1136-1144
  J24E005    1145-1153
  J24E006    1154-1162
  J24E007    1163-1171
  J24E008    1172-1180
  J24E009    1181-1189
  J24E010    1190-1198
  J24E011    1199-1207
  J24E012    1208-1216
  J24E013    1217-1225
  J24E014    1226-1234
  J24E015    1235-1243
  J24E016    1244-1252
  J24E017    1253-1261
  J24E018    1262-1270
  J24E019    1271-1279
  J24E020    1280-1288
  J24E021    1289-1297
  J24E022    1298-1306
  J24E023    1307-1315
  J24E024    1316-1324
  J24E025    1325-1333
  J24E026    1334-1342
  J24E027    1343-1351
  J24E028    1352-1360
  J24E029    1361-1369
  J24E030    1370-1378
  J24E031    1379-1387
  J24E032    1388-1396
  J24E033    1397-1405
  J24E034    1406-1414
  J24E035    1415-1423
  J24E036    1424-1432
  J24E037    1433-1441
  J24E038    1442-1450
  J24E039    1451-1459
  J24E040    1460-1468
  J24E041    1469-1477
  J24E042    1478-1486
  J24E043    1487-1495
  J24E044    1496-1504
  J24E045    1505-1513
  J24E046    1514-1522
  J24E047    1523-1531
  J24E048    1532-1540
  J24E049    1541-1549
  J24E050    1550-1558
  J24E051    1559-1567
  J24E052    1568-1576
  J24E053    1577-1585
  J24E054    1586-1594
  J24E055    1595-1603
  J24E056    1604-1612
  J24E057    1613-1621
  J24E058    1622-1630
  J24E059    1631-1639
  J30E001    1640-1648
  J30E002    1649-1657
  J30E003    1658-1666
  J30E004    1667-1675
  J30E005    1676-1684
  J30E006    1685-1693
  J30E007    1694-1702
  J30E008    1703-1711
  J30E009    1712-1720
  J30E010    1721-1729
  J30E011    1730-1738
  J30E012    1739-1747
  J30E013    1748-1756
  J30E014    1757-1765
  J30E015    1766-1774
  J30E016    1775-1783
  J30E017    1784-1792
  J6NE001    1793-1801
  J6NE002    1802-1810
  J6NE003    1811-1819
  J6ZE001    1820-1828
  J6ZE002    1829-1837
  J6ZE003    1838-1846
  J6ZE004    1847-1855
  J6ZE005    1856-1864
  J6ZE006    1865-1873
  J6ZE007    1874-1882
  J6ZE008    1883-1891
  J6ZE009    1892-1900
  J6ZE010    1901-1909
  J6ZE011    1910-1918
  J6ZE012    1919-1927
  J6ZE013    1928-1936
  J6ZE014    1937-1945
  J6ZE015    1946-1954
  J6ZE016    1955-1963
  J6ZE017    1964-1972
  J6ZE018    1973-1981
  J6ZE019    1982-1990
  J6ZE020    1991-1999
  J6ZE021    2000-2008
  J6ZE022    2009-2017
  J6ZE023    2018-2026
  J6ZE024    2027-2035
  J6ZE025    2036-2044
  J6ZE026    2045-2053
  J6ZE027    2054-2062
  J75E001    2063-2071
  J75E002    2072-2080
  J75E003    2081-2089
  NAME_M     2090-2289 (a)
  JXHM001    2290-2298
  JXHM002    2299-2307
  JXHM003    2308-2316
  JXHM004    2317-2325
  JXHM005    2326-2334
  JYHM001    2335-2343
  JYHM002    2344-2352
  JYHM003    2353-2361
  JYHM004    2362-2370
  JYHM005    2371-2379
  JYHM006    2380-2388
  JZGM001    2389-2397
  JZGM002    2398-2406
  JZGM003    2407-2415
  JZGM004    2416-2424
  JZGM005    2425-2433
  JZGM006    2434-2442
  JZGM007    2443-2451
  J0HM001    2452-2460
  J0HM002    2461-2469
  J0HM003    2470-2478
  J0HM004    2479-2487
  J0HM005    2488-2496
  J0HM006    2497-2505
  J0HM007    2506-2514
  J05M001    2515-2523
  J05M002    2524-2532
  J05M003    2533-2541
  J05M004    2542-2550
  J05M005    2551-2559
  J05M006    2560-2568
  J05M007    2569-2577
  J05M008    2578-2586
  J05M009    2587-2595
  J05M010    2596-2604
  J05M011    2605-2613
  J05M012    2614-2622
  J05M013    2623-2631
  J1SM001    2632-2640
  J1SM002    2641-2649
  J1SM003    2650-2658
  J1SM004    2659-2667
  J1SM005    2668-2676
  J1SM006    2677-2685
  J1SM007    2686-2694
  J2DM001    2695-2703
  J2DM002    2704-2712
  J2DM003    2713-2721
  J2DM004    2722-2730
  J2DM005    2731-2739
  J2DM006    2740-2748
  J2DM007    2749-2757
  J2DM008    2758-2766
  J2DM009    2767-2775
  J2DM010    2776-2784
  J2DM011    2785-2793
  J2PM001    2794-2802
  J2PM002    2803-2811
  J2PM003    2812-2820
  J2PM004    2821-2829
  J2PM005    2830-2838
  J2PM006    2839-2847
  J2PM007    2848-2856
  J2PM008    2857-2865
  J2PM009    2866-2874
  J2PM010    2875-2883
  J2PM011    2884-2892
  J2QM001    2893-2901
  J2QM002    2902-2910
  J2QM003    2911-2919
  J2QM004    2920-2928
  J2QM005    2929-2937
  J2QM006    2938-2946
  J2QM007    2947-2955
  J2QM008    2956-2964
  J24M001    2965-2973
  J24M002    2974-2982
  J24M003    2983-2991
  J24M004    2992-3000
  J24M005    3001-3009
  J24M006    3010-3018
  J24M007    3019-3027
  J24M008    3028-3036
  J24M009    3037-3045
  J24M010    3046-3054
  J24M011    3055-3063
  J24M012    3064-3072
  J24M013    3073-3081
  J24M014    3082-3090
  J24M015    3091-3099
  J24M016    3100-3108
  J24M017    3109-3117
  J24M018    3118-3126
  J24M019    3127-3135
  J24M020    3136-3144
  J24M021    3145-3153
  J24M022    3154-3162
  J24M023    3163-3171
  J24M024    3172-3180
  J24M025    3181-3189
  J24M026    3190-3198
  J24M027    3199-3207
  J24M028    3208-3216
  J24M029    3217-3225
  J24M030    3226-3234
  J24M031    3235-3243
  J24M032    3244-3252
  J24M033    3253-3261
  J24M034    3262-3270
  J24M035    3271-3279
  J24M036    3280-3288
  J24M037    3289-3297
  J24M038    3298-3306
  J24M039    3307-3315
  J24M040    3316-3324
  J24M041    3325-3333
  J24M042    3334-3342
  J24M043    3343-3351
  J24M044    3352-3360
  J24M045    3361-3369
  J24M046    3370-3378
  J24M047    3379-3387
  J24M048    3388-3396
  J24M049    3397-3405
  J24M050    3406-3414
  J24M051    3415-3423
  J24M052    3424-3432
  J24M053    3433-3441
  J24M054    3442-3450
  J24M055    3451-3459
  J24M056    3460-3468
  J24M057    3469-3477
  J24M058    3478-3486
  J24M059    3487-3495
  J30M001    3496-3504
  J30M002    3505-3513
  J30M003    3514-3522
  J30M004    3523-3531
  J30M005    3532-3540
  J30M006    3541-3549
  J30M007    3550-3558
  J30M008    3559-3567
  J30M009    3568-3576
  J30M010    3577-3585
  J30M011    3586-3594
  J30M012    3595-3603
  J30M013    3604-3612
  J30M014    3613-3621
  J30M015    3622-3630
  J30M016    3631-3639
  J30M017    3640-3648
  J6NM001    3649-3657
  J6NM002    3658-3666
  J6NM003    3667-3675
  J6ZM001    3676-3684
  J6ZM002    3685-3693
  J6ZM003    3694-3702
  J6ZM004    3703-3711
  J6ZM005    3712-3720
  J6ZM006    3721-3729
  J6ZM007    3730-3738
  J6ZM008    3739-3747
  J6ZM009    3748-3756
  J6ZM010    3757-3765
  J6ZM011    3766-3774
  J6ZM012    3775-3783
  J6ZM013    3784-3792
  J6ZM014    3793-3801
  J6ZM015    3802-3810
  J6ZM016    3811-3819
  J6ZM017    3820-3828
  J6ZM018    3829-3837
  J6ZM019    3838-3846
  J6ZM020    3847-3855
  J6ZM021    3856-3864
  J6ZM022    3865-3873
  J6ZM023    3874-3882
  J6ZM024    3883-3891
  J6ZM025    3892-3900
  J6ZM026    3901-3909
  J6ZM027    3910-3918
  J75M001    3919-3927
  J75M002    3928-3936
  J75M003    3937-3945
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
  NAME_E       "Estimates: Area Name"
  JXHE001      "Estimates: Total"
  JXHE002      "Estimates: Born in state of residence"
  JXHE003      "Estimates: Born in other state in the United States"
  JXHE004      "Estimates: Native; born outside the United States"
  JXHE005      "Estimates: Foreign born"
  JYHE001      "Estimates: Total"
  JYHE002      "Estimates: Same house 1 year ago"
  JYHE003      "Estimates: Moved within same county"
  JYHE004      "Estimates: Moved from different county within same state"
  JYHE005      "Estimates: Moved from different state"
  JYHE006      "Estimates: Moved from abroad"
  JZGE001      "Estimates: Total"
  JZGE002      "Estimates: Car, truck, or van - drove alone"
  JZGE003      "Estimates: Car, truck, or van - carpooled"
  JZGE004      "Estimates: Public transportation (excluding taxicab)"
  JZGE005      "Estimates: Walked"
  JZGE006      "Estimates: Taxicab, motorcycle, bicycle, or other means"
  JZGE007      "Estimates: Worked at home"
  J0HE001      "Estimates: Total"
  J0HE002      "Estimates: Grandparent responsible for own grandchildren under 18 years"
  J0HE003      "Estimates: Grandparent responsible for own grandchildren under 18 years: 30 to 59 years"
  J0HE004      "Estimates: Grandparent responsible for own grandchildren under 18 years: 60 years and over"
  J0HE005      "Estimates: Grandparent not responsible for own grandchildren under 18 years"
  J0HE006      "Estimates: Grandparent not responsible for own grandchildren under 18 years: 30 to 59 years"
  J0HE007      "Estimates: Grandparent not responsible for own grandchildren under 18 years: 60 years and over"
  J05E001      "Estimates: Total"
  J05E002      "Estimates: Male"
  J05E003      "Estimates: Male: Never married"
  J05E004      "Estimates: Male: Now married (except separated)"
  J05E005      "Estimates: Male: Separated"
  J05E006      "Estimates: Male: Widowed"
  J05E007      "Estimates: Male: Divorced"
  J05E008      "Estimates: Female"
  J05E009      "Estimates: Female: Never married"
  J05E010      "Estimates: Female: Now married (except separated)"
  J05E011      "Estimates: Female: Separated"
  J05E012      "Estimates: Female: Widowed"
  J05E013      "Estimates: Female: Divorced"
  J1SE001      "Estimates: Total"
  J1SE002      "Estimates: Women who had a birth in the past 12 months"
  J1SE003      "Estimates: Women who had a birth in the past 12 months: Now married (including separated and spouse "
             + "absent)"
  J1SE004      "Estimates: Women who had a birth in the past 12 months: Unmarried (never married, widowed and divorc"
             + "ed)"
  J1SE005      "Estimates: Women who did not have a birth in the past 12 months"
  J1SE006      "Estimates: Women who did not have a birth in the past 12 months: Now married (including separated an"
             + "d spouse absent)"
  J1SE007      "Estimates: Women who did not have a birth in the past 12 months: Unmarried (never married, widowed a"
             + "nd divorced)"
  J2DE001      "Estimates: Total"
  J2DE002      "Estimates: Male"
  J2DE003      "Estimates: Male: Less than high school diploma"
  J2DE004      "Estimates: Male: High school graduate, GED, or alternative"
  J2DE005      "Estimates: Male: Some college or associate's degree"
  J2DE006      "Estimates: Male: Bachelor's degree or higher"
  J2DE007      "Estimates: Female"
  J2DE008      "Estimates: Female: Less than high school diploma"
  J2DE009      "Estimates: Female: High school graduate, GED, or alternative"
  J2DE010      "Estimates: Female: Some college or associate's degree"
  J2DE011      "Estimates: Female: Bachelor's degree or higher"
  J2PE001      "Estimates: Total"
  J2PE002      "Estimates: Native"
  J2PE003      "Estimates: Native: Speak only English"
  J2PE004      "Estimates: Native: Speak another language"
  J2PE005      "Estimates: Native: Speak another language: Speak English 'very well'"
  J2PE006      "Estimates: Native: Speak another language: Speak English less than 'very well'"
  J2PE007      "Estimates: Foreign born"
  J2PE008      "Estimates: Foreign born: Speak only English"
  J2PE009      "Estimates: Foreign born: Speak another language"
  J2PE010      "Estimates: Foreign born: Speak another language: Speak English 'very well'"
  J2PE011      "Estimates: Foreign born: Speak another language: Speak English less than 'very well'"
  J2QE001      "Estimates: Total"
  J2QE002      "Estimates: Speak only English"
  J2QE003      "Estimates: Speak Spanish"
  J2QE004      "Estimates: Speak Spanish: Speak English 'very well'"
  J2QE005      "Estimates: Speak Spanish: Speak English 'well'"
  J2QE006      "Estimates: Speak Spanish: Speak English 'not well'"
  J2QE007      "Estimates: Speak Spanish: Speak English 'not at all'"
  J2QE008      "Estimates: Speak other language"
  J24E001      "Estimates: Total"
  J24E002      "Estimates: Income in the past 12 months below poverty level"
  J24E003      "Estimates: Income in the past 12 months below poverty level: Male"
  J24E004      "Estimates: Income in the past 12 months below poverty level: Male: Under 5 years"
  J24E005      "Estimates: Income in the past 12 months below poverty level: Male: 5 years"
  J24E006      "Estimates: Income in the past 12 months below poverty level: Male: 6 to 11 years"
  J24E007      "Estimates: Income in the past 12 months below poverty level: Male: 12 to 14 years"
  J24E008      "Estimates: Income in the past 12 months below poverty level: Male: 15 years"
  J24E009      "Estimates: Income in the past 12 months below poverty level: Male: 16 and 17 years"
  J24E010      "Estimates: Income in the past 12 months below poverty level: Male: 18 to 24 years"
  J24E011      "Estimates: Income in the past 12 months below poverty level: Male: 25 to 34 years"
  J24E012      "Estimates: Income in the past 12 months below poverty level: Male: 35 to 44 years"
  J24E013      "Estimates: Income in the past 12 months below poverty level: Male: 45 to 54 years"
  J24E014      "Estimates: Income in the past 12 months below poverty level: Male: 55 to 64 years"
  J24E015      "Estimates: Income in the past 12 months below poverty level: Male: 65 to 74 years"
  J24E016      "Estimates: Income in the past 12 months below poverty level: Male: 75 years and over"
  J24E017      "Estimates: Income in the past 12 months below poverty level: Female"
  J24E018      "Estimates: Income in the past 12 months below poverty level: Female: Under 5 years"
  J24E019      "Estimates: Income in the past 12 months below poverty level: Female: 5 years"
  J24E020      "Estimates: Income in the past 12 months below poverty level: Female: 6 to 11 years"
  J24E021      "Estimates: Income in the past 12 months below poverty level: Female: 12 to 14 years"
  J24E022      "Estimates: Income in the past 12 months below poverty level: Female: 15 years"
  J24E023      "Estimates: Income in the past 12 months below poverty level: Female: 16 and 17 years"
  J24E024      "Estimates: Income in the past 12 months below poverty level: Female: 18 to 24 years"
  J24E025      "Estimates: Income in the past 12 months below poverty level: Female: 25 to 34 years"
  J24E026      "Estimates: Income in the past 12 months below poverty level: Female: 35 to 44 years"
  J24E027      "Estimates: Income in the past 12 months below poverty level: Female: 45 to 54 years"
  J24E028      "Estimates: Income in the past 12 months below poverty level: Female: 55 to 64 years"
  J24E029      "Estimates: Income in the past 12 months below poverty level: Female: 65 to 74 years"
  J24E030      "Estimates: Income in the past 12 months below poverty level: Female: 75 years and over"
  J24E031      "Estimates: Income in the past 12 months at or above poverty level"
  J24E032      "Estimates: Income in the past 12 months at or above poverty level: Male"
  J24E033      "Estimates: Income in the past 12 months at or above poverty level: Male: Under 5 years"
  J24E034      "Estimates: Income in the past 12 months at or above poverty level: Male: 5 years"
  J24E035      "Estimates: Income in the past 12 months at or above poverty level: Male: 6 to 11 years"
  J24E036      "Estimates: Income in the past 12 months at or above poverty level: Male: 12 to 14 years"
  J24E037      "Estimates: Income in the past 12 months at or above poverty level: Male: 15 years"
  J24E038      "Estimates: Income in the past 12 months at or above poverty level: Male: 16 and 17 years"
  J24E039      "Estimates: Income in the past 12 months at or above poverty level: Male: 18 to 24 years"
  J24E040      "Estimates: Income in the past 12 months at or above poverty level: Male: 25 to 34 years"
  J24E041      "Estimates: Income in the past 12 months at or above poverty level: Male: 35 to 44 years"
  J24E042      "Estimates: Income in the past 12 months at or above poverty level: Male: 45 to 54 years"
  J24E043      "Estimates: Income in the past 12 months at or above poverty level: Male: 55 to 64 years"
  J24E044      "Estimates: Income in the past 12 months at or above poverty level: Male: 65 to 74 years"
  J24E045      "Estimates: Income in the past 12 months at or above poverty level: Male: 75 years and over"
  J24E046      "Estimates: Income in the past 12 months at or above poverty level: Female"
  J24E047      "Estimates: Income in the past 12 months at or above poverty level: Female: Under 5 years"
  J24E048      "Estimates: Income in the past 12 months at or above poverty level: Female: 5 years"
  J24E049      "Estimates: Income in the past 12 months at or above poverty level: Female: 6 to 11 years"
  J24E050      "Estimates: Income in the past 12 months at or above poverty level: Female: 12 to 14 years"
  J24E051      "Estimates: Income in the past 12 months at or above poverty level: Female: 15 years"
  J24E052      "Estimates: Income in the past 12 months at or above poverty level: Female: 16 and 17 years"
  J24E053      "Estimates: Income in the past 12 months at or above poverty level: Female: 18 to 24 years"
  J24E054      "Estimates: Income in the past 12 months at or above poverty level: Female: 25 to 34 years"
  J24E055      "Estimates: Income in the past 12 months at or above poverty level: Female: 35 to 44 years"
  J24E056      "Estimates: Income in the past 12 months at or above poverty level: Female: 45 to 54 years"
  J24E057      "Estimates: Income in the past 12 months at or above poverty level: Female: 55 to 64 years"
  J24E058      "Estimates: Income in the past 12 months at or above poverty level: Female: 65 to 74 years"
  J24E059      "Estimates: Income in the past 12 months at or above poverty level: Female: 75 years and over"
  J30E001      "Estimates: Total"
  J30E002      "Estimates: Income in the past 12 months below poverty level"
  J30E003      "Estimates: Income in the past 12 months below poverty level: Under 5 years"
  J30E004      "Estimates: Income in the past 12 months below poverty level: 5 years"
  J30E005      "Estimates: Income in the past 12 months below poverty level: 6 to 11 years"
  J30E006      "Estimates: Income in the past 12 months below poverty level: 12 to 17 years"
  J30E007      "Estimates: Income in the past 12 months below poverty level: 18 to 64 years"
  J30E008      "Estimates: Income in the past 12 months below poverty level: 65 to 74 years"
  J30E009      "Estimates: Income in the past 12 months below poverty level: 75 years and over"
  J30E010      "Estimates: Income in the past 12 months at or above poverty level"
  J30E011      "Estimates: Income in the past 12 months at or above poverty level: Under 5 years"
  J30E012      "Estimates: Income in the past 12 months at or above poverty level: 5 years"
  J30E013      "Estimates: Income in the past 12 months at or above poverty level: 6 to 11 years"
  J30E014      "Estimates: Income in the past 12 months at or above poverty level: 12 to 17 years"
  J30E015      "Estimates: Income in the past 12 months at or above poverty level: 18 to 64 years"
  J30E016      "Estimates: Income in the past 12 months at or above poverty level: 65 to 74 years"
  J30E017      "Estimates: Income in the past 12 months at or above poverty level: 75 years and over"
  J6NE001      "Estimates: Total"
  J6NE002      "Estimates: Household received Food Stamps/SNAP in the past 12 months"
  J6NE003      "Estimates: Household did not receive Food Stamps/SNAP in the past 12 months"
  J6ZE001      "Estimates: Total"
  J6ZE002      "Estimates: Male"
  J6ZE003      "Estimates: Male: 16 to 64 years"
  J6ZE004      "Estimates: Male: 16 to 64 years: In labor force"
  J6ZE005      "Estimates: Male: 16 to 64 years: In labor force: In Armed Forces"
  J6ZE006      "Estimates: Male: 16 to 64 years: In labor force: Civilian"
  J6ZE007      "Estimates: Male: 16 to 64 years: In labor force: Civilian: Employed"
  J6ZE008      "Estimates: Male: 16 to 64 years: In labor force: Civilian: Unemployed"
  J6ZE009      "Estimates: Male: 16 to 64 years: Not in labor force"
  J6ZE010      "Estimates: Male: 65 years and over"
  J6ZE011      "Estimates: Male: 65 years and over: In labor force"
  J6ZE012      "Estimates: Male: 65 years and over: In labor force: Employed"
  J6ZE013      "Estimates: Male: 65 years and over: In labor force: Unemployed"
  J6ZE014      "Estimates: Male: 65 years and over: Not in labor force"
  J6ZE015      "Estimates: Female"
  J6ZE016      "Estimates: Female: 16 to 64 years"
  J6ZE017      "Estimates: Female: 16 to 64 years: In labor force"
  J6ZE018      "Estimates: Female: 16 to 64 years: In labor force: In Armed Forces"
  J6ZE019      "Estimates: Female: 16 to 64 years: In labor force: Civilian"
  J6ZE020      "Estimates: Female: 16 to 64 years: In labor force: Civilian: Employed"
  J6ZE021      "Estimates: Female: 16 to 64 years: In labor force: Civilian: Unemployed"
  J6ZE022      "Estimates: Female: 16 to 64 years: Not in labor force"
  J6ZE023      "Estimates: Female: 65 years and over"
  J6ZE024      "Estimates: Female: 65 years and over: In labor force"
  J6ZE025      "Estimates: Female: 65 years and over: In labor force: Employed"
  J6ZE026      "Estimates: Female: 65 years and over: In labor force: Unemployed"
  J6ZE027      "Estimates: Female: 65 years and over: Not in labor force"
  J75E001      "Estimates: Total"
  J75E002      "Estimates: 1.00 or less occupants per room"
  J75E003      "Estimates: 1.01 or more occupants per room"
  NAME_M       "Margins of error: Area Name"
  JXHM001      "Margins of error: Total"
  JXHM002      "Margins of error: Born in state of residence"
  JXHM003      "Margins of error: Born in other state in the United States"
  JXHM004      "Margins of error: Native; born outside the United States"
  JXHM005      "Margins of error: Foreign born"
  JYHM001      "Margins of error: Total"
  JYHM002      "Margins of error: Same house 1 year ago"
  JYHM003      "Margins of error: Moved within same county"
  JYHM004      "Margins of error: Moved from different county within same state"
  JYHM005      "Margins of error: Moved from different state"
  JYHM006      "Margins of error: Moved from abroad"
  JZGM001      "Margins of error: Total"
  JZGM002      "Margins of error: Car, truck, or van - drove alone"
  JZGM003      "Margins of error: Car, truck, or van - carpooled"
  JZGM004      "Margins of error: Public transportation (excluding taxicab)"
  JZGM005      "Margins of error: Walked"
  JZGM006      "Margins of error: Taxicab, motorcycle, bicycle, or other means"
  JZGM007      "Margins of error: Worked at home"
  J0HM001      "Margins of error: Total"
  J0HM002      "Margins of error: Grandparent responsible for own grandchildren under 18 years"
  J0HM003      "Margins of error: Grandparent responsible for own grandchildren under 18 years: 30 to 59 years"
  J0HM004      "Margins of error: Grandparent responsible for own grandchildren under 18 years: 60 years and over"
  J0HM005      "Margins of error: Grandparent not responsible for own grandchildren under 18 years"
  J0HM006      "Margins of error: Grandparent not responsible for own grandchildren under 18 years: 30 to 59 years"
  J0HM007      "Margins of error: Grandparent not responsible for own grandchildren under 18 years: 60 years and ove"
             + "r"
  J05M001      "Margins of error: Total"
  J05M002      "Margins of error: Male"
  J05M003      "Margins of error: Male: Never married"
  J05M004      "Margins of error: Male: Now married (except separated)"
  J05M005      "Margins of error: Male: Separated"
  J05M006      "Margins of error: Male: Widowed"
  J05M007      "Margins of error: Male: Divorced"
  J05M008      "Margins of error: Female"
  J05M009      "Margins of error: Female: Never married"
  J05M010      "Margins of error: Female: Now married (except separated)"
  J05M011      "Margins of error: Female: Separated"
  J05M012      "Margins of error: Female: Widowed"
  J05M013      "Margins of error: Female: Divorced"
  J1SM001      "Margins of error: Total"
  J1SM002      "Margins of error: Women who had a birth in the past 12 months"
  J1SM003      "Margins of error: Women who had a birth in the past 12 months: Now married (including separated and "
             + "spouse absent)"
  J1SM004      "Margins of error: Women who had a birth in the past 12 months: Unmarried (never married, widowed and"
             + " divorced)"
  J1SM005      "Margins of error: Women who did not have a birth in the past 12 months"
  J1SM006      "Margins of error: Women who did not have a birth in the past 12 months: Now married (including separ"
             + "ated and spouse abse"
  J1SM007      "Margins of error: Women who did not have a birth in the past 12 months: Unmarried (never married, wi"
             + "dowed and divorced)"
  J2DM001      "Margins of error: Total"
  J2DM002      "Margins of error: Male"
  J2DM003      "Margins of error: Male: Less than high school diploma"
  J2DM004      "Margins of error: Male: High school graduate, GED, or alternative"
  J2DM005      "Margins of error: Male: Some college or associate's degree"
  J2DM006      "Margins of error: Male: Bachelor's degree or higher"
  J2DM007      "Margins of error: Female"
  J2DM008      "Margins of error: Female: Less than high school diploma"
  J2DM009      "Margins of error: Female: High school graduate, GED, or alternative"
  J2DM010      "Margins of error: Female: Some college or associate's degree"
  J2DM011      "Margins of error: Female: Bachelor's degree or higher"
  J2PM001      "Margins of error: Total"
  J2PM002      "Margins of error: Native"
  J2PM003      "Margins of error: Native: Speak only English"
  J2PM004      "Margins of error: Native: Speak another language"
  J2PM005      "Margins of error: Native: Speak another language: Speak English 'very well'"
  J2PM006      "Margins of error: Native: Speak another language: Speak English less than 'very well'"
  J2PM007      "Margins of error: Foreign born"
  J2PM008      "Margins of error: Foreign born: Speak only English"
  J2PM009      "Margins of error: Foreign born: Speak another language"
  J2PM010      "Margins of error: Foreign born: Speak another language: Speak English 'very well'"
  J2PM011      "Margins of error: Foreign born: Speak another language: Speak English less than 'very well'"
  J2QM001      "Margins of error: Total"
  J2QM002      "Margins of error: Speak only English"
  J2QM003      "Margins of error: Speak Spanish"
  J2QM004      "Margins of error: Speak Spanish: Speak English 'very well'"
  J2QM005      "Margins of error: Speak Spanish: Speak English 'well'"
  J2QM006      "Margins of error: Speak Spanish: Speak English 'not well'"
  J2QM007      "Margins of error: Speak Spanish: Speak English 'not at all'"
  J2QM008      "Margins of error: Speak other language"
  J24M001      "Margins of error: Total"
  J24M002      "Margins of error: Income in the past 12 months below poverty level"
  J24M003      "Margins of error: Income in the past 12 months below poverty level: Male"
  J24M004      "Margins of error: Income in the past 12 months below poverty level: Male: Under 5 years"
  J24M005      "Margins of error: Income in the past 12 months below poverty level: Male: 5 years"
  J24M006      "Margins of error: Income in the past 12 months below poverty level: Male: 6 to 11 years"
  J24M007      "Margins of error: Income in the past 12 months below poverty level: Male: 12 to 14 years"
  J24M008      "Margins of error: Income in the past 12 months below poverty level: Male: 15 years"
  J24M009      "Margins of error: Income in the past 12 months below poverty level: Male: 16 and 17 years"
  J24M010      "Margins of error: Income in the past 12 months below poverty level: Male: 18 to 24 years"
  J24M011      "Margins of error: Income in the past 12 months below poverty level: Male: 25 to 34 years"
  J24M012      "Margins of error: Income in the past 12 months below poverty level: Male: 35 to 44 years"
  J24M013      "Margins of error: Income in the past 12 months below poverty level: Male: 45 to 54 years"
  J24M014      "Margins of error: Income in the past 12 months below poverty level: Male: 55 to 64 years"
  J24M015      "Margins of error: Income in the past 12 months below poverty level: Male: 65 to 74 years"
  J24M016      "Margins of error: Income in the past 12 months below poverty level: Male: 75 years and over"
  J24M017      "Margins of error: Income in the past 12 months below poverty level: Female"
  J24M018      "Margins of error: Income in the past 12 months below poverty level: Female: Under 5 years"
  J24M019      "Margins of error: Income in the past 12 months below poverty level: Female: 5 years"
  J24M020      "Margins of error: Income in the past 12 months below poverty level: Female: 6 to 11 years"
  J24M021      "Margins of error: Income in the past 12 months below poverty level: Female: 12 to 14 years"
  J24M022      "Margins of error: Income in the past 12 months below poverty level: Female: 15 years"
  J24M023      "Margins of error: Income in the past 12 months below poverty level: Female: 16 and 17 years"
  J24M024      "Margins of error: Income in the past 12 months below poverty level: Female: 18 to 24 years"
  J24M025      "Margins of error: Income in the past 12 months below poverty level: Female: 25 to 34 years"
  J24M026      "Margins of error: Income in the past 12 months below poverty level: Female: 35 to 44 years"
  J24M027      "Margins of error: Income in the past 12 months below poverty level: Female: 45 to 54 years"
  J24M028      "Margins of error: Income in the past 12 months below poverty level: Female: 55 to 64 years"
  J24M029      "Margins of error: Income in the past 12 months below poverty level: Female: 65 to 74 years"
  J24M030      "Margins of error: Income in the past 12 months below poverty level: Female: 75 years and over"
  J24M031      "Margins of error: Income in the past 12 months at or above poverty level"
  J24M032      "Margins of error: Income in the past 12 months at or above poverty level: Male"
  J24M033      "Margins of error: Income in the past 12 months at or above poverty level: Male: Under 5 years"
  J24M034      "Margins of error: Income in the past 12 months at or above poverty level: Male: 5 years"
  J24M035      "Margins of error: Income in the past 12 months at or above poverty level: Male: 6 to 11 years"
  J24M036      "Margins of error: Income in the past 12 months at or above poverty level: Male: 12 to 14 years"
  J24M037      "Margins of error: Income in the past 12 months at or above poverty level: Male: 15 years"
  J24M038      "Margins of error: Income in the past 12 months at or above poverty level: Male: 16 and 17 years"
  J24M039      "Margins of error: Income in the past 12 months at or above poverty level: Male: 18 to 24 years"
  J24M040      "Margins of error: Income in the past 12 months at or above poverty level: Male: 25 to 34 years"
  J24M041      "Margins of error: Income in the past 12 months at or above poverty level: Male: 35 to 44 years"
  J24M042      "Margins of error: Income in the past 12 months at or above poverty level: Male: 45 to 54 years"
  J24M043      "Margins of error: Income in the past 12 months at or above poverty level: Male: 55 to 64 years"
  J24M044      "Margins of error: Income in the past 12 months at or above poverty level: Male: 65 to 74 years"
  J24M045      "Margins of error: Income in the past 12 months at or above poverty level: Male: 75 years and over"
  J24M046      "Margins of error: Income in the past 12 months at or above poverty level: Female"
  J24M047      "Margins of error: Income in the past 12 months at or above poverty level: Female: Under 5 years"
  J24M048      "Margins of error: Income in the past 12 months at or above poverty level: Female: 5 years"
  J24M049      "Margins of error: Income in the past 12 months at or above poverty level: Female: 6 to 11 years"
  J24M050      "Margins of error: Income in the past 12 months at or above poverty level: Female: 12 to 14 years"
  J24M051      "Margins of error: Income in the past 12 months at or above poverty level: Female: 15 years"
  J24M052      "Margins of error: Income in the past 12 months at or above poverty level: Female: 16 and 17 years"
  J24M053      "Margins of error: Income in the past 12 months at or above poverty level: Female: 18 to 24 years"
  J24M054      "Margins of error: Income in the past 12 months at or above poverty level: Female: 25 to 34 years"
  J24M055      "Margins of error: Income in the past 12 months at or above poverty level: Female: 35 to 44 years"
  J24M056      "Margins of error: Income in the past 12 months at or above poverty level: Female: 45 to 54 years"
  J24M057      "Margins of error: Income in the past 12 months at or above poverty level: Female: 55 to 64 years"
  J24M058      "Margins of error: Income in the past 12 months at or above poverty level: Female: 65 to 74 years"
  J24M059      "Margins of error: Income in the past 12 months at or above poverty level: Female: 75 years and over"
  J30M001      "Margins of error: Total"
  J30M002      "Margins of error: Income in the past 12 months below poverty level"
  J30M003      "Margins of error: Income in the past 12 months below poverty level: Under 5 years"
  J30M004      "Margins of error: Income in the past 12 months below poverty level: 5 years"
  J30M005      "Margins of error: Income in the past 12 months below poverty level: 6 to 11 years"
  J30M006      "Margins of error: Income in the past 12 months below poverty level: 12 to 17 years"
  J30M007      "Margins of error: Income in the past 12 months below poverty level: 18 to 64 years"
  J30M008      "Margins of error: Income in the past 12 months below poverty level: 65 to 74 years"
  J30M009      "Margins of error: Income in the past 12 months below poverty level: 75 years and over"
  J30M010      "Margins of error: Income in the past 12 months at or above poverty level"
  J30M011      "Margins of error: Income in the past 12 months at or above poverty level: Under 5 years"
  J30M012      "Margins of error: Income in the past 12 months at or above poverty level: 5 years"
  J30M013      "Margins of error: Income in the past 12 months at or above poverty level: 6 to 11 years"
  J30M014      "Margins of error: Income in the past 12 months at or above poverty level: 12 to 17 years"
  J30M015      "Margins of error: Income in the past 12 months at or above poverty level: 18 to 64 years"
  J30M016      "Margins of error: Income in the past 12 months at or above poverty level: 65 to 74 years"
  J30M017      "Margins of error: Income in the past 12 months at or above poverty level: 75 years and over"
  J6NM001      "Margins of error: Total"
  J6NM002      "Margins of error: Household received Food Stamps/SNAP in the past 12 months"
  J6NM003      "Margins of error: Household did not receive Food Stamps/SNAP in the past 12 months"
  J6ZM001      "Margins of error: Total"
  J6ZM002      "Margins of error: Male"
  J6ZM003      "Margins of error: Male: 16 to 64 years"
  J6ZM004      "Margins of error: Male: 16 to 64 years: In labor force"
  J6ZM005      "Margins of error: Male: 16 to 64 years: In labor force: In Armed Forces"
  J6ZM006      "Margins of error: Male: 16 to 64 years: In labor force: Civilian"
  J6ZM007      "Margins of error: Male: 16 to 64 years: In labor force: Civilian: Employed"
  J6ZM008      "Margins of error: Male: 16 to 64 years: In labor force: Civilian: Unemployed"
  J6ZM009      "Margins of error: Male: 16 to 64 years: Not in labor force"
  J6ZM010      "Margins of error: Male: 65 years and over"
  J6ZM011      "Margins of error: Male: 65 years and over: In labor force"
  J6ZM012      "Margins of error: Male: 65 years and over: In labor force: Employed"
  J6ZM013      "Margins of error: Male: 65 years and over: In labor force: Unemployed"
  J6ZM014      "Margins of error: Male: 65 years and over: Not in labor force"
  J6ZM015      "Margins of error: Female"
  J6ZM016      "Margins of error: Female: 16 to 64 years"
  J6ZM017      "Margins of error: Female: 16 to 64 years: In labor force"
  J6ZM018      "Margins of error: Female: 16 to 64 years: In labor force: In Armed Forces"
  J6ZM019      "Margins of error: Female: 16 to 64 years: In labor force: Civilian"
  J6ZM020      "Margins of error: Female: 16 to 64 years: In labor force: Civilian: Employed"
  J6ZM021      "Margins of error: Female: 16 to 64 years: In labor force: Civilian: Unemployed"
  J6ZM022      "Margins of error: Female: 16 to 64 years: Not in labor force"
  J6ZM023      "Margins of error: Female: 65 years and over"
  J6ZM024      "Margins of error: Female: 65 years and over: In labor force"
  J6ZM025      "Margins of error: Female: 65 years and over: In labor force: Employed"
  J6ZM026      "Margins of error: Female: 65 years and over: In labor force: Unemployed"
  J6ZM027      "Margins of error: Female: 65 years and over: Not in labor force"
  J75M001      "Margins of error: Total"
  J75M002      "Margins of error: 1.00 or less occupants per room"
  J75M003      "Margins of error: 1.01 or more occupants per room"
.

execute.

