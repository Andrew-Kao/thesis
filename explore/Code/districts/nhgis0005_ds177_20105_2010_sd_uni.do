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
  str     concita    57-61      ///
  str     aianhha    62-65      ///
  str     res_onlya  66-69      ///
  str     trusta     70-73      ///
  str     aitscea    74-76      ///
  str     anrca      77-81      ///
  str     cbsaa      82-86      ///
  str     csaa       87-89      ///
  str     metdiva    90-94      ///
  str     nectaa     95-99      ///
  str     cnectaa    100-102    ///
  str     nectadiva  103-107    ///
  str     uaa        108-112    ///
  str     cdcurra    113-114    ///
  str     sldua      115-117    ///
  str     sldla      118-120    ///
  str     submcda    121-125    ///
  str     sdelma     126-130    ///
  str     sdseca     131-135    ///
  str     sduni      136-217    ///
  str     sdunia     218-222    ///
  str     puma5a     223-227    ///
  str     bttra      228-233    ///
  str     name_e     234-433    ///
  double  jxhe001    434-442    ///
  double  jxhe002    443-451    ///
  double  jxhe003    452-460    ///
  double  jxhe004    461-469    ///
  double  jxhe005    470-478    ///
  double  jyhe001    479-487    ///
  double  jyhe002    488-496    ///
  double  jyhe003    497-505    ///
  double  jyhe004    506-514    ///
  double  jyhe005    515-523    ///
  double  jyhe006    524-532    ///
  double  jzge001    533-541    ///
  double  jzge002    542-550    ///
  double  jzge003    551-559    ///
  double  jzge004    560-568    ///
  double  jzge005    569-577    ///
  double  jzge006    578-586    ///
  double  jzge007    587-595    ///
  double  j0he001    596-604    ///
  double  j0he002    605-613    ///
  double  j0he003    614-622    ///
  double  j0he004    623-631    ///
  double  j0he005    632-640    ///
  double  j0he006    641-649    ///
  double  j0he007    650-658    ///
  double  j05e001    659-667    ///
  double  j05e002    668-676    ///
  double  j05e003    677-685    ///
  double  j05e004    686-694    ///
  double  j05e005    695-703    ///
  double  j05e006    704-712    ///
  double  j05e007    713-721    ///
  double  j05e008    722-730    ///
  double  j05e009    731-739    ///
  double  j05e010    740-748    ///
  double  j05e011    749-757    ///
  double  j05e012    758-766    ///
  double  j05e013    767-775    ///
  double  j1se001    776-784    ///
  double  j1se002    785-793    ///
  double  j1se003    794-802    ///
  double  j1se004    803-811    ///
  double  j1se005    812-820    ///
  double  j1se006    821-829    ///
  double  j1se007    830-838    ///
  double  j2de001    839-847    ///
  double  j2de002    848-856    ///
  double  j2de003    857-865    ///
  double  j2de004    866-874    ///
  double  j2de005    875-883    ///
  double  j2de006    884-892    ///
  double  j2de007    893-901    ///
  double  j2de008    902-910    ///
  double  j2de009    911-919    ///
  double  j2de010    920-928    ///
  double  j2de011    929-937    ///
  double  j2pe001    938-946    ///
  double  j2pe002    947-955    ///
  double  j2pe003    956-964    ///
  double  j2pe004    965-973    ///
  double  j2pe005    974-982    ///
  double  j2pe006    983-991    ///
  double  j2pe007    992-1000   ///
  double  j2pe008    1001-1009  ///
  double  j2pe009    1010-1018  ///
  double  j2pe010    1019-1027  ///
  double  j2pe011    1028-1036  ///
  double  j2qe001    1037-1045  ///
  double  j2qe002    1046-1054  ///
  double  j2qe003    1055-1063  ///
  double  j2qe004    1064-1072  ///
  double  j2qe005    1073-1081  ///
  double  j2qe006    1082-1090  ///
  double  j2qe007    1091-1099  ///
  double  j2qe008    1100-1108  ///
  double  j24e001    1109-1117  ///
  double  j24e002    1118-1126  ///
  double  j24e003    1127-1135  ///
  double  j24e004    1136-1144  ///
  double  j24e005    1145-1153  ///
  double  j24e006    1154-1162  ///
  double  j24e007    1163-1171  ///
  double  j24e008    1172-1180  ///
  double  j24e009    1181-1189  ///
  double  j24e010    1190-1198  ///
  double  j24e011    1199-1207  ///
  double  j24e012    1208-1216  ///
  double  j24e013    1217-1225  ///
  double  j24e014    1226-1234  ///
  double  j24e015    1235-1243  ///
  double  j24e016    1244-1252  ///
  double  j24e017    1253-1261  ///
  double  j24e018    1262-1270  ///
  double  j24e019    1271-1279  ///
  double  j24e020    1280-1288  ///
  double  j24e021    1289-1297  ///
  double  j24e022    1298-1306  ///
  double  j24e023    1307-1315  ///
  double  j24e024    1316-1324  ///
  double  j24e025    1325-1333  ///
  double  j24e026    1334-1342  ///
  double  j24e027    1343-1351  ///
  double  j24e028    1352-1360  ///
  double  j24e029    1361-1369  ///
  double  j24e030    1370-1378  ///
  double  j24e031    1379-1387  ///
  double  j24e032    1388-1396  ///
  double  j24e033    1397-1405  ///
  double  j24e034    1406-1414  ///
  double  j24e035    1415-1423  ///
  double  j24e036    1424-1432  ///
  double  j24e037    1433-1441  ///
  double  j24e038    1442-1450  ///
  double  j24e039    1451-1459  ///
  double  j24e040    1460-1468  ///
  double  j24e041    1469-1477  ///
  double  j24e042    1478-1486  ///
  double  j24e043    1487-1495  ///
  double  j24e044    1496-1504  ///
  double  j24e045    1505-1513  ///
  double  j24e046    1514-1522  ///
  double  j24e047    1523-1531  ///
  double  j24e048    1532-1540  ///
  double  j24e049    1541-1549  ///
  double  j24e050    1550-1558  ///
  double  j24e051    1559-1567  ///
  double  j24e052    1568-1576  ///
  double  j24e053    1577-1585  ///
  double  j24e054    1586-1594  ///
  double  j24e055    1595-1603  ///
  double  j24e056    1604-1612  ///
  double  j24e057    1613-1621  ///
  double  j24e058    1622-1630  ///
  double  j24e059    1631-1639  ///
  double  j30e001    1640-1648  ///
  double  j30e002    1649-1657  ///
  double  j30e003    1658-1666  ///
  double  j30e004    1667-1675  ///
  double  j30e005    1676-1684  ///
  double  j30e006    1685-1693  ///
  double  j30e007    1694-1702  ///
  double  j30e008    1703-1711  ///
  double  j30e009    1712-1720  ///
  double  j30e010    1721-1729  ///
  double  j30e011    1730-1738  ///
  double  j30e012    1739-1747  ///
  double  j30e013    1748-1756  ///
  double  j30e014    1757-1765  ///
  double  j30e015    1766-1774  ///
  double  j30e016    1775-1783  ///
  double  j30e017    1784-1792  ///
  double  j6ne001    1793-1801  ///
  double  j6ne002    1802-1810  ///
  double  j6ne003    1811-1819  ///
  double  j6ze001    1820-1828  ///
  double  j6ze002    1829-1837  ///
  double  j6ze003    1838-1846  ///
  double  j6ze004    1847-1855  ///
  double  j6ze005    1856-1864  ///
  double  j6ze006    1865-1873  ///
  double  j6ze007    1874-1882  ///
  double  j6ze008    1883-1891  ///
  double  j6ze009    1892-1900  ///
  double  j6ze010    1901-1909  ///
  double  j6ze011    1910-1918  ///
  double  j6ze012    1919-1927  ///
  double  j6ze013    1928-1936  ///
  double  j6ze014    1937-1945  ///
  double  j6ze015    1946-1954  ///
  double  j6ze016    1955-1963  ///
  double  j6ze017    1964-1972  ///
  double  j6ze018    1973-1981  ///
  double  j6ze019    1982-1990  ///
  double  j6ze020    1991-1999  ///
  double  j6ze021    2000-2008  ///
  double  j6ze022    2009-2017  ///
  double  j6ze023    2018-2026  ///
  double  j6ze024    2027-2035  ///
  double  j6ze025    2036-2044  ///
  double  j6ze026    2045-2053  ///
  double  j6ze027    2054-2062  ///
  double  j75e001    2063-2071  ///
  double  j75e002    2072-2080  ///
  double  j75e003    2081-2089  ///
  str     name_m     2090-2289  ///
  double  jxhm001    2290-2298  ///
  double  jxhm002    2299-2307  ///
  double  jxhm003    2308-2316  ///
  double  jxhm004    2317-2325  ///
  double  jxhm005    2326-2334  ///
  double  jyhm001    2335-2343  ///
  double  jyhm002    2344-2352  ///
  double  jyhm003    2353-2361  ///
  double  jyhm004    2362-2370  ///
  double  jyhm005    2371-2379  ///
  double  jyhm006    2380-2388  ///
  double  jzgm001    2389-2397  ///
  double  jzgm002    2398-2406  ///
  double  jzgm003    2407-2415  ///
  double  jzgm004    2416-2424  ///
  double  jzgm005    2425-2433  ///
  double  jzgm006    2434-2442  ///
  double  jzgm007    2443-2451  ///
  double  j0hm001    2452-2460  ///
  double  j0hm002    2461-2469  ///
  double  j0hm003    2470-2478  ///
  double  j0hm004    2479-2487  ///
  double  j0hm005    2488-2496  ///
  double  j0hm006    2497-2505  ///
  double  j0hm007    2506-2514  ///
  double  j05m001    2515-2523  ///
  double  j05m002    2524-2532  ///
  double  j05m003    2533-2541  ///
  double  j05m004    2542-2550  ///
  double  j05m005    2551-2559  ///
  double  j05m006    2560-2568  ///
  double  j05m007    2569-2577  ///
  double  j05m008    2578-2586  ///
  double  j05m009    2587-2595  ///
  double  j05m010    2596-2604  ///
  double  j05m011    2605-2613  ///
  double  j05m012    2614-2622  ///
  double  j05m013    2623-2631  ///
  double  j1sm001    2632-2640  ///
  double  j1sm002    2641-2649  ///
  double  j1sm003    2650-2658  ///
  double  j1sm004    2659-2667  ///
  double  j1sm005    2668-2676  ///
  double  j1sm006    2677-2685  ///
  double  j1sm007    2686-2694  ///
  double  j2dm001    2695-2703  ///
  double  j2dm002    2704-2712  ///
  double  j2dm003    2713-2721  ///
  double  j2dm004    2722-2730  ///
  double  j2dm005    2731-2739  ///
  double  j2dm006    2740-2748  ///
  double  j2dm007    2749-2757  ///
  double  j2dm008    2758-2766  ///
  double  j2dm009    2767-2775  ///
  double  j2dm010    2776-2784  ///
  double  j2dm011    2785-2793  ///
  double  j2pm001    2794-2802  ///
  double  j2pm002    2803-2811  ///
  double  j2pm003    2812-2820  ///
  double  j2pm004    2821-2829  ///
  double  j2pm005    2830-2838  ///
  double  j2pm006    2839-2847  ///
  double  j2pm007    2848-2856  ///
  double  j2pm008    2857-2865  ///
  double  j2pm009    2866-2874  ///
  double  j2pm010    2875-2883  ///
  double  j2pm011    2884-2892  ///
  double  j2qm001    2893-2901  ///
  double  j2qm002    2902-2910  ///
  double  j2qm003    2911-2919  ///
  double  j2qm004    2920-2928  ///
  double  j2qm005    2929-2937  ///
  double  j2qm006    2938-2946  ///
  double  j2qm007    2947-2955  ///
  double  j2qm008    2956-2964  ///
  double  j24m001    2965-2973  ///
  double  j24m002    2974-2982  ///
  double  j24m003    2983-2991  ///
  double  j24m004    2992-3000  ///
  double  j24m005    3001-3009  ///
  double  j24m006    3010-3018  ///
  double  j24m007    3019-3027  ///
  double  j24m008    3028-3036  ///
  double  j24m009    3037-3045  ///
  double  j24m010    3046-3054  ///
  double  j24m011    3055-3063  ///
  double  j24m012    3064-3072  ///
  double  j24m013    3073-3081  ///
  double  j24m014    3082-3090  ///
  double  j24m015    3091-3099  ///
  double  j24m016    3100-3108  ///
  double  j24m017    3109-3117  ///
  double  j24m018    3118-3126  ///
  double  j24m019    3127-3135  ///
  double  j24m020    3136-3144  ///
  double  j24m021    3145-3153  ///
  double  j24m022    3154-3162  ///
  double  j24m023    3163-3171  ///
  double  j24m024    3172-3180  ///
  double  j24m025    3181-3189  ///
  double  j24m026    3190-3198  ///
  double  j24m027    3199-3207  ///
  double  j24m028    3208-3216  ///
  double  j24m029    3217-3225  ///
  double  j24m030    3226-3234  ///
  double  j24m031    3235-3243  ///
  double  j24m032    3244-3252  ///
  double  j24m033    3253-3261  ///
  double  j24m034    3262-3270  ///
  double  j24m035    3271-3279  ///
  double  j24m036    3280-3288  ///
  double  j24m037    3289-3297  ///
  double  j24m038    3298-3306  ///
  double  j24m039    3307-3315  ///
  double  j24m040    3316-3324  ///
  double  j24m041    3325-3333  ///
  double  j24m042    3334-3342  ///
  double  j24m043    3343-3351  ///
  double  j24m044    3352-3360  ///
  double  j24m045    3361-3369  ///
  double  j24m046    3370-3378  ///
  double  j24m047    3379-3387  ///
  double  j24m048    3388-3396  ///
  double  j24m049    3397-3405  ///
  double  j24m050    3406-3414  ///
  double  j24m051    3415-3423  ///
  double  j24m052    3424-3432  ///
  double  j24m053    3433-3441  ///
  double  j24m054    3442-3450  ///
  double  j24m055    3451-3459  ///
  double  j24m056    3460-3468  ///
  double  j24m057    3469-3477  ///
  double  j24m058    3478-3486  ///
  double  j24m059    3487-3495  ///
  double  j30m001    3496-3504  ///
  double  j30m002    3505-3513  ///
  double  j30m003    3514-3522  ///
  double  j30m004    3523-3531  ///
  double  j30m005    3532-3540  ///
  double  j30m006    3541-3549  ///
  double  j30m007    3550-3558  ///
  double  j30m008    3559-3567  ///
  double  j30m009    3568-3576  ///
  double  j30m010    3577-3585  ///
  double  j30m011    3586-3594  ///
  double  j30m012    3595-3603  ///
  double  j30m013    3604-3612  ///
  double  j30m014    3613-3621  ///
  double  j30m015    3622-3630  ///
  double  j30m016    3631-3639  ///
  double  j30m017    3640-3648  ///
  double  j6nm001    3649-3657  ///
  double  j6nm002    3658-3666  ///
  double  j6nm003    3667-3675  ///
  double  j6zm001    3676-3684  ///
  double  j6zm002    3685-3693  ///
  double  j6zm003    3694-3702  ///
  double  j6zm004    3703-3711  ///
  double  j6zm005    3712-3720  ///
  double  j6zm006    3721-3729  ///
  double  j6zm007    3730-3738  ///
  double  j6zm008    3739-3747  ///
  double  j6zm009    3748-3756  ///
  double  j6zm010    3757-3765  ///
  double  j6zm011    3766-3774  ///
  double  j6zm012    3775-3783  ///
  double  j6zm013    3784-3792  ///
  double  j6zm014    3793-3801  ///
  double  j6zm015    3802-3810  ///
  double  j6zm016    3811-3819  ///
  double  j6zm017    3820-3828  ///
  double  j6zm018    3829-3837  ///
  double  j6zm019    3838-3846  ///
  double  j6zm020    3847-3855  ///
  double  j6zm021    3856-3864  ///
  double  j6zm022    3865-3873  ///
  double  j6zm023    3874-3882  ///
  double  j6zm024    3883-3891  ///
  double  j6zm025    3892-3900  ///
  double  j6zm026    3901-3909  ///
  double  j6zm027    3910-3918  ///
  double  j75m001    3919-3927  ///
  double  j75m002    3928-3936  ///
  double  j75m003    3937-3945  ///
  using `"nhgis0005_ds177_20105_2010_sd_uni.dat"'


format jxhe001   %9.0f
format jxhe002   %9.0f
format jxhe003   %9.0f
format jxhe004   %9.0f
format jxhe005   %9.0f
format jyhe001   %9.0f
format jyhe002   %9.0f
format jyhe003   %9.0f
format jyhe004   %9.0f
format jyhe005   %9.0f
format jyhe006   %9.0f
format jzge001   %9.0f
format jzge002   %9.0f
format jzge003   %9.0f
format jzge004   %9.0f
format jzge005   %9.0f
format jzge006   %9.0f
format jzge007   %9.0f
format j0he001   %9.0f
format j0he002   %9.0f
format j0he003   %9.0f
format j0he004   %9.0f
format j0he005   %9.0f
format j0he006   %9.0f
format j0he007   %9.0f
format j05e001   %9.0f
format j05e002   %9.0f
format j05e003   %9.0f
format j05e004   %9.0f
format j05e005   %9.0f
format j05e006   %9.0f
format j05e007   %9.0f
format j05e008   %9.0f
format j05e009   %9.0f
format j05e010   %9.0f
format j05e011   %9.0f
format j05e012   %9.0f
format j05e013   %9.0f
format j1se001   %9.0f
format j1se002   %9.0f
format j1se003   %9.0f
format j1se004   %9.0f
format j1se005   %9.0f
format j1se006   %9.0f
format j1se007   %9.0f
format j2de001   %9.0f
format j2de002   %9.0f
format j2de003   %9.0f
format j2de004   %9.0f
format j2de005   %9.0f
format j2de006   %9.0f
format j2de007   %9.0f
format j2de008   %9.0f
format j2de009   %9.0f
format j2de010   %9.0f
format j2de011   %9.0f
format j2pe001   %9.0f
format j2pe002   %9.0f
format j2pe003   %9.0f
format j2pe004   %9.0f
format j2pe005   %9.0f
format j2pe006   %9.0f
format j2pe007   %9.0f
format j2pe008   %9.0f
format j2pe009   %9.0f
format j2pe010   %9.0f
format j2pe011   %9.0f
format j2qe001   %9.0f
format j2qe002   %9.0f
format j2qe003   %9.0f
format j2qe004   %9.0f
format j2qe005   %9.0f
format j2qe006   %9.0f
format j2qe007   %9.0f
format j2qe008   %9.0f
format j24e001   %9.0f
format j24e002   %9.0f
format j24e003   %9.0f
format j24e004   %9.0f
format j24e005   %9.0f
format j24e006   %9.0f
format j24e007   %9.0f
format j24e008   %9.0f
format j24e009   %9.0f
format j24e010   %9.0f
format j24e011   %9.0f
format j24e012   %9.0f
format j24e013   %9.0f
format j24e014   %9.0f
format j24e015   %9.0f
format j24e016   %9.0f
format j24e017   %9.0f
format j24e018   %9.0f
format j24e019   %9.0f
format j24e020   %9.0f
format j24e021   %9.0f
format j24e022   %9.0f
format j24e023   %9.0f
format j24e024   %9.0f
format j24e025   %9.0f
format j24e026   %9.0f
format j24e027   %9.0f
format j24e028   %9.0f
format j24e029   %9.0f
format j24e030   %9.0f
format j24e031   %9.0f
format j24e032   %9.0f
format j24e033   %9.0f
format j24e034   %9.0f
format j24e035   %9.0f
format j24e036   %9.0f
format j24e037   %9.0f
format j24e038   %9.0f
format j24e039   %9.0f
format j24e040   %9.0f
format j24e041   %9.0f
format j24e042   %9.0f
format j24e043   %9.0f
format j24e044   %9.0f
format j24e045   %9.0f
format j24e046   %9.0f
format j24e047   %9.0f
format j24e048   %9.0f
format j24e049   %9.0f
format j24e050   %9.0f
format j24e051   %9.0f
format j24e052   %9.0f
format j24e053   %9.0f
format j24e054   %9.0f
format j24e055   %9.0f
format j24e056   %9.0f
format j24e057   %9.0f
format j24e058   %9.0f
format j24e059   %9.0f
format j30e001   %9.0f
format j30e002   %9.0f
format j30e003   %9.0f
format j30e004   %9.0f
format j30e005   %9.0f
format j30e006   %9.0f
format j30e007   %9.0f
format j30e008   %9.0f
format j30e009   %9.0f
format j30e010   %9.0f
format j30e011   %9.0f
format j30e012   %9.0f
format j30e013   %9.0f
format j30e014   %9.0f
format j30e015   %9.0f
format j30e016   %9.0f
format j30e017   %9.0f
format j6ne001   %9.0f
format j6ne002   %9.0f
format j6ne003   %9.0f
format j6ze001   %9.0f
format j6ze002   %9.0f
format j6ze003   %9.0f
format j6ze004   %9.0f
format j6ze005   %9.0f
format j6ze006   %9.0f
format j6ze007   %9.0f
format j6ze008   %9.0f
format j6ze009   %9.0f
format j6ze010   %9.0f
format j6ze011   %9.0f
format j6ze012   %9.0f
format j6ze013   %9.0f
format j6ze014   %9.0f
format j6ze015   %9.0f
format j6ze016   %9.0f
format j6ze017   %9.0f
format j6ze018   %9.0f
format j6ze019   %9.0f
format j6ze020   %9.0f
format j6ze021   %9.0f
format j6ze022   %9.0f
format j6ze023   %9.0f
format j6ze024   %9.0f
format j6ze025   %9.0f
format j6ze026   %9.0f
format j6ze027   %9.0f
format j75e001   %9.0f
format j75e002   %9.0f
format j75e003   %9.0f
format jxhm001   %9.0f
format jxhm002   %9.0f
format jxhm003   %9.0f
format jxhm004   %9.0f
format jxhm005   %9.0f
format jyhm001   %9.0f
format jyhm002   %9.0f
format jyhm003   %9.0f
format jyhm004   %9.0f
format jyhm005   %9.0f
format jyhm006   %9.0f
format jzgm001   %9.0f
format jzgm002   %9.0f
format jzgm003   %9.0f
format jzgm004   %9.0f
format jzgm005   %9.0f
format jzgm006   %9.0f
format jzgm007   %9.0f
format j0hm001   %9.0f
format j0hm002   %9.0f
format j0hm003   %9.0f
format j0hm004   %9.0f
format j0hm005   %9.0f
format j0hm006   %9.0f
format j0hm007   %9.0f
format j05m001   %9.0f
format j05m002   %9.0f
format j05m003   %9.0f
format j05m004   %9.0f
format j05m005   %9.0f
format j05m006   %9.0f
format j05m007   %9.0f
format j05m008   %9.0f
format j05m009   %9.0f
format j05m010   %9.0f
format j05m011   %9.0f
format j05m012   %9.0f
format j05m013   %9.0f
format j1sm001   %9.0f
format j1sm002   %9.0f
format j1sm003   %9.0f
format j1sm004   %9.0f
format j1sm005   %9.0f
format j1sm006   %9.0f
format j1sm007   %9.0f
format j2dm001   %9.0f
format j2dm002   %9.0f
format j2dm003   %9.0f
format j2dm004   %9.0f
format j2dm005   %9.0f
format j2dm006   %9.0f
format j2dm007   %9.0f
format j2dm008   %9.0f
format j2dm009   %9.0f
format j2dm010   %9.0f
format j2dm011   %9.0f
format j2pm001   %9.0f
format j2pm002   %9.0f
format j2pm003   %9.0f
format j2pm004   %9.0f
format j2pm005   %9.0f
format j2pm006   %9.0f
format j2pm007   %9.0f
format j2pm008   %9.0f
format j2pm009   %9.0f
format j2pm010   %9.0f
format j2pm011   %9.0f
format j2qm001   %9.0f
format j2qm002   %9.0f
format j2qm003   %9.0f
format j2qm004   %9.0f
format j2qm005   %9.0f
format j2qm006   %9.0f
format j2qm007   %9.0f
format j2qm008   %9.0f
format j24m001   %9.0f
format j24m002   %9.0f
format j24m003   %9.0f
format j24m004   %9.0f
format j24m005   %9.0f
format j24m006   %9.0f
format j24m007   %9.0f
format j24m008   %9.0f
format j24m009   %9.0f
format j24m010   %9.0f
format j24m011   %9.0f
format j24m012   %9.0f
format j24m013   %9.0f
format j24m014   %9.0f
format j24m015   %9.0f
format j24m016   %9.0f
format j24m017   %9.0f
format j24m018   %9.0f
format j24m019   %9.0f
format j24m020   %9.0f
format j24m021   %9.0f
format j24m022   %9.0f
format j24m023   %9.0f
format j24m024   %9.0f
format j24m025   %9.0f
format j24m026   %9.0f
format j24m027   %9.0f
format j24m028   %9.0f
format j24m029   %9.0f
format j24m030   %9.0f
format j24m031   %9.0f
format j24m032   %9.0f
format j24m033   %9.0f
format j24m034   %9.0f
format j24m035   %9.0f
format j24m036   %9.0f
format j24m037   %9.0f
format j24m038   %9.0f
format j24m039   %9.0f
format j24m040   %9.0f
format j24m041   %9.0f
format j24m042   %9.0f
format j24m043   %9.0f
format j24m044   %9.0f
format j24m045   %9.0f
format j24m046   %9.0f
format j24m047   %9.0f
format j24m048   %9.0f
format j24m049   %9.0f
format j24m050   %9.0f
format j24m051   %9.0f
format j24m052   %9.0f
format j24m053   %9.0f
format j24m054   %9.0f
format j24m055   %9.0f
format j24m056   %9.0f
format j24m057   %9.0f
format j24m058   %9.0f
format j24m059   %9.0f
format j30m001   %9.0f
format j30m002   %9.0f
format j30m003   %9.0f
format j30m004   %9.0f
format j30m005   %9.0f
format j30m006   %9.0f
format j30m007   %9.0f
format j30m008   %9.0f
format j30m009   %9.0f
format j30m010   %9.0f
format j30m011   %9.0f
format j30m012   %9.0f
format j30m013   %9.0f
format j30m014   %9.0f
format j30m015   %9.0f
format j30m016   %9.0f
format j30m017   %9.0f
format j6nm001   %9.0f
format j6nm002   %9.0f
format j6nm003   %9.0f
format j6zm001   %9.0f
format j6zm002   %9.0f
format j6zm003   %9.0f
format j6zm004   %9.0f
format j6zm005   %9.0f
format j6zm006   %9.0f
format j6zm007   %9.0f
format j6zm008   %9.0f
format j6zm009   %9.0f
format j6zm010   %9.0f
format j6zm011   %9.0f
format j6zm012   %9.0f
format j6zm013   %9.0f
format j6zm014   %9.0f
format j6zm015   %9.0f
format j6zm016   %9.0f
format j6zm017   %9.0f
format j6zm018   %9.0f
format j6zm019   %9.0f
format j6zm020   %9.0f
format j6zm021   %9.0f
format j6zm022   %9.0f
format j6zm023   %9.0f
format j6zm024   %9.0f
format j6zm025   %9.0f
format j6zm026   %9.0f
format j6zm027   %9.0f
format j75m001   %9.0f
format j75m002   %9.0f
format j75m003   %9.0f

label var year      `"Data File Year"'
label var regiona   `"Region Code"'
label var divisiona `"Division Code"'
label var state     `"State Name"'
label var statea    `"State Code"'
label var countya   `"County Code"'
label var cousuba   `"County Subdivision Code"'
label var placea    `"Place Code"'
label var tracta    `"Census Tract Code"'
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
label var name_e    `"Estimates: Area Name"'
label var jxhe001   `"Estimates: Total"'
label var jxhe002   `"Estimates: Born in state of residence"'
label var jxhe003   `"Estimates: Born in other state in the United States"'
label var jxhe004   `"Estimates: Native; born outside the United States"'
label var jxhe005   `"Estimates: Foreign born"'
label var jyhe001   `"Estimates: Total"'
label var jyhe002   `"Estimates: Same house 1 year ago"'
label var jyhe003   `"Estimates: Moved within same county"'
label var jyhe004   `"Estimates: Moved from different county within same state"'
label var jyhe005   `"Estimates: Moved from different state"'
label var jyhe006   `"Estimates: Moved from abroad"'
label var jzge001   `"Estimates: Total"'
label var jzge002   `"Estimates: Car, truck, or van - drove alone"'
label var jzge003   `"Estimates: Car, truck, or van - carpooled"'
label var jzge004   `"Estimates: Public transportation (excluding taxicab)"'
label var jzge005   `"Estimates: Walked"'
label var jzge006   `"Estimates: Taxicab, motorcycle, bicycle, or other means"'
label var jzge007   `"Estimates: Worked at home"'
label var j0he001   `"Estimates: Total"'
label var j0he002   `"Estimates: Grandparent responsible for own grandchildren under 18 years"'
label var j0he003   `"Estimates: Grandparent responsible for own grandchildren under 18 years: 30 to 5"'
label var j0he004   `"Estimates: Grandparent responsible for own grandchildren under 18 years: 60 year"'
label var j0he005   `"Estimates: Grandparent not responsible for own grandchildren under 18 years"'
label var j0he006   `"Estimates: Grandparent not responsible for own grandchildren under 18 years: 30 "'
label var j0he007   `"Estimates: Grandparent not responsible for own grandchildren under 18 years: 60 "'
label var j05e001   `"Estimates: Total"'
label var j05e002   `"Estimates: Male"'
label var j05e003   `"Estimates: Male: Never married"'
label var j05e004   `"Estimates: Male: Now married (except separated)"'
label var j05e005   `"Estimates: Male: Separated"'
label var j05e006   `"Estimates: Male: Widowed"'
label var j05e007   `"Estimates: Male: Divorced"'
label var j05e008   `"Estimates: Female"'
label var j05e009   `"Estimates: Female: Never married"'
label var j05e010   `"Estimates: Female: Now married (except separated)"'
label var j05e011   `"Estimates: Female: Separated"'
label var j05e012   `"Estimates: Female: Widowed"'
label var j05e013   `"Estimates: Female: Divorced"'
label var j1se001   `"Estimates: Total"'
label var j1se002   `"Estimates: Women who had a birth in the past 12 months"'
label var j1se003   `"Estimates: Women who had a birth in the past 12 months: Now married (including s"'
label var j1se004   `"Estimates: Women who had a birth in the past 12 months: Unmarried (never married"'
label var j1se005   `"Estimates: Women who did not have a birth in the past 12 months"'
label var j1se006   `"Estimates: Women who did not have a birth in the past 12 months: Now married (in"'
label var j1se007   `"Estimates: Women who did not have a birth in the past 12 months: Unmarried (neve"'
label var j2de001   `"Estimates: Total"'
label var j2de002   `"Estimates: Male"'
label var j2de003   `"Estimates: Male: Less than high school diploma"'
label var j2de004   `"Estimates: Male: High school graduate, GED, or alternative"'
label var j2de005   `"Estimates: Male: Some college or associate's degree"'
label var j2de006   `"Estimates: Male: Bachelor's degree or higher"'
label var j2de007   `"Estimates: Female"'
label var j2de008   `"Estimates: Female: Less than high school diploma"'
label var j2de009   `"Estimates: Female: High school graduate, GED, or alternative"'
label var j2de010   `"Estimates: Female: Some college or associate's degree"'
label var j2de011   `"Estimates: Female: Bachelor's degree or higher"'
label var j2pe001   `"Estimates: Total"'
label var j2pe002   `"Estimates: Native"'
label var j2pe003   `"Estimates: Native: Speak only English"'
label var j2pe004   `"Estimates: Native: Speak another language"'
label var j2pe005   `"Estimates: Native: Speak another language: Speak English 'very well'"'
label var j2pe006   `"Estimates: Native: Speak another language: Speak English less than 'very well'"'
label var j2pe007   `"Estimates: Foreign born"'
label var j2pe008   `"Estimates: Foreign born: Speak only English"'
label var j2pe009   `"Estimates: Foreign born: Speak another language"'
label var j2pe010   `"Estimates: Foreign born: Speak another language: Speak English 'very well'"'
label var j2pe011   `"Estimates: Foreign born: Speak another language: Speak English less than 'very w"'
label var j2qe001   `"Estimates: Total"'
label var j2qe002   `"Estimates: Speak only English"'
label var j2qe003   `"Estimates: Speak Spanish"'
label var j2qe004   `"Estimates: Speak Spanish: Speak English 'very well'"'
label var j2qe005   `"Estimates: Speak Spanish: Speak English 'well'"'
label var j2qe006   `"Estimates: Speak Spanish: Speak English 'not well'"'
label var j2qe007   `"Estimates: Speak Spanish: Speak English 'not at all'"'
label var j2qe008   `"Estimates: Speak other language"'
label var j24e001   `"Estimates: Total"'
label var j24e002   `"Estimates: Income in the past 12 months below poverty level"'
label var j24e003   `"Estimates: Income in the past 12 months below poverty level: Male"'
label var j24e004   `"Estimates: Income in the past 12 months below poverty level: Male: Under 5 years"'
label var j24e005   `"Estimates: Income in the past 12 months below poverty level: Male: 5 years"'
label var j24e006   `"Estimates: Income in the past 12 months below poverty level: Male: 6 to 11 years"'
label var j24e007   `"Estimates: Income in the past 12 months below poverty level: Male: 12 to 14 year"'
label var j24e008   `"Estimates: Income in the past 12 months below poverty level: Male: 15 years"'
label var j24e009   `"Estimates: Income in the past 12 months below poverty level: Male: 16 and 17 yea"'
label var j24e010   `"Estimates: Income in the past 12 months below poverty level: Male: 18 to 24 year"'
label var j24e011   `"Estimates: Income in the past 12 months below poverty level: Male: 25 to 34 year"'
label var j24e012   `"Estimates: Income in the past 12 months below poverty level: Male: 35 to 44 year"'
label var j24e013   `"Estimates: Income in the past 12 months below poverty level: Male: 45 to 54 year"'
label var j24e014   `"Estimates: Income in the past 12 months below poverty level: Male: 55 to 64 year"'
label var j24e015   `"Estimates: Income in the past 12 months below poverty level: Male: 65 to 74 year"'
label var j24e016   `"Estimates: Income in the past 12 months below poverty level: Male: 75 years and "'
label var j24e017   `"Estimates: Income in the past 12 months below poverty level: Female"'
label var j24e018   `"Estimates: Income in the past 12 months below poverty level: Female: Under 5 yea"'
label var j24e019   `"Estimates: Income in the past 12 months below poverty level: Female: 5 years"'
label var j24e020   `"Estimates: Income in the past 12 months below poverty level: Female: 6 to 11 yea"'
label var j24e021   `"Estimates: Income in the past 12 months below poverty level: Female: 12 to 14 ye"'
label var j24e022   `"Estimates: Income in the past 12 months below poverty level: Female: 15 years"'
label var j24e023   `"Estimates: Income in the past 12 months below poverty level: Female: 16 and 17 y"'
label var j24e024   `"Estimates: Income in the past 12 months below poverty level: Female: 18 to 24 ye"'
label var j24e025   `"Estimates: Income in the past 12 months below poverty level: Female: 25 to 34 ye"'
label var j24e026   `"Estimates: Income in the past 12 months below poverty level: Female: 35 to 44 ye"'
label var j24e027   `"Estimates: Income in the past 12 months below poverty level: Female: 45 to 54 ye"'
label var j24e028   `"Estimates: Income in the past 12 months below poverty level: Female: 55 to 64 ye"'
label var j24e029   `"Estimates: Income in the past 12 months below poverty level: Female: 65 to 74 ye"'
label var j24e030   `"Estimates: Income in the past 12 months below poverty level: Female: 75 years an"'
label var j24e031   `"Estimates: Income in the past 12 months at or above poverty level"'
label var j24e032   `"Estimates: Income in the past 12 months at or above poverty level: Male"'
label var j24e033   `"Estimates: Income in the past 12 months at or above poverty level: Male: Under 5"'
label var j24e034   `"Estimates: Income in the past 12 months at or above poverty level: Male: 5 years"'
label var j24e035   `"Estimates: Income in the past 12 months at or above poverty level: Male: 6 to 11"'
label var j24e036   `"Estimates: Income in the past 12 months at or above poverty level: Male: 12 to 1"'
label var j24e037   `"Estimates: Income in the past 12 months at or above poverty level: Male: 15 year"'
label var j24e038   `"Estimates: Income in the past 12 months at or above poverty level: Male: 16 and "'
label var j24e039   `"Estimates: Income in the past 12 months at or above poverty level: Male: 18 to 2"'
label var j24e040   `"Estimates: Income in the past 12 months at or above poverty level: Male: 25 to 3"'
label var j24e041   `"Estimates: Income in the past 12 months at or above poverty level: Male: 35 to 4"'
label var j24e042   `"Estimates: Income in the past 12 months at or above poverty level: Male: 45 to 5"'
label var j24e043   `"Estimates: Income in the past 12 months at or above poverty level: Male: 55 to 6"'
label var j24e044   `"Estimates: Income in the past 12 months at or above poverty level: Male: 65 to 7"'
label var j24e045   `"Estimates: Income in the past 12 months at or above poverty level: Male: 75 year"'
label var j24e046   `"Estimates: Income in the past 12 months at or above poverty level: Female"'
label var j24e047   `"Estimates: Income in the past 12 months at or above poverty level: Female: Under"'
label var j24e048   `"Estimates: Income in the past 12 months at or above poverty level: Female: 5 yea"'
label var j24e049   `"Estimates: Income in the past 12 months at or above poverty level: Female: 6 to "'
label var j24e050   `"Estimates: Income in the past 12 months at or above poverty level: Female: 12 to"'
label var j24e051   `"Estimates: Income in the past 12 months at or above poverty level: Female: 15 ye"'
label var j24e052   `"Estimates: Income in the past 12 months at or above poverty level: Female: 16 an"'
label var j24e053   `"Estimates: Income in the past 12 months at or above poverty level: Female: 18 to"'
label var j24e054   `"Estimates: Income in the past 12 months at or above poverty level: Female: 25 to"'
label var j24e055   `"Estimates: Income in the past 12 months at or above poverty level: Female: 35 to"'
label var j24e056   `"Estimates: Income in the past 12 months at or above poverty level: Female: 45 to"'
label var j24e057   `"Estimates: Income in the past 12 months at or above poverty level: Female: 55 to"'
label var j24e058   `"Estimates: Income in the past 12 months at or above poverty level: Female: 65 to"'
label var j24e059   `"Estimates: Income in the past 12 months at or above poverty level: Female: 75 ye"'
label var j30e001   `"Estimates: Total"'
label var j30e002   `"Estimates: Income in the past 12 months below poverty level"'
label var j30e003   `"Estimates: Income in the past 12 months below poverty level: Under 5 years"'
label var j30e004   `"Estimates: Income in the past 12 months below poverty level: 5 years"'
label var j30e005   `"Estimates: Income in the past 12 months below poverty level: 6 to 11 years"'
label var j30e006   `"Estimates: Income in the past 12 months below poverty level: 12 to 17 years"'
label var j30e007   `"Estimates: Income in the past 12 months below poverty level: 18 to 64 years"'
label var j30e008   `"Estimates: Income in the past 12 months below poverty level: 65 to 74 years"'
label var j30e009   `"Estimates: Income in the past 12 months below poverty level: 75 years and over"'
label var j30e010   `"Estimates: Income in the past 12 months at or above poverty level"'
label var j30e011   `"Estimates: Income in the past 12 months at or above poverty level: Under 5 years"'
label var j30e012   `"Estimates: Income in the past 12 months at or above poverty level: 5 years"'
label var j30e013   `"Estimates: Income in the past 12 months at or above poverty level: 6 to 11 years"'
label var j30e014   `"Estimates: Income in the past 12 months at or above poverty level: 12 to 17 year"'
label var j30e015   `"Estimates: Income in the past 12 months at or above poverty level: 18 to 64 year"'
label var j30e016   `"Estimates: Income in the past 12 months at or above poverty level: 65 to 74 year"'
label var j30e017   `"Estimates: Income in the past 12 months at or above poverty level: 75 years and "'
label var j6ne001   `"Estimates: Total"'
label var j6ne002   `"Estimates: Household received Food Stamps/SNAP in the past 12 months"'
label var j6ne003   `"Estimates: Household did not receive Food Stamps/SNAP in the past 12 months"'
label var j6ze001   `"Estimates: Total"'
label var j6ze002   `"Estimates: Male"'
label var j6ze003   `"Estimates: Male: 16 to 64 years"'
label var j6ze004   `"Estimates: Male: 16 to 64 years: In labor force"'
label var j6ze005   `"Estimates: Male: 16 to 64 years: In labor force: In Armed Forces"'
label var j6ze006   `"Estimates: Male: 16 to 64 years: In labor force: Civilian"'
label var j6ze007   `"Estimates: Male: 16 to 64 years: In labor force: Civilian: Employed"'
label var j6ze008   `"Estimates: Male: 16 to 64 years: In labor force: Civilian: Unemployed"'
label var j6ze009   `"Estimates: Male: 16 to 64 years: Not in labor force"'
label var j6ze010   `"Estimates: Male: 65 years and over"'
label var j6ze011   `"Estimates: Male: 65 years and over: In labor force"'
label var j6ze012   `"Estimates: Male: 65 years and over: In labor force: Employed"'
label var j6ze013   `"Estimates: Male: 65 years and over: In labor force: Unemployed"'
label var j6ze014   `"Estimates: Male: 65 years and over: Not in labor force"'
label var j6ze015   `"Estimates: Female"'
label var j6ze016   `"Estimates: Female: 16 to 64 years"'
label var j6ze017   `"Estimates: Female: 16 to 64 years: In labor force"'
label var j6ze018   `"Estimates: Female: 16 to 64 years: In labor force: In Armed Forces"'
label var j6ze019   `"Estimates: Female: 16 to 64 years: In labor force: Civilian"'
label var j6ze020   `"Estimates: Female: 16 to 64 years: In labor force: Civilian: Employed"'
label var j6ze021   `"Estimates: Female: 16 to 64 years: In labor force: Civilian: Unemployed"'
label var j6ze022   `"Estimates: Female: 16 to 64 years: Not in labor force"'
label var j6ze023   `"Estimates: Female: 65 years and over"'
label var j6ze024   `"Estimates: Female: 65 years and over: In labor force"'
label var j6ze025   `"Estimates: Female: 65 years and over: In labor force: Employed"'
label var j6ze026   `"Estimates: Female: 65 years and over: In labor force: Unemployed"'
label var j6ze027   `"Estimates: Female: 65 years and over: Not in labor force"'
label var j75e001   `"Estimates: Total"'
label var j75e002   `"Estimates: 1.00 or less occupants per room"'
label var j75e003   `"Estimates: 1.01 or more occupants per room"'
label var name_m    `"Margins of error: Area Name"'
label var jxhm001   `"Margins of error: Total"'
label var jxhm002   `"Margins of error: Born in state of residence"'
label var jxhm003   `"Margins of error: Born in other state in the United States"'
label var jxhm004   `"Margins of error: Native; born outside the United States"'
label var jxhm005   `"Margins of error: Foreign born"'
label var jyhm001   `"Margins of error: Total"'
label var jyhm002   `"Margins of error: Same house 1 year ago"'
label var jyhm003   `"Margins of error: Moved within same county"'
label var jyhm004   `"Margins of error: Moved from different county within same state"'
label var jyhm005   `"Margins of error: Moved from different state"'
label var jyhm006   `"Margins of error: Moved from abroad"'
label var jzgm001   `"Margins of error: Total"'
label var jzgm002   `"Margins of error: Car, truck, or van - drove alone"'
label var jzgm003   `"Margins of error: Car, truck, or van - carpooled"'
label var jzgm004   `"Margins of error: Public transportation (excluding taxicab)"'
label var jzgm005   `"Margins of error: Walked"'
label var jzgm006   `"Margins of error: Taxicab, motorcycle, bicycle, or other means"'
label var jzgm007   `"Margins of error: Worked at home"'
label var j0hm001   `"Margins of error: Total"'
label var j0hm002   `"Margins of error: Grandparent responsible for own grandchildren under 18 years"'
label var j0hm003   `"Margins of error: Grandparent responsible for own grandchildren under 18 years: "'
label var j0hm004   `"Margins of error: Grandparent responsible for own grandchildren under 18 years: "'
label var j0hm005   `"Margins of error: Grandparent not responsible for own grandchildren under 18 yea"'
label var j0hm006   `"Margins of error: Grandparent not responsible for own grandchildren under 18 yea"'
label var j0hm007   `"Margins of error: Grandparent not responsible for own grandchildren under 18 yea"'
label var j05m001   `"Margins of error: Total"'
label var j05m002   `"Margins of error: Male"'
label var j05m003   `"Margins of error: Male: Never married"'
label var j05m004   `"Margins of error: Male: Now married (except separated)"'
label var j05m005   `"Margins of error: Male: Separated"'
label var j05m006   `"Margins of error: Male: Widowed"'
label var j05m007   `"Margins of error: Male: Divorced"'
label var j05m008   `"Margins of error: Female"'
label var j05m009   `"Margins of error: Female: Never married"'
label var j05m010   `"Margins of error: Female: Now married (except separated)"'
label var j05m011   `"Margins of error: Female: Separated"'
label var j05m012   `"Margins of error: Female: Widowed"'
label var j05m013   `"Margins of error: Female: Divorced"'
label var j1sm001   `"Margins of error: Total"'
label var j1sm002   `"Margins of error: Women who had a birth in the past 12 months"'
label var j1sm003   `"Margins of error: Women who had a birth in the past 12 months: Now married (incl"'
label var j1sm004   `"Margins of error: Women who had a birth in the past 12 months: Unmarried (never "'
label var j1sm005   `"Margins of error: Women who did not have a birth in the past 12 months"'
label var j1sm006   `"Margins of error: Women who did not have a birth in the past 12 months: Now marr"'
label var j1sm007   `"Margins of error: Women who did not have a birth in the past 12 months: Unmarrie"'
label var j2dm001   `"Margins of error: Total"'
label var j2dm002   `"Margins of error: Male"'
label var j2dm003   `"Margins of error: Male: Less than high school diploma"'
label var j2dm004   `"Margins of error: Male: High school graduate, GED, or alternative"'
label var j2dm005   `"Margins of error: Male: Some college or associate's degree"'
label var j2dm006   `"Margins of error: Male: Bachelor's degree or higher"'
label var j2dm007   `"Margins of error: Female"'
label var j2dm008   `"Margins of error: Female: Less than high school diploma"'
label var j2dm009   `"Margins of error: Female: High school graduate, GED, or alternative"'
label var j2dm010   `"Margins of error: Female: Some college or associate's degree"'
label var j2dm011   `"Margins of error: Female: Bachelor's degree or higher"'
label var j2pm001   `"Margins of error: Total"'
label var j2pm002   `"Margins of error: Native"'
label var j2pm003   `"Margins of error: Native: Speak only English"'
label var j2pm004   `"Margins of error: Native: Speak another language"'
label var j2pm005   `"Margins of error: Native: Speak another language: Speak English 'very well'"'
label var j2pm006   `"Margins of error: Native: Speak another language: Speak English less than 'very "'
label var j2pm007   `"Margins of error: Foreign born"'
label var j2pm008   `"Margins of error: Foreign born: Speak only English"'
label var j2pm009   `"Margins of error: Foreign born: Speak another language"'
label var j2pm010   `"Margins of error: Foreign born: Speak another language: Speak English 'very well"'
label var j2pm011   `"Margins of error: Foreign born: Speak another language: Speak English less than "'
label var j2qm001   `"Margins of error: Total"'
label var j2qm002   `"Margins of error: Speak only English"'
label var j2qm003   `"Margins of error: Speak Spanish"'
label var j2qm004   `"Margins of error: Speak Spanish: Speak English 'very well'"'
label var j2qm005   `"Margins of error: Speak Spanish: Speak English 'well'"'
label var j2qm006   `"Margins of error: Speak Spanish: Speak English 'not well'"'
label var j2qm007   `"Margins of error: Speak Spanish: Speak English 'not at all'"'
label var j2qm008   `"Margins of error: Speak other language"'
label var j24m001   `"Margins of error: Total"'
label var j24m002   `"Margins of error: Income in the past 12 months below poverty level"'
label var j24m003   `"Margins of error: Income in the past 12 months below poverty level: Male"'
label var j24m004   `"Margins of error: Income in the past 12 months below poverty level: Male: Under "'
label var j24m005   `"Margins of error: Income in the past 12 months below poverty level: Male: 5 year"'
label var j24m006   `"Margins of error: Income in the past 12 months below poverty level: Male: 6 to 1"'
label var j24m007   `"Margins of error: Income in the past 12 months below poverty level: Male: 12 to "'
label var j24m008   `"Margins of error: Income in the past 12 months below poverty level: Male: 15 yea"'
label var j24m009   `"Margins of error: Income in the past 12 months below poverty level: Male: 16 and"'
label var j24m010   `"Margins of error: Income in the past 12 months below poverty level: Male: 18 to "'
label var j24m011   `"Margins of error: Income in the past 12 months below poverty level: Male: 25 to "'
label var j24m012   `"Margins of error: Income in the past 12 months below poverty level: Male: 35 to "'
label var j24m013   `"Margins of error: Income in the past 12 months below poverty level: Male: 45 to "'
label var j24m014   `"Margins of error: Income in the past 12 months below poverty level: Male: 55 to "'
label var j24m015   `"Margins of error: Income in the past 12 months below poverty level: Male: 65 to "'
label var j24m016   `"Margins of error: Income in the past 12 months below poverty level: Male: 75 yea"'
label var j24m017   `"Margins of error: Income in the past 12 months below poverty level: Female"'
label var j24m018   `"Margins of error: Income in the past 12 months below poverty level: Female: Unde"'
label var j24m019   `"Margins of error: Income in the past 12 months below poverty level: Female: 5 ye"'
label var j24m020   `"Margins of error: Income in the past 12 months below poverty level: Female: 6 to"'
label var j24m021   `"Margins of error: Income in the past 12 months below poverty level: Female: 12 t"'
label var j24m022   `"Margins of error: Income in the past 12 months below poverty level: Female: 15 y"'
label var j24m023   `"Margins of error: Income in the past 12 months below poverty level: Female: 16 a"'
label var j24m024   `"Margins of error: Income in the past 12 months below poverty level: Female: 18 t"'
label var j24m025   `"Margins of error: Income in the past 12 months below poverty level: Female: 25 t"'
label var j24m026   `"Margins of error: Income in the past 12 months below poverty level: Female: 35 t"'
label var j24m027   `"Margins of error: Income in the past 12 months below poverty level: Female: 45 t"'
label var j24m028   `"Margins of error: Income in the past 12 months below poverty level: Female: 55 t"'
label var j24m029   `"Margins of error: Income in the past 12 months below poverty level: Female: 65 t"'
label var j24m030   `"Margins of error: Income in the past 12 months below poverty level: Female: 75 y"'
label var j24m031   `"Margins of error: Income in the past 12 months at or above poverty level"'
label var j24m032   `"Margins of error: Income in the past 12 months at or above poverty level: Male"'
label var j24m033   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m034   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m035   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m036   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m037   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m038   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m039   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m040   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m041   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m042   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m043   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m044   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m045   `"Margins of error: Income in the past 12 months at or above poverty level: Male: "'
label var j24m046   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m047   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m048   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m049   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m050   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m051   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m052   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m053   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m054   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m055   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m056   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m057   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m058   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j24m059   `"Margins of error: Income in the past 12 months at or above poverty level: Female"'
label var j30m001   `"Margins of error: Total"'
label var j30m002   `"Margins of error: Income in the past 12 months below poverty level"'
label var j30m003   `"Margins of error: Income in the past 12 months below poverty level: Under 5 year"'
label var j30m004   `"Margins of error: Income in the past 12 months below poverty level: 5 years"'
label var j30m005   `"Margins of error: Income in the past 12 months below poverty level: 6 to 11 year"'
label var j30m006   `"Margins of error: Income in the past 12 months below poverty level: 12 to 17 yea"'
label var j30m007   `"Margins of error: Income in the past 12 months below poverty level: 18 to 64 yea"'
label var j30m008   `"Margins of error: Income in the past 12 months below poverty level: 65 to 74 yea"'
label var j30m009   `"Margins of error: Income in the past 12 months below poverty level: 75 years and"'
label var j30m010   `"Margins of error: Income in the past 12 months at or above poverty level"'
label var j30m011   `"Margins of error: Income in the past 12 months at or above poverty level: Under "'
label var j30m012   `"Margins of error: Income in the past 12 months at or above poverty level: 5 year"'
label var j30m013   `"Margins of error: Income in the past 12 months at or above poverty level: 6 to 1"'
label var j30m014   `"Margins of error: Income in the past 12 months at or above poverty level: 12 to "'
label var j30m015   `"Margins of error: Income in the past 12 months at or above poverty level: 18 to "'
label var j30m016   `"Margins of error: Income in the past 12 months at or above poverty level: 65 to "'
label var j30m017   `"Margins of error: Income in the past 12 months at or above poverty level: 75 yea"'
label var j6nm001   `"Margins of error: Total"'
label var j6nm002   `"Margins of error: Household received Food Stamps/SNAP in the past 12 months"'
label var j6nm003   `"Margins of error: Household did not receive Food Stamps/SNAP in the past 12 mont"'
label var j6zm001   `"Margins of error: Total"'
label var j6zm002   `"Margins of error: Male"'
label var j6zm003   `"Margins of error: Male: 16 to 64 years"'
label var j6zm004   `"Margins of error: Male: 16 to 64 years: In labor force"'
label var j6zm005   `"Margins of error: Male: 16 to 64 years: In labor force: In Armed Forces"'
label var j6zm006   `"Margins of error: Male: 16 to 64 years: In labor force: Civilian"'
label var j6zm007   `"Margins of error: Male: 16 to 64 years: In labor force: Civilian: Employed"'
label var j6zm008   `"Margins of error: Male: 16 to 64 years: In labor force: Civilian: Unemployed"'
label var j6zm009   `"Margins of error: Male: 16 to 64 years: Not in labor force"'
label var j6zm010   `"Margins of error: Male: 65 years and over"'
label var j6zm011   `"Margins of error: Male: 65 years and over: In labor force"'
label var j6zm012   `"Margins of error: Male: 65 years and over: In labor force: Employed"'
label var j6zm013   `"Margins of error: Male: 65 years and over: In labor force: Unemployed"'
label var j6zm014   `"Margins of error: Male: 65 years and over: Not in labor force"'
label var j6zm015   `"Margins of error: Female"'
label var j6zm016   `"Margins of error: Female: 16 to 64 years"'
label var j6zm017   `"Margins of error: Female: 16 to 64 years: In labor force"'
label var j6zm018   `"Margins of error: Female: 16 to 64 years: In labor force: In Armed Forces"'
label var j6zm019   `"Margins of error: Female: 16 to 64 years: In labor force: Civilian"'
label var j6zm020   `"Margins of error: Female: 16 to 64 years: In labor force: Civilian: Employed"'
label var j6zm021   `"Margins of error: Female: 16 to 64 years: In labor force: Civilian: Unemployed"'
label var j6zm022   `"Margins of error: Female: 16 to 64 years: Not in labor force"'
label var j6zm023   `"Margins of error: Female: 65 years and over"'
label var j6zm024   `"Margins of error: Female: 65 years and over: In labor force"'
label var j6zm025   `"Margins of error: Female: 65 years and over: In labor force: Employed"'
label var j6zm026   `"Margins of error: Female: 65 years and over: In labor force: Unemployed"'
label var j6zm027   `"Margins of error: Female: 65 years and over: Not in labor force"'
label var j75m001   `"Margins of error: Total"'
label var j75m002   `"Margins of error: 1.00 or less occupants per room"'
label var j75m003   `"Margins of error: 1.01 or more occupants per room"'


