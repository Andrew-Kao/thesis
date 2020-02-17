#### MERGE FL BUSN DATA ####

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
} else if (Sys.info()["user"] == "andrewkao") {
  setwd('/project2/bursztyn/contour/Data/firms/florida') 
  dir.create(Sys.getenv("R_LIBS_USER"), recursive = TRUE)  # create personal library
  .libPaths(Sys.getenv("R_LIBS_USER"))
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
}

library(dplyr)
if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf",  dependencies = TRUE)}
if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal",  dependencies = TRUE)}
if("spdep" %in% rownames(installed.packages()) == FALSE) {install.packages("spdep",  dependencies = TRUE)}
require(sf)
require(rgdal)
require(spdep)

options(stringsAsFactors = FALSE)

### Check if name contains common Latin American words/food identifiers/countries
# substrings: taqueria, taco, empanada, huevo, pollo, burrito, arepa, pupusa, tamale, tortilla
# salsa, asado, lechon, mojo, ropa, vieja, chorizo, 
# countries: latin, mexic, bolivia, chile, argentin, venezuela, beliz, costa rica, salvador
# guatemala, hondur, nicaragua, panama, brazil, colombia, ecuador, guyana, paragua, peru
# surinam, urugu, cuba, dominican, haiti, puerto
# words: la, de, como, su, que, el, para, en, por, los, casa, caliente
latinCheck <- function(names, spec = 1) {
  
  output <- 0
  
  # spec:
  # == 1 : everythting
  # == 2 : no food
  
  patterns <- c('latin', 'mexic', 'bolivia', 'chile', 'argentin', 
                'venezuela', 'beliz', 'costa rica', 'salvador', 'guatemala', 'hondur',
                'nicaragua', 'panama', 'brazil', 'colombia', 'ecuador', 'guyana', 'paragua',
                'peru', 'surinam', 'urugu', 'cuba', 'dominican', 'haiti', 'puerto',
                '^la ', ' la ', '^de ', ' de ', '^como ', 'como ', '^su ', ' su ',
                ' que ', '^el ', ' el ', '^para ', ' para ', '^en ', ' en ',
                '^por ', ' por ', '^los ', ' los ', '^casa ', ' casa ',
                '^caliente ', ' caliente ')
  if (spec == 2) {
    patterns <- c(patterns,'taqueria', 'taco', 'empanada', 'huevo', 'pollo', 'burrito', 'arepa',
                  'pupusa', 'tamale', 'tortilla', 'salsa', 'asado', 'lechon', 'mojo', 'ropa',
                  'vieja', 'chorizo')
  }
  for (p in patterns) {
    output <- output + grepl(p, names, ignore.case=TRUE) 
  }
  
  
  return(output) 
}

# 1. Cleaned donor data (direct)
busn <- readRDS('tidy_merged.Rdata')
# business names with google cloud?

sbusn <- as_tibble(busn) %>%
  dplyr::select(COR_NAME, REPORT_YEAR_1, PRINC_NAME,
                PRINC_ADD_1, PRINC_CITY, PRINC_STATE,
                PRINC_ZIP5)

# 2. Name classification data
names <- read.csv('names_predict_FL.csv') 

# 3. Location data
rawLocs <- read.csv('FloridaAddresses_gov.csv') %>%
  mutate(PRINC_ADD_1 = sapply(PRINC_ADD_1, URLdecode), PRINC_CITY = sapply(PRINC_CITY, URLdecode))
busnAll <- st_as_sf(rawLocs, coords = c("long","lat"),crs = CRS("+proj=longlat +datum=NAD83"))
busnAll <- as_Spatial(busnAll)



# Per Location Data
locationNames <- sbusn %>%
  group_by(COR_NAME,PRINC_NAME,PRINC_ADD_1,PRINC_CITY,PRINC_STATE,PRINC_ZIP5) %>%
  summarise(count = n()) %>%
  left_join(names, by = c("PRINC_NAME")) %>%
  mutate(hispName1 = latinCheck(COR_NAME), hispFoodName1 = latinCheck(COR_NAME,2),
         hisp = hispanic * count,
         hispOne = hispanic, hispMaj = ifelse(hispanic > .5,1,0)) %>%
  group_by(PRINC_ADD_1,PRINC_CITY,PRINC_STATE,PRINC_ZIP5) %>%
  summarise(hispName = sum(hispName1), hispFoodName = sum(hispFoodName1),
            hisp_sum = sum(hisp),
            hispOne_sum = sum(hispOne), hispMaj_sum = sum(hispMaj)) %>%
  mutate(hispNameD = ifelse(hispName > 0, 1, 0))

locationNames <- readRDS('LocNames.Rdata')

locationCounts <- sbusn %>%
  group_by(PRINC_ADD_1,PRINC_CITY,PRINC_STATE,PRINC_ZIP5) %>%
  summarise(busnCount = n())

# Merge
# Names & Donor Data
busn2 <- merge(busnAll, locationNames, all.x=TRUE, by.x = c("PRINC_ADD_1", "PRINC_CITY","PRINC_STATE", "PRINC_ZIP5"),
                  by.y = c("PRINC_ADD_1", "PRINC_CITY","PRINC_STATE", "PRINC_ZIP5"))
busn2 <- merge(busn2, locationCounts, all.x=TRUE, by.x = c("PRINC_ADD_1", "PRINC_CITY","PRINC_STATE", "PRINC_ZIP5"),
                  by.y = c("PRINC_ADD_1", "PRINC_CITY","PRINC_STATE", "PRINC_ZIP5"))

saveRDS(busn2,file='BusnAll.Rdata')



########### SUMMARY STATS

test <- busn2@data %>%
  mutate(hispFoodNameD = ifelse(hispFoodName * hispMaj_sum > 0, 1, 0),
         hispNameD = ifelse(hispName * hispMaj_sum > 0, 1, 0))

firmHisp <- locationNames %>%
  filter(hispFoodName > 0)
test <- sbusn %>%
  filter(PRINC_ADD_1 == "100 MENLO PARK DR SUITE 302")
head(test)
######## NEXT

# hispName <- names %>%
#   filter(hispanic > .5)
# head(hispName)
# asianName <- names %>%
#   filter(asian > .5)
# head(asianName)

"Taco Bell Cantina Corp."


# [87] "100 E. 7TH STREET"                               "100 JAZZ DR."                                   
# [89] "100 KINGS POINT DR APT 1708"                     "100 KINGS POINT DRIVE, APT. 320"                
# [91] "100 LAKE SHORE DRIVE SUITE 108"                  "100 LAKE SHORE DRIVE, SUITE 108"                
# [93] "100 LEGER RD"                                    "100 LINCOLN RD #1445"                           
# [95] "100 LINCOLN RD, APT. 706"                        "100 LINCOLN RD."                                
# [97] "100 LINCOLN RD. #1114"                           "100 LINCON ROAD #607"                           
# [99] "100 MADISON AVE EAST"                            "100 N BISCAYNE BLVD  STE 2106"                  
# [101] "100 N BISCAYNE BLVD - STE 500"                   "100 N BISCAYNE BLVD SUITE 2800"                 
# [103] "100 N BISCAYNE BLVD, SUITE 2800"                 "100 N BISCAYNE BLVD., SUITE 2106"               
# [105] "100 N BISCAYNE BVD, SUITE 500"                   "100 N FEDERAL HWY"                              
# [107] "100 N SUMMERLIN AVE"                             "100 N TAMPA STREET STE 1600"                    
# [109] "100 N. BISCAYNE BLVD STE 3070"                   "100 N. BISCAYNE BOULEVARD, 3070"                
# [111] "100 N. TAMPA STREET, SUITE 1600"                 "100 NE 6TH AVE"                                 
# [113] "100 NE. 6TH. AVE. #709"                          "100 NORTH BISCAYNE BLVD., SUITE 500"            
# [115] "100 NORTH BISCAYNE BOULEVARD SUITE 500"          "100 NW 23 RD AVE APT 2501"                      
# [117] "100 NW 23RD AVE"                                 "100 NW 67TH CT"                                 
# [119] "100 NW 8TH AVE"                                  "100 PARADISE HARBOUR BLVD 308"                  
# [121] "100 PGA TOUR BLVD"                               "100 REA LANE"                                   
# [123] "100 S BISCAYNE BLVD"                             "100 S EOLA DR #1201"                            
# [125] "100 S EOLA DR., #605"                            "100 S POINTE DR  APT 702"                       
# [127] "100 S POINTE DR - STE 1009"                      "100 S POINTE DR APT 2807"                       
# [129] "100 S POINTE DR STE 1009"                        "100 S. EOLA DR. SUITE 1613"                     
# [131] "100 S. POINTE DR. #2304"                         "100 S. POINTE DRIVE, APT 1507"                  
# [133] "100 S.W. 110TH AVENUE SUITE 103"                 "100 SARAGOSSA AVENUE"                           
# [135] "100 SE 6TH ST"                                   "100 SE 6TH STREET"                              
# [137] "100 SE SECOND ST SUITE 3800"                     "100 SOUTH POINTE DRIVE, SUITE 1102"             
# [139] "100 SOUTHEAST 2ND STREET, SUITE 3105"            "100 SOUTHEAST THIRD AVENUE"                     
# [141] "100 SPARROW DR #15"                              "100 SUNRISE DR # 10"                            
# [143] "100 SUNRISE DRIVE STE 18"                        "100 SW 10TH STREET SUITE 308"                   
# [145] "100 W 29TH STREET"                               "100 WALLACE AVE. SUITE 360"                     
# [147] "100 WEST GRANT STREET"                           "100 WESTWARD DR."                               
# [149] "100 WILLOUGHBY STREET"                           "1000 5TH STREET"                                
# [151] "1000 5TH STREET, SUITE 200"                      "1000 A E 8TH AVE"                               
# [153] "1000 BRICELL AVE UNIT 300"                       "1000 BRICKELL AV SUITE 201"                     
# [155] "1000 BRICKELL AVE, 920"                          "1000 BRICKELL AVE, STE 1005"                    
# [157] "1000 BRICKELL AVE, STE. 400"                     "1000 BRICKELL AVE, SUITE 300"                   
# [159] "1000 BRICKELL AVE. SUITE 300"                    "1000 BRICKELL AVENEU, STE 215"                  
# [161] "1000 BRICKELL AVENUE"                            "1000 BRICKELL AVENUE, #400"                     
# [163] "1000 BRICKELL AVENUE, STE 640"                   "1000 BRICKELL AVENUE, SUITE 201"                
# [165] "1000 BRICKELL AVENUE, SUITE 300"                 "1000 BRICKELL AVENUE, SUITE 400"                
# [167] "1000 BRICKELL AVENUE, SUITE 420"                 "1000 BRICKELL AVENUE, SUITE 480"                
# [169] "1000 BRICKELL AVENUE, SUITE 920"                 "1000 BRICKELL AVENUE., SUITE 400"               
# [171] "1000 BROWARD RD STE 1403"                        "1000 CALIPH ST"                                 
# [173] "1000 CORPORATE DRIVE STE 700"                    "1000 CROSSWINDS LANDING #A304"                  
# [175] "1000 DOUGLAS AVE APT 100"                        "1000 E 47TH STREET"                             
# [177] "1000 E HALLANDALE BEACH BLVD"                    "1000 E HALLANDALE BEACH BLVD STE B"             
# [179] "1000 E HALLANDALE BEACH BLVD SUITE 21"           "1000 E HALLANDALE BEACH BLVD, STE 5"            
# [181] "1000 E HALLANDALE BEACH BLVS STE B"              "1000 E NEW YORK AVE"                            
# [183] "1000 E. HALLANDALE BEACH BLVD STE B"             "1000 E. HALLANDALE BEACH BLVD."                 
# [185] "1000 E. ISLAND BLVD."                            "1000 EAST PONCE DE LEON BLVD APT 1"             
# [187] "1000 EATON STREET"                               "1000 ISLAND BLVD #811"                          
# [189] "1000 LAKE OF THE WOODS BLVD., APT. B 201"        "1000 LINCOLN ROAD, SUITE 208"                   
# [191] "1000 NE 12TH AVENUE #402"                        "1000 NORTH US HIGHWAY ONE"                      
# [193] "1000 NW 57 COURT SUITE 170"                      "1000 NW 57TH CT STE 940"                        
# [195] "1000 OHIO AVENUE"                                "1000 PARK CENTRE BLVD"                          
# [197] "1000 PINEBROOK ROAD"                             "1000 S. OXFORD DR."                             
# [199] "1000 S. SEMORAN BLVD., STE. 308"                 "1000 SOUTH POINTE DRIVE #1902"                  
# [201] "1000 SPANISH RIVER ROAD 4P"                      "1000 SPRING GARDENS ROAD NW"                    
# [203] "1000 VENETIAN WAY # 812"                         "1000 W 31ST STREET"                             
# [205] "1000 W CENTRAL BLVD"                             "1000 W VINE ST"                                 
# [207] "1000 W WATERS AVE, SUITE 1"                      "1000 W. ISLAND BLVD, UNIT 1009"                 
# [209] "1000 WEST AVE APT 708"                           "1000 WEST AVE UNIT 1126"                        
# [211] "1000 WEST AVENUE"                                "1000 WEST AVENUE, UNIT NO. 502"                 
# [213] "1000 WEST MC NAB ROAD"                           "1000 WEST MCNAB ROAD, STE. 320"                 
# [215] "1000 WEST MCNAB ROAD, SUITE 320"                 "1000-A EAST 8TH AVENUE"                         
# [217] "10000 NW 80 COURT APT 2103"                      "10000 NW 80 CT APT 2451"                        
# [219] "10000 NW 80 CT SUITE 2519"                       "10000 NW 80TH COURT, APT. 2426"                 
# [221] "10000 SHERIDAN ST. APT. 210"                     "10000 VICTORIA PARK LN, APT. 10309"             
# [223] "10000 WEST FLAGLER ST."                          "10001 N.W. 60TH PLACE"                          
# [225] "10001 NW 133 ST"                                 "10001 SW 2 ST"                                  
# [227] "10001 VISTA POINTE DRIVE"                        "10001 W BAY HARBOR DRIVE #403"                  
# [229] "10004 CENTRE ST"                                 "10004 PERTHSHIRE CIR"                           
# [231] "10006 CROSS CREEK BLVD"                          "10006 CROSS CREEK BLVD 166"                     
# [233] "1001 36TH ST"                                    "1001 36TH ST E187"                              
# [235] "1001 36TH STREET SUITE E 185"                    "1001 BRICKELL BAY DR STE 2650"                  
# [237] "1001 BRICKELL BAY DR., SUITE 1800"               "1001 BRICKELL BAY DRIVE"                        
# [239] "1001 BRICKELL BAY DRIVE  STE 1200"               "1001 BRICKELL BAY DRIVE STE 2406"               
# [241] "1001 BRICKELL BAY DRIVE SUITE 1200"              "1001 BRICKELL BAY DRIVE, #3104"                 
# [243] "1001 BRICKELL BAY DRIVE, 9TH FL"                 "1001 BRICKELL BAY DRIVE, SUITE 1200"            
# [245] "1001 BRICKELL BAY DRIVE, SUITE 1800"             "1001 BRICKELL BAY SUITE 1712"                   
# [247] "1001 COCOANUT AVE"                               "1001 E 26TH AVE"                                
# [249] "1001 E 28TH ST"                                  "1001 E MCDONALDS AVENUE"                        
# [251] "1001 EAST 24TH AVENUE"                           "1001 EAST LAS OLAS BLVD., SUITE 200"            
# [253] "1001 FAIRWAY DRIVE"                              "1001 IVES DAIRY RD SUITE 206"                   
# [255] "1001 IVES DAIRY RD, SUITE 206"                   "1001 JUPITER PARK DRIVE #114"                   
# [257] "1001 LAUREL RUN LANE, APT 107"                   "1001 LISA LN APT B"                             
# [259] "1001 N FEDERAL HWY"                              "1001 NE 15 ST"                                  
# [261] "1001 NORTH HIGHWAY A1A ALTERNATE"                "1001 NW 13TH AVENUE CAPACITY"                   
# [263] "1001 PENNSYLVANIA AVE NW"                        "1001 S. ALEXANDER STREET"                       
# [265] "1001 SW 141 AVE #K204"                           "1001 SW 18 AVE"                                 
# [267] "1001 SW 2ND AVE SUITE 300"                       "1001 SW 82ND AVENUE"                            
# [269] "1001 SW SUN CIR"                                 "1001 SW SUN CIRCLE"                             
# [271] "1001 THREE ISLANDS BLVD APT 43"                  "1001 TURNER DR"                                 
# [273] "1001 W INDIANTOWN ROAD"                          "1001 W INDIANTOWN ROAD, SUITE 103"              
# [275] "1001 W NEWPORT CENTER DR 112"                    "1001 WEST INDIANTOWN RD"                        
# [277] "1001 WEST INDIANTOWN ROAD"                       "1001 WEST JASMINE DRIVE, SUITE K"               
# [279] "1001 WESTWARD DRIVE"                             "10010 SKINNER LAKE DR STE 1014"                 
# [281] "10012 NW 7 STREET, UNIT # 214"                   "10012 REGAL WOODS LANE"                         
# [283] "10013 LEYBURN COURT"                             "10013 SALINA ST"                                
# [285] "10015 HIDDEN DUNES LANE"                         "10015 NW 46 STREET # 205"                       
# [287] "10015 WINDING LAKE DR APT 102"                   "10016 EASTERN LAKE AVE # 204"                   
# [289] "10016 NW 89TH TERRACE"                           "10018 STRATFORD AVE."                           
# [291] "10019 N 14TH ST"                                 "10019 SILVER LAUREL WAY"                        
# [293] "1002 BRIARWOOD DRIVE"                            "1002 E 28TH AVE"                                
# [295] "1002 E. 27TH STREET"                             "1002 E. CYPRESS DRIVE"                          
# [297] "1002 KATY GAP RD APT 517"                        "1002 MISSOURI AVE"                              
# [299] "1002 N.W. 99TH CT"                               "1002 NW 136TH COURT"                            
# [301] "1002 PINDER ST"                                  "1002 PINE STREET"                               
# [303] "1002 VERONA STREET"                              "1002 W. 23RD STREET, SUITE 400"                 
# [305] "1002 WHITE DRIVE"                                "10021 ALCOCK RD"                                
# [307] "10021 CRYSTALLINE CT"                            "10021 CRYSTALLINE CT."                          
# [309] "10021 KENTUCKY STREET"                           "10021 SW 97TH CT"                               
# [311] "10022 DEAN CHASE BLVD"                           "10022 NW 7TH STREET"                            
# [313] "10025 NW 116TH WAY SUITE 17"                     "10025 VISTA COVE LN"                            
# [315] "10028 LOVEGRASS LN"                              "1003 CORNWALL CT"                               
# [317] "1003 LITTLEWOOD CT"                              "1003 WEST FLAGLER STREET"                       
# [319] "10030 NW 86 TERRACE"                             "10031 SW 20 ST."                                
# [321] "10035 NW 44 TERR APT 304"                        "10035 NW 44TH TER STE 302"                      
# [323] "10036 PINES BLVD"                                "10037 BRADWELL PL"                              
# [325] "10039-45 SW 72 ST"                               "1004 6TH ST W"                                  
# [327] "1004 PINE TREE DR"                               "1004 SARAH LEE LN"                              
# [329] "10040 NW 36TH ST APT 4"                          "10040 SW 197 STREET"                            
# [331] "10040 UNIVERSITY BLVD"                           "10041 SMARTY JONES DR"                          
# [333] "10043 GRAND CANAL DR #17201"                     "10045 NW 46 STREET APT 107"                     
# [335] "10045 NW 46TH ST APT 306"                        "10045 SAN JOSE BLVD"                            
# [337] "10045 SW 124TH AVE"                              "1005 DOWNMAN PLACE"                             
# [339] "1005 NE 5TH AVE"                                 "1005 NW 106 AVENUE CIR"                         
# [341] "1005 ROLLING HILLS DR"                           "1005 S 13TH AVENUE"                             
# [343] "1005 SW 8 STREET"                                "1005 SW 87TH AVE"                               
# [345] "1005 SW 8TH STREET"                              "1005 WEST 13TH STREET"                          
# [347] "10050 NW 44 TERRACE"                             "10052 HARTFORD MARRONE RD"                      
# [349] "10053 LAKE OAK CIR."                             "10055 VINTAGE PLACE"                            
# [351] "10057 LAKE DISTRICT LANE"                        "1006 STANDING REED PLACE"                       
# [353] "1006 WARREN AVENUE"                              "1006 WEST BRADDOCK STREET"                      
# [355] "10060 SW 137TH PLACE"                            "10065 NW 46 STREET SUITE 304"                   
# [357] "10065 NW 46TH ST - APT 304"                      "10065 NW 46TH ST UNIT 206"                      
# [359] "10067 PINES BOULEVARD, SUITE A"                  "10067 PINES BOULEVARD, SUITE A"                 
# [361] "10069 LEE VISTA BLVD, # 11209"                   "10069 RAMBLEWOOD DR"                            
# [363] "1007 EAST 26TH STREET"                           "10070 SW 166 CT"                                
# [365] "10072 BOYNTON PLACE"                             "10074 BENNINGTON CHACE DR"                      
# [367] "10076 SW 144 AVE"                                "10077 NW 88 TERRACE"                            
# [369] "10079 RIVERSIDE DR"                              "1008 CROYDONWOOD CIRCLE"                        
# [371] "1008 JOYCE RD"                                   "1008 NW 39TH CIR"                               
# [373] "1008 SOUTH CENTER ST"                            "10080 VESTAL PLACE"                             
# [375] "10082 IVERSON DR"                                "10086 COLONIAL CREEK LANE"                      
# [377] "1009 BILTMORE DRIVE"                             "1009 E ELLICOTT ST"                             
# [379] "1009 E. CAPITOL EXPRESSWAY # 605."               "1009 E. ELLICOTT STREET"                        
# [381] "1009 GREEN ST"                                   "1009 NW 11 COURT"                               
# [383] "1009 ROUTE DE BOULOUD"                           "1009 SW 2 ST SUITE 11"                          
# [385] "1009 SW 8 STREET"                                "10090 NW 80 CT APT 1303"                        
# [387] "10092 COBBLESTON CREEK DR"                       "10094 NW 88TH TER"                              
# [389] "10094 NW 88TH TERR"                              "10095 SW 88TH ST., STE 102"                     
# [391] "10097 CLEARY BLVD."                              "10097 NW 55 TERRACE"                            
# [393] "10099 BISCAYNE BLVD."                            "101 20TH STREET, #2205"                         
# [395] "101 2ND ST"                                      "101 30TH ST"                                    
# [397] "101 ARLINGTON HEIGHTS CIR"                       "101 BERKLEY ROAD APT 201"                       
# [399] "101 BOUNDARY BLVD. APT #2"                       "101 BUD HOLLOW DRIVE"                           
# [401] "101 CONVENTION CENTER DR, STE. 700"              "101 COSGROVE AVE, #220"                         
# [403] "101 CRANDON BLVD STE 276"                        "101 CRANDON BOULEVARD APTO 177"                 
# [405] "101 EAST FLETCHER"                               "101 EAST MAIN STREET STE 500"                   
# [407] "101 GRAND PLAZA DRIVE  UNIT L3"                  "101 LAKESIDE CIRCLE"                            
# [409] "101 LOCHINVAR DR"                                "101 MARKETSIDE AVE SUITE 404-324"               
# [411] "101 MILL COVE LANE"                              "101 N MISSOURI AVE"                             
# [413] "101 N. OCEAN DR. APT 411"                        "101 NE 3RD AVE # 1500"                          
# [415] "101 NE 3RD AVE, SUITE 1500"                      "101 OCEAN LANE DR #1012"                        
# [417] "101 OCEAN LANE DR. APT #2015"                    "101 PUTTER DRIVE"                               
# [419] "101 ROYAL PARK DRIVE  #3B"                       "101 S 33RD STREET"                              
# [421] "101 S EOLA DR #1205"                             "101 S. 18 ST."                                  
# [423] "101 S. EOLA DR."                                 "101 SANDAL LN."                                 
# [425] "101 SW 9 STREET # 2B"                            "101 W 9 ST APT 11"  

