#### MERGE FL BUSN DATA ####
library(dplyr)
library(sf)
library(rgdal)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)

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
  mutate(hispName1 = latinCheck(PRINC_NAME), hispFoodName1 = latinCheck(PRINC_NAME,2),
         hisp = hispanic * count,
         hispOne = hispanic, hispMaj = ifelse(hispanic > .5,1,0)) %>%
  group_by(PRINC_ADD_1,PRINC_CITY,PRINC_STATE,PRINC_ZIP5) %>%
  summarise(hispName = sum(hispName1), hispFoodName = sum(hispFoodName1),
            hisp_sum = sum(hisp),
            hispOne_sum = sum(hispOne), hispMaj_sum = sum(hispMaj)) %>%
  mutate(hispNameD = ifelse(hispName > 0, 1, 0))

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




######## NEXT

hispName <- names %>%
  filter(hispanic > .5)
head(hispName)
asianName <- names %>%
  filter(asian > .5)
head(asianName)


