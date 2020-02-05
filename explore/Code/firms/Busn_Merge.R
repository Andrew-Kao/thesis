#### MERGE FL BUSN DATA ####

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
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
  
  mutate(hisp = count*hispanic, non_hisp = count*black + count*white,
         hispOne = hispanic, # don't weight by multiple donations
         hispMaj = ifelse(hispanic > .5,1,0)) %>%
  group_by(contributor_street_1,contributor_city,contributor_state, postcode) %>%
  summarise(hisp_sum = sum(hisp), non_hisp_sum = sum(non_hisp),
            hispOne_sum = sum(hispOne), hispMaj_sum = sum(hispMaj)) %>%
  mutate(race = ifelse(hisp_sum >= non_hisp_sum, 1, 0)) ## race is a dummy for Hispanic

locationCounts <- donations %>%
  group_by(contributor_street_1,contributor_city,contributor_state, postcode) %>%
  summarise(donationCount = n())

# Merge
# Names & Donor Data
clinton2 <- merge(clinton, locationNames, all.x=TRUE, by.x = c("street", "city","state2", "zip"),
                  by.y = c("contributor_street_1","contributor_city","contributor_state","postcode" ))
clinton2 <- merge(clinton2, locationCounts, all.x=TRUE, by.x = c("street", "city","state2", "zip"),
                  by.y = c("contributor_street_1","contributor_city","contributor_state","postcode" ))

saveRDS(clinton2,file='ClintonAll.Rdata')


test <- c('Tacot','aob','etacor')

### Check if name contains common Latin American words/food identifiers/countries
# substrings: taqueria, taco, empanada, huevo, pollo, burrito, arepa, pupusa, tamale, tortilla
# salsa, asado, lechon, mojo, ropa, vieja, chorizo, 
# countries: latin, mexic, bolivia, chile, argentin, venezuela, beliz, costa rica, salvador
# guatemala, hondur, nicaragua, panama, brazil, colombia, ecuador, guyana, paragua, peru
# surinam, urugu, cuba, dominican, haiti, puerto
# words: la, de, como, su, que, el, para, en, por, los, casa, caliente
latinCheck <- function(names) {
 
  count <- 0
  output <- grepl('taco', names,ignore.case=TRUE)
  
  patterns <- c('taqueria', 'taco', 'empanada', 'huevo', 'pollo', 'burrito', 'arepa',
                'pupusa', 'tamale', 'tortilla', 'salsa', 'asado', 'lechon', 'mojo', 'ropa',
                'vieja', 'chorizo', 'latin', 'mexic', 'bolivia', 'chile', 'argentin', 
                'venezuela', 'beliz', 'costa rica', 'salvador', 'guatemala', 'hondur',
                'nicaragua', 'panama', 'brazil', 'colombia', 'ecuador', 'guyana', 'paragua',
                'peru', 'surinam', 'urugu', 'cuba', 'dominican', 'haiti', 'puerto',
                '^la ', ' la ', '^de ', ' de ', '^como ', 'como ', '^su ', ' su ',
                ' que ', '^el ', ' el ', '^para ', ' para ', '^en ', ' en ',
                '^por ', ' por ', '^los ', ' los ', '^casa ', ' casa ',
                '^caliente ', ' caliente ')
  for (p in patterns) {
    output <- output + grepl(p, names, ignore.case=TRUE) 
  }
  
  
  return(output) 
}








