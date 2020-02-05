#### MERGE FL BUSN DATA ####

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

# 1. Cleaned donor data (direct)
busn <- readRDS('tidy_merged.Rdata')

# 2. Name classification data
names <- read.csv('names_predict_FL.csv') 

# 3. Location data
rawLocs <- read.csv('FloridaAddresses_gov.csv') %>%
  mutate(PRINC_ADD_1 = sapply(PRINC_ADD_1, URLdecode), PRINC_CITY = sapply(PRINC_CITY, URLdecode))
busnAll <- st_as_sf(rawLocs, coords = c("long","lat"),crs = CRS("+proj=longlat +datum=NAD83"))
busnAll <- as_Spatial(busnAll)



# Per Location Data
locationNames <- busn %>%
  group_by(contributor_street_1,contributor_city,contributor_state, postcode,contributor_first_name, contributor_last_name) %>%
  summarise(count = n()) %>%
  left_join(census, by = c("contributor_first_name","contributor_last_name")) %>%
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