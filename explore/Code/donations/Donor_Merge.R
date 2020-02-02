##### MERGE ALL DONOR DATA ######

library(raster)
library(rgdal)
library(dplyr)
library(sf)

# Data sources
# 1. Cleaned donor data (direct)
# 2. Name classification data
# 3. Location data


options(stringsAsFactors = FALSE)

###### TRUMP ########
if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

# 1. Cleaned donor data (direct)
donations <- read.csv('TrumpDonations.csv')

# 2. Name classification data
FL <- read.csv('names_predict_FL.csv') 
census <- read.csv('names_predict_census.csv')
wiki <- read.csv('names_predict_wiki.csv')

# 3. Location data
trump <- sf::st_read(dsn='a00000009.gdbtable', layer = 'a00000009') %>%
  filter(Status == "M")  ## only keep precise matches
trump <- as_Spatial(trump)

# Per Location Data
locationNames <- donations %>%
  group_by(contributor_street_1,contributor_city,contributor_state, contributor_zip,contributor_first_name, contributor_last_name) %>%
  summarise(count = n()) %>%
  left_join(census, by = c("contributor_first_name","contributor_last_name")) %>%
  mutate(hisp = count*hispanic, non_hisp = count*black + count*white,
         hispOne = hispanic, # don't weight by multiple donations
         hispMaj = ifelse(hispanic > .5,1,0)) %>%
  group_by(contributor_street_1,contributor_city,contributor_state, contributor_zip) %>%
  summarise(hisp_sum = sum(hisp), non_hisp_sum = sum(non_hisp)) %>%
  mutate(race = ifelse(hisp_sum >= non_hisp_sum, 1, 0)) ## race is a dummy for Hispanic
  
locationCounts <- donations %>%
  group_by(contributor_street_1,contributor_city,contributor_state, contributor_zip) %>%
  summarise(donationCount = n())

# Merge
# Names & Donor Data
trump2 <- merge(trump, locationNames, all.x=TRUE, by.x = c("street", "city_1","state2", "zip"),
                by.y = c("contributor_street_1","contributor_city","contributor_state","contributor_zip" ))
trump2 <- merge(trump2, locationCounts, all.x=TRUE, by.x = c("street", "city_1","state2", "zip"),
                by.y = c("contributor_street_1","contributor_city","contributor_state","contributor_zip" ))

saveRDS(trump2,file='TrumpAll.Rdata')
  

##
  # location mismatches: determining why there are mismatches between locations and donations data
  # unclear for zip 10001 (test[3,])
  # 2/3 don't match either for 10002, one of them is a text parsing difference. Probably no good leads.
test <- trump@data[is.na(trump@data$memo_code), ]
test2 <- donations %>%  
  filter(contributor_zip == 10002)


###### CLINTON #######
if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/clinton_donations') 
}

# 1. Cleaned donor data (direct)
donations <- read.csv('ClintonDonations.csv')

# 2. Name classification data
FL <- read.csv('names_predict_FL.csv') 
census <- read.csv('names_predict_census.csv')
wiki <- read.csv('names_predict_wiki.csv')

# 3. Location data
rawLocs <- read.csv('ClintonAddresses_gov.csv')
clinton <- st_as_sf(rawLocs, coords = c("long","lat"),crs = CRS("+proj=longlat +datum=NAD83"))
clinton <- as_Spatial(clinton)
plot(clinton)



# Per Location Data
locationNames <- donations %>%
  group_by(contributor_street_1,contributor_city,contributor_state, contributor_zip,contributor_first_name, contributor_last_name) %>%
  summarise(count = n()) %>%
  left_join(census, by = c("contributor_first_name","contributor_last_name")) %>%
  mutate(hisp = count*hispanic, non_hisp = count*black + count*white,
         hispOne = hispanic, # don't weight by multiple donations
         hispMaj = ifelse(hispanic > .5,1,0)) %>%
  group_by(contributor_street_1,contributor_city,contributor_state, contributor_zip) %>%
  summarise(hisp_sum = sum(hisp), non_hisp_sum = sum(non_hisp)) %>%
  mutate(race = ifelse(hisp_sum >= non_hisp_sum, 1, 0)) ## race is a dummy for Hispanic

locationCounts <- donations %>%
  group_by(contributor_street_1,contributor_city,contributor_state, contributor_zip) %>%
  summarise(donationCount = n())

# Merge
# Names & Donor Data
trump2 <- merge(trump, locationNames, all.x=TRUE, by.x = c("street", "city_1","state2", "zip"),
                by.y = c("contributor_street_1","contributor_city","contributor_state","contributor_zip" ))
trump2 <- merge(trump2, locationCounts, all.x=TRUE, by.x = c("street", "city_1","state2", "zip"),
                by.y = c("contributor_street_1","contributor_city","contributor_state","contributor_zip" ))

saveRDS(trump2,file='TrumpAll.Rdata')

