##### MERGE ALL DONOR DATA ######

library(raster)
library(rgdal)
library(dplyr)
library(sf)

# Data sources
# 1. Cleaned donor data (direct)
# 2. Name classification data
# 3. Location data

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

options(stringsAsFactors = FALSE)

# 1. Cleaned donor data (direct)
donations <- read.csv('TrumpDonations.csv')

# 2. Name classification data
# I think we want to try just using the base name, but also try keeping the Hispanic names

FL <- read.csv('names_predict_FL.csv') 
census <- read.csv('names_predict_census.csv')
wiki <- read.csv('names_predict_wiki.csv')

# 3. Location data
trump <- sf::st_read(dsn='a00000009.gdbtable', layer = 'a00000009') %>%
  filter(Status == "M")  ## only keep precise matches
trump <- as_Spatial(trump)

# Merge
# Names & Donor Data
trump@data <- trump@data %>%
  left_join(donations, by = c("street" = "contributor_street_1", "city_1" = "contributor_city",
                              "state2" = "contributor_state", "zip" = "contributor_zip")) # 28,868 NAs

test <- trump@data[is.na(trump@data$memo_code), ]
test2 <- donations %>%  ### figure this out
  filter( "contributor_zip" > 100030)
# temp goal: determining why there are mismatches between locations and donations data
# then, merge in the name matching data too

%>%
  left_join(census, by = c("contributor_first_name","contributor_last_name")) %>%  ## 47 NAs (not dropped)

locations@
