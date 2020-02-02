# This file is used to interface with the US Census Geocode for Clinton Donations
# Located here: https://geocoding.geo.census.gov/

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/clinton_donations') 
}

options(stringsAsFactors = FALSE)

addresses <- read.csv("ClintonAddresses.csv") %>%
  mutate(lat = 0, long = 0) %>%
  mutate(street = sapply(street, URLencode), city = sapply(city,URLencode))

url <- "https://geocoding.geo.census.gov/" # geographies for other info (tiger etc.)

### Step 1: Iterate through all the postcodes for a collection of line-ups
# http://developer.tmsapi.com/docs/data_v1_1/lineups/Lineups_by_postal_code

# call API
call = 'geocoder/locations/address?'

size <- nrow(addresses)
progress <- 1


while (progress < size) {
  raw_output <- GET(url = url, path = paste0(call,'street=',addresses$street[progress],'&city=',addresses$city[progress],
                                             '&state=',addresses$state2[progress],'&zip=',addresses$zip[progress],
                                             '&benchmark=Public_AR_Current&format=json'))
  text_output <- rawToChar(raw_output$content)
  api_output <- fromJSON(text_output)
  
  if (!is.null(api_output$result$addressMatches$coordinates$y)) {
    addresses$lat[progress] = api_output$result$addressMatches$coordinates$y
    addresses$long[progress] = api_output$result$addressMatches$coordinates$x 
  }
  
  progress <- progress + 1
}


## save matches
match_address <- addresses %>%
  filter(lat != 0)
write.csv(match_address,"ClintonAddresses_gov.csv")

## extract uncompleted ones
unmatch_address <- addresses %>%
  filter(lat == 0)
write.csv(match_address,"ClintonAddresses_fail.csv")









