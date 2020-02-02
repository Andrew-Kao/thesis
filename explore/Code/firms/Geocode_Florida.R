# This file is used to interface with the US Census Geocode for Florida Businesses
# Located here: https://geocoding.geo.census.gov/

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)
library(httr)
library(jsonlite)
library(purrr)
library(stringr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)

addresses <- read.csv("FLAddresses.csv") %>%
  mutate(lat = 0, long = 0) %>%
  mutate(PRINC_ADD_1 = sapply(PRINC_ADD_1, URLencode), PRINC_CITY = sapply(PRINC_CITY,URLencode))

url <- "https://geocoding.geo.census.gov/" # geographies for other info (tiger etc.)

### Step 1: Iterate through all the postcodes for a collection of line-ups
# http://developer.tmsapi.com/docs/data_v1_1/lineups/Lineups_by_postal_code

# call API
call = 'geocoder/locations/address?'

size <- nrow(addresses)
progress <- 1


while (progress < size) {
  raw_output <- GET(url = url, path = paste0(call,'street=',addresses$PRINC_ADD_1[progress],'&city=',addresses$PRINC_CITY[progress],
                                             '&state=',addresses$PRINC_STATE[progress],'&zip=',addresses$PRINC_ZIP5[progress],
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
write.csv(match_address,"FloridaAddresses_gov.csv")

## extract uncompleted ones
unmatch_address <- addresses %>%
  filter(lat == 0)
write.csv(match_address,"FloridaAddresses_fail.csv")









