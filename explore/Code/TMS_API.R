# This file is used to get the TMS API data
# Goal: Produce a list of Stations that broadcast in Spanish language

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)
library(httr)
library(jsonlite)
library(readstata13)
library(purrr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument/TMS') 
}

options(stringsAsFactors = FALSE)

url <- "http://data.tmsapi.com/"
# Census Key
keys <- read.csv('../../keys.csv', stringsAsFactors = FALSE)
key <- keys[keys$Name %in% 'TMS',]$Keys
key <- paste0('&api_key=',key)

### Step 1: Iterate through all the postcodes for a collection of line-ups
# http://developer.tmsapi.com/docs/data_v1_1/lineups/Lineups_by_postal_code

# call API
postcodes <- read.dta13("../../trump_donations/postcode.dta") %>%
  filter(postcode == 210 | postcode == 1368) %>%
  mutate(api = getPostcodeAPI(postcode)) %>%
  save(file="postcode.Rda")

getPostcodeAPI <- function(postcode) {
  call = 'v1.1/lineups?country=USA&postalCode='
  # Actual API Call
  raw_output <- GET(url = url, path = paste0(call,postcode,key))
  text_output <- rawToChar(raw_output$content)
  api_output <- fromJSON(text_output)
  df <- as.data.frame(api_output)
  return(df)
}


# Make list of lineups
lineups <- load("postcode.Rda")
  
  
### Step 2: Iterate through all the line-ups for a list of the stations
# http://developer.tmsapi.com/docs/data_v1_1/lineups/Lineup_channel_listing
  

  
  
### Step 3: Iterate through all the stations for broadcast languages
# http://developer.tmsapi.com/docs/data_v1_1/stations/Stations_details


  
  
  
  



