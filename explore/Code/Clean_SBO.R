# This file is (currently) used for exploring the SBO data
# When completed, it will be used to produce a clean SBO dataset


# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)
library(httr)
library(jsonlite)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firm_data/sbo') 
}

# The 2007 data is only national, without location. Instead, we will work with the 2012 API
# sbo <- fread('sbo_2007.csv')

# API Call. 
# Credit to https://www.r-bloggers.com/accessing-apis-from-r-and-a-little-r-programming/
# TODO: customize call as needed

# list of variable names: https://api.census.gov/data/2012/sbo/variables.html
# example calls: https://api.census.gov/data/2012/sbo/examples.html

options(stringsAsFactors = FALSE)

url <- 'https://api.census.gov/'
call <- 'data/2012/sbo?get='
fields <- 'NAICS2012_TTL,RCPSZFI,GEO_TTL,FIRMPDEMP,RCPSZFI_TTL,ETH_GROUP'
geo <- '&for=metropolitan%20statistical%20area/micropolitan%20statistical%20area:*&key='
# Census Key
keys <- read.csv('../../keys.csv', stringsAsFactors = FALSE)
key <- keys[keys$Name %in% 'Census',]$Keys

# Actual API Call
raw_output <- GET(url = url, path = paste0(call,fields,geo,key))

# Transform into df
text_output <- rawToChar(raw_output$content)
api_output <- fromJSON(text_output)
api_df <- as.data.frame(api_output)

names(api_df) <- as.character(unlist(api_df[1,]))
api_df <- api_df[-1,]




