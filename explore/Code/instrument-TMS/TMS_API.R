# This file is used to get the TMS API data
# Goal: Produce a list of Stations that broadcast in Spanish language

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)
library(httr)
library(jsonlite)
library(readstata13)
library(purrr)
library(stringr)

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
call = 'v1.1/lineups?country=USA&postalCode='

postcodes <- read.dta13("../../politics/trump_donations/postcode.dta") %>%
  group_by(statecode, countycode) %>%
  summarise(first(postcode)) %>%
  rename(postcode = "first(postcode)")

## Progress (RUN BEFORE DOING API CALL)
filelist = list.files("postcode")
completed = lapply(filelist, str_extract, "[0-9]+")

## Call
lapply(unique(postcodes$postcode), getPostcodeAPI)

getPostcodeAPI <- function(postcode) {
  # check that we haven't done this postcode before
  if (!(postcode %in% completed)) {
    # Actual API Call
    raw_output <- GET(url = url, path = paste0(call,postcode,key))
    text_output <- rawToChar(raw_output$content)
    api_output <- fromJSON(text_output)
    saveRDS(api_output,file=paste0("postcode/postcode_",postcode,".Rdata"))
    Sys.sleep(.5)
  }
}

## Make list of lineups
fakepaste <- partial(paste0,"postcode/postcode_")
lineupIds <- completed %>%
  map(fakepaste) %>%
  map(paste0,".Rdata") %>%
  map(readRDS) %>%
  map(dplyr::select, lineupId) %>%
  reduce(bind_rows) %>%
  distinct()

  
### Step 2: Iterate through all the line-ups for a list of the stations
# http://developer.tmsapi.com/docs/data_v1_1/lineups/Lineup_channel_listing
  
call = 'v1.1/lineups/'
key <- keys[keys$Name %in% 'TMS',]$Keys
key <- paste0('?api_key=',key)

## Progress (RUN BEFORE DOING API CALL)
fakegsub <- partial(gsub, pattern = '.{6}$',replacement='')
filelist = list.files("lineup")
completed = lapply(filelist, fakegsub)

## Call
lapply(lineupIds$lineupId, getLineupAPI)

getLineupAPI <- function(lineup) {
  if (!(lineup %in% completed)) {
    raw_output <- GET(url = url, path = paste0(call,lineup,'/channels',key))
    text_output <- rawToChar(raw_output$content)
    api_output <- do.call(rbind, lapply(paste(text_output, collapse=""),jsonlite::fromJSON))
    saveRDS(api_output,file=paste0("lineup/",lineup,".Rdata"))
    Sys.sleep(.5)
  }
}


## Make list of stations
parameters <- read.csv('../20151020UCM-SampleData/20151020UCM-Full/parameters.csv', skip = 6) %>%
  filter(InCountry == "US") %>%  # one SrcKey per observation
  select(Call) %>%
  distinct() %>%
  mutate(Call = if_else(grepl("(.*)-.+$", Call), 
                        gsub('-.+$',replacement='',Call), Call)) ## need to regex out the -TV etc.

callSigns <- apply(parameters,2,as.list) ## properly coerce
  
fakepaste <- partial(paste0,"lineup/")
stationIds <- completed %>%
  map(fakepaste) %>%
  map(paste0,".Rdata") %>%
  map(readRDS)
onlyDF <- sapply(stationIds, function(x) class(x)=="data.frame")
stationIds <- stationIds[onlyDF] %>%
  map(dplyr::select, stationId, callSign)%>%
  reduce(bind_rows) %>%
  distinct() %>%
  mutate(callSign = if_else(grepl("(.*)\\d+$", callSign), 
                            gsub('\\d+$',replacement='',callSign), callSign)) %>%  ## remove trailing digits
  filter(callSign %in% callSigns$Call) 


### Step 3: Iterate through all the stations for broadcast languages
# http://developer.tmsapi.com/docs/data_v1_1/stations/Stations_details

call = ''

# recursively update file list to check for callsigns.
  
  
# callsign-language pairs



### DEBUGGING

test <- lapply(filelist, readRDS)

saveRDS(api_output,file=paste0("postcode/postcode_","sample",".Rdata"))
y <- list(a = 1, b = TRUE, c = "oops")
omg <- readRDS("postcode/postcode_20001.Rdata")

raw_output <- GET(url = url, path = paste0(call,"36003",key))
text_output <- rawToChar(raw_output$content)
api_output <- fromJSON(text_output)
df <- as.data.frame(api_output)


filter(postcode == 210 | postcode == 1368) %>%
  mutate(api = getPostcodeAPI(postcode)) %>%
  save(file="postcode.Rda")