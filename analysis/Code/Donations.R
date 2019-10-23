### Donor Regressions ###

library(raster)
library(rgdal)
library(dplyr)
library(sf)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

## go back to the trump data and recheck this works
trump <- readRDS(file='TrumpAll.Rdata')

## approach 1: county level spatial regression
trump <-spTransform(trump, CRS("+proj=longlat +datum=NAD83"))

instrument <- readRDS("../../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))

### need a spatial level county dataset
origCounties <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(origCty = origcounty, origState = origstate) %>%
  mutate(origState = str_pad(origState,3,side="left","0"), origCty = str_pad(origCty,3,side="left","0"))

destCounties <- instrument %>%
  rename_all(~ paste0("dest",.)) %>%
  rename(destCty = destcounty, destState = deststate) %>%
  mutate(destState = str_pad(destState,3,side="left","0"), destCty = str_pad(destCty,3,side="left","0"))

## distances
distances <- fread('../counties/distances/sf12010countydistancemiles.csv') %>%
  mutate(county1 = str_pad(county1, 6, side = "left", "0")) %>%
  mutate(county2 = str_pad(county2, 6, side = "left", "0")) %>%
  mutate(destState = str_sub(county2,start=1,end=3), destCty = str_sub(county2,start=4,end=6)) %>%
  mutate(origState = str_sub(county1,start=1,end=3), origCty = str_sub(county1,start=4,end=6))


counties



## approach 2: point pattern



