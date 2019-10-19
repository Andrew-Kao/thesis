### Donor Regressions ###

library(raster)
library(rgdal)
library(dplyr)
library(sf)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}


trump <- readRDS(file='TrumpAll.Rdata')

## approach 1: county level spatial regression


## approach 2: point pattern



