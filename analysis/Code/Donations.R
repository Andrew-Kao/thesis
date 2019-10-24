### Donor Regressions ###

library(raster)
library(rgdal)
library(dplyr)
library(sf)
library(stargazer)
library(stringr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

## go back to the trump data and recheck this works
trump <- readRDS(file='TrumpAll.Rdata')

## approach 1: point pattern 
trump <-spTransform(trump, CRS("+proj=longlat +datum=NAD83"))

### some summary stats
trumpData <- trump@data %>%
  select(hisp_sum, non_hisp_sum, race, donationCount)
stargazer(trumpData, out="../../../Output/Summary/TrumpDonations.tex", title="Trump Donors",
          summary = TRUE, font.size = 'scriptsize')

### keep only in lower 48

strump <- trump[1:1000,]

## need to match up data to counties and merge

instrument <- readRDS("../../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))

### need a spatial level county dataset
instrument <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(COUNTY = origcounty, STATE = origstate) %>%
  mutate(STATE = str_pad(STATE,3,side="right","0"), COUNTY = str_pad(COUNTY,4,side="right","0"))

### point pattern
DonationArea <- raster::area(counties)
allArea <- sum(DonationArea)
r <- raster(counties)
res(r) <- 1000 ## try different values? 

### make raster
### use calc to run regressions?
r <- rasterize(counties, r)
plot(r)
# quads <- as(r, 'SpatialPolygons')
# plot(quads, add=TRUE)
points(trump, col='red', cex=.5) ## something has gone very wrong


## approach 2: county level spatial regression
## not preferred because too few counties



