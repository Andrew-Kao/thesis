### Donor Regressions ###

library(raster)
library(rgdal)
library(rgeos)
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

### keep only in lower 48?

strump <- trump[1:1000,]

## need to match up data to counties and merge

instrument <- readRDS("../../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
counties@data <- counties@data %>%
  mutate(stateCounty = paste0(STATE,COUNTY))

### need a spatial level county dataset
instrument <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(COUNTY = origcounty, STATE = origstate) %>%
  mutate(STATE = str_pad(STATE,3,side="right","0"), COUNTY = str_pad(COUNTY,4,side="right","0"),
         stateCounty = paste0(STATE,COUNTY)) %>%
  filter(!is.na(COUNTY) & !is.na(STATE))

### oh god distances to contours now
contours1 <- readRDS('../../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
trump_project <- spTransform(trump, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourTrumpDist <- gDistance(contours_project,trump_project, byid = TRUE)
contourTrumpMinDist <- apply(contourTrumpDist,1,FUN=min)  # 428 counties that intersect!
saveRDS(contourTrumpMinDist,'contourTrumpMinDist.Rdata')
stargazer(matrix(contourTrumpMinDist,ncol=1), out="../../../Output/Summary/ContourTrumpMinDist.tex", title="Contour-Donation Minimum Distances", summary = TRUE)

contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourTrumpIntersect <- gIntersects(contours_poly,trump_project, byid = TRUE)
contourTrumpIntersect[contourTrumpIntersect == "FALSE"] <- 0
contourTrumpIntersect[contourTrumpIntersect == "TRUE"] <- 1
contourTrumpInterAll <- apply(contourTrumpIntersect,1,FUN=sum)  # 590 counties inside/intersecting!
saveRDS(contourTrumpInterAll,'contourTrumpInterAll.Rdata')
stargazer(matrix(contourTrumpInterAll,ncol=1), out="../../../Output/Summary/ContourTrumpInterAll.tex", title="Donations Within Contours", summary = TRUE)

trump@data <- trump@data %>%
  mutate(minDist = contourTrumpMinDist, inside = contourTrumpInterAll)

### merge in county level data
trumpCountyLink <- over(trump,counties,returnList = FALSE)
trump@data <- trump@data %>%
  mutate(stateCounty = trumpCountyLink$stateCounty)


trump2 <- merge(trump, instrument, by = 'stateCounty', all.x = TRUE)



inst2 <- instrument
inst2 <- inst2 %>%
  arrange(stateCounty) %>%
  mutate(leadStateCounty = lead(stateCounty)) %>%
  filter(stateCounty == leadStateCounty)
  

### do the reshape

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



