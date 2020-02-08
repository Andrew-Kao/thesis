### Business Regressions ###

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(stargazer)
library(stringr)
library(purrr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)


## go back to the trump data and recheck this works
busn <- readRDS(file='BusnAll.Rdata')

## approach 1: point pattern 
busn <-spTransform(busn, CRS("+proj=longlat +datum=NAD83"))

### some summary stats
busnData <- busn@data %>%
  dplyr::select(hispName, hispFoodName, hispNameD)
stargazer(busnData, out="../../../Output/Summary/FirmNames.tex", title="Firm Names",
          summary = TRUE, font.size = 'scriptsize')

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

contours1 <- readRDS('../../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


#### raster
r <- raster( xmn =-124.784, xmx=-66.951,ymn=24.743,ymx=49.346,crs= "+proj=longlat +datum=NAD83",
             nrow = 100, ncol = 200)
rDonCount <- rasterize(trump2, r, field=trump2$donationCount,fun=sum)
values(rDonCount) <- ifelse(is.na(values(rDonCount)),0,values(rDonCount))
rHispSum <- rasterize(trump2, r, field=trump2$hisp_sum,fun=mean)
values(rHispSum) <- ifelse(is.na(values(rHispSum)),0,values(rHispSum))
rRace <- rasterize(trump2, r, field=trump2$race,fun=mean)
values(rRace) <- ifelse(is.na(values(rRace)),0,values(rRace))

#### COUNTIES
rgdf <- as(rPop,'SpatialGridDataFrame')
testLink <- over(rgdf,counties,returnList=FALSE)
rgdf@data <- rgdf@data %>%
  mutate(stateCounty = testLink$stateCounty)
rgdf2 <- merge(rgdf, instrument, by = 'stateCounty', all.x = TRUE)

rPCHisp <- raster(rgdf2,layer=14)
rPop <- raster(rgdf2,layer=15)
rIncome <- raster(rgdf2,layer=17)

#### INSTRUMENT
rspdf <- as(rDonCount,'SpatialPointsDataFrame')
rspdf <- spTransform(rspdf, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
contourDist <- gDistance(contours_project,rspdf, byid = TRUE)
contourMinDist <- apply(contourDist,1,FUN=min)

contourIntersect <- gIntersects(contours_poly,rspdf, byid = TRUE)
contourIntersect[contourIntersect == "FALSE"] <- 0
contourIntersect[contourIntersect == "TRUE"] <- 1
contourInterAll <- apply(contourIntersect,1,FUN=sum)  # 590 counties inside/intersecting!

rspdf@data <- rspdf@data %>%
  mutate(minDist = contourMinDist, inside = contourInterAll)
rspdf <- spTransform(rspdf, CRS("+proj=longlat +datum=NAD83"))

rMinDist <- rasterize(rspdf, r, field=rspdf$minDist,fun=mean)
rIntersect <- rasterize(rspdf, r, field=rspdf$inside,fun=mean)











