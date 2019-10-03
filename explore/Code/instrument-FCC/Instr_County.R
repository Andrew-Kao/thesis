###### Instrument to County ####
# Goal: move from spatial data to a dataframe. Observation is a Station-County, only keeping those that 
#   either directly intersect, or are 'close' by
# approach: calculate distance to border (can't use centroid, physical rewrite necessary?)
# drop all that don't intersect and are far enough away
# for those that intersect, calculate % that lies within
# for those that do not, determine if 'inside' or 'outside'

library(maptools)
library(rgdal)
library(dplyr)
library(spdep)
library(stringr)
library(class)
library(wrapr)
library(raster)
library(tmap)
library(rgeos)
library(purrr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument') 
}

contours1 <- readRDS('spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
counties <- rgdal::readOGR("nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties_transform<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
counties_project <- spTransform(counties, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourCountyDist <- gDistance(contours_project,counties_project, byid = TRUE)
saveRDS(contourCountyDist,'contourCountyDist.Rdata')
contourCountyMinDist <- apply(contourCountyDist,1,FUN=min)  # 428 counties that intersect!

gDistCont <- partial(gDistance, contours_project)
x <- map(counties_project,gDistCont)



sapply(smallCounties,gDistCont)  


## distance returns meters for projected





# gDistance function from library rgeos 
smallCounties <- counties_project[1:2,]
smallContours <- contours_project[1,]
x <- gDistance(smallContours,counties_project)



# dist2Line 
# # this doesn't work
# knn2nb
# # https://gis.stackexchange.com/questions/83722/calculate-minimal-distance-between-polygons-in-r