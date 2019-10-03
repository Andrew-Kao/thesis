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
library(mapview)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument') 
}

contours1 <- readRDS('spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
counties <- rgdal::readOGR("nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties_transform<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
counties_project <- spTransform(counties, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# Distances
contourCountyDist <- gDistance(contours_project,counties_project, byid = TRUE)
saveRDS(contourCountyDist,'contourCountyDist.Rdata')
contourCountyMinDist <- apply(contourCountyDist,1,FUN=min)  # 428 counties that intersect!
stargazer(matrix(contourCountyMinDist,ncol=1), out="../../Output/Summary/ContourCountyMinDist.tex", title="Contour-County Minimum Distances", summary = TRUE)

# Intersections
contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
contourCountyIntersect <- gIntersects(contours_poly,counties_project, byid = TRUE)
contourCountyIntersect[contourCountyIntersect == "FALSE"] <- 0
contourCountyIntersect[contourCountyIntersect == "FALSE"] <- 1
contourCountyInterAll <- apply(contourCountyIntersect,1,FUN=sum)  # 590 counties inside/intersecting!
stargazer(matrix(contourCountyInterAll,ncol=1), out="../../Output/Summary/ContourCountyInterAll.tex", title="Counties Intersecting Contours", summary = TRUE)

# Merge data



##### DISCARD?
# nrow(contours_project)
for (i in 1:1) {
  assign(paste0('cp_',i), as(st_polygonize(st_as_sf(contours_project[i,])),"Spatial"))
}
test <- st_as_sf(contours_project[78,])
test2 <- st_polygonize(test)


# gDistance function from library rgeos 
smallCounties <- counties_project[1:2,]
smallContours <- contours_project[1,]
x <- gDistance(smallContours,counties_project)

contours_agg <- aggregate(contours_project)
test <- st_as_sf(contours_agg)
test2 <- st_polygonize(test)
test3 <- as(test2,"Spatial")

multi <- st_cast(contours_project,MULTIPOLYGON)

## polygonize contours
sf_contours <- st_as_sf(contours_project) 

test <- apply(sf_contours,2,st_polygonize)

  mutate(group = 1)
sf_contours_poly <- st_polygonize(sf_contours) 
agg2 <- aggregate(sf_contours, by=list(Group = group))

test <- sf_contours %>%
  st_polygonize()
purrr::map(mutate,x2=Site)

map(sf_contours,st_polygonize)
lapply(seq_along(sf_contours), function(x){
  st_polygonize(sf_contours[x])
})
contours_poly <- as(sf_contours_poly, "Spatial")

gDistCont <- partial(gDistance, contours_project)
x <- map(counties_project,gDistCont)



sapply(smallCounties,gDistCont)  

# dist2Line 
# # this doesn't work
# knn2nb
# # https://gis.stackexchange.com/questions/83722/calculate-minimal-distance-between-polygons-in-r