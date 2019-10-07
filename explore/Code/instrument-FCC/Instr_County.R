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
library(stargazer)
library(ipumsr)

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
## credit: https://stackoverflow.com/questions/34390829/calculate-area-in-each-of-the-spatiallines-in-a-spatiallinesdataframe
contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourCountyIntersect <- gIntersects(contours_poly,counties_project, byid = TRUE)
contourCountyIntersect[contourCountyIntersect == "FALSE"] <- 0
contourCountyIntersect[contourCountyIntersect == "TRUE"] <- 1
contourCountyInterAll <- apply(contourCountyIntersect,1,FUN=sum)  # 590 counties inside/intersecting!
stargazer(matrix(contourCountyInterAll,ncol=1), out="../../Output/Summary/ContourCountyInterAll.tex", title="Counties Intersecting Contours", summary = TRUE)

# Area intersections
countyAreas <- gArea(counties_project, byid=TRUE)
contoursUnion <- gUnaryUnion(contours_poly) # alternative: contoursUnion <- gUnionCascaded(contours_poly)
contoursUnion <- spTransform(contoursUnion, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
contourCountyIntersection <- gIntersection(contoursUnion,counties_project,byid = TRUE)  ## only 590 intersect
  # now keep IDs and areas
countyInterAreas <- sapply(slot(contourCountyIntersection, 'polygons'), function(i) slot(i, 'area')) 
countyInterIDs <- sapply(slot(contourCountyIntersection, 'polygons'), function(i) slot(i, 'ID')) 
countyInterIDs <- lapply(countyInterIDs, str_sub, start=3)
countyInter <- do.call(rbind, Map(data.frame, ID=countyInterIDs,interArea=countyInterAreas)) %>%
  mutate(ID = as.numeric(as.character(ID)))

distances <- readRDS('contourCountyDist.Rdata')
distances <- apply(distances,1,FUN=min) 

countiesMerged <- counties_transform@data %>%
  dplyr::select(STATE, COUNTY) %>%
  rename(state = STATE, county = COUNTY) %>%
  mutate(ID = 1:nrow(counties_transform)-1, dist = distances, intersectCount = contourCountyInterAll, 
         intersects = ifelse(intersectCount >= 1, 1, 0), area = countyAreas) %>%
  left_join(countyInter, by = 'ID') %>%
  mutate(areaRatio = interArea/area, areaRatio = ifelse(areaRatio > 1, 1, areaRatio), areaRatio = ifelse(is.na(areaRatio), 0, areaRatio)) %>% # 4 instances over 1, probably spatial hiccup
  mutate(state = str_sub(state,end=2), county = str_sub(county, end = 3))

saveRDS(countiesMerged,'countyInstrument.Rdata')
stargazer(countiesMerged, out="../../Output/Summary/CountiesMerged.tex", title="County Instrument Spatial Characteristics",
          summary = TRUE, font.size = 'scriptsize')


######## merge in data for instrument ########

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/counties') 
}

ddi <- read_ipums_ddi("usa_00007.xml")
census <- read_ipums_micro(ddi)

censusClean <- census %>%
  rename(state = STATEFIP, county = COUNTYFIP, hisp = HISPAN, employ = EMPSTAT, income = INCTOT) %>%
  mutate(state = str_pad(state, width = 2, side = "left", pad = "0"), county = str_pad(county, width = 3, side = "left", pad = "0")) %>%
  mutate(hisp = ifelse(hisp ==9, NA, hisp), hisp = ifelse(hisp > 0, 1, 0)) %>% ## hispanic dummy, no response as missing
  mutate(employ = ifelse(employ == 3, NA, employ), employ = employ - 1) %>% ## employment dummy
  group_by(state, county) %>%
  summarise(hisp = stats::weighted.mean(hisp, HHWT), employ = stats::weighted.mean(employ, HHWT), income = stats::weighted.mean(income, HHWT) )
  
  


share_col_25o_1970 share_hs_25o_1970 population pdens_2010


