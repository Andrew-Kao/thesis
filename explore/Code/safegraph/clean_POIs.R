###### Safegraph data ####

library(dplyr)
library(data.table)
library(rgdal)
library(sf)
library(raster)
library(rgeos)
library(stringr)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/safegraph')
  sdir <- '~/Dropbox/safegraph/coreplaces'
}


# do for each of the coreplaces

for (i in 1:5) {
  tdf <- fread(paste0(sdir,'/core_poi-part1.csv'))
  
  tdf <- tdf %>%
    filter(iso_country_code == 'US') %>%
    mutate(sector = floor(naics_code/10000)) %>%
    filter(sector == 51 | sector == 52 | sector == 61 | sector == 71 | sector == 72)
  
  if (i == 1) {
    df = tdf
  }
  else {
    df = rbind(df,tdf) 
  }
   
}

coordinates(df) <- c('longitude','latitude')
crs_nad83 <- CRS(SRS_string = "EPSG:4269")
# wkt(crs_nad83)
proj4string(df) <- crs_nad83

## need to match up data to counties and merge

instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
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

### distances to contours 
contours1 <- readRDS('../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
df_project <- spTransform(df, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourdfDist <- gDistance(contours_project,df_project, byid = TRUE)
contourLEAMinDist <- apply(contourLEADist,1,FUN=min)  # 428 counties that intersect!
saveRDS(contourLEAMinDist,'contourLEAMinDist.Rdata')
stargazer(matrix(contourLEAMinDist,ncol=1), out="../../Output/Summary/ContourLEAMinDist.tex", title="Contour-LEA Minimum Distances", summary = TRUE)

contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourLEAIntersect <- gIntersects(contours_poly,leas_project, byid = TRUE)
contourLEAIntersect[contourLEAIntersect == "FALSE"] <- 0
contourLEAIntersect[contourLEAIntersect == "TRUE"] <- 1
contourLEAInterAll <- apply(contourLEAIntersect,1,FUN=sum)  
saveRDS(contourLEAInterAll,'contourLEAInterAll.Rdata')
stargazer(matrix(contourLEAInterAll,ncol=1), out="../../Output/Summary/ContourLEAInterAll.tex", title="LEA Within Contours", summary = TRUE)

contourLEAMinDist <- readRDS('contourLEAMinDist.Rdata')
contourLEAInterAll <- readRDS('contourLEAInterAll.Rdata')

leas@data <- leas@data %>%
  mutate(minDist = contourLEAMinDist, inside = contourLEAInterAll)

### merge in county level data
leaCountyLink <- over(leas,counties,returnList = FALSE)
leas@data <- leas@data %>%
  mutate(stateCounty = leaCountyLink$stateCounty)

leas2 <- merge(leas, instrument, by = 'stateCounty', all.x = TRUE)
saveRDS(leas2, 'LEAReadyRaster.Rdata')


