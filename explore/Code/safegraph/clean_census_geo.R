###### Safegraph data -- POI cleaning ####

library(dplyr)
library(data.table)
library(rgdal)
library(sf)
library(raster)
library(rgeos)
library(stringr)
library(spatial)
library(geojsonR)
library(purrr)
library(readr)
library(progress)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/safegraph')
} else {
  setwd('/n/holyscratch01/dyang_lab/sltv/explore/Data/safegraph')
}


######### merge to instrument #########

# instrument data
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

# contour data
contours1 <- readRDS('../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))


####### census block data
# gj = system.file("safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson",package="spData")
# gjsf = st_read("safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson")

gjsf = FROM_GeoJson("safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson")
gjsf <- gjsf[[1]]

crs_nad83 <- CRS(SRS_string = "EPSG:4269")

# list of spatial dfs
pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(gjsf))
pb$tick(0)
l_sdf <- map(gjsf[1:length(gjsf)], makeSPoly)  # 

# iterate through features and create list for distance and intersects
map()

gjsf[[1]]    # map over
testdf <- gjsf[[1]][1:10] # for individual
x <- gjsf[[1]][1]

# z <- gjsf[[1754]][[1]][2]
z <- gjsf[[2475]][[1]][2]
z3 <- gjsf[[3775]][[1]][2]
zc <- z$coordinates
zcdfl <- map(zc, as.data.frame)
zcrbd <- rbindlist(zcdfl)
z2 <- gjsf[[1]][[1]][2]

dfbind <- function(x) {
  if (typeof(x) == "list") {
    x <- map(x,as.data.frame)
    x <- rbindlist(x) 
  }
  return(x)
}

makeSPoly <- function(x) {
  pb$tick(1)
  x <- x[[1]][2]     # get coordinates
  # print(typeof(x$coordinates[[1]][1]))
  
  # first test if multiple layers of coordinates
  if (length(x$coordinates) > 1 ) {
    x <- map(x$coordinates, dfbind)
    x <- map(x, as.data.frame)
    x <- rbindlist(x) 
    coordinates(x) <- c('V1','V2')
  } else if (typeof(x$coordinates[[1]][1]) == "double") {
    # double - one set of coords
    x <- as.data.frame(x)          # df 
    coordinates(x) <- c('coordinates.1','coordinates.2')
  } else if (typeof(x$coordinates[[1]][1]) == "list") {
    # list with multiple coords inside
    x <- map(x$coordinates[[1]], as.data.frame)
    x <- rbindlist(x) 
    coordinates(x) <- c('V1','V2')
  }
  proj4string(x) <- crs_nad83
  # print(x)
  return(x)
}



makeSPoly <- function(x) {
  pb$tick(1)
  x <- x[[1]][2]     # get coordinates
  # print(typeof(x$coordinates[[1]][1]))
  
  # first test if multiple layers of coordinates
  if (length(x$coordinates) > 1 ) {
    x <- map(x$coordinates, as.data.frame)
    x <- rbindlist(x) 
    coordinates(x) <- c('V1','V2')
  } else if (typeof(x$coordinates[[1]][1]) == "double") {
    # double - one set of coords
    x <- as.data.frame(x)          # df 
    coordinates(x) <- c('coordinates.1','coordinates.2')
  } else if (typeof(x$coordinates[[1]][1]) == "list") {
    # list with multiple coords inside
    x <- map(x$coordinates[[1]], as.data.frame)
    x <- rbindlist(x) 
    coordinates(x) <- c('V1','V2')
  }
  proj4string(x) <- crs_nad83
  # print(x)
  return(x)
}



# TODO: regression where you see relative coming/going from census block groups inside/outside the border

