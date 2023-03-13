###### Census Tract data ####
# Data sourced: https://ocrdata.ed.gov/DownloadDataFile (Oct 18, 2019)

library(data.table)
library(rgdal)
library(sf)
library(raster)
library(rgeos)
library(dplyr)
library(stringr)
library(stargazer)
library(readxl)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/counties/tracts/geo/tiger/TIGER2010/TRACT/2010') 
}



contours1 <- readRDS('../../../../../../../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## outer for loop over all states, 2 digit regex
files <- list.files(path=".", pattern="tl_2010_[[:digit:]][[:digit:]]_tract10.zip", full.names=TRUE, recursive=FALSE)
lapply(files, function(file) {
  
  print(file)
  
  # get unzipped file
  r <- regexpr("_[[:digit:]][[:digit:]]_",file)
  statenum <- gsub('^.|.$', '', regmatches(file,r))
  file_base <- paste0('tl_2010_',statenum,'_tract10')
  unzip(file,exdir=file_base)
  
  # get geodata
  tract <- rgdal::readOGR(paste0(file_base,'/',file_base,'.shp'))
  tract <- spTransform(tract, CRS("+proj=longlat +datum=NAD83"))
  tract_project <- spTransform(tract, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
  
  print("MinDist")
  # spatial, min dist and overlap with contours 
  contourtractDist <- gDistance(contours_project,tract_project, byid = TRUE)
  contourtractMinDist <- apply(contourtractDist,1,FUN=min)  
  saveRDS(contourtractMinDist,paste0("../state_clean/",file_base,"_mindist.Rdata"))
  
  print("Intersects")
  contourtractIntersect <- gIntersects(contours_poly,tract_project, byid = TRUE)
  contourtractIntersect[contourtractIntersect == "FALSE"] <- 0
  contourtractIntersect[contourtractIntersect == "TRUE"] <- 1
  contourtractInterAll <- apply(contourtractIntersect,1,FUN=sum)  
  saveRDS(contourtractInterAll,paste0("../state_clean/",file_base,"_interall.Rdata"))
  
  tract@data <- tract@data %>%
    mutate(minDist = contourtractMinDist, intersect = contourtractInterAll)
  saveRDS(tract,paste0("../state_clean/",file_base,"_main.Rdata"))
}
)



