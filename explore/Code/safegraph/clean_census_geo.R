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

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/safegraph')
} else {
  setwd('/n/holyscratch01/dyang_lab/sltv/explore/Data/safegraph')
}


gj = system.file("safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson",package="spData")
gjsf = FROM_GeoJson("safegraph_open_census_data_2010_to_2019_geometry/cbg.geojson")



