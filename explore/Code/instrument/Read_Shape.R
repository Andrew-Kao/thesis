# This file is (currently) used for exploring the FCC contour data
# When completed, it will be used to produce SLTV contours.

# SETUP -------------------------------------------------------------------

library(maptools)
library(rgdal)
library(dplyr)
library(spdep)
library(stringr)
library(class)
library(wrapr)
library(raster)
library(tmap)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument/20151020UCM-SampleData') 
}

contours_shp <- shapefile('20151020UCM-Full/contours')
# coverpts_shp <- shapefile('20151020UCM-Full/coverpts')
# points_shp <- shapefile('20151020UCM-Full/points')
sources_shp <- shapefile('20151020UCM-Full/sources')

# plot(contours)
# plot(coverpts)  ## this one is real slow
# plot(points)
# plot(sources)

point_tab <- read.csv('20151020UCM-Full/points.csv')
parameters <- read.csv('20151020UCM-Full/parameters.csv', skip = 6)


## TODO: join contours and sourcekey






