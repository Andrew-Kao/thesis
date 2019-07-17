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

##### FILE REFERENCE
# coverpts_shp <- shapefile('20151020UCM-Full/coverpts')
# points_shp <- shapefile('20151020UCM-Full/points')
sources_shp <- shapefile('20151020UCM-Full/sources')

plot(contours_shp)
# plot(coverpts)  ## this one is real slow
# plot(points)
# plot(sources)

point_tab <- read.csv('20151020UCM-Full/points.csv')
###########


# Main Code 

contours_shp <- shapefile('20151020UCM-Full/contours')
parameters <- read.csv('20151020UCM-Full/parameters.csv', skip = 6) %>%
  filter(InCountry == "US") # one SrcKey per observation

contours <- merge(contours_shp, parameters, by.x = "SOURCEKEY", by.y = "SrcKey")


# TODO: Given a set of points, determine which are inside contours and which are outside
# TODO: Given a set of points, determine distance to contour boundary


## TODO: join in Spanish Language, find areas only covered by one station
## TODO: figure out county level analysis :: Ideally, counties that are close to the boundary, but only a small % (or 0%) actually intersects with it






