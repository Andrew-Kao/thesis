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

options(stringsAsFactors = FALSE)

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

spanishStations <- readRDS('../TMS/spanishStations.Rdata')

contours_shp <- shapefile('20151020UCM-Full/contours')
parameters <- read.csv('20151020UCM-Full/parameters.csv', skip = 6) %>%
  filter(InCountry == "US") %>% # one SrcKey per observation 
  mutate(simpleCall = if_else(grepl("(.*)-.+$", Call), 
                      gsub('-.+$',replacement='',Call), Call)) %>% ## need to regex out the -TV etc.
  right_join(spanishStations, by = c("simpleCall" = "callSign"))
# there are three stations for which the regular simpleCall dual-identifies

# All contours with data  
contours <- merge(contours_shp, parameters, by.x = "SOURCEKEY", by.y = "SrcKey")

# Keeping the Spanish subset we want
spanishContours <- contours
keepList <- contours@data %>%
  mutate(keep = !is.na(bcastLangs))
keepList <- keepList$keep  ## use purrr/reduce 
spanishContours@data <- contours@data[keepList,]
spanishContours@lines <- contours@lines[keepList]

saveRDS(spanishContours,file='../spanishCountourSLDF.Rdata')


# TODO: Given a set of points, determine which are inside contours and which are outside
# TODO: Given a set of points, determine distance to contour boundary

## TODO: how much more do we have to clean this data? this is probably good - then break it down into spatials and county level
## given # of stations, doing it by geographical region might be feasible?

## TODO: figure out county level analysis :: Ideally, counties that are close to the boundary, but only a small % (or 0%) actually intersects with it


# DESCRIPTIVES
plot(contours)

## some Mexican states. We can still use the international coverage boundary here (maybe?)  [probably not, downward bias bc border]
paraDesc <- parameters %>%
  group_by(St) %>%
  summarise(count = n())

## lots of our analysis will be in TX, FL, NM - though we do have 3 in Illinois.


