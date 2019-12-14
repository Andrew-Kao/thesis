## Graphics and Mapping ##
# see: http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html

library(spdep)
library(leaflet)
library(RColorBrewer)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument') 
}

spanishCont <- readRDS('spanishCountourSLDF.Rdata')

leaflet(spanishCont) %>%
  addPolygons(stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5) %>%
  addTiles()
# save img/SpanishContours.png

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}

educ <- readRDS('LEAReadyRaster.Rdata')

leaflet(educ) %>%
  addCircleMarkers(opacity = .001, radius = 1) %>%
  addTiles()
