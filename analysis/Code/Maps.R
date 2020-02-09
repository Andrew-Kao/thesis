## Graphics and Mapping ##
# see: http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html

library(spdep)
library(sp)
library(leaflet)
library(RColorBrewer)
library(maps)
library(ggplot2)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument') 
}

spanishCont <- readRDS('spanishCountourSLDF.Rdata')

baseData <- map_data("state")
spplot(baseData,region)
state <- data(stateMapEnv)
map('state',fill = FALSE)


spplot(spanishCont,"ERP",col.regions=brewer.pal(n=5, "Reds"))
ggplot() + geom_polygon(data = baseData, aes(x=long,y=lat,group=group))
ggplot() + geom_polygon(data = contours_poly, aes(x=Lon,y=Lat))
spplot(contours_poly,contour=TRUE)

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
