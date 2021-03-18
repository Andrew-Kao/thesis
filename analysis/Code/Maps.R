## Graphics and Mapping ##
# see: http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html

library(spdep)
library(sp)
library(leaflet)
library(RColorBrewer)
library(maps)
library(ggplot2)
library(mapdata)

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

counties <- map_data("county")
states <- map_data("state")
gg_f <- ggplot(data=states, mapping=aes(x=long, y=lat, group=group)) +
  geom_polygon(color="black", fill="gray") +
  geom_polygon(data=counties, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_polygon(data = spanishCont, aes(alpha = 0.1, colour = "cornsilk", fill = "ivory")) +
  ylim(24,50) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(legend.position = "none")
plot(gg_f)
ggsave("../../../analysis/Output/img/SpanishContours_pretty.pdf", width = 16, height = 10)
system2(command = "pdfcrop", 
        args    = c("../../../analysis/Output/img/SpanishContours_pretty.pdf", 
                    "../../../analysis/Output/img/SpanishContours_pretty.pdf"))


# gotta do more classification
spplot(busn2,"hispFoodName")

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
