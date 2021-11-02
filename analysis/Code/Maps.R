## Graphics and Mapping ##
# see: http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html

library(spdep)
library(sp)
library(leaflet)
library(RColorBrewer)
library(maps)
library(maptools)
library(grid)
library(ggplot2)
library(mapdata)
devtools::install_github("3wen/legendMap")
library('')

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


counties <- map_data("county")
states <- map_data("state")
ed <- data.frame(educ)
gg_f <- ggplot(data=states, mapping=aes(x=long, y=lat,group = group)) +
  geom_polygon(color="black", fill="gray") +
  geom_polygon(data=counties, fill=NA, color="white") + 
  geom_polygon(color="black", fill=NA) + 
  geom_point(data = ed, aes(x = coords.x1, y = coords.x2, colour = "ivory", group = optional), alpha = 0.3) +
  geom_polygon(data = spanishCont, aes(alpha = 0.1, colour = "cornsilk", fill = "blue")) +
  ylim(24,50) +
  xlim(-125,-65) +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  theme(legend.position = "none") +
  scale_bar(lon = -120, lat = 26, 
              distance_lon = 500, distance_lat = 100, distance_legend = 200, 
              dist_unit = "km", orientation = FALSE,
            legend_size = 5)
plot(gg_f)
ggsave("../../../analysis/Output/img/Schools_pretty.pdf", width = 16, height = 10)
system2(command = "pdfcrop", 
        args    = c("../../../analysis/Output/img/Schools_pretty.pdf", 
                    "../../../analysis/Output/img/Schools_pretty.pdf"))

## manually add legend  

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


# credit: https://egallic.fr/en/scale-bar-and-north-arrow-on-a-ggplot2-map/
# Result #
#--------#
# Return a list whose elements are :
# 	- rectangle : a data.frame containing the coordinates to draw the first rectangle ;
# 	- rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
# 	- legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distance_lon : length of each rectangle ;
# distance_lat : width of each rectangle ;
# distance_legend : distance between rectangles and legend texts ;
# dist_units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
create_scale_bar <- function(lon,lat,distance_lon,distance_lat,distance_legend, dist_units = "km"){
  # First rectangle
  bottom_right <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon, dist.units = dist_units, model = "WGS84")
  
  topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_lat, dist.units = dist_units, model = "WGS84")
  rectangle <- cbind(lon=c(lon, lon, bottom_right[1,"long"], bottom_right[1,"long"], lon),
                     lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
  rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
  
  # Second rectangle t right of the first rectangle
  bottom_right2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distance_lon*2, dist.units = dist_units, model = "WGS84")
  rectangle2 <- cbind(lon = c(bottom_right[1,"long"], bottom_right[1,"long"], bottom_right2[1,"long"], bottom_right2[1,"long"], bottom_right[1,"long"]),
                      lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
  rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
  
  # Now let's deal with the text
  on_top <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance_legend, dist.units = dist_units, model = "WGS84")
  on_top2 <- on_top3 <- on_top
  on_top2[1,"long"] <- bottom_right[1,"long"]
  on_top3[1,"long"] <- bottom_right2[1,"long"]
  
  legend <- rbind(on_top, on_top2, on_top3)
  legend <- data.frame(cbind(legend, text = c(0, distance_lon, distance_lon*2)), stringsAsFactors = FALSE, row.names = NULL)
  return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}

#
# Result #
#--------#
# This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow #
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distance_lon : length of each rectangle ;
# distance_lat : width of each rectangle ;
# distance_legend : distance between rectangles and legend texts ;
# dist_units : units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles) ;
# rec_fill, rec2_fill : filling colour of the rectangles (default to white, and black, resp.);
# rec_colour, rec2_colour : colour of the rectangles (default to black for both);
# legend_colour : legend colour (default to black);
# legend_size : legend size (default to 3);
# orientation : (boolean) if TRUE (default), adds an orientation arrow to the plot ;
# arrow_length : length of the arrow (default to 500 km) ;
# arrow_distance : distance between the scale bar and the bottom of the arrow (default to 300 km) ;
# arrow_north_size : size of the "N" letter (default to 6).
scale_bar <- function(lon, lat, distance_lon, distance_lat, distance_legend, dist_unit = "km", rec_fill = "white", rec_colour = "black", rec2_fill = "black", rec2_colour = "black", legend_colour = "black", legend_size = 3, orientation = TRUE, arrow_length = 500, arrow_distance = 300, arrow_north_size = 6){
  the_scale_bar <- create_scale_bar(lon = lon, lat = lat, distance_lon = distance_lon, distance_lat = distance_lat, distance_legend = distance_legend, dist_unit = dist_unit)
  # First rectangle
  rectangle1 <- geom_polygon(data = the_scale_bar$rectangle, aes(x = lon, y = lat, group = FALSE), fill = rec_fill, colour = rec_colour)
  
  # Second rectangle
  rectangle2 <- geom_polygon(data = the_scale_bar$rectangle2, aes(x = lon, y = lat, group = FALSE), fill = rec2_fill, colour = rec2_colour)
  
  # Legend
  scale_bar_legend <- annotate("text", label = paste(the_scale_bar$legend[,"text"], dist_unit, sep=""), x = the_scale_bar$legend[,"long"], y = the_scale_bar$legend[,"lat"], size = legend_size, colour = legend_colour)
  
  res <- list(rectangle1, rectangle2, scale_bar_legend)
  
  if(orientation){# Add an arrow pointing North
    coords_arrow <- create_orientation_arrow(scale_bar = the_scale_bar, length = arrow_length, distance = arrow_distance, dist_unit = dist_unit)
    arrow <- list(geom_segment(data = coords_arrow$res, aes(x = x, y = y, xend = xend, yend = yend)), annotate("text", label = "N", x = coords_arrow$coords_n[1,"x"], y = coords_arrow$coords_n[1,"y"], size = arrow_north_size, colour = "black"))
    res <- c(res, arrow)
  }
  return(res)
}
