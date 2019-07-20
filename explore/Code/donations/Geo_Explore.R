### ELECTION EXPLORE ###

# requires contours to already exist from Read_Shape

library(raster)
library(rgdal)
library(nstauffer)
library(dplyr)
library(geosphere)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

# WGS84
trump <- sf::st_read(dsn='a00000009.gdbtable', layer = 'a00000009')
trump <- trump %>%
  filter(Status == "M") %>%
  select(-Status,-Score,-Match_type,-Phone,-URL,-AddNumFrom,-AddNumTo,-AddRange,-StPreDir,-StPreType,-StDir,-BldgType,
         -BldgName,-LevelType,-LevelName,-UnitType,-UnitName,-SubAddr,-Block,-Sector,-Nbrhd,-District,-Territory,-Zone,
         -LangCode,-ARC_Address2,-ARC_Address3,-ARC_Neighborhood,-ARC_Subregion,-ARC_PostalExt)
trump <- trump %>%
  filter(state2 == "MA")
trump <- as_Spatial(trump)
## convert CRS


## need to join back to Trump donation data; so collapse that on the address level
## need to coerce state level check?

##### contours from Read_Shape
# CRS: NAD83 to WGS84
contours <- contours[contours$St == "MA",]
newcrs <- CRS("+proj=longlat +datum=WGS84 +no_defs")
contoursT <- spTransform(contours, newcrs)

# Transform into SpatialPolyDF and SpatialPoly (spg)
sf_contours <- st_as_sf(contoursT)
sf_contours_poly <- st_cast(sf_contours,'POLYGON')
spdf_contours <- as_Spatial(sf_contours_poly)
sp_contours <- as(st_geometry(sf_contours_poly),"Spatial")

# in testing before state coercion, 94/10697 fall outside of any boundaries (this is 0.85% failure rate max)
trumpover <- over(trump,sp_contours)

tdistance <- dist2Line(trump, sp_contours)

#### PROCESS
# just Spanish language contours
# Match state to state; maybe coerce trump data too
# use sp::over to find ones that are within
  # how do we get the distance for these?
# use geosphere:: pointDistance to find ones that are without

## need to coerce Trump into XY?

smallTrump <- trump[1:100,]

plot(oneContourT)
plot(smallTrump, add=TRUE)
graphics.off()
plot(smallTrump)
plot(oneContourT, add= TRUE)
