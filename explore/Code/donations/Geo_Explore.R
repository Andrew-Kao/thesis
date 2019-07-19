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

# trump2 <- ogrListLayers('Export_Output.prj')

trump <- sf::st_read(dsn='a00000009.gdbtable', layer = 'a00000009')
trump <- trump %>%
  filter(Status == "M") %>%
  select(-Status,-Score,-Match_type,-Phone,-URL,-AddNumFrom,-AddNumTo,-AddRange,-StPreDir,-StPreType,-StDir,-BldgType,
         -BldgName,-LevelType,-LevelName,-UnitType,-UnitName,-SubAddr,-Block,-Sector,-Nbrhd,-District,-Territory,-Zone,
         -LangCode,-ARC_Address2,-ARC_Address3,-ARC_Neighborhood,-ARC_Subregion,-ARC_PostalExt)
trump <- trump %>%
  filter(state2 == "MA")
trump <- trump %>%
  as_Spatial(trump)
## convert CRS


## need to join back to Trump donation data; so collapse that on the address level
## need to coerce state level check?

contours <- contours[contours$St == "MA",]

oneContour <- contours[1,]

tdistance <- pointDistance(trumpXY, line)

#### PROCESS
# just Spanish language contours
# Match state to state; maybe coerce trump data too
# use sp::over to find ones that are within
  # how do we get the distance for these?
# use geosphere:: pointDistance to find ones that are without

trumpover <- over(test,oneContour)
## need to coerce Trump into XY


plot(trump)
plot(contours)


