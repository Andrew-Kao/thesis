### ELECTION EXPLORE ###

# requires contours to already exist from Read_Shape

library(raster)
library(rgdal)
library(nstauffer)
library(dplyr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}


trump <- sf::st_read(dsn='a00000009.gdbtable', layer = 'a00000009')
trump <- trump %>%
  filter(Status == "M") %>%
  select(-Status,-Score,-Match_type,-Phone,-URL,-AddNumFrom,-AddNumTo,-AddRange,-StPreDir,-StPreType,-StDir,-BldgType,
         -BldgName,-LevelType,-LevelName,-UnitType,-UnitName,-SubAddr,-Block,-Sector,-Nbrhd,-District,-Territory,-Zone,
         -LangCode,-ARC_Address2,-ARC_Address3,-ARC_Neighborhood,-ARC_Subregion,-ARC_PostalExt)
trump <- trump %>%
  filter(state2 == "MA")

## need to join back to Trump donation data; so collapse that on the address level
## need to coerce state level check?

contours <- contours[contours$St == "MA",]

plot(trump)
plot(contours)


