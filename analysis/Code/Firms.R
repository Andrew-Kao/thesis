### Business Regressions ###

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(stargazer)
library(stringr)
library(purrr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)


## go back to the trump data and recheck this works
busn <- readRDS(file='BusnAll.Rdata')

## approach 1: point pattern 
busn <-spTransform(busn, CRS("+proj=longlat +datum=NAD83"))

### some summary stats
busnData <- busn@data %>%
  dplyr::select(hispName, hispFoodName, hispNameD)
stargazer(busnData, out="../../../Output/Summary/FirmNames.tex", title="Firm Names",
          summary = TRUE, font.size = 'scriptsize')

instrument <- readRDS("../../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
counties@data <- counties@data %>%
  mutate(stateCounty = paste0(STATE,COUNTY))

### need a spatial level county dataset
instrument <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(COUNTY = origcounty, STATE = origstate) %>%
  mutate(STATE = str_pad(STATE,3,side="right","0"), COUNTY = str_pad(COUNTY,4,side="right","0"),
         stateCounty = paste0(STATE,COUNTY)) %>%
  filter(!is.na(COUNTY) & !is.na(STATE))

contours1 <- readRDS('../../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
busn_project <- spTransform(busn, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourBusnDist <- gDistance(contours_project,busn_project, byid = TRUE)
contourBusnMinDist <- apply(contourBusnDist,1,FUN=min)  # 428 counties that intersect!
saveRDS(contourBusnMinDist,'contourBusnMinDist.Rdata')
stargazer(matrix(contourBusnMinDist,ncol=1), out="../../../Output/Summary/ContourBusnMinDist.tex", title="Contour-Firm Minimum Distances", summary = TRUE)


contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourBusnIntersect <- gIntersects(contours_poly,busn_project, byid = TRUE)
contourBusnIntersect[contourBusnIntersect == "FALSE"] <- 0
contourBusnIntersect[contourBusnIntersect == "TRUE"] <- 1
contourBusnInterAll <- apply(contourBusnIntersect,1,FUN=sum)  # 590 counties inside/intersecting!
saveRDS(contourBusnInterAll,'contourBusnInterAll.Rdata')
stargazer(matrix(contourBusnInterAll,ncol=1), out="../../../Output/Summary/ContourBusnInterAll.tex", title="Firms Within Contours", summary = TRUE)

busn@data <- busn@data %>%
  mutate(minDist = contourBusnMinDist, inside = contourBusnInterAll)

### merge in county level data
busnCountyLink <- over(busn,counties,returnList = FALSE)
busn@data <- busn@data %>%
  mutate(stateCounty = busnCountyLink$stateCounty)

### TODO: fix the pdens and hs and college

busn2 <- merge(busn, instrument, by = 'stateCounty', all.x = TRUE)
saveRDS(busn2, 'BusnReadyRaster.Rdata')

#### raster
# florida bounds: https://openmaptiles.com/downloads/north-america/us/florida/
r <- raster( xmn =-88.47, xmx=-24.2,ymn=-79.4,ymx=31.0,crs= "+proj=longlat +datum=NAD83",
             nrow = 100, ncol = 200)
rBusnCount <- rasterize(busn, r, field=busn$busnCount,fun=sum)
values(rBusnCount) <- ifelse(is.na(values(rBusnCount)),0,values(rBusnCount))
rHispName <- rasterize(busn, r, field=busn$hispName,fun=sum)
values(rHispName) <- ifelse(is.na(values(rHispName)),0,values(rHispName))
rHispFoodName <- rasterize(busn, r, field=busn$hispFoodName,fun=sum)
values(rHispFoodName) <- ifelse(is.na(values(rHispFoodName)),0,values(rHispFoodName))
rHispNameD <- rasterize(busn, r, field=busn$hispNameD,fun=sum)
values(rHispNameD) <- ifelse(is.na(values(rHispNameD)),0,values(rHispNameD))


#### COUNTIES
rgdf <- as(rBusnCount,'SpatialGridDataFrame')
testLink <- over(rgdf,counties,returnList=FALSE)
rgdf@data <- rgdf@data %>%
  mutate(stateCounty = testLink$stateCounty)
rgdf2 <- merge(rgdf, instrument, by = 'stateCounty', all.x = TRUE)

rPCHisp <- raster(rgdf2,layer=14)
rPop <- raster(rgdf2,layer=15)
rIncome <- raster(rgdf2,layer=17)

#### INSTRUMENT
rspdf <- as(rBusnCount,'SpatialPointsDataFrame')
rspdf <- spTransform(rspdf, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
contourDist <- gDistance(contours_project,rspdf, byid = TRUE)
contourMinDist <- apply(contourDist,1,FUN=min)

contourIntersect <- gIntersects(contours_poly,rspdf, byid = TRUE)
contourIntersect[contourIntersect == "FALSE"] <- 0
contourIntersect[contourIntersect == "TRUE"] <- 1
contourInterAll <- apply(contourIntersect,1,FUN=sum)  # 590 counties inside/intersecting!

rspdf@data <- rspdf@data %>%
  mutate(minDist = contourMinDist, inside = contourInterAll)
rspdf <- spTransform(rspdf, CRS("+proj=longlat +datum=NAD83"))

rMinDist <- rasterize(rspdf, r, field=rspdf$minDist,fun=mean)
rIntersect <- rasterize(rspdf, r, field=rspdf$inside,fun=mean)

# stack
s <- stack(rBusnCount, rHispName, rHispFoodName, rHispNameD, rPop, rPCHisp, rIntersect, rMinDist, rIncome)
regDataF <- data.frame(na.omit(values(s)))
names(regDataF) <- c('busnCount', 'hispName', 'hispFoodName', 'hispNameD', 'population', 'pcHispanic', 'intersects', 'distance',
                     'income')

regF2 <- regDataF %>%
  mutate(logPop = log(population), # ceiling(dummy)
         distance = distance/1000, dist2 = distance^2) %>%
  filter(distance < 100)

# only 27 obs, won't fly
m1 <- lm(busnCount ~ intersects*distance + intersects*dist2  , data=regF2)
m2 <- lm(busnCount ~ intersects*distance + intersects*dist2 + logPop, data=regF2) 
m3 <- lm(busnCount ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2)
m4 <- lm(busnCount ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rastern2.tex", title="Effect of TV on Hispanic Owned Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')


regF2 <- busn2@data %>%
  mutate(logPop = log(origpopulation), # ceiling(dummy)
         distance = minDist/1000, dist2 = minDist^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1,0),
         hispNameD = ifelse(hispName > 0, 1,0),) %>%
  filter(distance < 100)

m1 <- lm(busnCount ~ inside*distance + inside*dist2  , data=regF2)
m2 <- lm(busnCount ~ inside*distance + inside*dist2 + logPop, data=regF2) 
m3 <- lm(busnCount ~ inside*distance + inside*dist2 + logPop + origpcHisp, data=regF2)
m4 <- lm(busnCount ~ inside*distance + inside*dist2 + logPop + origpcHisp + origincome, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_n2.tex", title="Effect of TV on Hispanic Owned Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')

m1 <- lm(hispFoodName ~ inside*distance + inside*dist2  , data=regF2)
m2 <- lm(hispFoodName ~ inside*distance + inside*dist2 + logPop, data=regF2) 
m3 <- lm(hispFoodName ~ inside*distance + inside*dist2 + logPop + origpcHisp, data=regF2)
m4 <- lm(hispFoodName ~ inside*distance + inside*dist2 + logPop + origpcHisp + origincome, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_fname2.tex", title="Effect of TV on Hispanic Name Businesses (Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- glm(hispFoodNameD ~ inside*distance + inside*dist2  , data=regF2, family = binomial)
m2 <- glm(hispFoodNameD ~ inside*distance + inside*dist2 + logPop, data=regF2, family = binomial) 
m3 <- glm(hispFoodNameD ~ inside*distance + inside*dist2 + logPop + origpcHisp, data=regF2, family = binomial)
m4 <- glm(hispFoodNameD ~ inside*distance + inside*dist2 + logPop + origpcHisp + origincome, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_fname2_bin.tex", title="Effect of TV on Hispanic Name Businesses (Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- glm(hispNameD ~ inside*distance + inside*dist2  , data=regF2, family = binomial)
m2 <- glm(hispNameD ~ inside*distance + inside*dist2 + logPop, data=regF2, family = binomial) 
m3 <- glm(hispNameD ~ inside*distance + inside*dist2 + logPop + origpcHisp, data=regF2, family = binomial)
m4 <- glm(hispNameD ~ inside*distance + inside*dist2 + logPop + origpcHisp + origincome, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_name2_bin.tex", title="Effect of TV on Hispanic Name Businesses (No Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')



