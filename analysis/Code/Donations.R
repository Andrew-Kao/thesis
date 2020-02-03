### Donor Regressions ###

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(stargazer)
library(stringr)
library(purrr)


###### TRUMP ######
if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

## go back to the trump data and recheck this works
trump <- readRDS(file='TrumpAll.Rdata')

## approach 1: point pattern 
trump <-spTransform(trump, CRS("+proj=longlat +datum=NAD83"))

### some summary stats
trumpData <- trump@data %>%
  select(hisp_sum, non_hisp_sum, race, donationCount)
stargazer(trumpData, out="../../../Output/Summary/TrumpDonations.tex", title="Trump Donors",
          summary = TRUE, font.size = 'scriptsize')

### keep only in lower 48?

strump <- trump[1:1000,]

## need to match up data to counties and merge

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

### oh god distances to contours now
contours1 <- readRDS('../../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
trump_project <- spTransform(trump, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourTrumpDist <- gDistance(contours_project,trump_project, byid = TRUE)
contourTrumpMinDist <- apply(contourTrumpDist,1,FUN=min)  # 428 counties that intersect!
saveRDS(contourTrumpMinDist,'contourTrumpMinDist.Rdata')
stargazer(matrix(contourTrumpMinDist,ncol=1), out="../../../Output/Summary/ContourTrumpMinDist.tex", title="Contour-Donation Minimum Distances", summary = TRUE)

contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourTrumpIntersect <- gIntersects(contours_poly,trump_project, byid = TRUE)
contourTrumpIntersect[contourTrumpIntersect == "FALSE"] <- 0
contourTrumpIntersect[contourTrumpIntersect == "TRUE"] <- 1
contourTrumpInterAll <- apply(contourTrumpIntersect,1,FUN=sum)  # 590 counties inside/intersecting!
saveRDS(contourTrumpInterAll,'contourTrumpInterAll.Rdata')
stargazer(matrix(contourTrumpInterAll,ncol=1), out="../../../Output/Summary/ContourTrumpInterAll.tex", title="Donations Within Contours", summary = TRUE)

contourTrumpMinDist <- readRDS('contourTrumpMinDist.Rdata')
contourTrumpInterAll <- readRDS('contourTrumpInterAll.Rdata')

trump@data <- trump@data %>%
  mutate(minDist = contourTrumpMinDist, inside = contourTrumpInterAll)

### merge in county level data
trumpCountyLink <- over(trump,counties,returnList = FALSE)
trump@data <- trump@data %>%
  mutate(stateCounty = trumpCountyLink$stateCounty)

### TODO: fix the pdens and hs and college
 
trump2 <- merge(trump, instrument, by = 'stateCounty', all.x = TRUE)
saveRDS(trump2, 'TrumpReadyRaster.Rdata')

###### CLINTON ######
if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/clinton_donations') 
}

clinton <- readRDS(file='ClintonAll.Rdata')

## approach 1: point pattern 
clinton <-spTransform(clinton, CRS("+proj=longlat +datum=NAD83"))

### some summary stats
clintonData <- clinton@data %>%
  dplyr::select(hisp_sum, hispOne_sum,hispMaj_sum, non_hisp_sum, race, donationCount)
stargazer(clintonData, out="../../../Output/Summary/ClintonDonations.tex", title="Clinton Donors",
          summary = TRUE, font.size = 'scriptsize')

## need to match up data to counties and merge

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

### oh god distances to contours now
contours1 <- readRDS('../../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
clinton_project <- spTransform(clinton, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourClintonDist <- gDistance(contours_project,clinton_project, byid = TRUE)
contourClintonMinDist <- apply(contourClintonDist,1,FUN=min)  # 428 counties that intersect!
saveRDS(contourClintonMinDist,'contourClintonMinDist.Rdata')
stargazer(matrix(contourClintonMinDist,ncol=1), out="../../../Output/Summary/ContourClintonMinDist.tex", title="Contour-Clinton Donation Minimum Distances", summary = TRUE)

contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourClintonIntersect <- gIntersects(contours_poly,clinton_project, byid = TRUE)
contourClintonIntersect[contourClintonIntersect == "FALSE"] <- 0
contourClintonIntersect[contourClintonIntersect == "TRUE"] <- 1
contourClintonInterAll <- apply(contourClintonIntersect,1,FUN=sum)  # 590 counties inside/intersecting!
saveRDS(contourClintonInterAll,'contourClintonInterAll.Rdata')
stargazer(matrix(contourClintonInterAll,ncol=1), out="../../../Output/Summary/ContourClintonInterAll.tex", title="Clinton Donations Within Contours", summary = TRUE)

contourClintonMinDist <- readRDS('contourClintonMinDist.Rdata')
contourClintonInterAll <- readRDS('contourClintonInterAll.Rdata')

clinton@data <- clinton@data %>%
  mutate(minDist = contourClintonMinDist, inside = contourClintonInterAll)

### merge in county level data
clintonCountyLink <- over(clinton,counties,returnList = FALSE)
clinton@data <- clinton@data %>%
  mutate(stateCounty = clintonCountyLink$stateCounty)

### TODO: fix the pdens and hs and college

clinton2 <- merge(clinton, instrument, by = 'stateCounty', all.x = TRUE)
saveRDS(clinton2, 'ClintonReadyRaster.Rdata')

####### REGRESSIONS #########
### hand rasterize
#### determine boundaries of trump data :: source jsundram/cull.py
clinton2 <- readRDS('ClintonReadyRaster.Rdata')
r <- raster( xmn =-124.784, xmx=-66.951,ymn=24.743,ymx=49.346,crs= "+proj=longlat +datum=NAD83",
             nrow = 100, ncol = 200)
rDonCount <- rasterize(clinton2, r, field=clinton2$donationCount,fun=sum)
values(rDonCount) <- ifelse(is.na(values(rDonCount)),0,values(rDonCount))
rHispSum <- rasterize(clinton2, r, field=clinton2$hisp_sum,fun=mean)
values(rHispSum) <- ifelse(is.na(values(rHispSum)),0,values(rHispSum))
rRace <- rasterize(clinton2, r, field=clinton2$race,fun=mean)
values(rRace) <- ifelse(is.na(values(rRace)),0,values(rRace))
rPop <- rasterize(clinton2, r, field=clinton2$origpopulation,fun=mean)
rPCHisp <- rasterize(clinton2, r, field=clinton2$origpcHisp,fun=mean)
rIntersect <- rasterize(clinton2, r, field=clinton2$inside,fun=mean)
rMinDist <- rasterize(clinton2, r, field=clinton2$minDist,fun=mean)
rIncome <- rasterize(clinton2, r, field=clinton2$origincome,fun=mean)

test <- rPop

plot(r)
## TODO: figure out how to change size of plots

### run regressions with point pattern
s <- stack(rDonCount, rHispSum, rRace, rPop, rPCHisp, rIntersect, rMinDist, rIncome)
regData <- data.frame(na.omit(values(s)))
names(regData) <- c('rawDonations', 'hispanicSum', 'hispanicDummy' , 'population', 'pcHispanic', 'intersects', 'distance',
                    'income')

# no restrictions
reg1 <- regData %>%
  mutate(donations = rawDonations*hispanicSum)

m1 <- lm(donations ~ intersects + distance + population , data=reg1)

# 100 km distance
reg2 <- regData %>%
  mutate(donations = rawDonations*hispanicSum, logPop = log(population),
         donations_d = rawDonations * hispanicDummy,
         dist2 = distance^2) %>%
  filter(distance < 100000)


m1 <- lm(donations ~ intersects*distance + intersects*dist2  , data=reg2)
m2 <- lm(donations ~ intersects*distance + intersects*dist2 + logPop, data=reg2) 
m3 <- lm(donations ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=reg2)
m4 <- lm(donations ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=reg2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/clinton_b2.tex", title="Effect of TV on Hispanic Donations to Clinton, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- lm(donations ~ intersects*distance  , data=reg2)
m2 <- lm(donations ~ intersects*distance + logPop, data=reg2) 
m3 <- lm(donations ~ intersects*distance + logPop + pcHispanic, data=reg2)
m4 <- lm(donations ~ intersects*distance + logPop + pcHispanic + income, data=reg2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/clinton_b.tex", title="Effect of TV on Hispanic Donations to Clinton, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')

## 'd' is dummy for race instead of summing in 'b'
m1 <- lm(donations_d ~ intersects*distance + intersects*dist2  , data=reg2)
m2 <- lm(donations_d ~ intersects*distance + intersects*dist2 + logPop, data=reg2) 
m3 <- lm(donations_d ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=reg2)
m4 <- lm(donations_d ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=reg2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/clinton_d2.tex", title="Effect of TV on Hispanic Donations to Clinton, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- lm(donations_d ~ intersects*distance  , data=reg2)
m2 <- lm(donations_d ~ intersects*distance + logPop, data=reg2) 
m3 <- lm(donations_d ~ intersects*distance + logPop + pcHispanic, data=reg2)
m4 <- lm(donations_d ~ intersects*distance + logPop + pcHispanic + income, data=reg2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/clinton_d.tex", title="Effect of TV on Hispanic Donations to Clinton, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')


# 100 km distance placebo
reg2 <- regData %>%
  mutate(donations = rawDonations*(1-hispanicSum), logPop = log(population)) %>%
  filter(distance < 100000)

m1 <- lm(donations ~ intersects + distance + logPop , data=reg2)
m2 <- lm(donations ~ intersects + distance + logPop + pcHispanic, data=reg2)
m3 <- lm(donations ~ intersects + distance + logPop + pcHispanic + income, data=reg2)
stargazer(m1,m2,m3, out = "../../../Output/Regs/trump_100placebo.tex", title="Effect of TV on Hispanic Donations to Trump, 100 KM Radius Placebo")


# 25 km distance
reg3 <- regData %>%
  mutate(donations = rawDonations*hispanicSum, logPop = log(population)) %>%
  filter(distance < 25000)

m1 <- lm(donations ~ intersects + distance + logPop , data=reg3)
m2 <- lm(donations ~ intersects + distance + logPop + pcHispanic, data=reg3)
m3 <- lm(donations ~ intersects + distance + logPop + pcHispanic + income, data=reg3)
stargazer(m1,m2,m3, out = "../../../Output/Regs/trump_25.tex", title="Effect of TV on Hispanic Donations to Trump, 25 KM Radius")


## approach 2: county level spatial regression
## not preferred because too few counties



