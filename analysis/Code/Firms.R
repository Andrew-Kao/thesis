### Business Regressions ###

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(stargazer)
library(stringr)
library(purrr)
library(survival)
library(clusterSEs)

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

busn <- readRDS('BusnReadyRaster.Rdata')
#### raster
# florida bounds: https://openmaptiles.com/downloads/north-america/us/florida/
r <- raster( xmn =-87, xmx=-80,ymn=29,ymx=31.1,crs= "+proj=longlat +datum=NAD83",
             resolution = c(.01,.01)) #baseline, c(.02,.02)
r2 <- raster( xmn =-83.4, xmx=-80,ymn=24.3,ymx=29,crs= "+proj=longlat +datum=NAD83",
             resolution = c(.01,.01))
r <- merge(r,r2, tolerance = .1)
rBusnCount <- rasterize(busn, r, field=busn$busnCount,fun=sum)
values(rBusnCount) <- ifelse(is.na(values(rBusnCount)),0,values(rBusnCount))
rHispSum <- rasterize(busn, r, field=busn$hisp_sum,fun=sum)
values(rHispSum) <- ifelse(is.na(values(rHispSum)),0,values(rHispSum))
rHispMajSum <- rasterize(busn, r, field=busn$hispMaj_sum,fun=sum)
values(rHispMajSum) <- ifelse(is.na(values(rHispMajSum)),0,values(rHispMajSum))
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
rgdf2 <- raster::merge(rgdf, instrument, by = 'stateCounty', all.x = TRUE)

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
s <- stack(rBusnCount, rHispName, rHispSum, rHispMajSum, rHispFoodName, rHispNameD,
           rPop, rPCHisp, rIntersect, rMinDist, rIncome)
regDataF <- data.frame(na.omit(values(s)))
names(regDataF) <- c('busnCount', 'hispName', 'hispSum', 'hispMajSum',
                     'hispFoodName', 'hispNameD', 'population', 'pcHispanic', 'intersects', 'distance',
                     'income')
saveRDS(regDataF, 'FirmStackDF01.Rdata')


##### REGRESSIONS #####
regDataF <- readRDS('FirmStackDF.Rdata')
busn2 <- readRDS('BusnReadyRaster.Rdata')



regF2 <- regDataF %>%
  mutate(logPop = log(population), # ceiling(dummy)
         distance = distance/1000, dist2 = distance^2,
         hispFoodNameD = ifelse(hispFoodName * hispSum > 0, 1, 0),
         busn = busnCount * hispSum,
         busnD = busnCount * hispMajSum) %>%
  filter(distance < 100)

m1 <- lm(busn ~ intersects*distance + intersects*dist2  , data=regF2)
m2 <- lm(busn ~ intersects*distance + intersects*dist2 + logPop, data=regF2) 
m3 <- lm(busn ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2)
m4 <- lm(busn ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rastern2.tex", title="Effect of TV on Hispanic Owned Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance','intersects:dist2'))
m1 <- lm(ihs(busn) ~ intersects*distance + intersects*dist2  , data=regF2)
m2 <- lm(ihs(busn) ~ intersects*distance + intersects*dist2 + logPop, data=regF2) 
m3 <- lm(ihs(busn) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2)
m4 <- lm(ihs(busn) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2)
m5 <- lm(ihs(busn) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income + busnCount, data=regF2)
stargazer(m1,m2,m3,m4, m5, out = "../../../Output/Regs/firms_rastern2_ihs.tex", title="Effect of TV on IHS Hispanic Owned Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- lm(ihs(busnD) ~ intersects*distance + intersects*dist2  , data=regF2)
m2 <- lm(ihs(busnD) ~ intersects*distance + intersects*dist2 + logPop, data=regF2) 
m3 <- lm(ihs(busnD) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2)
m4 <- lm(ihs(busnD) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rastern2_dihs.tex", title="Effect of TV on IHS Hispanic Owned Businesses (50\\% threshold), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- lm(ihs(hispFoodName) ~ intersects*distance + intersects*dist2  , data=regF2)
m2 <- lm(ihs(hispFoodName) ~ intersects*distance + intersects*dist2 + logPop, data=regF2) 
m3 <- lm(ihs(hispFoodName) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2)
m4 <- lm(ihs(hispFoodName) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rasterfname2_ihs.tex", title="Effect of TV on IHS Hispanic Name Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2  , data=regF2, family = binomial)
m2 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2 + logPop, data=regF2, family = binomial) 
m3 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2, family = binomial)
m4 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rasterfname2_bin.tex", title="Effect of TV on Binomial Hispanic Name Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')

regF2 <- regDataF %>%
  mutate(logPop = log(population), # ceiling(dummy)
         distance = distance/1000, dist2 = distance^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1, 0)) %>%
  filter(distance < 50)
m1 <- lm(ihs(busnCount) ~ intersects*distance + intersects*dist2  , data=regF2)
m2 <- lm(ihs(busnCount) ~ intersects*distance + intersects*dist2 + logPop, data=regF2) 
m3 <- lm(ihs(busnCount) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2)
m4 <- lm(ihs(busnCount) ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rastern2_ihs_50.tex", title="Effect of TV on IHS Hispanic Owned Businesses, 50 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2  , data=regF2, family = binomial)
m2 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2 + logPop, data=regF2, family = binomial) 
m3 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2 + logPop + pcHispanic, data=regF2, family = binomial)
m4 <- glm(hispFoodNameD ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rasterfname2_bin_50.tex", title="Effect of TV on Binomial Hispanic Name Businesses, 50 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')


regF2 <- busn2@data %>%
  mutate(logPop = log(origpopulation), # ceiling(dummy)
         distance = minDist/1000, dist2 = minDist^2,
         hispFoodNameD = ifelse(hispFoodName * hisp_sum > 0, 1,0),
         hispNameD = ifelse(hispName * hisp_sum > 0, 1,0)) %>%
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



regF2 <- busn2@data %>%
  mutate(logPop = log(origpopulation), # ceiling(dummy)
         distance = minDist/1000, dist2 = minDist^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1,0),
         hispNameD = ifelse(hispName > 0, 1,0),
         nhispFoodNameD = ifelse(hispFoodName * (busnCount-hisp_sum) > 0, 1,0)) %>%
  filter(distance < 100 & hispMaj_sum > 0 )

m1 <- glm(hispFoodNameD ~ inside*distance  , data=regF2, family = binomial)
m2 <- glm(hispFoodNameD ~ inside*distance + logPop, data=regF2, family = binomial) 
m3 <- glm(hispFoodNameD ~ inside*distance + logPop + origpcHisp, data=regF2, family = binomial)
m4 <- glm(hispFoodNameD ~ inside*distance + logPop + origpcHisp + origincome, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_fname_bin.tex", title="Effect of TV on Hispanic Name Businesses (Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- glm(hispNameD ~ inside*distance  , data=regF2, family = binomial)
m2 <- glm(hispNameD ~ inside*distance + logPop, data=regF2, family = binomial) 
m3 <- glm(hispNameD ~ inside*distance + logPop + origpcHisp, data=regF2, family = binomial)
m4 <- glm(hispNameD ~ inside*distance + logPop + origpcHisp + origincome, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_name_bin.tex", title="Effect of TV on Hispanic Name Businesses (Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')

regF2 <- busn2@data %>%
  mutate(logPop = log(origpopulation), # ceiling(dummy)
         distance = minDist/1000, dist2 = minDist^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1,0),
         hispNameD = ifelse(hispName > 0, 1,0),
         nhispFoodNameD = ifelse(hispFoodName * (busnCount-hisp_sum) > 0, 1,0)) %>%
  filter(distance < 100 & hispMaj_sum == 0 )

m1 <- glm(hispFoodNameD ~ inside*distance  , data=regF2, family = binomial)
m2 <- glm(hispFoodNameD ~ inside*distance + logPop, data=regF2, family = binomial) 
m3 <- glm(hispFoodNameD ~ inside*distance + logPop + origpcHisp, data=regF2, family = binomial)
m4 <- glm(hispFoodNameD ~ inside*distance + logPop + origpcHisp + origincome, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_fnamenh_bin.tex", title="Effect of TV on Hispanic Name Businesses (Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')
m1 <- glm(hispNameD ~ inside*distance  , data=regF2, family = binomial)
m2 <- glm(hispNameD ~ inside*distance + logPop, data=regF2, family = binomial) 
m3 <- glm(hispNameD ~ inside*distance + logPop + origpcHisp, data=regF2, family = binomial)
m4 <- glm(hispNameD ~ inside*distance + logPop + origpcHisp + origincome, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_namenh_bin.tex", title="Effect of TV on Hispanic Name Businesses (Food), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt')

### MAIN ###

label_spec3 <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 'Log(Population)','County \\% Hispanic','Log(Income)')

regF2 <- regDataF %>%
  mutate(logPop = log(population), # ceiling(dummy)
         distance = distance/1000, dist2 = distance^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1, 0),
         hhispFoodNameD = ifelse(hispFoodName * hispMajSum > 0, 1, 0),
         nhispFoodNameD = ifelse(hispFoodName * (busnCount - hispMajSum) > 0, 1, 0),
         busn = busnCount * hispSum,
         busnD = busnCount * hispMajSum,
         income = log(income)) %>%
  filter(distance < 100)


m1 <- lm(ihs(busnD) ~ intersects*distance  , data=regF2)
m2 <- lm(ihs(busnD) ~ intersects*distance + logPop, data=regF2) 
m3 <- lm(ihs(busnD) ~ intersects*distance + logPop + pcHispanic, data=regF2)
m4 <- lm(ihs(busnD) ~ intersects*distance + logPop + pcHispanic + income, data=regF2)
stargazer(m1,m2,m3,m4, out = "../../../Output/Regs/firms_rastern_ihs.tex", title="Effect of TV on IHS(\\# Hispanic Owned Businesses), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance','distance'),
          covariate.labels = label_spec3,
          dep.var.labels = 'IHS(\\# Hispanic Owned Businesses)',
          omit = c('Constant'))
m1 <- glm(hispFoodNameD ~ intersects*distance  , data=regF2, family = binomial)
m2 <- glm(hispFoodNameD ~ intersects*distance + logPop, data=regF2, family = binomial) 
m3 <- glm(hispFoodNameD ~ intersects*distance + logPop + pcHispanic, data=regF2, family = binomial)
m4 <- glm(hispFoodNameD ~ intersects*distance + logPop + pcHispanic + income, data=regF2, family = binomial)
m5 <- glm(hhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income, data=regF2, family = binomial)
m6 <- glm(nhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income, data=regF2, family = binomial)
stargazer(m1,m2,m3,m4,m5,m6, out = "../../../Output/Regs/firms_rasterfname_bin.tex", title="Effect of TV on Binomial Hispanic Name Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance','distance'),
          covariate.labels = label_spec3,
          dep.var.labels = 'IHS(\\# Hispanic Owned Businesses)',
          omit = c('Constant'))

regF2 <- regDataF %>%
  mutate(logPop = log(population), 
         distance = distance/1000, dist2 = distance^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1, 0),
         hhispFoodNameD = ifelse(hispFoodName * hispMajSum > 0, 1, 0),
         nhispFoodNameD = ifelse(hispFoodName * (busnCount - hispMajSum) > 0, 1, 0),
         hhispNameD = ifelse(hispNameD * hispMajSum > 0, 1, 0),
         busn = busnCount * hispSum,
         busnD = busnCount * hispMajSum,
         income = log(income)) %>%
  filter(distance < 100)

m1 <- glm(hhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income,
          data=regF2, family = binomial)
m2 <- glm(hhispFoodNameD ~ intersects*distance + intersects*dist2 + logPop + pcHispanic + income,
          data=regF2, family = binomial)
# no cluster bc all in same state
m3 <- glm(hhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income + busnCount,
          data=regF2, family = binomial)
m4 <- glm(hhispNameD ~ intersects*distance + logPop + pcHispanic + income + busnCount,
          data=regF2, family = binomial)

regF2 <- regF2 %>%
  filter(distance < 50)

m5 <- glm(hhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income,
          data=regF2, family = binomial)

regF2 <- regF2 %>%
  filter(distance < 25)

m6 <- glm(hhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income,
          data=regF2, family = binomial)

regDataF <- readRDS('FirmStackDF.Rdata')
regF2 <- regDataF %>%
  mutate(logPop = log(population), 
         distance = distance/1000, dist2 = distance^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1, 0),
         hhispFoodNameD = ifelse(hispFoodName * hispMajSum > 0, 1, 0),
         nhispFoodNameD = ifelse(hispFoodName * (busnCount - hispMajSum) > 0, 1, 0),
         hhispNameD = ifelse(hispNameD * hispMajSum > 0, 1, 0),
         busn = busnCount * hispSum,
         busnD = busnCount * hispMajSum,
         income = log(income)) %>%
  filter(distance < 100)
m7 <- glm(hhispFoodNameD ~ intersects*distance + logPop + pcHispanic + income,
          data=regF2, family = binomial)

stargazer(m1,m2,m3,m4,m5,m6,m7, out = "../../../Output/Regs/firms_rasterfname_bin_robust.tex", title="Effect of TV on Binomial Hispanic Name Businesses, 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          # order = c('intersects','intersects:distance','distance'),
          # covariate.labels = label_spec3,
          # dep.var.labels = 'IHS(\\# Hispanic Owned Businesses)',
          omit = c('Constant'))


# Different Grid Sizes



regDataFA <- data.frame(values(s)) %>%
  mutate(x = coordinates(r)[,1],
         y = coordinates(r)[,2])
regDataFA <- na.omit(regDataFA)
names(regDataFA) <- c('busnCount', 'hispName', 'hispSum', 'hispMajSum',
                     'hispFoodName', 'hispNameD', 'population', 'pcHispanic', 'intersects', 'distance',
                     'income','X','Y')

regF2 <- regDataFA %>%
  mutate(logPop = log(population), 
         distance = distance/1000, dist2 = distance^2,
         hispFoodNameD = ifelse(hispFoodName > 0, 1, 0),
         hhispFoodNameD = ifelse(hispFoodName * hispMajSum > 0, 1, 0),
         nhispFoodNameD = ifelse(hispFoodName * (busnCount - hispMajSum) > 0, 1, 0),
         busn = busnCount * hispSum,
         busnD = busnCount * hispMajSum,
         income = log(income)) %>%
  filter(distance < 100)

m1 <- lm(ihs(busnD) ~ intersects*distance + logPop, data=regF2)
coordinates(regF2) <- c('X','Y')
nb4<-knearneigh(coordinates(regF2), k=4, longlat = TRUE)
nb4 <- knn2nb(nb4)
wt4<-nb2listw(nb4, style="W")

lm.morantest(m1,listw=wt4, zero.policy = TRUE)
m1.lag <- lagsarlm(ihs(busnD) ~ intersects*distance + logPop, data=regF2,
                   listw=wt4, type="lag",method="MC")
summary(m1.lag, Nagelkerke=T)
m1.err <- errorsarlm(ihs(busnD) ~ intersects*distance + logPop, data=regF2,
                     listw=wt4, etype="error",method="MC")
summary(m1.err, Nagelkerke=T)

stargazer(m1,m1.lag,m1.err, out = "../../../Output/Regs/firms_rastern_ihs_spatial.tex", title="Effect of TV on IHS(\\# Hispanic Owned Businesses), 100 KM Radius",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects'),
          covariate.labels = label_spec3,
          dep.var.labels = 'IHS(\\# Hispanic Owned Businesses)',
          omit = c('Constant', 'intersects:distance', 'logPop', 'distance'))
# why this doesn't recognize object now? packages? can manual...



ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}

