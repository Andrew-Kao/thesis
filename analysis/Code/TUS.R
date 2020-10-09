# ATUS Regressions

library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(stargazer)
library(stringr)
library(purrr)
library(spdep)
library(spatialreg)
library(texreg)
library(tidyverse)


###### ATUS Data ######
if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/TUS') 
}

# # county data
# counties <- rgdal::readOGR("../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
# counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
# counties@data <- counties@data %>%
#   mutate(COUNTY = as.numeric(COUNTY) %% 10) %>%
#   mutate(COUNTY = str_pad(COUNTY,3,side = "left", pad = "0")) %>%
#   mutate(stateCounty = paste0(STATE,COUNTY))

# get tv county boundary
instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata") %>%
  mutate(TV = ifelse(intersects == 1 & (areaRatio > .95 | dist > 0),1,0)) %>%
  mutate(stateCounty = paste0(state, county))

# atus data and merge
atus2015 <- read.csv('2015/atus_indiv_tv.csv') %>%
  mutate(stateCounty = str_pad(county,5,side = "left", pad = "0"))

atusCounty = inner_join(atus2015, instrument, by = "stateCounty") %>%
  mutate(logPop = log(population), income = log(income)) %>%
  mutate(dist2 = dist*dist)



#### REGRESSIONS ####
m2 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop + pcHisp, data=atusCounty)
m4 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop + pcHisp + income, data=atusCounty)
stargazer(m2,m3,m4, out = "../../Output/Regs/atus_2015_ext1.tex", title="Effect of TV on Amount of TV Watched",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
                               'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = '\\# Hispanic Campaign Contributors')


m1 <- lm(duration_ext ~ TV, data=atusCounty) 
m2 <- lm(duration_ext ~ TV + logPop, data=atusCounty) 
m3 <- lm(duration_ext ~ TV + logPop + pcHisp, data=atusCounty)
m4 <- lm(duration_ext ~ TV + logPop + pcHisp + income, data=atusCounty)
stargazer(m1,m2,m3,m4, out = "../../Output/Regs/atus_2015_ext2.tex", title="Effect of TV on Amount of TV Watched",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance'),
          covariate.labels = c('TV Dummy','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = '\\# Hispanic Campaign Contributors')

# restrict to just hispanics!










