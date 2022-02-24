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
library(sandwich)
library(lmtest)
library(plyr)



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
  mutate(stateCounty = paste0(state, county)) %>%
  filter(dist < 100000)

# atus data and merge
atus2015 <- read.csv('2015/atus_indiv_tv.csv') %>%
  mutate(stateCounty = str_pad(county,5,side = "left", pad = "0"))

atus2015 <- read.csv('atus_indiv_tv_all.csv') %>%
  mutate(stateCounty = str_pad(county,5,side = "left", pad = "0"))


atusCounty = inner_join(atus2015, instrument, by = "stateCounty") %>%
  mutate(logPop = log(population), income = log(income)) %>%
  mutate(dist2 = dist*dist, hispanic_d = if_else(hispan != "Not Hispanic",1,0),
         age2 = age*age,
         foreign = if_else(citizen == "Foreign born, not a U.S. citizen" | citizen == "Foreign born, U.S. citizen by naturalization",
                           1, 0))



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
          dep.var.labels = 'Minutes TV watched')


m1 <- lm(duration_ext ~ TV, data=atusCounty) 
m2 <- lm(duration_ext ~ TV + logPop, data=atusCounty) 
m3 <- lm(duration_ext ~ TV + logPop + pcHisp, data=atusCounty)
m4 <- lm(duration_ext ~ TV + logPop + pcHisp + income, data=atusCounty)
stargazer(m1,m2,m3,m4, out = "../../Output/Regs/atus_2015_ext2.tex", title="Effect of TV on Amount of TV Watched",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance'),
          covariate.labels = c('TV Dummy','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched')

# restrict to just hispanics! and use person weights!

m2 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop, data=atusCounty, subset = hispan != "Not Hispanic") 
m3 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop + pcHisp, data=atusCounty, subset = hispan != "Not Hispanic")
m4 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop + pcHisp + income, data=atusCounty, subset = hispan != "Not Hispanic")
m5 <- lm(duration_ext ~ TV*dist + TV*dist2 + logPop + pcHisp + income, data=atusCounty, subset = hispan != "Not Hispanic",
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext3.tex", title="Effect of TV on Amount of TV Watched, Hispanics",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('intersects','intersects:distance'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
                               'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")

m2 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + pcHisp, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + pcHisp + income, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + pcHisp + income, data=atusCounty,
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext4.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV:hispanic_d','intersects:distance'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
                               # 'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")

m2 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + age + sex + age2, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d + TV*dist + TV*dist2 + logPop + pcHisp + income  + age + sex + age2, data=atusCounty,
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext5.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV:hispanic_d','intersects:distance'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
          # 'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")

m2 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + age + sex + age2, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2, data=atusCounty,
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext6.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV:hispanic_d','intersects:distance'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
          # 'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")


m2 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + age + sex + age2 + foreign*hispanic_d, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty,
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext7.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV:hispanic_d','intersects:distance'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
          # 'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")



######## ATUS ALL #########
# atus data and merge
atusAll <- read.csv('atus_indiv_tv_all.csv') %>%
  mutate(stateCounty = str_pad(county,5,side = "left", pad = "0"))

atusCounty = inner_join(atusAll, instrument, by = "stateCounty") %>%
  mutate(logPop = log(population), income = log(income)) %>%
  mutate(dist2 = dist*dist, hispanic_d = if_else(hispan != "Not Hispanic",1,0),
         age2 = age*age,
         foreign = if_else(citizen == "Foreign born, not a U.S. citizen" | citizen == "Foreign born, U.S. citizen by naturalization",
                           1, 0))


m2 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + age + sex + age2 + foreign*hispanic_d, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty,
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext8.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV:hispanic_d','intersects:distance'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
          # 'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")


m2 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + age + sex + age2 + cases, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp  + age + sex + age2 + cases, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases, data=atusCounty,
         weight = wt06)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext9.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV:hispanic_d','intersects:distance'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ County Distance to Boundary ',
          # 'County Distance to Boundary (KM)','Log(Population)','County \\% Hispanic','Log(Income)'),
          omit = c('Constant','dist2'),
          dep.var.labels = 'Minutes TV watched',
          notes = "Col 4 includes person weights")


m2 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + age + sex + age2 + cases, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp  + age + sex + age2 + cases, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty)
### what distance restriction works?
# m6 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty,
         # subset = dist < 10000)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext10.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','dist$','TV:hispanic_d:dist','TV:dist','hispanic_d:dist','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy', 'County Distance to Boundary (KM)', 'TV $\\times$ Distance $\\times$ Hispanic',
                               'TV $\\times$ Distance', 'Hispanic $\\times$ Distance',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="")

m2 <- lm(duration_ext ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty) 
m3 <- lm(duration_ext ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm(duration_ext ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm(duration_ext ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
### what distance restriction works?
# m6 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty,
# subset = dist < 10000)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext11.tex", title="Effect of TV on Amount of TV Watched, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm(duration_ext ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty, subset = foreign == 1) 
m3 <- lm(duration_ext ~ TV*hispanic_d + income + pcHisp  + age + sex + age2 , data=atusCounty, subset = foreign == 1)
m4 <- lm(duration_ext ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty, subset = foreign == 1)
### what distance restriction works?
# m6 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty,
# subset = dist < 10000)
stargazer(m2,m3,m4, out = "../../Output/Regs/atus_2015_ext12.tex", title="Effect of TV on Amount of TV Watched on foreign-born, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm(duration_family ~ TV*hispanic_d + logPop + age + sex + age2 + cases, data=atusCounty) 
m3 <- lm(duration_family ~ TV*hispanic_d + logPop + pcHisp  + age + sex + age2 + cases, data=atusCounty)
m4 <- lm(duration_family ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2 + cases, data=atusCounty)
m5 <- lm(duration_family ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_extfam.tex", title="Effect of TV on Amount of TV Watched with family, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm(duration_social ~ TV*hispanic_d + logPop + age + sex + age2 + cases, data=atusCounty) 
m3 <- lm(duration_social ~ TV*hispanic_d + logPop + pcHisp  + age + sex + age2 + cases, data=atusCounty)
m4 <- lm(duration_social ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2 + cases, data=atusCounty)
m5 <- lm(duration_social ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_extsoc.tex", title="Effect of TV on Amount of TV Watched socially, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm((duration_parent) ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty) 
m3 <- lm((duration_parent) ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm((duration_parent) ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm((duration_parent) ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_extpar.tex", title="Effect of TV on Amount of TV Watched with parent, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm((duration_child) ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty) 
m3 <- lm((duration_child) ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm((duration_child) ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm((duration_child) ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_extchild.tex", title="Effect of TV on Amount of TV Watched with children, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm((duration_parent) ~ TV*hispanic_d + logPop + age + sex + age2, data=atusCounty, subset = foreign == 1) 
m3 <- lm((duration_parent) ~ TV*hispanic_d + logPop + pcHisp  + age + sex + age2, data=atusCounty, subset = foreign == 1)
m4 <- lm((duration_parent) ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty, subset = foreign == 1)
stargazer(m2,m3,m4, out = "../../Output/Regs/atus_2015_extparf.tex", title="Effect of TV on Amount of TV Watched with parent, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))


## children activities
m2 <- lm(nonedu ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty) 
m3 <- lm(nonedu ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm(nonedu ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm(nonedu ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_nonedu.tex", title="Effect of TV on Child care, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Child care',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm(edu ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty) 
m3 <- lm(edu ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm(edu ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm(edu ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_edu.tex", title="Effect of TV on Child edu, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Child edu',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

m2 <- lm(nonedu_child ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty) 
m3 <- lm(nonedu_child ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atusCounty)
m4 <- lm(nonedu_child ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atusCounty)
m5 <- lm(nonedu_child ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atusCounty)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_nonedu_child.tex", title="Effect of TV on Child care, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Child care',
          notes ="", se = makeRobust4(m2,m3,m4,m5))

atus_u19 <- atusCounty %>%
  filter(age < 19)

m2 <- lm(duration_ext ~ TV*hispanic_d + income + age + sex + age2, data=atus_u19) 
m3 <- lm(duration_ext ~ TV*hispanic_d + income + pcHisp  + age + sex + age2, data=atus_u19)
m4 <- lm(duration_ext ~ TV*hispanic_d+ logPop + pcHisp + income  + age + sex + age2, data=atus_u19)
m5 <- lm(duration_ext ~ TV*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + foreign*hispanic_d, data=atus_u19)
### what distance restriction works?
# m6 <- lm(duration_ext ~ TV*hispanic_d*dist + TV*dist2*hispanic_d + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusCounty,
# subset = dist < 10000)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_ext11child.tex", title="Effect of TV on Amount of TV Watched, DD, 18 or under",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               'Hispanic dummy',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="", se = makeRobust4(m2,m3,m4,m5))


###### CLUSTER ########

m2 <- feols(duration_ext ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m3 <- feols(duration_ext ~ TV*hispanic_d + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m4 <- feols(duration_ext ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m5 <- feols(duration_ext ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2 + foreign*hispanic_d, data=atusCounty, cluster = c('stateCounty'))
etable(m2,m3,m4,m5, tex = TRUE, file = "../../Output/Regs/atus_2015_ext11_cl.tex",
       order = etable_order, replace = TRUE, keep = c('TV'), digits = "r3")

m2 <- feols(duration_child ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m3 <- feols(duration_child ~ TV*hispanic_d + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m4 <- feols(duration_child ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m5 <- feols(duration_child ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2 + foreign*hispanic_d, data=atusCounty, cluster = c('stateCounty'))
etable(m2,m3,m4,m5, tex = TRUE, file = "../../Output/Regs/atus_2015_child_cl.tex",
       order = etable_order, replace = TRUE, keep = c('TV'), digits = "r3")

m2 <- feols(duration_parent ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m3 <- feols(duration_parent ~ TV*hispanic_d + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m4 <- feols(duration_parent ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m5 <- feols(duration_parent ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2 + foreign*hispanic_d, data=atusCounty, cluster = c('stateCounty'))
etable(m2,m3,m4,m5, tex = TRUE, file = "../../Output/Regs/atus_2015_parent_cl.tex",
       order = etable_order, replace = TRUE, keep = c('TV'), digits = "r3")

m2 <- feols(duration_ext ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty, cluster = c('stateCounty'), subset = atusCounty$foreign == 1)
m3 <- feols(duration_ext ~ TV*hispanic_d + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'), subset = atusCounty$foreign == 1)
m4 <- feols(duration_ext ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'), subset = atusCounty$foreign == 1)
etable(m2,m3,m4, tex = TRUE, file = "../../Output/Regs/atus_2015_foreign_cl.tex",
       order = etable_order, replace = TRUE, keep = c('TV'), digits = "r3")


m2 <- feols(edu ~ TV*hispanic_d + income + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m3 <- feols(edu ~ TV*hispanic_d + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m4 <- feols(edu ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2, data=atusCounty, cluster = c('stateCounty'))
m5 <- feols(edu ~ TV*hispanic_d + logPop + income + pcHisp + age + sex + age2 + foreign*hispanic_d, data=atusCounty, cluster = c('stateCounty'))
etable(m2,m3,m4,m5, tex = TRUE, file = "../../Output/Regs/atus_2015_edu_cl.tex",
       order = etable_order, replace = TRUE, keep = c('TV'), digits = "r3")


nonparent <- atusCounty %>%
  mutate(duration = duration - duration_parent) %>%
  dplyr::select(duration, TV, hispanic_d, logPop, pcHisp, income, age, sex, age2, cases, foreign) %>%
  mutate(parent = 0)
parent <- atusCounty %>%
  mutate(duration = duration_parent) %>%
  dplyr::select(duration, TV, hispanic_d, logPop, pcHisp, income, age, sex, age2, cases, foreign) %>%
  mutate(parent = 1, se = makeRobust4(m2,m3,m4,m5))

atusMerge <- rbind(nonparent, parent)

m2 <- lm(duration ~ TV*hispanic_d*parent + logPop + age + sex + age2 + cases, data=atusMerge) 
m3 <- lm(duration ~ TV*hispanic_d*parent + logPop + pcHisp  + age + sex + age2 + cases, data=atusMerge)
m4 <- lm(duration ~ TV*hispanic_d*parent + logPop + pcHisp + income  + age + sex + age2 + cases, data=atusMerge)
m5 <- lm(duration ~ TV*hispanic_d*parent + logPop + pcHisp + income  + age + sex + age2 + cases + foreign*hispanic_d, data=atusMerge)
stargazer(m2,m3,m4,m5, out = "../../Output/Regs/atus_2015_extpar3d.tex", title="Effect of TV on Amount of TV Watched with parent, DD",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          # order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          # covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Hispanic ',
                               # 'Hispanic dummy',
                               # 'Log(Population)','County \\% Hispanic','Log(Income)',
                               # 'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Minutes TV watched',
          notes ="")



makeRobust4 <- function(m1,m2,m3,m4) {
  om1r <- sqrt(diag(vcovHC(m1, type="HC1")))
  om2r <- sqrt(diag(vcovHC(m2, type="HC1")))
  om3r <- sqrt(diag(vcovHC(m3, type="HC1")))
  om4r <- sqrt(diag(vcovHC(m4, type="HC1")))
  return(list(om1r,om2r,om3r,om4r))
}

############## descriptive time ############
indiv_times <- atusCounty %>%
  select(caseid, TV, hispanic_d, time.ztv=duration_ext, time.sleep = sleep, time.household = hhactivity,
         time.workedu = work_edu, time.leisure = leisure, time.other = other) %>%
  pivot_longer(starts_with("time"),names_to = "activity", values_to = "duration") %>%
  mutate(group = ifelse(TV == 0 & hispanic_d == 0, 1, 0),
         group = ifelse(TV == 0 & hispanic_d == 1, 2, group),
         group = ifelse(TV == 1 & hispanic_d == 0, 3, group),
         group = ifelse(TV == 1 & hispanic_d == 1, 4, group),
         d2 = ifelse(TV == 0, duration/12036002*24, duration/12678818*24), # for Hispanics
         d3 = ifelse(TV == 0, duration/35288069*24, duration/38511627*24)) # for whites

ggplot(data=indiv_times[indiv_times$hispanic_d == 1,], aes(x=TV, y=d2, fill=factor(activity,labels =
                              c("household work","leisure","other","sleep/self care","work & education","TV")))) +
  geom_bar(stat="identity") +
  labs(y = "Duration (hours)", fill = "Activity") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  scale_y_continuous(breaks = seq(0,24,2))
ggsave("../../../analysis/Output/graphs/time_breakdown.pdf")

ggplot(data=indiv_times[indiv_times$hispanic_d == 1 & indiv_times$activity == "time.ztv",], aes(x=TV, y=d2, fill=activity)) +
  geom_bar(stat="identity") +
  labs(y = "Duration") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  scale_y_continuous(breaks = seq(0,100,10))

ggplot(data=indiv_times[indiv_times$hispanic_d == 0,], aes(x=TV, y=d3, fill=factor(activity,labels =
                                                                                     c("household work","leisure","other","sleep/self care","work & education","TV")))) +
  geom_bar(stat="identity") +
  labs(y = "Duration (hours)", fill = "Activity") +
  scale_x_continuous(breaks = seq(0,1,1)) +
  scale_y_continuous(breaks = seq(0,24,2))
ggsave("../../../analysis/Output/graphs/time_breakdown_nonhispanic.pdf")


summary(atusCounty$sleep)
summary(atusCounty$sleep[atusCounty$TV == 1])

