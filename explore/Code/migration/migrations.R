###### MIGRATION BUILD ####
# The US census provides info on migration at the county x county level here:
# https://www.census.gov/data/tables/2015/demo/geographic-mobility/county-to-county-migration-2011-2015.html
# Using the Hispanic origin, we're going to build a county x county panel that better fits our data needs

library(data.table)
library(tidyverse)
library(estimatr)
library(lfe)
library(stargazer)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/migration') 
}


year0610 <- fread('ctyxcty_hisp_us0610Clean.txt')

# goal: reshape into county-county level
cty0610 <- year0610 %>%
  rename(ethnicity = V2, mig = V3, err = V4, ctys = V1) %>%
  mutate(ethnicity = str_sub(ethnicity,start=2,end=2)) %>%  # 1 is white, 2 is non-white/Hispanic, 3 is Hispanic
  mutate(destState = str_sub(ctys,start=7,end=9), destCty = str_sub(ctys,start=10,end=12)) %>%
  mutate(origState = str_sub(ctys,start=1,end=3), origCty = str_sub(ctys,start=4,end=6)) %>%
  dplyr::select(-ctys)

## reverse migration
revMig <- cty0610 %>%
  rename(revMig = mig) %>%
  mutate(tempState=destState, tempCty=destCty) %>%
  select(-destState,-destCty, -err) %>%
  rename(destState=origState, destCty=origCty) %>%
  rename(origState=tempState,origCty=tempCty)
  
instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata")

origCounties <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(origCty = origcounty, origState = origstate) %>%
  mutate(origState = str_pad(origState,3,side="left","0"), origCty = str_pad(origCty,3,side="left","0"))

destCounties <- instrument %>%
  rename_all(~ paste0("dest",.)) %>%
  rename(destCty = destcounty, destState = deststate) %>%
  mutate(destState = str_pad(destState,3,side="left","0"), destCty = str_pad(destCty,3,side="left","0"))

## distances
distances <- fread('../counties/distances/sf12010countydistancemiles.csv') %>%
  mutate(county1 = str_pad(county1, 6, side = "left", "0")) %>%
  mutate(county2 = str_pad(county2, 6, side = "left", "0")) %>%
  mutate(destState = str_sub(county2,start=1,end=3), destCty = str_sub(county2,start=4,end=6)) %>%
  mutate(origState = str_sub(county1,start=1,end=3), origCty = str_sub(county1,start=4,end=6))

## merge
migrations <- cty0610 %>%
  left_join(origCounties, by= c('origState','origCty')) %>%
  left_join(destCounties, by= c('destState','destCty')) %>%
  left_join(distances, by = c('origState', 'origCty','destState','destCty')) %>%
  left_join(revMig, by = c('origState', 'origCty','destState','destCty', 'ethnicity'))

saveRDS(migrations,'0610migrations.Rdata')
stargazer(migrations, out="../../Output/Summary/Migrations0610.tex", title="County-County Migrations 2006-10",
          summary = TRUE, font.size = 'scriptsize')

# areas: 5% and 95% tolerance?


##### regressions! #####

# TODO:
# need to control for county-county distance!
# 20 nearest neighbors? 
# net migration?

migrations <- readRDS('0610migrations.Rdata')

# with distances
distDummy <- migrations %>%
  mutate(TV = ifelse(destintersects == 1 & (destareaRatio > .95 | destdist > 0),1,0)) %>%
  filter(origintersects == 0 & origdist < 100000) %>%
  filter(!(destintersects == 1 & destdist > 25000)) %>%
  mutate(orig = 1000*as.numeric(origState) + as.numeric(origCty)) %>% # unique per orig
  mutate(origLogPop = log(origpopulation), destLogPop = log(destpopulation),
         origLogInc = log(origincome), destLogInc = log(destincome), migLog = log(mig)) %>%
  filter(ethnicity == 3)

## log
m1 <- felm(migLog ~ TV + origLogPop + destLogPop + mi_to_county|0|0|orig, data=distDummy)
m2 <- felm(migLog ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp + mi_to_county |0|0|orig, data=distDummy)
m3 <- felm(migLog ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc + mi_to_county
           |0|0|orig, data=distDummy)
m4 <- felm(migLog ~ origdist + destdist + mi_to_county
           |0|0|0, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/miglog_distdummyOITV.tex", title="Effect of TV on Log Migration, Outside Sample Distance Dummy")
## no log
m1 <- felm(mig ~ TV + origLogPop + destLogPop + mi_to_county|0|0|orig, data=distDummy)
m2 <- felm(mig ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp + mi_to_county |0|0|orig, data=distDummy)
m3 <- felm(mig ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc + mi_to_county
           |0|0|orig, data=distDummy)
m4 <- felm(mig ~ origdist + destdist + mi_to_county
           |0|0|0, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/mig_distdummyOITV.tex", title="Effect of TV on Migration, Outside Sample Distance Dummy")
## reverse mig
m1 <- felm(revMig ~ TV + origLogPop + destLogPop + mi_to_county|0|0|orig, data=distDummy)
m2 <- felm(revMig ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp + mi_to_county |0|0|orig, data=distDummy)
m3 <- felm(revMig ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc + mi_to_county
           |0|0|orig, data=distDummy)
m4 <- felm(revMig ~ origdist + destdist + mi_to_county
           |0|0|0, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/migrev_distdummyOITV.tex", title="Effect of TV on Reverse Migration, Outside Sample Distance Dummy")


## placebo
distDummy <- migrations %>%
  mutate(TV = ifelse(destintersects == 1 & (destareaRatio > .95 | destdist > 0),1,0)) %>%
  filter(origintersects == 0 & origdist < 100000) %>%
  filter(!(destintersects == 1 & destdist > 25000)) %>%
  mutate(orig = 1000*as.numeric(origState) + as.numeric(origCty)) %>% # unique per orig
  mutate(origLogPop = log(origpopulation), destLogPop = log(destpopulation),
         origLogInc = log(origincome), destLogInc = log(destincome), migLog = log(mig)) %>%
  filter(ethnicity == 1 | ethnicity == 2)

## log
m1 <- felm(migLog ~ TV + origLogPop + destLogPop + mi_to_county|0|0|orig, data=distDummy)
m2 <- felm(migLog ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp + mi_to_county|0|0|orig, data=distDummy)
m3 <- felm(migLog ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc + mi_to_county
           |0|0|orig, data=distDummy)
m4 <- felm(migLog ~ origdist + destdist + mi_to_county
           |0|0|0, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/miglog_distdummyOITVP.tex", title="Effect of TV on Log Migration, Outside Sample Distance Dummy, Placebo")
## no log
m1 <- felm(mig ~ TV + origLogPop + destLogPop + mi_to_county|0|0|orig, data=distDummy)
m2 <- felm(mig ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp + mi_to_county|0|0|orig, data=distDummy)
m3 <- felm(mig ~ TV + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc + mi_to_county
           |0|0|orig, data=distDummy)
m4 <- felm(mig ~ origdist + destdist + mi_to_county
           |0|0|0, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/mig_distdummyOITVP.tex", title="Effect of TV on Migration, Outside Sample Distance Dummy, Placebo")

  
# from outside to 'inside'
# where outside is: dummy 
distDummy <- migrations %>%
  filter(origintersects == 0 & origdist < 100000) %>%
  mutate(orig = 1000*as.numeric(origState) + as.numeric(origCty)) %>% # unique per orig
  mutate(origLogPop = log(origpopulation), destLogPop = log(destpopulation),
         origLogInc = log(origincome), destLogInc = log(destincome),
         origdist = origdist/1000, destdist = destdist/1000) %>%
  filter(ethnicity == 3) %>%
  mutate(odist2 = origdist^2, ddist2 = destdist^2)
  
m1 <- felm(ihs(mig) ~ destintersects*origdist + destintersects*destdist  + origLogPop + destLogPop|0|0|orig, data=distDummy)
m2 <- felm(ihs(mig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp|0|0|orig, data=distDummy)
m3 <- felm(ihs(mig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc
           |0|0|orig, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/mig_outsideDistDummy.tex", title="Effect of TV on Migration, Outside Sample Distance Dummy",
          omit.stat = c('f','ser'),
          order = c('destintersects','destintersects:origdist','destintersects:destdist'),
          covariate.labels = c('Dummy: Destination in TV Contour', 'TV Dummy $\\times$ Distance to Origin',
                               'TV Dummy $\\times$ Distance to Destination', 'Distance from Contor to Origin (KM)',
                               'Distance from Contour to Destination (KM)', 'Origin Log(Population)',
                               'Destination Log(Population)', 'Origin \\% Hispanic', 'Destination \\% Hispanic',
                               'Origin Log(Income)', 'Destination Log(Income)'),
          dep.var.labels = '\\# Hispanic Migrants',
          omit = 'Constant')
m1 <- felm(ihs(revMig) ~ destintersects*origdist + destintersects*destdist  + origLogPop + destLogPop|0|0|orig, data=distDummy)
m2 <- felm(ihs(revMig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp|0|0|orig, data=distDummy)
m3 <- felm(ihs(revMig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc
           |0|0|orig, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/migrev_outsideDistDummy.tex", title="Effect of TV on Reverse Migration, Outside Sample Distance Dummy",
          omit.stat = c('f','ser'),
          order = c('destintersects','destintersects:origdist','destintersects:destdist'),
          covariate.labels = c('Dummy: Origin in TV Contour', 'TV Dummy $\\times$ Distance to Destination',
                               'TV Dummy $\\times$ Distance to Origin', 'Distance from Contor to Destination (KM)',
                               'Distance from Contour to Origin (KM)', 'Destination Log(Population)',
                               'Origin Log(Population)', 'Destination \\% Hispanic', 'Origin \\% Hispanic',
                               'Destination Log(Income)', 'Origin Log(Income)'),
          dep.var.labels = '\\# Hispanic Migrants',
          omit = 'Constant')

m1 <- feols(ihs(mig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2  + origLogPop + destLogPop| orig, cluster = c("orig","destID") , data=distDummy)
m2 <- feols(ihs(mig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp| orig, cluster = c("orig","destID"), data=distDummy)
m3 <- feols(ihs(mig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc | orig, cluster = c("orig","destID"), data=distDummy)
etable(m1,m2,m3, tex = TRUE, file = "../../Output/Regs/mig_o_cl.tex",
       order = etable_order, replace = TRUE, keep = c('destintersects'))

m1 <- feols(ihs(revMig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2  + origLogPop + destLogPop| orig, cluster = c("orig","destID") , data=distDummy)
m2 <- feols(ihs(revMig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp| orig, cluster = c("orig","destID"), data=distDummy)
m3 <- feols(ihs(revMig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc | orig, cluster = c("orig","destID"), data=distDummy)
etable(m1,m2,m3, tex = TRUE, file = "../../Output/Regs/migrev_o_cl.tex",
       order = etable_order, replace = TRUE, keep = c('destintersects'))


# from inside to 'outside'
# where outside is: dummy 
distDummyIO <- migrations %>%
  filter(origintersects == 1 & origdist < 100000) %>%
  mutate(orig = 1000*as.numeric(origState) + as.numeric(origCty)) %>% # unique per orig
  mutate(origLogPop = log(origpopulation), destLogPop = log(destpopulation),
         origLogInc = log(origincome), destLogInc = log(destincome),
         origdist = origdist/1000, destdist = destdist/1000,
         destintersects = 1 - destintersects) %>%
  filter(ethnicity == 3) %>%
  mutate(odist2 = origdist^2, ddist2 = destdist^2)

m1 <- felm(ihs(mig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop|0|0|orig, data=distDummyIO)
m2 <- felm(ihs(mig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp |0|0|orig, data=distDummyIO)
m3 <- felm(ihs(mig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc
           |0|0|orig, data=distDummyIO)
stargazer(m1,m2,m3, out = "../../Output/Regs/mig_distdummyIO.tex", title="Effect of TV on Migration, Inside Sample Distance Dummy",
          omit.stat = c('f','ser'),
          order = c('destintersects','destintersects:origdist','destintersects:destdist'),
          covariate.labels = c('Dummy: Destination Outside TV Contour', 'TV Dummy $\\times$ Distance to Origin',
                               'TV Dummy $\\times$ Distance to Destination', 'Distance from Contor to Origin (KM)',
                               'Distance from Contour to Destination (KM)', 'Origin Log(Population)',
                               'Destination Log(Population)', 'Origin \\% Hispanic', 'Destination \\% Hispanic',
                               'Origin Log(Income)', 'Destination Log(Income)'),
          dep.var.labels = '\\# Hispanic Migrants',
          omit = 'Constant')
#reverse
m1 <- felm(ihs(revMig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop|0|0|orig, data=distDummyIO)
m2 <- felm(ihs(revMig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp |0|0|orig, data=distDummyIO)
m3 <- felm(ihs(revMig) ~ destintersects*origdist + destintersects*destdist + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc 
           |0|0|orig, data=distDummyIO)
stargazer(m1,m2,m3, out = "../../Output/Regs/migrev_distdummyIO.tex", title="Effect of TV on Reverse Migration, Inside Sample Distance Dummy",
          omit.stat = c('f','ser'),
          order = c('destintersects','destintersects:origdist','destintersects:destdist'),
          covariate.labels = c('Dummy: Origin Outside TV Contour', 'TV Dummy $\\times$ Distance to Destination',
                               'TV Dummy $\\times$ Distance to Origin', 'Distance from Contor to Destination (KM)',
                               'Distance from Contour to Origin (KM)', 'Destination Log(Population)',
                               'Origin Log(Population)', 'Destination \\% Hispanic', 'Origin \\% Hispanic',
                               'Destination Log(Income)', 'Origin Log(Income)'),
          dep.var.labels = '\\# Hispanic Migrants',
          omit = 'Constant')

m1 <- feols(ihs(mig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2  + origLogPop + destLogPop| orig, cluster = c("orig","destID") , data=distDummyIO)
m2 <- feols(ihs(mig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp| orig, cluster = c("orig","destID"), data=distDummyIO)
m3 <- feols(ihs(mig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc | orig, cluster = c("orig","destID"), data=distDummyIO)
etable(m1,m2,m3, tex = TRUE, file = "../../Output/Regs/mig_d_cl.tex",
       order = etable_order, replace = TRUE, keep = c('destintersects'))

m1 <- feols(ihs(revMig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2  + origLogPop + destLogPop| orig, cluster = c("orig","destID") , data=distDummyIO)
m2 <- feols(ihs(revMig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp| orig, cluster = c("orig","destID"), data=distDummyIO)
m3 <- feols(ihs(revMig) ~ destintersects*origdist*odist2 + destintersects*destdist*ddist2 + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc | orig, cluster = c("orig","destID"), data=distDummyIO)
etable(m1,m2,m3, tex = TRUE, file = "../../Output/Regs/migrev_d_cl.tex",
       order = etable_order, replace = TRUE, keep = c('destintersects'))


distDummy <- migrations %>%
  filter(origdist < 100000 & destdist < 100000) %>%
  mutate(orig = 1000*as.numeric(origState) + as.numeric(origCty)) %>% # unique per orig
  mutate(origLogPop = log(origpopulation), destLogPop = log(destpopulation),
         origLogInc = log(origincome), destLogInc = log(destincome),
         origdist = origdist/1000, destdist = destdist/1000) %>%
  filter(ethnicity == 3)

sum2 <- distDummy %>%
  mutate(intersects_d = ifelse(origintersects > 0, 1, 0),
         ihsmig = ihs(mig)) %>%
  group_by(intersects_d) %>%
  select(ihsmig,origLogPop,origLogInc,origpcHisp, origdist, intersects_d) %>%
  summarize_all(funs( mean = mean(.,na.rm=TRUE), sd = sd(.,na.rm=TRUE))) %>%
  gather("stat", "val", -intersects_d) %>%
  mutate(intersects_d = paste0("TV:", intersects_d)) %>%
  unite(stat, stat, intersects_d, sep = ".") %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, "mean.TV:0", "sd.TV:0", "mean.TV:1", "sd.TV:1") %>%
  as.data.frame()

sum2a <- distDummy %>%
  mutate(ihsmig = ihs(mig)) %>%
  select(ihsmig,origLogPop,origLogInc,origpcHisp,origdist) %>%
  summarize_all(funs( mean = mean(.,na.rm=TRUE), sd = sd(.,na.rm=TRUE)))


# have meaningful movements between 75,000 county pairs; 30,500 contain Hispanic movements
# note: there are XXXs when movement is to state as a whole
