###### MIGRATION BUILD ####
# The US census provides info on migration at the county x county level here:
# https://www.census.gov/data/tables/2015/demo/geographic-mobility/county-to-county-migration-2011-2015.html
# Using the Hispanic origin, we're going to build a county x county panel that better fits our data needs

library(data.table)
library(tidyverse)
library(estimatr)
library(lfe)

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

instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata")

origCounties <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(origCty = origcounty, origState = origstate) %>%
  mutate(origState = str_pad(origState,3,side="left","0"), origCty = str_pad(origCty,3,side="left","0"))

destCounties <- instrument %>%
  rename_all(~ paste0("dest",.)) %>%
  rename(destCty = destcounty, destState = deststate) %>%
  mutate(destState = str_pad(destState,3,side="left","0"), destCty = str_pad(destCty,3,side="left","0"))

migrations <- cty0610 %>%
  left_join(origCounties, by= c('origState','origCty')) %>%
  left_join(destCounties, by= c('destState','destCty'))

saveRDS(migrations,'0610migrations.Rdata')
stargazer(migrations, out="../../Output/Summary/Migrations0610.tex", title="County-County Migrations 2006-10",
          summary = TRUE, font.size = 'scriptsize')

# areas: 5% and 95% tolerance?

##### regressions! #####

# from outside to 'inside'
# where outside is: dummy 
distDummy <- migrations %>%
  filter(origintersects == 0 & origdist < 100000) %>%
  mutate(orig = 1000*as.numeric(origState) + as.numeric(origCty)) %>% # unique per orig
  mutate(origLogPop = log(origpopulation), destLogPop = log(destpopulation),
         origLogInc = log(origincome), destLogInc = log(destincome))
  
attach(distDummy)
m1 <- felm(mig ~ destintersects + origLogPop + destLogPop|0|0|orig, data=distDummy)
m2 <- felm(mig ~ destintersects + origLogPop + destLogPop+ origpcHisp + destpcHisp|0|0|orig, data=distDummy)
m3 <- felm(mig ~ destintersects + origLogPop + destLogPop+ origpcHisp + destpcHisp+ origLogInc + destLogInc
           |0|0|orig, data=distDummy)
stargazer(m1,m2,m3, out = "../../Output/Regs/mig_outsideDistDummy.tex", title="Effect of TV on Migration, Outside Sample Distance Dummy")

# cluster by origin county


# drop XXX
# merge in variables
#   population, Hisp. population, income, education,
# then make
#   log diff pop
# 20 nearest neighbors? 

# have meaningful movements between 75,000 county pairs; 30,500 contain Hispanic movements
# note: there are XXXs when movement is to state as a whole
