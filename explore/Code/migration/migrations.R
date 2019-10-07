###### MIGRATION BUILD ####
# The US census provides info on migration at the county x county level here:
# https://www.census.gov/data/tables/2015/demo/geographic-mobility/county-to-county-migration-2011-2015.html
# Using the Hispanic origin, we're going to build a county x county panel that better fits our data needs

library(data.table)
library(tidyverse)

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
  select(-ctys)

# drop XXX
# merge in variables
#   population, Hisp. population, income, education,
# then make
#   log diff pop
# 20 nearest neighbors? 

# have meaningful movements between 75,000 county pairs; 30,500 contain Hispanic movements
# note: there are XXXs when movement is to state as a whole

# TODO: setting up the merge to the tv-county dataset
