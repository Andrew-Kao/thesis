###### SCS data cleaning ####

library(tidyverse)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/student surveys') 
}

load("SCS/DS0001/36354-0001-Data.rda")
scs <- da36354.0001

load("NCVS-2015-address/DS0001/36448-0001-Data.rda")

## not tractable: only region level, nothing more granular