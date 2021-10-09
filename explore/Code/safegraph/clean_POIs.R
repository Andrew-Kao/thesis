###### Safegraph data ####

library(dplyr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/safegraph')
  sdir <- '~/Dropbox/safegraph/coreplaces'
}


# do for each of the coreplaces

df <- fread(paste0(sdir,'/core_poi-part1.csv'))



