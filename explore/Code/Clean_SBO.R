# This file is (currently) used for exploring the SBO data
# When completed, it will be used to produce a clean SBO dataset


# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firm_data/sbo') 
}

sbo <- fread('sbo_2007.csv')
