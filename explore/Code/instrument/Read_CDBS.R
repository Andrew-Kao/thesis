# This file is (currently) used for exploring the FCC contour data
# When completed, it will be used to produce SLTV contours.

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument/all-cdbs-files') 
}

ownership_structure <- fread('ownership_structure_cleaned.dat',header = FALSE, sep = '|', fill=TRUE)
