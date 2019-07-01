# This file is (currently) used for exploring the FCC contour data
# When completed, it will be used to produce SLTV contours.

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument/all-cdbs-files') 
}

# Structure is cleaned with Haskell to drop extra fields
ownership_structure <- fread('ownership_structure_clean.dat',header = FALSE, sep = '|', fill=TRUE)
  