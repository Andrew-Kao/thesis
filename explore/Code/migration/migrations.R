###### MIGRATION BUILD ####
# The US census provides info on migration at the county x county level here:
# https://www.census.gov/data/tables/2015/demo/geographic-mobility/county-to-county-migration-2011-2015.html
# Using the Hispanic origin, we're going to build a county x county panel that better fits our data needs

library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/migration') 
}


year0610 <- read.delim('ctyxcty_hisp_us0610.txt')





