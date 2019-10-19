###### Education Cleaning ####
# Data sourced: https://ocrdata.ed.gov/DownloadDataFile (Oct 18, 2019)

library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}

toClassify <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 LEA Data.csv') %>%
  select('LEA_ADDRESS','LEA_CITY','LEA_ZIP')

write.csv(toClassify, '2015-16-crdc-data/Output/LEA_Adresses.csv')


## merge in data


schoolMaster <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 School Data.csv')



## of interest
#   see general school features
#   chronic absenteeism
#   suspension, expulsion (transfers?)
#   referrals, arrests, bullying