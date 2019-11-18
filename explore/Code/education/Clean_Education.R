###### Education Cleaning ####
# Data sourced: https://ocrdata.ed.gov/DownloadDataFile (Oct 18, 2019)

library(data.table)
library(rgdal)
library(sf)
library(raster)
library(dplyr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}

toClassify <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 LEA Data.csv') %>%
  select('LEA_ADDRESS','LEA_CITY','LEA_ZIP')

write.csv(toClassify, '2015-16-crdc-data/Output/LEA_Adresses.csv')


## merge in data

# 17,337 unique LEAs, all have att least one school
leaMaster <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 LEA Data.csv')

schoolMaster <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 School Data.csv')

ged <- leaMaster %>%
  select('LEA_ADDRESS','LEA_CITY','LEA_ZIP', 
         'LEA_GED_IND', 'LEA_GEDPART_HI_M', 'LEA_GEDPART_HI_F', 'TOT_GEDPART_M', 'TOT_GEDPART_F', # participate, hispanic m/f, total m/f
          'LEA_GEDCRED_HI_M', 'LEA_GEDCRED_HI_F', 'TOT_GEDCRED_M', 'TOT_GEDCRED_F') %>% #credit
  filter(LEA_GED_IND == "Yes") # remove negatives/recode to 0... figure it out. TOT_GEDCRED_M >= 0

specialSchool <- schoolMaster %>%
  select('SCHID',
          'SCH_STATUS_MAGNET', 'SCH_STATUS_CHARTER', 'SCH_STATUS_ALT') # special status; can also get grades

enroll <- schoolMaster %>%
  select('SCHID',
         'SCH_ENR_HI_M', 'SCH_ENR_HI_F', 'TOT_ENR_M', 'TOT_ENR_F', # overall enrollment
         'SCH_LEPENR_HI_M', 'SCH_LEPENR_HI_F', 'TOT_LEPENR_F', 'TOT_LEPENR_M')  # limited english proficiency

gifted <- schoolMaster %>%
  select('SCHID','SCH_GT_IND', # has gifted?
          'SCH_GTENR_HI_M', 'SCH_GTENR_HI_F', 'TOT_GTENR_M', 'TOT_GTENR_F') 

algebra1 <- schoolMaster %>%
  select('SCHID', 'SCH_ALGCLASSES_GS0708', 'SCH_ALGCERT_GS0708', # number of classes, teachers
          'SCH_ALGENR_G08_HI_M', 'SCH_ALGENR_G08_HI_F', 'TOT_ALGENR_G08_M', 'TOT_ALGENR_G08_F', # Grade 8
          'SCH_ALGENR_GS0910_HI_M', 'SCH_ALGENR_GS0910_HI_F', 'TOT_ALGENR_GS0910_M', 'TOT_ALGENR_GS0910_F', # G9/10
          'SCH_ALGENR_GS1112_HI_M', 'SCH_ALGENR_GS1112_HI_F', 'TOT_ALGENR_GS1112_M', 'TOT_ALGENR_GS1112_M', # G11/12
          'SCH_ALGPASS_G08_HI_M', 'SCH_ALGPASS_G08_HI_F', 'TOT_ALGPASS_G08_M', 'TOT_ALGPASS_G08_F', # pass g8
          'SCH_ALGPASS_GS0910_HI_M', 'SCH_ALGPASS_GS0910_HI_F', 'TOT_ALGPASS_GS0910_M', 'TOT_ALGPASS_GS0910_F', # pass g9/10
          'SCH_ALGPASS_GS1112_HI_M', 'SCH_ALGPASS_GS1112_HI_F', 'TOT_ALGPASS_GS1112_M', 'TOT_ALGPASS_GS1112_F') # pass g11/12

# geometry/alg 2/advanced math has no pass rate
# same with chem/phys/bio

calculus <- schoolMaster %>%
  select('SCHID','SCH_MATHCLASSES_CALC', 'SCH_MATHCERT_CALC', # classes/teachers
         'SCH_MATHENR_CALC_HI_M', 'SCH_MATHENR_CALC_HI_F', 'TOT_MATHENR_CALC_M', 'TOT_MATHENR_CALC_M') # taking

stargazer(ged, out="../../Output/Summary/EduDFGed.tex", title="GED Completions", summary = TRUE)

## AP next


## need to get the .shp
schoolLoc <- rgdal::readOGR("2015-16-crdc-data/Output/Export_Output.shp")
leas <- spTransform(schoolLoc, CRS("+proj=longlat +datum=NAD83"))




## of interest
#   see general school features
#   chronic absenteeism
#   suspension, expulsion (transfers?)
#   referrals, arrests, bullying