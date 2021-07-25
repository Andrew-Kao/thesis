###### Education Cleaning ####
# Data sourced: https://ocrdata.ed.gov/DownloadDataFile (Oct 18, 2019)

library(data.table)
library(rgdal)
library(sf)
library(raster)
library(rgeos)
library(dplyr)
library(stringr)
library(stargazer)

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

# TODO: investigate duplicate schlea
# 133602430
# 242100540
# 6482803360
# 6522803360
# 6612803450

ged <- leaMaster %>%
  select('LEA_ADDRESS','LEA_CITY','LEA_ZIP', 'LEAID',
         'LEA_GED_IND', 'LEA_GEDPART_HI_M', 'LEA_GEDPART_HI_F', 'TOT_GEDPART_M', 'TOT_GEDPART_F', # participate, hispanic m/f, total m/f
          'LEA_GEDCRED_HI_M', 'LEA_GEDCRED_HI_F', 'TOT_GEDCRED_M', 'TOT_GEDCRED_F') %>% #credit
  filter(LEA_GED_IND == "Yes") # remove negatives/recode to 0... figure it out. TOT_GEDCRED_M >= 0
saveRDS(ged, 'LEAGED.Rdata')

specialSchool <- schoolMaster %>%
  select('SCHID','LEAID',
          'SCH_STATUS_MAGNET', 'SCH_STATUS_CHARTER', 'SCH_STATUS_ALT') # special status; can also get grades
saveRDS(specialSchool, 'SchSpecial.Rdata')

enroll <- schoolMaster %>%
  select('SCHID', 'LEAID',
         'SCH_ENR_HI_M', 'SCH_ENR_HI_F', 'TOT_ENR_M', 'TOT_ENR_F', # overall enrollment
         'SCH_LEPENR_HI_M', 'SCH_LEPENR_HI_F', 'TOT_LEPENR_F', 'TOT_LEPENR_M')  # limited english proficiency
saveRDS(enroll, 'SchEnroll.Rdata')

gifted <- schoolMaster %>%
  select('SCHID','SCH_GT_IND', 'LEAID', # has gifted?
          'SCH_GTENR_HI_M', 'SCH_GTENR_HI_F', 'TOT_GTENR_M', 'TOT_GTENR_F','SCH_GTENR_WH_M','SCH_GTENR_WH_F') 
saveRDS(gifted, 'SchGifted.Rdata')

algebra1 <- schoolMaster %>%
  select('SCHID', 'SCH_ALGCLASSES_GS0708', 'SCH_ALGCERT_GS0708', 'LEAID', # number of classes, teachers
          'SCH_ALGENR_G08_HI_M', 'SCH_ALGENR_G08_HI_F', 'TOT_ALGENR_G08_M', 'TOT_ALGENR_G08_F', # Grade 8
          'SCH_ALGENR_GS0910_HI_M', 'SCH_ALGENR_GS0910_HI_F', 'TOT_ALGENR_GS0910_M', 'TOT_ALGENR_GS0910_F', # G9/10
          'SCH_ALGENR_GS1112_HI_M', 'SCH_ALGENR_GS1112_HI_F', 'TOT_ALGENR_GS1112_M', 'TOT_ALGENR_GS1112_M', # G11/12
          'SCH_ALGPASS_G08_HI_M', 'SCH_ALGPASS_G08_HI_F', 'TOT_ALGPASS_G08_M', 'TOT_ALGPASS_G08_F', # pass g8
          'SCH_ALGPASS_GS0910_HI_M', 'SCH_ALGPASS_GS0910_HI_F', 'TOT_ALGPASS_GS0910_M', 'TOT_ALGPASS_GS0910_F', # pass g9/10
          'SCH_ALGPASS_GS1112_HI_M', 'SCH_ALGPASS_GS1112_HI_F', 'TOT_ALGPASS_GS1112_M', 'TOT_ALGPASS_GS1112_F') # pass g11/12
saveRDS(algebra1, 'SchAlg1.Rdata')

# geometry/alg 2/advanced math has no pass rate
# same with chem/phys/bio

calculus <- schoolMaster %>%
  select('SCHID', 'LEAID',
         'SCH_MATHCLASSES_CALC', 'SCH_MATHCERT_CALC', # classes/teachers
         'SCH_MATHENR_CALC_HI_M', 'SCH_MATHENR_CALC_HI_F', 'TOT_MATHENR_CALC_M', 'TOT_MATHENR_CALC_M') # taking
saveRDS(calculus, 'SchCalc.Rdata')

ap <- schoolMaster %>%
  select('SCHID','LEAID','SCH_APENR_IND', 'SCH_APMATHENR_IND', 'SCH_APSCIENR_IND', # AP indicator/math/sci
         'SCH_APENR_HI_M','SCH_APENR_HI_F','TOT_APENR_M','TOT_APENR_F', # number enrolled in AP classes
         'SCH_APMATHENR_HI_M', 'SCH_APMATHENR_HI_F', 'TOT_APMATHENR_M', 'TOT_APMATHENR_F', # math
         'SCH_APSCIENR_HI_M','SCH_APSCIENR_HI_F','TOT_APSCIENR_M','TOT_APSCIENR_F', # science
         'SCH_APEXAM_ONEORMORE_HI_M', 'SCH_APEXAM_ONEORMORE_HI_F', 'TOT_APEXAM_ONEORMORE_M', 'TOT_APEXAM_ONEORMORE_F', # actual test taken?
         'SCH_APPASS_ONEORMORE_HI_M', 'SCH_APPASS_ONEORMORE_HI_F', 'TOT_APPASS_ONEORMORE_M', 'TOT_APPASS_ONEORMORE_F') # pass?
saveRDS(ap, 'SchAP.Rdata')

## page 82
exam <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_SATACT_HI_M', 'SCH_SATACT_HI_F', 'TOT_SATACT_M', 'TOT_SATACT_F')
saveRDS(exam, 'SchExam.Rdata')

absent <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_ABSENT_HI_M','SCH_ABSENT_HI_F','TOT_ABSENT_M','TOT_ABSENT_F')
saveRDS(absent, 'SchAbsent.Rdata')

## RETENTION: do we care? seems messy, but can fight identification of moving
retention <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_RET_G01_HI_M', 'SCH_RET_G01_HI_F', 'SCH_RET_G01_HI_M', 'TOT_RET_G01_M', 'TOT_RET_G01_F',
         'SCH_RET_G02_HI_M', 'SCH_RET_G02_HI_F', 'SCH_RET_G02_HI_M', 'TOT_RET_G02_M', 'TOT_RET_G02_F',
         'SCH_RET_G03_HI_M', 'SCH_RET_G03_HI_F', 'SCH_RET_G03_HI_M', 'TOT_RET_G03_M', 'TOT_RET_G03_F',
         'SCH_RET_G04_HI_M', 'SCH_RET_G04_HI_F', 'SCH_RET_G04_HI_M', 'TOT_RET_G04_M', 'TOT_RET_G04_F',
         'SCH_RET_G05_HI_M', 'SCH_RET_G05_HI_F', 'SCH_RET_G05_HI_M', 'TOT_RET_G05_M', 'TOT_RET_G05_F',
         'SCH_RET_G06_HI_M', 'SCH_RET_G06_HI_F', 'SCH_RET_G06_HI_M', 'TOT_RET_G06_M', 'TOT_RET_G06_F',
         'SCH_RET_G07_HI_M', 'SCH_RET_G07_HI_F', 'SCH_RET_G07_HI_M', 'TOT_RET_G07_M', 'TOT_RET_G07_F',
         'SCH_RET_G08_HI_M', 'SCH_RET_G08_HI_F', 'SCH_RET_G08_HI_M', 'TOT_RET_G08_M', 'TOT_RET_G08_F',
         'SCH_RET_G09_HI_M', 'SCH_RET_G09_HI_F', 'SCH_RET_G09_HI_M', 'TOT_RET_G09_M', 'TOT_RET_G09_F',
         'SCH_RET_G10_HI_M', 'SCH_RET_G10_HI_F', 'SCH_RET_G10_HI_M', 'TOT_RET_G10_M', 'TOT_RET_G10_F',
         'SCH_RET_G11_HI_M', 'SCH_RET_G11_HI_F', 'SCH_RET_G11_HI_M', 'TOT_RET_G11_M', 'TOT_RET_G11_F',
         'SCH_RET_G12_HI_M', 'SCH_RET_G12_HI_F', 'SCH_RET_G12_HI_M', 'TOT_RET_G12_M', 'TOT_RET_G12_F') 
saveRDS(retention, 'SchRetention.Rdata') 
         

# there is a disabilities category, not sure it matters for our analysis
punish <- schoolMaster %>%
  select('SCHID','LEAID', 'SCH_CORPINSTANCES_IND',  
         'SCH_PSDISC_CORP_HI_M','SCH_PSDISC_CORP_HI_F', 'SCH_PSCORPINSTANCES_ALL', # preschool corporal punish
         'SCH_DISCWODIS_CORP_HI_M', 'SCH_DISCWODIS_CORP_HI_F', 'TOT_DISCWODIS_CORP_M', 'TOT_DISCWODIS_CORP_F')  %>%
  filter(SCH_CORPINSTANCES_IND == "Yes")
saveRDS(punish, 'SchPunish.Rdata')
  
suspend <- schoolMaster %>%
  select('SCHID','LEAID', 
         'SCH_PSDISC_SINGOOS_HI_M', 'SCH_PSDISC_SINGOOS_HI_F', 'TOT_PSDISC_SINGOOS_M', 'TOT_PSDISC_SINGOOS_F', # preschool one OsS
         'SCH_PSDISC_MULTOOS_HI_M', 'SCH_PSDISC_MULTOOS_HI_F', 'TOT_PSDISC_MULTOOS_M', 'TOT_PSDISC_MULTOOS_F', # preschool multiple oss
         'SCH_DISCWODIS_ISS_HI_M', 'SCH_DISCWODIS_ISS_HI_F', 'TOT_DISCWODIS_ISS_M', 'TOT_DISCWODIS_ISS_F', # ever got ISS
         'SCH_DISCWODIS_SINGOOS_HI_M', 'SCH_DISCWODIS_SINGOOS_HI_F', 'SCH_DISCWODIS_SINGOOS_HI_M', 'SCH_DISCWODIS_SINGOOS_HI_F', # one OSS
         'SCH_DISCWODIS_MULTOOS_HI_M', 'SCH_DISCWODIS_MULTOOS_HI_F', 'TOT_DISCWODIS_MULTOOS_M', 'TOT_DISCWODIS_MULTOOS_F', # multiple oss
         'SCH_DAYSMISSED_HI_M', 'SCH_DAYSMISSED_HI_F', 'TOT_DAYSMISSED_M', 'TOT_DAYSMISSED_F')  # days missed bc OSS
saveRDS(suspend, 'SchSuspend.Rdata')
         
expulsion <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_PSDISC_EXP_HI_M', 'SCH_PSDISC_EXP_HI_F', 'TOT_PSDISC_EXP_M', 'TOT_PSDISC_EXP_F', # preschool
         'SCH_DISCWODIS_EXPWE_HI_M', 'SCH_DISCWODIS_EXPWE_HI_F', 'TOT_DISCWODIS_EXPWE_M', 'TOT_DISCWODIS_EXPWE_F', # w/ educ services
         'SCH_DISCWODIS_EXPWOE_HI_M', 'SCH_DISCWODIS_EXPWOE_HI_F', 'TOT_DISCWODIS_EXPWOE_M', 'TOT_DISCWODIS_EXPWOE_F',  #w/o educ services
         'SCH_DISCWODIS_EXPZT_HI_M', 'SCH_DISCWODIS_EXPZT_HI_F', 'TOT_DISCWODIS_EXPZT_M', 'TOT_DISCWODIS_EXPZT_F') # zero tolerance
saveRDS(expulsion, 'SchExpulsion.Rdata')

transfer <- schoolMaster %>%
  select('SCHID','LEAID',
        'SCH_DISCWODIS_TFRALT_HI_M', 'SCH_DISCWODIS_TFRALT_HI_F', 'TOT_DISCWODIS_TFRALT_M', 'TOT_DISCWODIS_TFRALT_F') # transfer for disiciplinary 
saveRDS(transfer, 'SchTransfer.Rdata')                 

law <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_DISCWODIS_REF_HI_M', 'SCH_DISCWODIS_REF_HI_F', 'SCH_DISCWODIS_REF_HI_M', 'SCH_DISCWODIS_REF_HI_F', # law enforce referral
         'SCH_DISCWODIS_ARR_HI_M', 'SCH_DISCWODIS_ARR_HI_F', 'TOT_DISCWODIS_ARR_M', 'TOT_DISCWODIS_ARR_F') # school related arrest
saveRDS(law, 'SchLaw.Rdata')                 

## OFFENSES: can look at overall rate, but not control..

harass <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_HBREPORTED_SEX_HI_M', 'SCH_HBREPORTED_SEX_HI_F', 'TOT_HBREPORTED_SEX_M', 'TOT_HBREPORTED_SEX_F', # victim sex
         'SCH_HBREPORTED_RAC_HI_M', 'SCH_HBREPORTED_RAC_HI_F', 'TOT_HBREPORTED_RAC_M', 'TOT_HBREPORTED_RAC_F', # victim race
         'SCH_HBDISCIPLINED_SEX_HI_M', 'SCH_HBDISCIPLINED_SEX_HI_F', 'TOT_HBDISCIPLINED_SEX_M', 'TOT_HBDISCIPLINED_SEX_F', # offender sex
         'SCH_HBDISCIPLINED_RAC_HI_M', 'SCH_HBDISCIPLINED_RAC_HI_F', 'TOT_HBDISCIPLINED_RAC_M', 'TOT_HBDISCIPLINED_RAC_F') # offender race
saveRDS(harass, 'SchHarass.Rdata')   

restraint <- schoolMaster %>%
  select('SCHID','LEAID',
         'SCH_RS_NONIDEA_MECH_HI_M', 'SCH_RS_NONIDEA_MECH_HI_F', 'TOT_RS_NONIDEA_MECH_M', 'TOT_RS_NONIDEA_MECH_F', # mechanical restraint
         'SCH_RS_NONIDEA_PHYS_HI_M', 'SCH_RS_NONIDEA_PHYS_HI_F', 'TOT_RS_NONIDEA_PHYS_M', 'TOT_RS_NONIDEA_PHYS_F', # physical restraint 
         'SCH_RS_NONIDEA_SECL_HI_M', 'SCH_RS_NONIDEA_SECL_HI_F', 'TOT_RS_NONIDEA_SECL_M', 'TOT_RS_NONIDEA_SECL_M') # seclusion
saveRDS(restraint, 'SchRestraint.Rdata')   

## funding... how to interpret?
controls <- schoolMaster %>%
  select('SCHID', 'LEAID',
         'SCH_SAL_TOTPERS_WFED', 'SCH_NPE_WFED', 'SCH_TEACHERS_CURR_TOT', # personal salaries, other expenditures, teachers
         'SCH_GRADE_G01', 'SCH_GRADE_G02', 'SCH_GRADE_G03', 'SCH_GRADE_G04', 'SCH_GRADE_G05', 'SCH_GRADE_G06', # students in grade
         'SCH_GRADE_G07', 'SCH_GRADE_G08', 'SCH_GRADE_G09', 'SCH_GRADE_G10', 'SCH_GRADE_G11', 'SCH_GRADE_G12') 
saveRDS(controls, 'SchControls.Rdata')             


stargazer(ged, out="../../Output/Summary/EduDFGed.tex", title="GED Completions", summary = TRUE)

## AP next


#### SPATIAL MERGE ####
# Strategy: do all spatial computations with the LEAs, merge in to school data as needed
schoolLoc <- rgdal::readOGR("2015-16-crdc-data/Output/Export_Output.shp")
leas <- spTransform(schoolLoc, CRS("+proj=longlat +datum=NAD83"))

## need to match up data to counties and merge

instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata")
counties <- rgdal::readOGR("../instrument/nhgis0002_shapefile_tl2000_us_county_1990/US_county_1990.shp")
counties<-spTransform(counties, CRS("+proj=longlat +datum=NAD83"))
counties@data <- counties@data %>%
  mutate(stateCounty = paste0(STATE,COUNTY))

### need a spatial level county dataset
instrument <- instrument %>%
  rename_all(~ paste0("orig",.)) %>%
  rename(COUNTY = origcounty, STATE = origstate) %>%
  mutate(STATE = str_pad(STATE,3,side="right","0"), COUNTY = str_pad(COUNTY,4,side="right","0"),
         stateCounty = paste0(STATE,COUNTY)) %>%
  filter(!is.na(COUNTY) & !is.na(STATE))

### distances to contours 
contours1 <- readRDS('../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
leas_project <- spTransform(leas, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

contourLEADist <- gDistance(contours_project,leas_project, byid = TRUE)
contourLEAMinDist <- apply(contourLEADist,1,FUN=min)  # 428 counties that intersect!
saveRDS(contourLEAMinDist,'contourLEAMinDist.Rdata')
stargazer(matrix(contourLEAMinDist,ncol=1), out="../../Output/Summary/ContourLEAMinDist.tex", title="Contour-LEA Minimum Distances", summary = TRUE)

contours_poly <-SpatialPolygons(
  lapply(1:length(contours_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(contours_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
contourLEAIntersect <- gIntersects(contours_poly,leas_project, byid = TRUE)
contourLEAIntersect[contourLEAIntersect == "FALSE"] <- 0
contourLEAIntersect[contourLEAIntersect == "TRUE"] <- 1
contourLEAInterAll <- apply(contourLEAIntersect,1,FUN=sum)  
saveRDS(contourLEAInterAll,'contourLEAInterAll.Rdata')
stargazer(matrix(contourLEAInterAll,ncol=1), out="../../Output/Summary/ContourLEAInterAll.tex", title="LEA Within Contours", summary = TRUE)

contourLEAMinDist <- readRDS('contourLEAMinDist.Rdata')
contourLEAInterAll <- readRDS('contourLEAInterAll.Rdata')

leas@data <- leas@data %>%
  mutate(minDist = contourLEAMinDist, inside = contourLEAInterAll)

### merge in county level data
leaCountyLink <- over(leas,counties,returnList = FALSE)
leas@data <- leas@data %>%
  mutate(stateCounty = leaCountyLink$stateCounty)

leas2 <- merge(leas, instrument, by = 'stateCounty', all.x = TRUE)
saveRDS(leas2, 'LEAReadyRaster.Rdata')



######## mechanism analysis ########
# need to merge only Univision/Telemundo
# only detect by LEA, and then can merge into main dataset

contours1 <- readRDS('../instrument/spanishCountourSLDF.Rdata')
contours <- spTransform(contours1, CRS("+proj=longlat +datum=NAD83"))
contours_project <- spTransform(contours1, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# station data
station_word_data <- readRDS("../transcripts/station_word_clean.Rdata")

telemundo <- station_word_data %>%
  filter(parent == "telemundo")
univision <- station_word_data %>%
  filter(parent == "univision")

telemundo_contours <- merge(contours,telemundo,by.x = 'simpleCall', by.y = 'callSign', all.x = FALSE)
univision_contours <- merge(contours,univision,by.x = 'simpleCall', by.y = 'callSign', all.x = FALSE)
telemundo_project <- spTransform(telemundo_contours, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
univision_project <- spTransform(univision_contours, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# lea data
schoolLoc <- rgdal::readOGR("2015-16-crdc-data/Output/Export_Output.shp")
leas <- spTransform(schoolLoc, CRS("+proj=longlat +datum=NAD83"))
leas_project <- spTransform(leas, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

# intersection
telemundo_poly <-SpatialPolygons(
  lapply(1:length(telemundo_project), 
         function(i) Polygons(lapply(coordinates(contours_project)[[i]], function(y) Polygon(y)), as.character(i))))
crs(telemundo_poly) <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
telemundoLEAIntersect <- gIntersects(telemundo_poly,leas_project, byid = TRUE)
telemundoLEAIntersect[telemundoLEAIntersect == "FALSE"] <- 0
telemundoLEAIntersect[telemundoLEAIntersect == "TRUE"] <- 1
telemundoLEAInterAll <- apply(telemundoLEAIntersect,1,FUN=sum)  
saveRDS(telemundoLEAInterAll,'telemundoLEAInterAll.Rdata')




# let's see if we can get something more fine than school districts - school name and state might get us close enough
# state and school is 90,962/96,360 and 96,326 for LEA


## of interest
#   see general school features
#   chronic absenteeism
#   suspension, expulsion (transfers?)
#   referrals, arrests, bullying