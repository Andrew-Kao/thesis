###### Education Regs ####


# TODO: 
# 1. clean data and get summary stats
# 2. tease out causal relationships that make sense
# 3. there must be a way to get graduation data.
# 4. do as Ferrell commands
# 5. read spatial RD metrics
# 6. 

library(stargazer)
library(dplyr)
library(sp)
library(purrr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}

educ <- readRDS('LEAReadyRaster.Rdata')
leaMaster <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 LEA Data.csv') %>%
  select(LEAID)
educ@data <- educ@data %>%
  mutate(LEAID = leaMaster$LEAID)
  
## how to deal with non-uniqueness  (i.e., we're going to have to geolocate schools)

#### master school data ####

special <- readRDS('SchSpecial.Rdata') %>%
  left_join(educ@data, by = 'LEAID' ) %>%
  mutate(schlea = paste0(SCHID,LEAID)) %>%
  select(-LEAID, -SCHID) %>%
  filter(schlea != 133602430 & schlea != 242100540 & schlea !=  6482803360 & schlea !=  6522803360 & schlea != 6612803450)

dataList <- c('SchEnroll.Rdata', 'SchGifted.Rdata', 'SchAlg1.Rdata', 'SchCalc.Rdata', 'SchAP.Rdata',
              'SchExam.Rdata', 'SchAbsent.Rdata', 'SchPunish.Rdata', 'SchSuspend.Rdata',
              'SchExpulsion.Rdata', 'SchTransfer.Rdata', 'SchLaw.Rdata', 'SchHarass.Rdata',
              'SchRestraint.Rdata')

mergeByName <- function(n1,n2) {
  readRDS(n2) %>%
    mutate(schlea = paste0(SCHID,LEAID)) %>%
    filter(schlea != 133602430 & schlea != 242100540 & schlea !=  6482803360 & schlea !=  6522803360 & schlea != 6612803450) %>%
    select(-LEAID, -SCHID) %>%
    right_join(n1, by = 'schlea', copy = TRUE)
}


schoolAll <- reduce(dataList,.f = mergeByName, .init = special)

varList <- c('SCH_HBREPORTED_RAC_HI_','TOT_HBREPORTED_RAC_' )

# cred: https://stackoverflow.com/questions/59294898/creating-a-function-in-dplyr-that-operates-on-columns-through-variable-string-ma
adder <- function(data, name) {
  data %>%
    mutate(!! name := select(., starts_with(name)) %>% 
             map(function(x) ifelse(x <0, NA,x)) %>% 
             reduce(`+`))
}

cleanSchoolAll <- reduce(varList, .f = function(data,varname) adder(data,varname), .init = schoolAll)

test <- adder(schoolAll, 'SCH_HBREPORTED_RAC_HI_')

test <- schoolAll %>%
  mutate(y = adder("SCH_HBREPORTED_RAC_HI_"))

cleanSchoolAll <- schoolAll %>%
  mutate(hisp_students = SCH_ENR_HI_M + SCH_ENR_HI_F, total_students = TOT_ENR_M + TOT_ENR_F,
         hisp_harassVicRaceDum = if_else( SCH_HBREPORTED_RAC_HI_M > 0 | SCH_HBREPORTED_RAC_HI_F > 0,1,0),  # dummies 
         total_harassVicRaceDum = if_else( TOT_HBREPORTED_RAC_M > 0 | TOT_HBREPORTED_RAC_F > 0,1,0),
         hisp_offendVicRaceDum = if_else( SCH_HBDISCIPLINED_RAC_HI_M > 0 | SCH_HBDISCIPLINED_RAC_HI_F > 0,1,0),
         total_offendVicRaceDum = if_else( TOT_HBDISCIPLINED_RAC_M > 0 | TOT_HBDISCIPLINED_RAC_F > 0,1,0),
         hisp_harassVicRace = max(0, as.integer(SCH_HBREPORTED_RAC_HI_M)) + max(0, as.integer(SCH_HBREPORTED_RAC_HI_F)),         # raw count
         hisp_harassVicRaceRate = hisp_harassVicRace/hisp_students,                                      # % students
         total_harassVicRace = max(0, TOT_HBREPORTED_RAC_M) + max(0, TOT_HBREPORTED_RAC_M),
         total_harassVicRaceRate = total_harassVicRace/total_students,
         his_harassVicComp = hisp_harassVicRace/total_harassVicRace                                      # % of all incidents 
  )

  

summaryData <- cleanSchoolAll %>%
  select(hisp_harassVicRace, hisp_harassVicRaceRate, total_harassVicRace, total_harassVicRaceRate)
stargazer(cleanSchoolAll, out = "../../Output/Summary/EduClean.tex", title="School Level Summary Statistics",
          keep = c('hisp_harassVicRace', 'total_harassVicRace'),
          covariate.labels = c('Hispanic Harassment Victims', 'All Harassment Victims',
                               '\\% of Hispanic Students Harassment Victims', '\\% of All Students Harassment Victims'),
          notes = "Harassment restricted to rase-based harassment", summary = TRUE)


#### GED ####
ged <- readRDS('LEAGED.Rdata')


# how much obscured data are we working with?
cleanGED <- ged %>%
  mutate(qualityFlag = ifelse(LEA_GEDCRED_HI_F == -2 | LEA_GEDCRED_HI_M == -2 , 1,0),  ### TODO: fix qualityFlag
         LEA_GEDPART_HI_F = ifelse(LEA_GEDPART_HI_F == -2, 0, LEA_GEDPART_HI_F),
         LEA_GEDPART_HI_M = ifelse(LEA_GEDPART_HI_M == -2, 0, LEA_GEDPART_HI_M),
         LEA_GEDCRED_HI_F = ifelse(LEA_GEDCRED_HI_F == -2, 0, LEA_GEDCRED_HI_F),
         LEA_GEDCRED_HI_M = ifelse(LEA_GEDCRED_HI_M == -2, 0, LEA_GEDCRED_HI_M),
         TOT_GEDPART_F = ifelse(TOT_GEDPART_F == -2, 0, TOT_GEDPART_F),
         TOT_GEDPART_M = ifelse(TOT_GEDPART_M == -2, 0, TOT_GEDPART_M),
         TOT_GEDCRED_F = ifelse(TOT_GEDCRED_F == -2, 0, TOT_GEDCRED_F),
         TOT_GEDCRED_M = ifelse(TOT_GEDCRED_M == -2, 0, TOT_GEDCRED_M),
         hisp_attempt_ged = LEA_GEDPART_HI_F + LEA_GEDPART_HI_M,
         hisp_cred_ged = LEA_GEDCRED_HI_F + LEA_GEDCRED_HI_M,
         total_attempt_ged = TOT_GEDPART_F + TOT_GEDPART_M,
         total_cred_ged = TOT_GEDCRED_F + TOT_GEDCRED_M)

### GED REGS ###

educGed <- merge(educ, cleanGED, by = 'LEAID', all.x = TRUE)
educData <- educGed@data

# distances : 100 km
distDummy <- educData %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 & hisp_attempt_ged >0 ) %>%
  mutate(pcHisp_ged = hisp_cred_ged/hisp_attempt_ged, pcTot_ged = total_cred_ged/total_attempt_ged,
         origdist = origdist/1000) %>%
  mutate(origLogPop = log(origpopulation), origLogInc = log(origincome))

m1 <- lm(pcHisp_ged ~ TV*origdist,data=distDummy)
m2 <- lm(pcHisp_ged ~ TV*origdist + origLogPop + origpcHisp,data=distDummy)
m3 <- lm(pcHisp_ged ~ TV*origdist + origLogPop + origpcHisp + origLogInc,data=distDummy)
m4 <- lm(pcHisp_ged ~ TV*origdist + origLogPop + origpcHisp + origLogInc + pcTot_ged,data=distDummy)
stargazer(m1, m2, m3, m4, out = "../../Output/Regs/edu_gedh.tex", title="Effect of TV on Hispanic \\% GED Completed",
          notes = "Distance in KM, 100 KM cutoff")

# distances : 50 km
distDummy <- educData %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 50000 & hisp_attempt_ged >0 ) %>%
  mutate(pcHisp_ged = hisp_cred_ged/hisp_attempt_ged, pcTot_ged = total_cred_ged/total_attempt_ged,
         origdist = origdist/1000) %>%
  mutate(origLogPop = log(origpopulation), origLogInc = log(origincome))

m1 <- lm(pcHisp_ged ~ TV*origdist,data=distDummy)
m2 <- lm(pcHisp_ged ~ TV*origdist + origLogPop + origpcHisp,data=distDummy)
m3 <- lm(pcHisp_ged ~ TV*origdist + origLogPop + origpcHisp + origLogInc,data=distDummy)
m4 <- lm(pcHisp_ged ~ TV*origdist + origLogPop + origpcHisp + origLogInc + pcTot_ged,data=distDummy)
stargazer(m1, m2, m3, m4, out = "../../Output/Regs/edu_gedh50.tex", title="Effect of TV on Hispanic \\% GED Completed",
          notes = "Distance in KM, 50 KM cutoff")





#### Gifted ####
enroll <- readRDS('SchEnroll.Rdata') %>%
  mutate(schlea = paste0(SCHID,LEAID)) %>%
  select(-LEAID)
gifted <- readRDS('SchGifted.Rdata')%>%
  filter(SCH_GT_IND == "Yes") %>%
  mutate(schlea = paste0(SCHID,LEAID)) %>%
  left_join(enroll, by = 'schlea') %>%
  mutate(pcHisp_gifted = (SCH_GTENR_HI_M + SCH_GTENR_HI_F)/(SCH_ENR_HI_M + SCH_ENR_HI_F),
         pcTot_gifted = (TOT_GTENR_M + TOT_GTENR_F)/(TOT_ENR_M + TOT_ENR_F)) %>%
  left_join(educ@data, by = 'LEAID' )

### Gifted regs ###

# distances : 100 km
distDummy <- gifted %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

m1 <- lm(pcHisp_gifted ~ TV*origdist,data=distDummy)
m2 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp,data=distDummy)
m3 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp + origLogInc,data=distDummy)
m4 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp + origLogInc + pcTot_gifted,data=distDummy)
stargazer(m1, m2, m3, m4, out = "../../Output/Regs/edu_giftedh.tex", title="Effect of TV on Hispanic \\% Gifted",
          notes = "Distance in KM, 100 KM cutoff",
          omit.stat = c('f','ser'))

# distances : 50 km
distDummy <- gifted %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 50000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

m1 <- lm(pcHisp_gifted ~ TV*origdist,data=distDummy)
m2 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp,data=distDummy)
m3 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp + origLogInc,data=distDummy)
m4 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp + origLogInc + pcTot_gifted,data=distDummy)
stargazer(m1, m2, m3, m4, out = "../../Output/Regs/edu_giftedh50.tex", title="Effect of TV on Hispanic \\% Gifted",
          notes = "Distance in KM, 50 KM cutoff",
          omit.stat = c('f','ser'))

# distances : 25 km
distDummy <- gifted %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 25000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

m1 <- lm(pcHisp_gifted ~ TV*origdist,data=distDummy)
m2 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp,data=distDummy)
m3 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp + origLogInc,data=distDummy)
m4 <- lm(pcHisp_gifted ~ TV*origdist + origLogPop + origpcHisp + origLogInc + pcTot_gifted,data=distDummy)
stargazer(m1, m2, m3, m4, out = "../../Output/Regs/edu_giftedh25.tex", title="Effect of TV on Hispanic \\% Gifted",
          notes = "Distance in KM, 25 KM cutoff",
          omit.stat = c('f','ser'))













#### Algebra ####




