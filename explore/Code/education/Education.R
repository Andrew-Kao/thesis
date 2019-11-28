###### Education Regs ####

library(stargazer)
library(dplyr)
library(sp)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}

educ <- readRDS('LEAReadyRaster.Rdata')
leaMaster <- fread('2015-16-crdc-data/Data Files and Layouts/CRDC 2015-16 LEA Data.csv') %>%
  select(LEAID)
educ@data <- educ@data %>%
  mutate(LEAID = leaMaster$LEAID)
  
## how to deal with non-uniqueness  (i.e., we're going to have to geolocate schools)


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

### GEED REGS ###

educ <- merge(educ, cleanGED, by = 'LEAID', all.x = TRUE)
educData <- educ@data

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



