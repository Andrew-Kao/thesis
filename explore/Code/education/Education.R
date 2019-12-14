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
library(stringr)
library(sandwich)
library(lfe)
library(ggplot2)
library(DescTools)
library(glmnet)
library(hdm)
library(pscl)

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


## merge the data back together

special <- readRDS('SchSpecial.Rdata') %>%
  left_join(educ@data, by = 'LEAID' ) %>%
  mutate(schlea = paste0(SCHID,LEAID)) %>%
  filter(schlea != 133602430 & schlea != 242100540 & schlea !=  6482803360 & schlea !=  6522803360 & schlea != 6612803450)

dataList <- c('SchEnroll.Rdata', 'SchGifted.Rdata', 'SchAlg1.Rdata', 'SchCalc.Rdata', 'SchAP.Rdata',
              'SchExam.Rdata', 'SchAbsent.Rdata', 'SchPunish.Rdata', 'SchSuspend.Rdata',
              'SchExpulsion.Rdata', 'SchTransfer.Rdata', 'SchLaw.Rdata', 'SchHarass.Rdata',
              'SchRestraint.Rdata', 'SchControls.Rdata', 'SchRetention.Rdata')

mergeByName <- function(n1,n2) {
  readRDS(n2) %>%
    mutate(schlea = paste0(SCHID,LEAID)) %>%
    filter(schlea != 133602430 & schlea != 242100540 & schlea !=  6482803360 & schlea !=  6522803360 & schlea != 6612803450) %>%
    select(-LEAID, -SCHID) %>%
    right_join(n1, by = 'schlea', copy = TRUE)
}

schoolAll <- reduce(dataList,.f = mergeByName, .init = special)
saveRDS(schoolAll, file='SchAll.Rdata')

## clean the variables
schoolAll <- readRDS('SchAll.Rdata')

varList <- c('SCH_HBREPORTED_RAC_HI_','TOT_HBREPORTED_RAC_', 'SCH_HBDISCIPLINED_RAC_HI_', 'TOT_HBDISCIPLINED_RAC_',
             'SCH_DISCWODIS_EXPWE_HI_M', 'SCH_DISCWODIS_SINGOOS_HI_', 'SCH_ABSENT_HI_')

# cred: https://stackoverflow.com/questions/59294898/creating-a-function-in-dplyr-that-operates-on-columns-through-variable-string-ma
adder <- function(data, name) {
  data %>%
    mutate(!! str_to_lower(str_sub(name, end=-2)) := select(., starts_with(name)) %>% 
             map(function(x) ifelse(x <0, NA,x)) %>% 
             reduce(`+`))
}

cleanSchoolAll <- reduce(varList, .f = function(data,varname) adder(data,varname), .init = schoolAll) %>%
  mutate(hisp_students = SCH_ENR_HI_M + SCH_ENR_HI_F, total_students = TOT_ENR_M + TOT_ENR_F,
         hisp_harassVicRaceDum = if_else(sch_hbreported_rac_hi > 0,1,0),  # dummies 
         total_harassVicRaceDum = if_else(tot_hbreported_rac > 0,1,0),
         hisp_offendVicRaceDum = if_else(sch_hbdisciplined_rac_hi > 0,1,0),
         total_offendVicRaceDum = if_else(tot_hbdisciplined_rac > 0,1,0),
         hisp_harassVicRaceRate = sch_hbreported_rac_hi/hisp_students * 100,                                      # % students
         total_harassVicRaceRate = tot_hbreported_rac/total_students * 100,
         his_harassVicComp = sch_hbreported_rac_hi/tot_hbreported_rac * 100,                                      # % of all incidents 
         hisp_harassOffRaceDum = if_else(sch_hbdisciplined_rac_hi > 0,1,0),  # dummies 
         total_harassOffRaceDum = if_else(tot_hbdisciplined_rac > 0,1,0),
         hisp_OOSDum = if_else(sch_discwodis_singoos_hi > 0,1,0),
         hisp_AbsDum = if_else(sch_absent_hi > 0,1,0)
  )

  

summaryData <- cleanSchoolAll %>%
  select(sch_hbreported_rac_hi, hisp_harassVicRaceRate, tot_hbreported_rac, total_harassVicRaceRate,
         sch_hbdisciplined_rac_hi, tot_hbdisciplined_rac)
stargazer(summaryData, out = "../../Output/Summary/EduClean.tex", title="School Level Summary Statistics",
          covariate.labels = c('Hispanic Harassment Victims', '\\% of Hispanic Students Harassment Victims',
                               'All Harassment Victims', '\\% of All Students Harassment Victims',
                               'Hispanic Harassment Offenders', 'All Harassment Offenders'),
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




##### Harrassment ####
# distances : 100 km
distDummy <- cleanSchoolAll %>%
  filter(!is.na(hisp_harassVicRaceRate)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

m1 <- felm(hisp_harassVicRaceRate ~ TV*origdist |0|0|LEAID,data=distDummy, keepX = TRUE)
m2 <- felm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
             origpcHisp |0|0|LEAID ,data=distDummy)
m3 <- felm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc |0|0|LEAID,data=distDummy)
m4 <- felm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT |0|0|LEAID,data=distDummy)
stargazer(m1, m2, m3, m4, out = "../../Output/Regs/edu_harh.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )

# TODO: https://stackoverflow.com/questions/38886442/formatting-notes-in-rs-stargazer-tables
note.latex <- "\\parbox[t]{\\textwidth}{Distance in KM, 100 KM cutoff. Demographic controls at county level. Errors clustered by school district aoesnuhasnteouh saeotnusaou  aoesutn}"
star[grepl("Note",star)] <- note.latex
# mess with this later
# \begin{tablenotes}[flushleft]
# \item \textit{Notes:}  No flexible betas used in the IV. Adjustment involves adjusting voteshare by national Hispanic voterate to compute non-Arab/Latin American voteshares.   \end{tablenotes}
cat(star, sep = "\n", file = "../../Output/Regs/edu_harh.tex")

# OLS
om1 <- lm(hisp_harassVicRaceRate ~ TV*origdist ,data=distDummy)
om2 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
             origpcHisp, data=distDummy)
om3 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc,data=distDummy)
om4 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=distDummy)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLS.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )

# next: regression diagnostics and model building
diagnostics <- distDummy %>%
  mutate( rs1 = rstudent(om1), rs2 = rstudent(om2), rs3 = rstudent(om3), rs4 = rstudent(om4),
          y1 = predict(om1,distDummy), y2 = predict(om2,distDummy), y3 = predict(om3,distDummy), y4 = predict(om4,distDummy)
      )
ggplot(data=diagnostics) + geom_point(aes(x=y1,y=rs1))
ggsave('../../Output/Diagnostics/edu_harOLSPlot1.pdf')
ggplot(data=diagnostics) + geom_histogram(aes(x=rs1), bins = 15)
ggsave('../../Output/Diagnostics/edu_harOLSHist1.pdf')
ggplot(data=diagnostics) + geom_qq(aes(sample=rs1)) + geom_abline(intercept=0,slope=1)
ggsave('../../Output/Diagnostics/edu_harOLSQQ1.pdf')
ggplot(data=diagnostics) + geom_point(aes(x=y2,y=rs2))
ggsave('../../Output/Diagnostics/edu_harOLSPlot2.pdf')
ggplot(data=diagnostics) + geom_histogram(aes(x=rs2), bins = 15)
ggsave('../../Output/Diagnostics/edu_harOLSHist2.pdf')
ggplot(data=diagnostics) + geom_qq(aes(sample=rs2)) + geom_abline(intercept=0,slope=1)
ggsave('../../Output/Diagnostics/edu_harOLSQQ2.pdf')
ggplot(data=diagnostics) + geom_point(aes(x=y3,y=rs3))
ggsave('../../Output/Diagnostics/edu_harOLSPlot3.pdf')
ggplot(data=diagnostics) + geom_histogram(aes(x=rs3), bins = 15)
ggsave('../../Output/Diagnostics/edu_harOLSHist3.pdf')
ggplot(data=diagnostics) + geom_qq(aes(sample=rs3)) + geom_abline(intercept=0,slope=1)
ggsave('../../Output/Diagnostics/edu_harOLSQQ3.pdf')
ggplot(data=diagnostics) + geom_point(aes(x=y4,y=rs4))
ggsave('../../Output/Diagnostics/edu_harOLSPlot4.pdf')
ggplot(data=diagnostics) + geom_histogram(aes(x=rs4), bins = 15)
ggsave('../../Output/Diagnostics/edu_harOLSHist4.pdf')
ggplot(data=diagnostics) + geom_qq(aes(sample=rs4)) + geom_abline(intercept=0,slope=1)
ggsave('../../Output/Diagnostics/edu_harOLSQQ4.pdf')

ggplot(data=diagnostics) + geom_point(aes(y=y1,x=origdist))

## winsorize
wins <- distDummy %>%
  mutate(hisp_harassVicRaceRate = Winsorize(hisp_harassVicRaceRate,probs=c(0,.99)))
om1 <- lm(hisp_harassVicRaceRate ~ TV*origdist ,data=wins)
om2 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
            origpcHisp, data=wins)
om3 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc,data=wins)
om4 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=wins)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSWins.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )
wins <- wins %>%
  mutate( rs1 = rstudent(om1), rs2 = rstudent(om2), rs3 = rstudent(om3), rs4 = rstudent(om4),
          y1 = predict(om1,distDummy), y2 = predict(om2,distDummy), y3 = predict(om3,distDummy), y4 = predict(om4,distDummy)
  )
ggplot(data=wins) + geom_point(aes(x=y1,y=rs1))
makePlots(wins,wins$y1,wins$rs1,'../../Output/Diagnostics/edu_harOLSWins1')
makePlots(wins,wins$y2,wins$rs2,'../../Output/Diagnostics/edu_harOLSWins2')
makePlots(wins,wins$y3,wins$rs3,'../../Output/Diagnostics/edu_harOLSWins3')
makePlots(wins,wins$y4,wins$rs4,'../../Output/Diagnostics/edu_harOLSWins4')

## IHS
ihs <- distDummy %>%
  mutate(hisp_harassVicRaceRate = ihs(hisp_harassVicRaceRate))
om1 <- lm(hisp_harassVicRaceRate ~ TV*origdist ,data=ihs)
om2 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
            origpcHisp, data=ihs)
om3 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc,data=ihs)
om4 <- lm(hisp_harassVicRaceRate ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=ihs)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )
ihs <- ihs %>%
  mutate( rs1 = rstudent(om1), rs2 = rstudent(om2), rs3 = rstudent(om3), rs4 = rstudent(om4),
          y1 = predict(om1,distDummy), y2 = predict(om2,distDummy), y3 = predict(om3,distDummy), y4 = predict(om4,distDummy)
  )
makePlots(ihs,ihs$y1,ihs$rs1,'../../Output/Diagnostics/edu_harOLSIHS1')
makePlots(ihs,ihs$y2,ihs$rs2,'../../Output/Diagnostics/edu_harOLSIHS2')
makePlots(ihs,ihs$y3,ihs$rs3,'../../Output/Diagnostics/edu_harOLSIHS3')
makePlots(ihs,ihs$y4,ihs$rs4,'../../Output/Diagnostics/edu_harOLSIHS4')


# Logit on Dummy
bm1 <- glm(hisp_harassVicRaceDum ~ TV*origdist ,data=ihs, family = binomial)
bm2 <- glm(hisp_harassVicRaceDum ~ TV*origdist + origLogPop +
            origpcHisp, data=ihs, family = binomial)
bm3 <- glm(hisp_harassVicRaceDum ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc,data=ihs,family = binomial)
bm4 <- glm(hisp_harassVicRaceDum ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=ihs,family = binomial)
stargazer(bm1, bm2, bm3, bm4, out = "../../Output/Regs/edu_harhLogit.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )
logit <- distDummy %>%
  mutate( rs1 = rstudent(bm1), rs2 = rstudent(bm2), rs3 = rstudent(bm3), rs4 = rstudent(bm4),
          y1 = predict(bm1,distDummy), y2 = predict(bm2,distDummy), y3 = predict(bm3,distDummy), y4 = predict(bm4,distDummy)
  )
makePlots(logit,logit$y1,logit$rs1,'../../Output/Diagnostics/edu_harOLSLogit1')
makePlots(logit,logit$y2,logit$rs2,'../../Output/Diagnostics/edu_harOLSLogit2')
makePlots(logit,logit$y3,logit$rs3,'../../Output/Diagnostics/edu_harOLSLogit3')
makePlots(logit,logit$y4,logit$rs4,'../../Output/Diagnostics/edu_harOLSLogit4')

temp <- sample_n(logit, 300) %>%
  select(rs3,y3,TV,origdist, hisp_harassVicRaceDum)

ggplot(data = temp) + geom_point(aes(x=y3, y= rs3, colour=hisp_harassVicRaceDum))

# Logit on Dummy, dist sq
bm1 <- glm(hisp_harassVicRaceDum ~ TV*origdist + TV*I(origdist^2) ,data=ihs, family = binomial)
bm2 <- glm(hisp_harassVicRaceDum ~ TV*origdist + TV*I(origdist^2) + origLogPop +
             origpcHisp, data=ihs, family = binomial)
bm3 <- glm(hisp_harassVicRaceDum ~ TV*origdist + TV*I(origdist^2) + origLogPop +
             origpcHisp + origLogInc,data=ihs,family = binomial)
bm4 <- glm(hisp_harassVicRaceDum ~ TV*origdist +TV*I(origdist^2) + origLogPop +
             origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=ihs,family = binomial)
stargazer(bm1, bm2, bm3, bm4, out = "../../Output/Regs/edu_harhLogitSq.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'TV Dummy $\\times$ Distance\\^2',
                               'Distance to Boundary (meters)', 'Distance\\^2',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )
logit <- distDummy %>%
  mutate( rs1 = rstudent(bm1), rs2 = rstudent(bm2), rs3 = rstudent(bm3), rs4 = rstudent(bm4),
          y1 = predict(bm1,distDummy), y2 = predict(bm2,distDummy), y3 = predict(bm3,distDummy), y4 = predict(bm4,distDummy)
  )
makePlots(logit,logit$y1,logit$rs1,'../../Output/Diagnostics/edu_harOLSLogit1')
makePlots(logit,logit$y2,logit$rs2,'../../Output/Diagnostics/edu_harOLSLogit2')
makePlots(logit,logit$y3,logit$rs3,'../../Output/Diagnostics/edu_harOLSLogit3')
makePlots(logit,logit$y4,logit$rs4,'../../Output/Diagnostics/edu_harOLSLogit4')
ggplot(data=logit) + geom_point(aes(x=y2,y=rs2,colour=hisp_harassVicRaceDum))


##### Suspensions ####
suspend <- cleanSchoolAll %>%
  filter(!is.na(hisp_OOSDum)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

bm1 <- glm(hisp_OOSDum ~ TV*origdist ,data=suspend, family = binomial)
bm2 <- glm(hisp_OOSDum ~ TV*origdist + origLogPop +
            origpcHisp, data=suspend, family = binomial)
bm3 <- glm(hisp_OOSDum ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc,data=suspend,family = binomial)
bm4 <- glm(hisp_OOSDum ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=suspend,family = binomial)
stargazer(bm1, bm2, bm3, bm4, out = "../../Output/Regs/edu_OOSLogit.tex", title="Effect of TV on Hispanic Out of School Suspension Dummy",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )
logit <- suspend %>%
  mutate( rs1 = rstudent(bm1), rs2 = rstudent(bm2), rs3 = rstudent(bm3), rs4 = rstudent(bm4),
          y1 = predict(bm1,suspend), y2 = predict(bm2,suspend), y3 = predict(bm3,suspend), y4 = predict(bm4,suspend)
  )
makePlots(logit,logit$y1,logit$rs1,'../../Output/Diagnostics/edu_OOSLogit1')
makePlots(logit,logit$y2,logit$rs2,'../../Output/Diagnostics/edu_OOSLogit2')
makePlots(logit,logit$y3,logit$rs3,'../../Output/Diagnostics/edu_OOSLogit3')
makePlots(logit,logit$y4,logit$rs4,'../../Output/Diagnostics/edu_OOSLogit4')

anova(bm4,bm5, test="LRT")

base <- glm(hisp_OOSDum ~ TV*origdist ,data=suspend, family = binomial)
full <- glm(hisp_OOSDum ~ TV*origdist + origLogPop + origpcHisp + origLogInc +
              SCH_TEACHERS_CURR_TOT + SCH_SAL_TOTPERS_WFED + SCH_NPE_WFED +
              SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + SCH_GRADE_G12 +
              hisp_students + total_students
              ,data=suspend, family = binomial)
fwdBIC <- step(base, scope=formula(full), direction="forward", k=log(62438))

bm5 <- glm(hisp_OOSDum ~ TV + origdist + origLogInc + origpcHisp + 
  origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
    total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + TV:origdist,data=suspend,family = binomial)
stargazer(bm1, bm2, bm3, bm4, bm5, out = "../../Output/Regs/edu_OOSLogit.tex", title="Effect of TV on Hispanic Out of School Suspension Dummy",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students', 'Contains Grade 1', 'Contains Grade 6',
                               'Contains Grade 9'),
          dep.var.labels = 'Dummy for Hispanic Out of School Suspension')
logit <- logit %>%
  mutate( rs5 = rstudent(bm5), y5 = predict(bm5,suspend)
  )
makePlots(logit,logit$y5,logit$rs5,'../../Output/Diagnostics/edu_OOSLogit5')
            
X <- model.matrix(~ TV*origdist + origLogPop + origpcHisp + origLogInc +
                    SCH_TEACHERS_CURR_TOT + SCH_SAL_TOTPERS_WFED + SCH_NPE_WFED +
                    SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + SCH_GRADE_G12 +
                    hisp_students + total_students
                  ,data=suspend) 
#need to subtract the intercept
X <- X[,-1]
hdm.ci <- rlassoEffects(x = X, y = suspend$hisp_OOSDum, index=c("TV","origdist","TV:origdist"), family = binomial)


bm0 <- glm(hisp_OOSDum ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc + hisp_students + total_students,data=suspend,family = binomial)
z1 <- zeroinfl(sch_discwodis_singoos_hi ~ TV*origdist, data = suspend, link = "logit", dist = "poisson")
z2 <- zeroinfl(sch_discwodis_singoos_hi ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc, data = suspend, link = "logit", dist = "poisson")
z3 <- zeroinfl(sch_discwodis_singoos_hi ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = suspend, link = "logit", dist = "poisson")
stargazer(z1,z2,z3, out = "../../Output/Regs/edu_OOSZi.tex", title="Effect of TV on Hispanic Out of School Suspension Dummy, Zero-Inflated",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students', 'Contains Grade 1', 'Contains Grade 6',
                               'Contains Grade 9'),
          dep.var.labels = '\\# Hispanic Out of School Suspensions')

  # this one works...
z3 <- zeroinfl(hisp_OOSDum ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = suspend, link = "logit", dist = "poisson")


######### ABSENCES ##########

absent <- cleanSchoolAll %>%
  filter(!is.na(sch_absent_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         ihs_absent_hi = ihs(sch_absent_hi))


# Postal FE fails
# LEAID Cluster loses significance similar to Postal FE but reverse sign
m1 <- felm(ihs_absent_hi ~ TV*origdist |0|0|Postal,data=absent)
m2 <- felm(ihs_absent_hi ~ TV*origdist + origLogPop +
             origpcHisp |0|0|Postal ,data=absent)
m3 <- felm(ihs_absent_hi ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc |0|0|Postal,data=absent)
m4 <- felm(ihs_absent_hi ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT |0|0|Postal,data=absent)
m5 <- felm(ihs_absent_hi ~ TV + TV:origdist + origdist + origLogPop +
            origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT + hisp_students +
            +  total_students + SCH_GRADE_G06 | 0 |0 |Postal
          ,data=absent)
stargazer(m1, m2, m3, m4, m5, out = "../../Output/Regs/edu_abshFE.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )


# OLS
om1 <- lm(ihs_absent_hi ~ TV*origdist ,data=absent)
om2 <- lm(ihs_absent_hi ~ TV*origdist + origLogPop +
            origpcHisp, data=absent)
om3 <- lm(ihs_absent_hi ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc,data=absent)
om4 <- lm(ihs_absent_hi ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=absent)
base <- glm(ihs_absent_hi ~ TV*origdist ,data=absent)
full <- glm(ihs_absent_hi ~ TV*origdist + origLogPop + origpcHisp + origLogInc +
              SCH_TEACHERS_CURR_TOT + SCH_SAL_TOTPERS_WFED + SCH_NPE_WFED +
              SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + SCH_GRADE_G12 +
              hisp_students + total_students
            ,data=absent)
fwdBIC <- step(base, scope=formula(full), direction="forward", k=log(45952))

om5 <- lm(ihs_absent_hi ~ TV + TV:origdist + origdist + origLogPop +
            origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT + hisp_students +
               +  total_students + SCH_GRADE_G06 
            ,data=absent)
stargazer(om1, om2, om3, om4, om5, out = "../../Output/Regs/edu_absh.tex", title="Effect of TV on Hispanic Absentees",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students' ,'Contains Grade 6'),
          dep.var.labels = "IHS(Hispanic Absentees)")

# next: regression diagnostics and model building
diagnostics <- absent %>%
  mutate( rs1 = rstudent(om1), rs2 = rstudent(om2), rs3 = rstudent(om3), rs4 = rstudent(om4), rs5 = rstudent(om5),
          y1 = predict(om1,absent), y2 = predict(om2,absent), y3 = predict(om3,absent), y4 = predict(om4,absent),
          y5 = predict(om5,absent)
  )
makePlots(diagnostics,diagnostics$y1,diagnostics$rs1,'../../Output/Diagnostics/edu_AbsOLS1')
makePlots(diagnostics,diagnostics$y2,diagnostics$rs2,'../../Output/Diagnostics/edu_AbsOLS2')
makePlots(diagnostics,diagnostics$y3,diagnostics$rs3,'../../Output/Diagnostics/edu_AbsOLS3')
makePlots(diagnostics,diagnostics$y4,diagnostics$rs4,'../../Output/Diagnostics/edu_AbsOLS4')
makePlots(diagnostics,diagnostics$y5,diagnostics$rs5,'../../Output/Diagnostics/edu_AbsOLS5')

anova(om1, om2)
anova(om2, om3)
anova(om3, om4)
anova(om4, om5)


X <- model.matrix(~ TV*origdist + origLogPop + origpcHisp + origLogInc +
                    SCH_TEACHERS_CURR_TOT + SCH_SAL_TOTPERS_WFED + SCH_NPE_WFED +
                    SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + SCH_GRADE_G12 +
                    hisp_students + total_students
                  ,data=absent) 
#need to subtract the intercept
X <- X[,-1]
hdm.ci <- rlassoEffects(x = X, y = absent$ihs_absent_hi, index=c("TV","origdist","TV:origdist"), family = binomial)
summary(hdm.ci)


absent75 <- cleanSchoolAll %>%
  filter(!is.na(sch_absent_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 75000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         ihs_absent_hi = ihs(sch_absent_hi))

absent50 <- cleanSchoolAll %>%
  filter(!is.na(sch_absent_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 50000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         ihs_absent_hi = ihs(sch_absent_hi))

omd1 <- lm(ihs_absent_hi ~ TV + TV:origdist + origdist + origLogPop +
            origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT + hisp_students +
            +  total_students + SCH_GRADE_G06 
          ,data=absent)
omd2 <- lm(ihs_absent_hi ~ TV + TV:origdist + origdist + origLogPop +
             origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT + hisp_students +
             +  total_students + SCH_GRADE_G06 
           ,data=absent75)
omd3 <- lm(ihs_absent_hi ~ TV + TV:origdist + origdist + origLogPop +
             origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT + hisp_students +
             +  total_students + SCH_GRADE_G06 
           ,data=absent50)
stargazer(omd1, omd2, omd3, m5, out = "../../Output/Regs/edu_abshDist.tex", title="Effect of TV on Hispanic Absentees",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students' ,'Contains Grade 6'),
          dep.var.labels = "IHS(Hispanic Absentees)", model.names = FALSE)

### FUNCTIONS ###
makePlots <- function(data, y, r, string) {
  ggplot(data=data) + geom_point(aes(x=y,y=r))
  ggsave(paste0(string,'Plot','.pdf'))
  ggplot(data=data) + geom_histogram(aes(x=r), bins = 15)
  ggsave(paste0(string,'Hist','.pdf'))
  ggplot(data=data) + geom_qq(aes(sample=r)) + geom_abline(intercept=0,slope=1)
  ggsave(paste0(string,'QQ','.pdf'))
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


# need a more efficient implementation
student <- function(model) {
  model$residuals/(
    (1/(model$df - model$rank - 1))*(sum(model$residuals^2) - model$residuals^2) *
      diag(m1$X %*% solve(t(m1$X) %*% m1$X)  %*% t(m1$X))
  )
  
}
