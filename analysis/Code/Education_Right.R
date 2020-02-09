###### Education: Main Regs #####

library(stargazer)
library(dplyr)
library(purrr)
library(stringr)
library(sandwich)
library(lfe)
library(ggplot2)
library(DescTools)
library(glmnet)
library(hdm)
library(pscl)
library(spdep)
library(spatialreg)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}



cleanSchoolAll <- readRDS("CleanSchlAll.Rdata")

harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

label_spec3 <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 '\\# Hispanic Students')

om1 <- lm(ihs(sch_absent_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_absent_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_absent_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_absentIHS_spec3.tex", title="Effect of TV on IHS(\\# Hispanic Chronically Absent)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Chronically Absent)')

om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_OOSOLSIHS_spec3.tex", title="Effect of TV on IHS(Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Out of School Suspension)')

om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc + tot_discwodis_multoos ,data=harass)

om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  +
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc + tot_hbreported_rac,data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_harhOLSIHS_spec3.tex", title="Effect of TV on IHS(Hispanic \\# Harassment Victims)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                  'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')

om1 <- lm(ihs(sch_hbdisciplined_rac_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_hbdisciplined_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbdisciplined_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
# om4 <- lm(ihs(sch_hbdisciplined_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
#             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#             origpcHisp + origLogInc + tot_hbdisciplined_rac,data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_harpOLSIHS_spec3.tex", title="Effect of TV on IHS(Hispanic \\# Harassment Perpetrators)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Perpetrators of Harassment)')

om1 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_apenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
# om4 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
#             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#             origpcHisp + origLogInc + tot_apenr,data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_aptIHS_spec3.tex", title="Effect of TV on APs Taken",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = '\\# IHS(Hispanic Students Taking AP)')

om1 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
# om4 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
#             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#             origpcHisp + origLogInc + tot_appass_oneormore,data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_appIHS_spec3.tex", title="Effect of TV on APs Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Passing AP)')


om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
# om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
#             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#             origpcHisp + origLogInc + tot_lepenr,data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_lepIHS_spec3.tex", title="Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic \\# Limited English Proficiency)')


om1 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
# om4 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
#             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#             origpcHisp + origLogInc + tot_gtenr ,data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_giftedIHS_spec3.tex", title="Effect of TV on IHS(Gifted)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic \\# Gifted Students)')


harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))


om1 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)

m1 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)

m6 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + TV*dist2 +
           origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)


harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 50000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

om3 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)

m3 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist +
           origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)

om5 <- felm(ihs(sch_gtenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  +
             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
             origpcHisp + origLogInc  | STATE | 0 | 0 ,data=harass)

m5 <- felm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  +
               total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
               origpcHisp + origLogInc  | 0 | 0 | STATE ,data=harass)

om2 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc + sch_gtenr_hi,data=harass)

m2 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist +
           origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + tot_appass_oneormore, data=harass)

harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 33000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

om4 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)

m4 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist +
           origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)


stargazer(om1, om3, om5, om2, om4, out = "../../Output/Regs/edu_giftedIHS_spec3_robust.tex", title="Robustness Check - Gifted Students",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3,'Total Gifted Students'),
          dep.var.labels = 'IHS(Hispanic Gifted Students)')

stargazer(m1, m6, m3, m5, m2, m4, out = "../../Output/Regs/edu_harhOLSIHS_spec3_robust.tex", title="Robustness Check - APs Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes',
                   'dist2','TV:dist2'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3,'Total APs Passed'),
          dep.var.labels = 'IHS(Hispanic APs Passed)')


# 
# om4 <- felm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  +
#               total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#               origpcHisp + origLogInc  | LEAID | 0 | 0 ,data=harass)

# Spatial Autocorr
# credit: https://rpubs.com/corey_sparks/109650

harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome)) %>%
  filter(!is.na(sch_hbreported_rac_hi) & !is.na(TV) & !is.na(origdist) &
           !is.na(origpcHisp) & !is.na(hisp_students))

m1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
           origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)

set.seed(42)
jitterX <- runif(n = nrow(harass), min = -.01, max = .01)
jitterY <- runif(n = nrow(harass), min = -.01, max = .01)
harass <- harass %>%
  mutate(X = X + jitterX,
         Y = Y + jitterY)
coordinates(harass) <- c('X','Y')
nb4<-knearneigh(coordinates(harass), k=4, longlat = TRUE)
nb4 <- knn2nb(nb4)
wt4<-nb2listw(nb4, style="W")

lm.morantest(m1,listw=wt4, zero.policy = TRUE)
m1.lag <- lagsarlm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
           origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass,
         listw=wt4, type="lag",method="MC")
summary(m1.lag, Nagelkerke=T)
m1.lag <- lagsarlm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
                     origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                     total_students, data=harass,
                   listw=wt4, type="lag",method="MC")
m2.lag <- lagsarlm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
                     origpcHisp + origLogInc + origLogPop +  hisp_students, data=harass,
                   listw=wt4, type="lag",method="MC")
summary(m2.lag, Nagelkerke=T)
m1.err <- errorsarlm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
                     origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                     total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass,
                   listw=wt4, etype="error",method="MC")
summary(m1.err, Nagelkerke=T)
m2.err <- errorsarlm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
                       origpcHisp + origLogInc + origLogPop +  hisp_students, data=harass,
                     listw=wt4, etype="error",method="MC")
summary(m2.err, Nagelkerke=T)
m1.durb <- lagsarlm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
                       origpcHisp + origLogInc + origLogPop +  hisp_students, data=harass,
                     listw=wt4, type="mixed",method="MC")
summary(m1.durb, Nagelkerke=T)

W <- as(wt4, "CsparseMatrix")
trMC <- trW(W, type="MC")
im.durb <- impacts(m1.durb, tr=trMC, R=100)
im.lag <- impacts(m1.lag, tr=trMC, R=100)

om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)

stargazer(om1,m2.lag, m2.err, out = "../../Output/Regs/edu_harhOLSIHS_spec3_spatial.tex", title="Spatial Robustness - Harassment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          keep = c('TV' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')




spplot(spdat,"mortrate", at=quantile(spdat$mortrate), col.regions=brewer.pal(n=5, "Reds"), main="Spatial Distribution of US Mortality Rate")

# conleyHAC(ihs(harass$sch_hbreported_rac_hi),m1$fitted.values,harass$TV,harass[,c("X","Y")],coefficients=m1$coefficients,timeid=harass$Country,panelid=harass$Country,dist_cutoff=10000,lag_cutoff=10)






#####VECTORISED FUNCTION
# credit: https://gist.github.com/devmag/f18ec223df7aef78402b
library(data.table)
library(geosphere)
library(foreign)
library(lfe)
library(reshape)


iterateObs<-function(y1,e1,X1,fordist,coefficients,cutoff=250000) {
  
  ##recognise whether it is lat/long or single dimension (i.e. time) for distance computation
  if(ncol(fordist)==2) {
    distances<-lapply(1:nrow(X1), function(k) distHaversine(fordist[k,],as.matrix(fordist)))
    XeeXhs<-lapply(1:nrow(X1), function(k) (  t(t(X1[k,])) %*% matrix(nrow=1,ncol=nrow(X1),data=e1[k])   * (matrix(nrow=length(coefficients),ncol=1,1)%*% (t(e1) * (distances[[k]]<=cutoff)))) %*% X1) 
    
  } else {
    abstimediff <-lapply(1:nrow(fordist), function(k) abs(fordist[k]-fordist) )
    window<-lapply(1:nrow(fordist), function(k) ((abstimediff[[k]] <= cutoff) * (1-abstimediff[[k]])/(cutoff+1)) * (fordist!=fordist[k])  )	
    
    XeeXhs<-lapply(1:nrow(X1), function(k) (  t(t(X1[k,])) %*% matrix(nrow=1,ncol=nrow(X1),data=e1[k])   * (matrix(nrow=length(coefficients),ncol=1,1)%*% (t(e1) * (t(window[[k]]))))) %*% X1) 
  }
  XeeXhs <- Reduce("+", XeeXhs)
  
  XeeXhs
}
###try to vectorise the function



conleyHAC<-function(y,yhat,X,coords,coefficients=NULL,timeid,panelid, dist_cutoff=200000,lag_cutoff=3) {
  
  e<-y-yhat
  XeeX = matrix(data=0, nrow=length(coefficients),ncol=length(coefficients))
  times<-names(table(timeid))
  n=length(y)
  X<-as.matrix(X)
  
  ###spatial correlation correction
  XeeXhs<-lapply(times, function(x) iterateObs(y[timeid==x],e[timeid==x],matrix(X[timeid==x,]),as.matrix(coords[timeid==x,]),coefficients,cutoff=dist_cutoff))
  
  ##first reduce
  XeeX <- Reduce("+", XeeXhs)
  
  invXX = solve(t(X)%*%X) * n
  
  XeeX_spatial = XeeX / n
  
  V = (invXX %*% XeeX_spatial %*% invXX) / n
  
  V
  
  
  #####serial correlation correction
  panel<-names(table(panelid))
  XeeXhs<-lapply(panel, function(x) iterateObs(y[panelid==x],e[panelid==x],matrix(X[panelid==x,]),matrix(timeid[panelid==x]),coefficients,cutoff=lag_cutoff))
  XeeX <- XeeX+Reduce("+", XeeXhs)
  
  XeeX_spatial_HAC = XeeX / n
  
  V = (invXX %*% XeeX_spatial_HAC %*% invXX) / n
  
  V
  
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}