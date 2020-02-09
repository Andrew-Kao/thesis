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



if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}



cleanSchoolAll <- readRDS("CleanSchoolAll.Rdata")

harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

label_spec3 <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 '\\# Hispanic Students')

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
# om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
#             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#             origpcHisp + origLogInc + tot_hbreported_rac,data=harass)
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

stargazer(m1, m3, m5, m2, m4, out = "../../Output/Regs/edu_harhOLSIHS_spec3_robust.tex", title="Robustness Check - APs Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3,'Total APs Passed'),
          dep.var.labels = 'IHS(Hispanic APs Passed)')


# 
# om4 <- felm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  +
#               total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
#               origpcHisp + origLogInc  | LEAID | 0 | 0 ,data=harass)

# Spatial Autocorr
set.seed(42)
jitter <- runif(n = nrow(harass), min = -.001, max = .001)
harass <- harass %>%
  mutate(jitter = X + jitter)
coordinates(harass) <- c('X','Y')
nb4<-knearneigh(coordinates(harass), k=10, longlat = TRUE)

