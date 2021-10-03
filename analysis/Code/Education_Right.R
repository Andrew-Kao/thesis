###### Education: Main Regs #####

library(stargazer)
library(dplyr)
library(purrr)
library(stringr)
library(sandwich)
library(lmtest)
library(lfe)
library(ggplot2)
library(DescTools)
library(glmnet)
library(hdm)
library(pscl)
library(spdep)
library(spatialreg)
library(missForest)
library(fixest)


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

om1 <- lm(ihs(sch_algpass_g08_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_algpass_g08_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_algpass_g08_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_alg8IHS_spec3.tex", title="Effect of TV on Algebra Gr 8 Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Passing Gr 8 Algebra)')

om1 <- lm(ihs(sch_algpass_gs0910_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_algpass_gs0910_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_algpass_gs0910_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_alg910IHS_spec3.tex", title="Effect of TV on Algebra Gr 9-10 Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Passing Gr 9-10 Algebra)')

om1 <- lm(ihs(sch_algpass_gs1112_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_algpass_gs1112_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_algpass_gs1112_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_alg1112IHS_spec3.tex", title="Effect of TV on Algebra Gr 11-12 Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Passing Gr 11-12 Algebra)')


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

om1 <- lm(ihs(sch_apmathenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_apmathenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apmathenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_apmathIHS_spec3.tex", title="Effect of TV on AP Math Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled AP Math)')

om1 <- lm(ihs(sch_apscienr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_apscienr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apscienr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_apsciIHS_spec3.tex", title="Effect of TV on AP Science Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled AP Science)')

om1 <- lm(ihs(sch_mathenr_advm_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_mathenr_advm_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_mathenr_advm_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_advmIHS_spec3.tex", title="Effect of TV on Adv. Math Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled Adv. Math)')

om1 <- lm(ihs(sch_mathenr_calc_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_mathenr_calc_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_mathenr_calc_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_calcIHS_spec3.tex", title="Effect of TV on Calculus Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled Calculus)')

om1 <- lm(ihs(sch_scienr_biol_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_scienr_biol_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_scienr_biol_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_biolIHS_spec3.tex", title="Effect of TV on Biology Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled Biology)')

om1 <- lm(ihs(sch_scienr_chem_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_scienr_chem_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_scienr_chem_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_chemIHS_spec3.tex", title="Effect of TV on Chemisty Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled Chemistry)')

om1 <- lm(ihs(sch_scienr_phys_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_scienr_phys_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_scienr_phys_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_physIHS_spec3.tex", title="Effect of TV on Physics Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled Physics)')

om1 <- lm(ihs(sch_satact_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(sch_satact_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_satact_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_satactIHS_spec3.tex", title="Effect of TV on SAT/ACT Enrollment",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students Enrolled SAT/ACT)')

om1 <- lm(ihs(lea_gedcred_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(lea_gedcred_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(lea_gedcred_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_gedcIHS_spec3.tex", title="Effect of TV on GED Credit",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students GED Credit)')

om1 <- lm(ihs(lea_gedpart_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + hisp_students, data=harass)
om2 <- lm(ihs(lea_gedpart_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(lea_gedpart_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_gedpIHS_spec3.tex", title="Effect of TV on GED Participation",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(Hispanic Students GED Participation)')




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


origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
  total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 

sum2 <- harass %>%
  mutate(intersects_d = ifelse(TV > 0, 1, 0),
         ihsschabsent = ihs(sch_absent_hi),
         ihsschdiscwodissingoos = ihs(sch_discwodis_singoos_hi),
         ihsschhbreportedrac = ihs(sch_hbreported_rac_hi),
         ihsschapenr = ihs(sch_apenr_hi),
         ihsschappassoneormore = ihs(sch_appass_oneormore_hi),
         ihsschlepenr = ihs(sch_lepenr_hi),
         ihsschgtenr = ihs(sch_gtenr_hi),
         schteachers = SCH_TEACHERS_CURR_TOT,
         hispstudents = hisp_students,
         totalstudents = total_students) %>%
  group_by(intersects_d) %>%
  select(ihsschabsent, ihsschdiscwodissingoos, ihsschhbreportedrac,
         ihsschapenr, ihsschappassoneormore, ihsschlepenr,
         ihsschgtenr, origpcHisp, origLogInc, origLogPop, schteachers,
         hispstudents, totalstudents, intersects_d) %>%
  summarize_all(funs( mean = mean(.,na.rm=TRUE), sd = sd(.,na.rm=TRUE))) %>%
  gather("stat", "val", -intersects_d) %>%
  mutate(intersects_d = paste0("TV:", intersects_d)) %>%
  unite(stat, stat, intersects_d, sep = ".") %>%
  separate(stat, into = c("var", "stat"), sep = "_") %>%
  spread(stat, val) %>%
  select(var, "mean.TV:0", "sd.TV:0", "mean.TV:1", "sd.TV:1") %>%
  as.data.frame()

sum2a <- harass %>%
  mutate(ihsschabsent = ihs(sch_absent_hi),
         ihsschdiscwodissingoos = ihs(sch_discwodis_singoos_hi),
         ihsschhbreportedrac = ihs(sch_hbreported_rac_hi),
         ihsschapenr = ihs(sch_apenr_hi),
         ihsschappassoneormore = ihs(sch_appass_oneormore_hi),
         ihsschlepenr = ihs(sch_lepenr_hi),
         ihsschgtenr = ihs(sch_gtenr_hi),
         schteachers = SCH_TEACHERS_CURR_TOT,
         hispstudents = hisp_students,
         totalstudents = total_students) %>%
  select(ihsschabsent, ihsschdiscwodissingoos, ihsschhbreportedrac,
         ihsschapenr, ihsschappassoneormore, ihsschlepenr,
         ihsschgtenr, origpcHisp, origLogInc, origLogPop, schteachers,
         hispstudents, totalstudents) %>%
  summarize_all(funs( mean = mean(.,na.rm=TRUE), sd = sd(.,na.rm=TRUE)))


###### DIFFERENCE BETWEEN WHITES AND HISPANICS ######
hisp_to_summarize <- cleanSchoolAll %>%
  mutate(gifted_pc_hisp = sch_gtenr_hi/hisp_students,
         ap_pc_hisp = sch_apenr_hi/hisp_students,
         ap_one_pc_hisp = sch_appass_oneormore_hi/hisp_students,
         suspend = sch_discwodis_singoos_hi/hisp_students,
         absent = sch_absent_hi/hisp_students)
summary(hisp_to_summarize$gifted_pc_hisp)
summary(hisp_to_summarize$ap_pc_hisp)
summary(hisp_to_summarize$ap_one_pc_hisp)
summary(hisp_to_summarize$suspend)
summary(hisp_to_summarize$absent)

# schoolMaster from Clean_Education.R
white_to_summarize <- schoolMaster %>%
  filter(SCH_GTENR_WH_F > 0 & SCH_GTENR_WH_M > 0 & SCH_ENR_WH_M > 0 & SCH_ENR_WH_F > 0) %>%
  mutate(gifted_pc_white = (SCH_GTENR_WH_F + SCH_GTENR_WH_M)/(SCH_ENR_WH_M + SCH_ENR_WH_F))
summary(white_to_summarize$gifted_pc_white)
white_to_summarize <- schoolMaster %>%
  filter(SCH_APENR_WH_F > 0 & SCH_APENR_WH_M > 0 & SCH_ENR_WH_M > 0 & SCH_ENR_WH_F > 0) %>%
  mutate(ap_pc_white = (SCH_APENR_WH_F + SCH_APENR_WH_M)/(SCH_ENR_WH_M + SCH_ENR_WH_F))
summary(white_to_summarize$ap_pc_white)
white_to_summarize <- schoolMaster %>%
  filter(SCH_APPASS_ONEORMORE_WH_M > 0 & SCH_APPASS_ONEORMORE_WH_F > 0 & SCH_ENR_WH_M > 0 & SCH_ENR_WH_F > 0) %>%
  mutate(ap_pc_white = (SCH_APPASS_ONEORMORE_WH_M + SCH_APPASS_ONEORMORE_WH_F)/(SCH_ENR_WH_M + SCH_ENR_WH_F))
summary(white_to_summarize$ap_pc_white)
white_to_summarize <- schoolMaster %>%
  filter(SCH_PSDISC_SINGOOS_WH_M > 0 & SCH_PSDISC_SINGOOS_WH_F > 0 & SCH_ENR_WH_M > 0 & SCH_ENR_WH_F > 0) %>%
  mutate(suspend = (SCH_PSDISC_SINGOOS_WH_M + SCH_PSDISC_SINGOOS_WH_F)/(SCH_ENR_WH_M + SCH_ENR_WH_F))
summary(white_to_summarize$suspend)
white_to_summarize <- schoolMaster %>%
  filter(SCH_ABSENT_WH_M >= 0 & SCH_ABSENT_WH_F >= 0 & SCH_ENR_WH_M > 0 & SCH_ENR_WH_F > 0) %>%
  mutate(absent = (SCH_ABSENT_WH_M + SCH_ABSENT_WH_F)/(SCH_ENR_WH_M + SCH_ENR_WH_F)) %>%
  filter(absent <= 1)
summary(white_to_summarize$absent)

###### Mechanisms ########

affiliatesLEA <- readRDS('affiliatesLEA.Rdata')

station_word_data <- readRDS("../transcripts/station_word_clean.Rdata")
telemundo <- station_word_data %>%
  filter(parent == "telemundo") %>%
  dplyr::select(-callSign) %>%
  head(1) %>%
  mutate(telemundo_word_edu = word_education/(word_all*20), telemundo_word_latin = word_latin/(word_all*26),
         telemundo_word_rolemodel = word_rolemodel/(word_all*16), telemundo_word_bad = word_bad/(word_all*29),
         telemundo = 1) %>%
  dplyr::select(telemundo_word_edu, telemundo_word_latin, telemundo_word_rolemodel, telemundo_word_bad, telemundo)
univision <- station_word_data %>%
  filter(parent == "univision") %>%
  dplyr::select(-callSign) %>%
  head(1) %>%
  mutate(univision_word_edu = word_education/(word_all*20), univision_word_latin = word_latin/(word_all*26),
         univision_word_rolemodel = word_rolemodel/(word_all*16), univision_word_bad = word_bad/(word_all*29),
         univision = 1) %>%
  dplyr::select(univision_word_edu, univision_word_latin, univision_word_rolemodel, univision_word_bad, univision)
pbs <- station_word_data %>%
  filter(parent == "pbs") %>%
  dplyr::select(-callSign) %>%
  head(1) %>%
  mutate(pbs_word_edu = word_education/(word_all*20), pbs_word_latin = word_latin/(word_all*26),
         pbs_word_rolemodel = word_rolemodel/(word_all*16), pbs_word_bad = word_bad/(word_all*29),
         pbs = 1) %>%
  dplyr::select(pbs_word_edu, pbs_word_latin, pbs_word_rolemodel, pbs_word_bad, pbs)
unimas <- station_word_data %>%
  filter(parent == "unimas") %>%
  dplyr::select(-callSign) %>%
  head(1) %>%
  mutate(unimas_word_edu = word_education/(word_all*20), unimas_word_latin = word_latin/(word_all*26),
         unimas_word_rolemodel = word_rolemodel/(word_all*16), unimas_word_bad = word_bad/(word_all*29),
         unimas = 1) %>%
  dplyr::select(unimas_word_edu, unimas_word_latin, unimas_word_rolemodel, unimas_word_bad, unimas)
azteca <- station_word_data %>%
  filter(parent == "azteca") %>%
  dplyr::select(-callSign) %>%
  head(1) %>%
  mutate(azteca_word_edu = word_education/(word_all*20), azteca_word_latin = word_latin/(word_all*26),
         azteca_word_rolemodel = word_rolemodel/(word_all*16), azteca_word_bad = word_bad/(word_all*29),
         azteca = 1) %>%
  dplyr::select(azteca_word_edu, azteca_word_latin, azteca_word_rolemodel, azteca_word_bad, azteca)
# word frequency = number of hits/(total hits * number words)

# only keep obs in at least one of the affiliate networks
harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome)) %>%
  right_join(affiliatesLEA, by = 'LEAID') %>%
  mutate(univision = ifelse(univision >= 1, 1, 0),
         telemundo = ifelse(telemundo >= 1, 1, 0),
         pbs = ifelse(pbs >= 1, 1, 0),
         unimas = ifelse(unimas >= 1, 1 ,0),
         azteca = ifelse(azteca >= 1, 1, 0)) %>%
  filter(univision == 1 | telemundo == 1 | pbs == 1 | unimas == 1 | azteca == 1) %>%
  left_join(telemundo, by = "telemundo") %>%
  left_join(univision, by = 'univision') %>%
  left_join(pbs, by = 'pbs') %>%
  left_join(unimas, by = 'unimas') %>%
  left_join(azteca, by = 'azteca') %>%
  rowwise() %>%
  mutate(word_edu_mean = mean(c(telemundo_word_edu,univision_word_edu,pbs_word_edu,unimas_word_edu, azteca_word_edu),na.rm = TRUE),
         word_latin_mean = mean(c(telemundo_word_latin,univision_word_latin,pbs_word_latin,unimas_word_latin, azteca_word_latin),na.rm = TRUE),
         word_rolemodel_mean = mean(c(telemundo_word_rolemodel,univision_word_rolemodel,pbs_word_rolemodel,unimas_word_rolemodel, azteca_word_rolemodel),na.rm = TRUE),
         word_edu_max = max(c(telemundo_word_edu,univision_word_edu,pbs_word_edu,unimas_word_edu, azteca_word_edu),na.rm = TRUE),
         word_latin_max = max(c(telemundo_word_latin,univision_word_latin,pbs_word_latin,unimas_word_latin, azteca_word_latin),na.rm = TRUE),
         word_rolemodel_max = max(c(telemundo_word_rolemodel,univision_word_rolemodel,pbs_word_rolemodel,unimas_word_rolemodel, azteca_word_rolemodel),na.rm = TRUE)) %>%
  ungroup()
  


label_spec3 <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 '\\% Programs on Education', '\\% Programs on Hispanic Identity', '\\% Programs with Good Role Models')
label_mech1 <- c('TV Dummy', '\\% Programs on Education', '\\% Programs on Hispanic Identity',
                 '\\% Programs with Good Role Models')


om1 <- lm(ihs(sch_absent_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_edu_mean + word_latin_mean + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_absentIHS_mech1.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Chronically Absent)",
          omit.stat = c('f','ser','rsq','adj.rsq'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students',
                   'TV:origdist','origdist'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_mech1),
          dep.var.labels = 'IHS(\\# Hispanic Chronically Absent)')

om1 <- lm(ihs(sch_absent_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_edu_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_latin_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_rolemodel_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_absent_hi) ~ TV*origdist + word_edu_max + word_latin_max + word_rolemodel_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_absentIHS_mech2.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Chronically Absent)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Chronically Absent)')

om1 <- lm(ihs(sch_absent_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_absent_hi) ~ TV*origdist + TV*word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_absent_hi) ~ TV*origdist + TV*word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_absent_hi) ~ TV*origdist + TV*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_absent_hi) ~ TV*origdist + TV*word_edu_mean + TV*word_latin_mean + TV*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_absentIHS_mech3.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Chronically Absent)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          # covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Chronically Absent)')

om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_edu_mean + word_latin_mean + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_OOSIHS_mech1.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Out of School Suspension)')

om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_edu_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_latin_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_rolemodel_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + word_edu_max + word_latin_max + word_rolemodel_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_OOSIHS_mech2.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Out of School Suspension)')

om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_edu_mean + word_latin_mean + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_LEPIHS_mech1.tex", title="Mechanisms: Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser','rsq','adj.rsq'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students',
                   'TV:origdist','origdist'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_mech1),
          dep.var.labels = 'IHS(\\# Hispanic Limited English Proficiency)')

om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_edu_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_latin_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_rolemodel_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + word_edu_max + word_latin_max + word_rolemodel_max +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_LEPIHS_mech2.tex", title="Mechanisms: Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Limited English Proficiency)')

om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_edu_mean + TV*word_latin_mean + TV*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_LEPIHS_mech3.tex", title="Mechanisms: Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          # covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Limited English Proficiency)')

om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + word_edu_mean + word_latin_mean + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_harhIHS_mech1.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Harassment Victims)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Harassment Victims)')

om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*word_edu_mean + TV*word_latin_mean + TV*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_LEPIHS_mech3.tex", title="Mechanisms: Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          # covariate.labels = c(label_spec3),
          dep.var.labels = 'IHS(\\# Hispanic Limited English Proficiency)')

om1 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + word_edu_mean + word_latin_mean + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_giftedIHS_mech1.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic Gifted Students)",
          omit.stat = c('f','ser','rsq','adj.rsq'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students',
                   'TV:origdist','origdist'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_mech1),
          dep.var.labels = 'IHS(\\# Hispanic Gifted Students)')

om1 <- lm(ihs(sch_apenr_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om2 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + word_edu_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om3 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om5 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + word_edu_mean + word_latin_mean + word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, om5, out = "../../Output/Regs/edu_aptIHS_mech1.tex", title="Mechanisms: Effect of TV on IHS(\\# Hispanic APs Taken)",
          omit.stat = c('f','ser','rsq','adj.rsq'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes', 'hisp_students',
                   'TV:origdist','origdist'),
          order = c('TV','TV:origdist','origdist','hisp_students' ), 
          covariate.labels = c(label_mech1),
          dep.var.labels = 'IHS(\\# Hispanic APs Taken)')


###### Asian/White placebo ###### 

harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))


# %>% mutate(sch_apenr_as = ifelse(is.na(sch_apenr_as), 0, sch_apenr_as))
# sch_appass_oneormore_as = ifelse(is.na(sch_appass_oneormore_as) & !is.na(sch_appass_oneormore_hi), mean(harass$sch_appass_oneormore_as, na.rm = TRUE), sch_appass_oneormore_as)) 

label_spec3_asian <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 '\\# Asian Students')
label_spec3_white <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 '\\# White Students')

## imputing only reduces SEs
harass_imp <- harass %>%
  select(sch_apenr_as,TV,origdist,origpcHisp,origLogInc,origLogPop,SCH_TEACHERS_CURR_TOT,asian_students,
           total_students) %>%
  missForest()

## maybe cut AP pass if doing DD + RD? but interpretation makess little sense


## absence
# Asian
om1 <- lm(ihs(sch_absent_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students, data=harass)
om2 <- lm(ihs(sch_absent_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_absent_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_absentIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian Chronically Absent)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian Chronically Absent)')

# Black
om1 <- lm(ihs(sch_absent_bl) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students, data=harass)
om2 <- lm(ihs(sch_absent_bl) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_absent_bl) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_bl_absentIHS_spec3.tex", title="Effect of TV on IHS(\\# Black Chronically Absent)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Black Chronically Absent)')

## Suspension
# Asian
om1 <- lm(ihs(sch_discwodis_singoos_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students, data=harass)
om2 <- lm(ihs(sch_discwodis_singoos_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_discwodis_singoos_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_OOSOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian Suspended)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian Suspended)')


## Report for race
# Asian
om1 <- lm(ihs(sch_hbreported_rac_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_harhOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian reported bullying)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian reported bullying)')


## Victim of race
# Asian
om1 <- lm(ihs(sch_hbdisciplined_rac_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students, data=harass)
om2 <- lm(ihs(sch_hbdisciplined_rac_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbdisciplined_rac_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_harpOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian victim bullying)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian victim bullying)')


## AP enrolled
# Asian
om1 <- lm(ihs(sch_apenr_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students + ihs(asian_students) + hisp_students, data=harass)
om2 <- lm(ihs(sch_apenr_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + ihs(asian_students) + hisp_students, data=harass)
om3 <- lm(ihs(sch_apenr_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + ihs(asian_students) + hisp_students, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_aptOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian APs Taken)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian APs Taken)')




## AP pass
# Asian
om1 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students + ihs(asian_students), data=harass)
om2 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_appOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian APs Passed)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian APs Passed)')

om1 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students + ihs(asian_students), data=harass, subset = minDist < 
            50000 )
om2 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass, subset = minDist < 
            50000)
om3 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass, subset = minDist < 
            50000)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_appOLSIHS_spec3_50.tex", title="50 KM Effect of TV on IHS(\\# Asian APs Passed)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian APs Passed)')

om1 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students + ihs(asian_students), data=harass, subset = minDist < 
            25000 )
om2 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass, subset = minDist < 
            25000)
om3 <- lm(ihs(sch_appass_oneormore_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass, subset = minDist < 
            25000)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_appOLSIHS_spec3_25.tex", title="25 KM Effect of TV on IHS(\\# Asian APs Passed)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian APs Passed)')



## LEP
# Asian
om1 <- lm(ihs(sch_lepenr_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + asian_students, data=harass)
om2 <- lm(ihs(sch_lepenr_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_lepenr_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_lepOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian Limited English Proficiency)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian Limited English Proficiency)')

## Gifted
# Asian
om1 <- lm(ihs(sch_gtenr_as) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop  + asian_students, data=harass)
om2 <- lm(ihs(sch_gtenr_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_gtenr_as) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_as_giftedOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# Asian Gifted)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','asian_students' ), 
          covariate.labels = c(label_spec3_asian),
          dep.var.labels = 'IHS(\\# Asian Gifted)')

# White
om1 <- lm(ihs(sch_absent_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_absent_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_absent_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_absentIHS_spec3.tex", title="Effect of TV on IHS(\\# White Chronically Absent)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White Chronically Absent)')

# White
om1 <- lm(ihs(sch_discwodis_singoos_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_discwodis_singoos_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_discwodis_singoos_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_OOSOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# White Suspended)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White Suspended)')

# White
om1 <- lm(ihs(sch_hbreported_rac_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_harhOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# White reported bullying)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White reported bullying)')

# White
om1 <- lm(ihs(sch_hbdisciplined_rac_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_hbdisciplined_rac_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbdisciplined_rac_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_harpOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# White victim bullying)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White victim bullying)')

# White
om1 <- lm(ihs(sch_apenr_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_apenr_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apenr_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_aptOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# White APs Taken)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White APs Taken)')

# White
om1 <- lm(ihs(sch_appass_oneormore_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_appass_oneormore_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_appass_oneormore_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_appOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# White APs Passed)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White APs Passed)')

# White
om1 <- lm(ihs(sch_gtenr_wh) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop + white_students, data=harass)
om2 <- lm(ihs(sch_gtenr_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_gtenr_wh) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  white_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_wh_giftedOLSIHS_spec3.tex", title="Effect of TV on IHS(\\# White Gifted)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV','TV:origdist','origdist','white_students' ), 
          covariate.labels = c(label_spec3_white),
          dep.var.labels = 'IHS(\\# White Gifted)')



## TODO: magnitude tablle with Asians
## TODO: see ACS data for mechs/outcomes (college attendance etc.)
## TODO: robust: controls with distance, dist2
## TODO: bullying on gender

###### DD Spec ###### 

harass_as <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         origdist = ifelse(TV == 1,0,origdist)) %>%
  select(TV, origdist, dist2, origpcHisp , origLogInc , origLogPop , SCH_TEACHERS_CURR_TOT , hisp_students,  
         asian_students, white_students, 
         total_students , SCH_GRADE_G01 , SCH_GRADE_G06 , SCH_GRADE_G09,
          sch_gtenr_as, sch_appass_oneormore_as, lea_gedcred_as, sch_absent_as, sch_discwodis_singoos_as,
         sch_hbreported_rac_as, sch_hbdisciplined_rac_as, sch_apenr_as, sch_lepenr_as, sch_algpass_g08_as,
         sch_apmathenr_as, sch_apscienr_as, sch_mathenr_advm_as,
         sch_mathenr_calc_as, sch_scienr_biol_as, sch_scienr_chem_as, sch_scienr_phys_as, sch_satact_as,
         lea_gedpart_as, LEAID) %>%
  rename(sch_gtenr = sch_gtenr_as, sch_appass_oneormore = sch_appass_oneormore_as, lea_gedcred = lea_gedcred_as,
         sch_absent = sch_absent_as, sch_discwodis_singoos = sch_discwodis_singoos_as, 
         sch_hbreported_rac = sch_hbreported_rac_as, sch_hbdisciplined_rac = sch_hbdisciplined_rac_as,
         sch_apenr = sch_apenr_as, sch_lepenr = sch_lepenr_as, sch_algpass_g08 = sch_algpass_g08_as,
         sch_apmathenr = sch_apmathenr_as, sch_apscienr = sch_apscienr_as, sch_mathenr_advm = sch_mathenr_advm_as,
         sch_mathenr_calc = sch_mathenr_calc_as, sch_scienr_biol= sch_scienr_biol_as,
         sch_scienr_chem = sch_scienr_chem_as, sch_scienr_phys = sch_scienr_phys_as,
         sch_satact = sch_satact_as, lea_gedpart = lea_gedpart_as) %>%
  mutate(eth = 0)


harass_hi <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         origdist = ifelse(TV == 1,0,origdist)) %>%
  select(TV, origdist, dist2, origpcHisp , origLogInc , origLogPop , SCH_TEACHERS_CURR_TOT , hisp_students,  
         asian_students, white_students, 
         total_students , SCH_GRADE_G01 , SCH_GRADE_G06 , SCH_GRADE_G09,
         sch_gtenr_hi, sch_appass_oneormore_hi, lea_gedcred_hi, sch_absent_hi, sch_discwodis_singoos_hi,
         sch_hbreported_rac_hi, sch_hbdisciplined_rac_hi, sch_apenr_hi, sch_lepenr_hi, sch_algpass_g08_hi,
         sch_apmathenr_hi, sch_apscienr_hi, sch_mathenr_advm_hi,
         sch_mathenr_calc_hi, sch_scienr_biol_hi, sch_scienr_chem_hi, sch_scienr_phys_hi, sch_satact_hi,
         lea_gedpart_hi, LEAID) %>%
  rename(sch_gtenr = sch_gtenr_hi, sch_appass_oneormore = sch_appass_oneormore_hi, lea_gedcred = lea_gedcred_hi,
         sch_absent = sch_absent_hi, sch_discwodis_singoos = sch_discwodis_singoos_hi, 
         sch_hbreported_rac = sch_hbreported_rac_hi, sch_hbdisciplined_rac = sch_hbdisciplined_rac_hi,
         sch_apenr = sch_apenr_hi, sch_lepenr = sch_lepenr_hi, sch_algpass_g08 = sch_algpass_g08_hi,
         sch_apmathenr = sch_apmathenr_hi, sch_apscienr = sch_apscienr_hi, sch_mathenr_advm = sch_mathenr_advm_hi,
         sch_mathenr_calc = sch_mathenr_calc_hi, sch_scienr_biol= sch_scienr_biol_hi,
         sch_scienr_chem = sch_scienr_chem_hi, sch_scienr_phys = sch_scienr_phys_hi,
         sch_satact = sch_satact_hi, lea_gedpart = lea_gedpart_hi) %>%
  mutate(eth = 1)

harass <- harass_hi %>%
  rbind(harass_as) %>%
  right_join(affiliatesLEA, by = 'LEAID') %>%
  mutate(univision = ifelse(univision >= 1, 1, 0),
         telemundo = ifelse(telemundo >= 1, 1, 0),
         pbs = ifelse(pbs >= 1, 1, 0),
         unimas = ifelse(unimas >= 1, 1 ,0),
         azteca = ifelse(azteca >= 1, 1, 0)) %>%
  filter(univision == 1 | telemundo == 1 | pbs == 1 | unimas == 1 | azteca == 1) %>%
  left_join(telemundo, by = "telemundo") %>%
  left_join(univision, by = 'univision') %>%
  left_join(pbs, by = 'pbs') %>%
  left_join(unimas, by = 'unimas') %>%
  left_join(azteca, by = 'azteca') %>%
  rowwise() %>%
  mutate(word_edu_mean = mean(c(telemundo_word_edu,univision_word_edu,pbs_word_edu,unimas_word_edu, azteca_word_edu),na.rm = TRUE),
         word_latin_mean = mean(c(telemundo_word_latin,univision_word_latin,pbs_word_latin,unimas_word_latin, azteca_word_latin),na.rm = TRUE),
         word_rolemodel_mean = mean(c(telemundo_word_rolemodel,univision_word_rolemodel,pbs_word_rolemodel,unimas_word_rolemodel, azteca_word_rolemodel),na.rm = TRUE),
         word_bad_mean = mean(c(telemundo_word_bad,univision_word_bad,pbs_word_bad,unimas_word_bad, azteca_word_bad),na.rm = TRUE),
         word_edu_max = max(c(telemundo_word_edu,univision_word_edu,pbs_word_edu,unimas_word_edu, azteca_word_edu),na.rm = TRUE),
         word_latin_max = max(c(telemundo_word_latin,univision_word_latin,pbs_word_latin,unimas_word_latin, azteca_word_latin),na.rm = TRUE),
         word_rolemodel_max = max(c(telemundo_word_rolemodel,univision_word_rolemodel,pbs_word_rolemodel,unimas_word_rolemodel, azteca_word_rolemodel),na.rm = TRUE)) %>%
  ungroup()

label_spec3_dda <- c('TV $\\times$ Hispanic', 'TV Dummy',
                       'Hispanic') #  'TV Dummy $\\times$ Distance $\\times$ Hispanic', 'TV Dummy $\\times$ Distance', 'Distance to Boundary $\\times$ Hispanic',

om1 <- lm(ihs(sch_gtenr) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_gtenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_gtenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_giftedOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Gifted) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3),
          dep.var.labels = 'IHS(\\# Gifted)')

om1 <- lm(ihs(sch_appass_oneormore) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_appass_oneormore) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_appass_oneormore) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_appOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic APs Passed) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3),
          dep.var.labels = 'IHS(\\# AP Passed)')

om1 <- lm(ihs(lea_gedcred) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(lea_gedcred) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(lea_gedcred) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_gedcOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic GEDs) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# GEDs)')

om1 <- lm(ihs(sch_absent) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_absent) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_absent) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_absentOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Chronic Absences) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Chronic Absent)')

om1 <- lm(ihs(sch_discwodis_singoos) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_discwodis_singoos) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_discwodis_singoos) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_OOSOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Suspended) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Suspended)')

om1 <- lm(ihs(sch_hbreported_rac) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass, subset = !is.na(sch_gtenr))
om2 <- lm(ihs(sch_hbreported_rac) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass, subset = !is.na(sch_gtenr))
om3 <- lm(ihs(sch_hbreported_rac) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass, subset = !is.na(sch_gtenr))
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_harhOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Bullied) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Bullied)')

z1 <- glm(sch_hbreported_rac ~  TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data = harass, family = poisson)
z2 <- glm(sch_hbreported_rac ~  TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students + total_students, data = harass, family = poisson)
z3 <- glm(sch_hbreported_rac ~  TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students +  total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = harass, family = poisson)

stargazer(z1, z2, z3, out = "../../Output/Regs/edu_dda_harhziflIHS_spec3.tex", title="Poisson Differential Effect of TV on \\# Hispanic Bullied vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = '\\# Bullied')

om1 <- lm(ihs(sch_hbdisciplined_rac) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_hbdisciplined_rac) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbdisciplined_rac) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_harpOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Bullying) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Bullying)')

om1 <- lm(ihs(sch_apenr) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_apenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_aptOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic APs Taken) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# APs Taken)')

om1 <- lm(ihs(sch_lepenr) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_lepenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_lepenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_lepOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Limited English Proficiency) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Limited English Proficiency)')


om1 <- lm(ihs(sch_algpass_g08) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_algpass_g08) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_algpass_g08) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_alg8OLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Passing Algebra) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Passing Algebra)')

om1 <- lm(ihs(sch_apmathenr) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_apmathenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apmathenr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_apmathOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic AP Math) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# AP Math)')

om1 <- lm(ihs(sch_apscienr) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_apscienr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_apscienr) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_apsciOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic AP Science) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# AP Science)')

om1 <- lm(ihs(sch_mathenr_advm) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_mathenr_advm) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_mathenr_advm) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_advmOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Advanced Math) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Advanced Math)')

om1 <- lm(ihs(sch_mathenr_calc) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_mathenr_calc) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_mathenr_calc) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_calcOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Calculus) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Calculus)')

om1 <- lm(ihs(sch_scienr_biol) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_scienr_biol) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_scienr_biol) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_biolOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Biology) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Biology)')

om1 <- lm(ihs(sch_scienr_chem) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_scienr_chem) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_scienr_chem) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_chemOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Chemistry) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Chemistry)')

om1 <- lm(ihs(sch_scienr_phys) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_scienr_phys) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_scienr_phys) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_physOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Physics) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Physics)')

om1 <- lm(ihs(sch_satact) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_satact) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_satact) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_satactOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic SAT/ACT) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# SAT/ACT)')

om1 <- lm(ihs(lea_gedpart) ~ TV*eth + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(lea_gedpart) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(lea_gedpart) ~ TV*eth +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, out = "../../Output/Regs/edu_dda_gedpOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic GED Participate) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_dda), se= makeRobust3(om1,om2,om3) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# GED Participate)')


##### Diff in diff mech #######

## TODO: framing idea, only use identity mech, maybe educ as placebo
## TODO: get words relating to drugs, crime? 
## see https://www.americamagazine.org/arts-culture/2018/04/06/why-telenovelas-are-powerful-and-problematic-part-latino-culture
## and https://insightcrime.org/news/analysis/mexico-narco-soap-operas-do-more-than-just-glorify-drug-trade/
## maybe find something academic

label_spec3_ddaR <- c('TV $\\times$ Hispanic $\\times$ \\% programs on education',
                      'TV $\\times$ Hispanic $\\times$ \\% programs on identity',
                      'TV $\\times$ Hispanic $\\times$ \\% programs with role models',
                      'TV $\\times$ Hispanic $\\times$ \\% programs with bad content',
                      'TV $\\times$ Hispanic', 'TV Dummy',
                     'Hispanic')


om1 <- lm(ihs(sch_satact) ~ TV*eth*word_edu_mean + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students , data=harass)
om2 <- lm(ihs(sch_satact) ~ TV*eth*word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_satact) ~ TV*eth*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_satact) ~ TV*eth*word_bad_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_ddaR_satactOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic SAT/ACT) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth:word_edu_mean', 'TV:eth:word_latin_mean', 'TV:eth:word_rolemodel_mean','TV:eth:word_bad_mean','TV:eth','TV','eth'),
          #order = c('TV:eth', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth'), 
          covariate.labels = c(label_spec3_ddaR), se= makeRobust4(om1,om2,om3, om4) ,
          dep.var.labels = 'IHS(\\# SAT/ACT)')

om1 <- lm(ihs(sch_appass_oneormore) ~ TV*eth*word_edu_mean + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_appass_oneormore) ~ TV*eth*word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_appass_oneormore) ~ TV*eth*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_appass_oneormore) ~ TV*eth*word_bad_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3,om4, out = "../../Output/Regs/edu_ddaR_appOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic APs Passed) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth:word_edu_mean', 'TV:eth:word_latin_mean', 'TV:eth:word_rolemodel_mean','TV:eth:word_bad_mean','TV:eth','TV','eth'),
          covariate.labels = c(label_spec3_ddaR), se= makeRobust4(om1,om2,om3,om4),
          dep.var.labels = 'IHS(\\# AP Passed)')


om1 <- lm(ihs(sch_lepenr) ~ TV*eth*word_edu_mean + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_lepenr) ~ TV*eth*word_latin_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_lepenr) ~ TV*eth*word_rolemodel_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_lepenr) ~ TV*eth*word_bad_mean +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_ddaR_lepOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Limited English Proficiency) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth:word_edu_mean', 'TV:eth:word_latin_mean', 'TV:eth:word_rolemodel_mean', 'TV:eth:word_bad_mean','TV:eth','TV','eth'),
          covariate.labels = c(label_spec3_ddaR), se= makeRobust4(om1,om2,om3,om4) , 
          dep.var.labels = 'IHS(\\# Limited English Proficiency)')

om1 <- lm(ihs(sch_absent) ~ TV*eth*word_edu_mean + 
            origpcHisp + origLogInc + origLogPop + hisp_students + asian_students, data=harass)
om2 <- lm(ihs(sch_absent) ~ TV*eth*word_latin_mean + +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students, data=harass)
om3 <- lm(ihs(sch_absent) ~ TV*eth*word_rolemodel_mean + +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students, data=harass)
om4 <- lm(ihs(sch_absent) ~ TV*eth*word_bad_mean + +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students, data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_ddaR_absentOLSIHS_spec3.tex", title="Differential Effect of TV on IHS(\\# Hispanic Chronic Absences) vs. Asian",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('TV:eth:word_edu_mean', 'TV:eth:word_latin_mean', 'TV:eth:word_rolemodel_mean','TV:eth:word_bad_mean','TV:eth','TV','eth'),
          covariate.labels = c(label_spec3_ddaR), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Chronic Absent)')

# texreg
# modelsummary
# fixest




##### VECTORISED FUNCTION #######
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

makeRobust3 <- function(m1,m2,m3) {
  om1r <- sqrt(diag(vcovHC(m1, type="HC1")))
  om2r <- sqrt(diag(vcovHC(m2, type="HC1")))
  om3r <- sqrt(diag(vcovHC(m3, type="HC1")))
  return(list(om1r,om2r,om3r))
}

makeRobust4 <- function(m1,m2,m3,m4) {
  om1r <- sqrt(diag(vcovHC(m1, type="HC1")))
  om2r <- sqrt(diag(vcovHC(m2, type="HC1")))
  om3r <- sqrt(diag(vcovHC(m3, type="HC1")))
  om4r <- sqrt(diag(vcovHC(m4, type="HC1")))
  return(list(om1r,om2r,om3r,om4r))
}

ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}
