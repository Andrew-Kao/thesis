###### Safegraph: Main Regs #####

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
  setwd('~/Documents/College/All/thesis/explore/Data/safegraph')
} 

poi_pattern <- readRDS('POI/POI_pattern.Rdata') %>%
  rename(inside = inside.x) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))
  # filter(origdist < 3 )
  #origdist = ifelse(inside == 1,0,origdist)) %>%  
  
label_spec_sg <- c('TV $\\times$ Hispanic', 'TV Dummy',
                     'Hispanic') 


om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = sector == 61)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = sector == 61)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = sector == 61)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = sector == 61)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_61.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to education) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')


om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = sector == 71)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = sector == 71)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = sector == 71)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = sector == 71)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_71.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to recreation) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = sector == 72)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = sector == 72)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = sector == 72)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = sector == 72)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_72.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to restaurants) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = sector == 51)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = sector == 51)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = sector == 51)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = sector == 51)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_51.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to information) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = sector == 52)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = sector == 52)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = sector == 52)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = sector == 52)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_52.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to finance) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = hispanic_loc == 1)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = hispanic_loc == 1)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = hispanic_loc == 1)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = hispanic_loc == 1)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_hisploc.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to Hispanic places) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = hispanic_food == 1)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = hispanic_food == 1)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = hispanic_food == 1)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = hispanic_food == 1)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_hispfood.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to Hispanic food) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = hispanic_food == 0 & sector == 72)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = hispanic_food == 0  & sector == 72)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = hispanic_food == 0  & sector == 72)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code , data=poi_pattern, subset = hispanic_food == 0  & sector == 72)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_nonhispfood.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to non-Hispanic food) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')

## TODO: triple diff (food, Hispanic locations)
## see if data cleaning can solve DD


####### FUNCTIONS ############
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






