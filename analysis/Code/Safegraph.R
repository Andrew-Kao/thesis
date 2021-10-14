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
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         origdist = ifelse(inside == 1,0,origdist))
  
label_spec_sg <- c('TV $\\times$ Hispanic', 'TV Dummy',
                     'Hispanic') 


om1 <- lm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop, data=poi_pattern, subset = sector == 61)
om2 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | naics_code, data=poi_pattern, subset = sector == 61)
om3 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE, data=poi_pattern, subset = sector == 61)
om4 <- felm(ihs(visitors) ~ hispanic*inside + origpcHisp + origLogInc + origLogPop | STATE + naics_code, data=poi_pattern, subset = sector == 61)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/sg_61.tex", title="Differential Effect of TV on IHS(\\# Hispanic Visitors to schools) vs. non-Hispanic",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE,
          omit = c("Constant",'origpcHisp','origLogInc','origLogPop','SCH_TEACHERS_CURR_TOT',
                   'total_students','SCH_GRADE_G01Yes','SCH_GRADE_G06Yes','SCH_GRADE_G09Yes'),
          order = c('hispanic:inside','inside','hispanic'),
          covariate.labels = c(label_spec_sg), se= makeRobust4(om1,om2,om3,om4) , #coeftest(om1),  list(om1r,om2r,om3r) 
          dep.var.labels = 'IHS(\\# Visitors)')


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






