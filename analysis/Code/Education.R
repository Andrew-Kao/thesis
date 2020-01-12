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

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}



## clean the variables
schoolAll <- readRDS('SchAll.Rdata')

varList <- c('SCH_HBREPORTED_RAC_HI_','TOT_HBREPORTED_RAC_', 'SCH_HBDISCIPLINED_RAC_HI_', 'TOT_HBDISCIPLINED_RAC_',
             'SCH_DISCWODIS_EXPWE_HI_M', 'SCH_DISCWODIS_SINGOOS_HI_', 'SCH_ABSENT_HI_', 'TOT_ABSENT_')

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


absent <- cleanSchoolAll %>%
  filter(!is.na(sch_absent_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         ihs_absent_hi = ihs(sch_absent_hi))

om4 <- lm(ihs_absent_hi ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +
            tot_absent,data=absent)

om5 <- lm(ihs_absent_hi ~ TV + TV:origdist + origdist + origLogPop +
            origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT + hisp_students +
            +  total_students + SCH_GRADE_G06 + tot_absent
          ,data=absent)
om4 <- lm(tot_absent ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT + ihs_absent_hi
            ,data=absent)


# TODO: try with a bunch of early stage results and explore interesting ones 

#### HARASS
harass <- cleanSchoolAll %>%
  filter(!is.na(sch_hbreported_rac_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

z1 <- zeroinfl(sch_hbreported_rac_hi ~ TV*origdist, data = harass, link = "logit", dist = "poisson")
z2 <- zeroinfl(sch_hbreported_rac_hi ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc, data = harass, link = "logit", dist = "poisson")
z3 <- zeroinfl(sch_hbreported_rac_hi ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = harass, link = "logit", dist = "poisson")
stargazer(z1,z2,z3, out = "../../Output/Regs/edu_harhZi.tex", title="Effect of TV on Hispanic Victim Harassment Dummy, Zero-Inflated",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students', 'Contains Grade 1', 'Contains Grade 6',
                               'Contains Grade 9'),
          dep.var.labels = '\\# Hispanic Victims of Harassment')

z1 <- zeroinfl(sch_hbdisciplined_rac_hi ~ TV*origdist, data = harass, link = "logit", dist = "poisson")
z2 <- zeroinfl(sch_hbdisciplined_rac_hi ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc, data = harass, link = "logit", dist = "poisson")
z3 <- zeroinfl(sch_hbdisciplined_rac_hi ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = harass, link = "logit", dist = "poisson")
stargazer(z1,z2,z3, out = "../../Output/Regs/edu_harhdZi.tex", title="Effect of TV on Hispanic Offender Harassment Dummy, Zero-Inflated",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students', 'Contains Grade 1', 'Contains Grade 6',
                               'Contains Grade 9'),
          dep.var.labels = '\\# Hispanic Offenders of Harassment')




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















