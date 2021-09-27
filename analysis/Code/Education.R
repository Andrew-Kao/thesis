###### Education: Old Regs #####

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
             'SCH_DISCWODIS_EXPWE_HI_', 'SCH_DISCWODIS_SINGOOS_HI_', 'TOT_DISCWODIS_MULTOOS_',
             'SCH_ABSENT_HI_', 'TOT_ABSENT_',
             'SCH_APENR_HI_','TOT_APENR_','SCH_APPASS_ONEORMORE_HI_','TOT_APPASS_ONEORMORE_',
             'SCH_LEPENR_HI_','TOT_LEPENR_', 'SCH_GTENR_HI_','TOT_GTENR_',
             'SCH_ALGPASS_G08_HI_', 'SCH_ALGPASS_GS0910_HI_', 'SCH_ALGPASS_GS1112_HI_',
             'SCH_APMATHENR_HI_', 'SCH_APSCIENR_HI_',
             'SCH_MATHENR_ADVM_HI_', 'SCH_MATHENR_CALC_HI_',
             'SCH_SCIENR_BIOL_HI_', 'SCH_SCIENR_CHEM_HI_',
             'SCH_SCIENR_PHYS_HI_', 'SCH_SATACT_HI_',
             'LEA_GEDCRED_HI_', 'LEA_GEDPART_HI_',
             'SCH_HBREPORTED_RAC_AS_', 'SCH_HBDISCIPLINED_RAC_AS_', 
             'SCH_DISCWODIS_SINGOOS_AS_', 
             'SCH_ABSENT_AS_', 'SCH_APENR_AS_','SCH_APPASS_ONEORMORE_AS_',
             'SCH_LEPENR_AS_', 'SCH_GTENR_AS_',
             'SCH_ALGPASS_G08_AS_',
             'SCH_APMATHENR_AS_', 'SCH_APSCIENR_AS_',
             'SCH_MATHENR_ADVM_AS_', 'SCH_MATHENR_CALC_AS_',
             'SCH_SCIENR_BIOL_AS_', 'SCH_SCIENR_CHEM_AS_',
             'SCH_SCIENR_PHYS_AS_', 'SCH_SATACT_AS_',
             'LEA_GEDCRED_AS_', 'LEA_GEDPART_AS_',
             'SCH_HBREPORTED_RAC_WH_', 'SCH_HBDISCIPLINED_RAC_WH_', 
             'SCH_DISCWODIS_SINGOOS_WH_', 
             'SCH_ABSENT_WH_', 'SCH_APENR_WH_','SCH_APPASS_ONEORMORE_WH_',
             'SCH_LEPENR_WH_', 'SCH_GTENR_WH_',
             'SCH_ALGPASS_G08_WH_',
             'SCH_APMATHENR_WH_', 'SCH_APSCIENR_WH_',
             'SCH_MATHENR_ADVM_WH_', 'SCH_MATHENR_CALC_WH_',
             'SCH_SCIENR_BIOL_WH_', 'SCH_SCIENR_CHEM_WH_',
             'SCH_SCIENR_PHYS_WH_', 'SCH_SATACT_WH_',
             'LEA_GEDCRED_WH_', 'LEA_GEDPART_WH_',
             'SCH_HBREPORTED_RAC_BL_', 'SCH_HBDISCIPLINED_RAC_BL_', 
             'SCH_DISCWODIS_SINGOOS_BL_', 
             'SCH_ABSENT_BL_', 'SCH_APENR_BL_','SCH_APPASS_ONEORMORE_BL_',
             'SCH_LEPENR_BL_', 'SCH_GTENR_BL_')

# cred: https://stackoverflow.com/questions/59294898/creating-a-function-in-dplyr-that-operates-on-columns-through-variable-string-ma
adder <- function(data, name) {
  data %>%
    mutate(!! str_to_lower(str_sub(name, end=-2)) := dplyr::select(., starts_with(name)) %>% 
             map(function(x) ifelse(x <0, NA,x)) %>% 
             reduce(`+`))
}

cleanSchoolAll <- reduce(varList, .f = function(data,varname) adder(data,varname), .init = schoolAll) %>%
  mutate(hisp_students = SCH_ENR_HI_M + SCH_ENR_HI_F, total_students = TOT_ENR_M + TOT_ENR_F,
         asian_students = SCH_ENR_AS_M + SCH_ENR_AS_F, white_students = SCH_ENR_WH_M + SCH_ENR_WH_F,
         black_students = SCH_ENR_BL_M + SCH_ENR_BL_F, 
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

saveRDS(cleanSchoolAll, "CleanSchlAll.Rdata")

label_spec <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                'Log(Population)','\\% County Hispanic', 'Log(Income)')
label2_spec <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary','TV Dummy $\\times$ Distance$^{2}$',
  'Distance to Boundary (meters)', 'Distance$^{2}$',
  "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
  'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
  'Log(Population)','\\% County Hispanic', 'Log(Income)')
label2_spec2 <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary','TV Dummy $\\times$ Distance$^{2}$',
                 'Distance to Boundary (meters)', 'Distance$^{2}$', '\\% County Hispanic', 'Log(Population)',
                 "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                 'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                 'Log(Income)')
label_spec2 <- c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary',
                  'Distance to Boundary (meters)', '\\% County Hispanic', 'Log(Population)',
                  "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                  'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                  'Log(Income)')

###### REGRESSIONS ########

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

#### SUSPENSION ####
suspend <- cleanSchoolAll %>%
  filter(!is.na(hisp_OOSDum)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

suspend <- cleanSchoolAll %>%
  filter(!is.na(hisp_OOSDum)) %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))


om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop, data=suspend)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=suspend)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=suspend)
om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=suspend)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_OOSOLSIHS_spec3.tex", title="Effect of TV on IHS(Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          #covariate.labels = label_spec2,
          dep.var.labels = 'IHS(Hispanic Out of School Suspension)')


bm1 <- glm(hisp_OOSDum ~ TV*origdist ,data=suspend, family = binomial)
bm2 <- glm(hisp_OOSDum ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students, data=suspend, family = binomial)
bm3 <- glm(hisp_OOSDum ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09,data=suspend,family = binomial)
bm4 <- glm(hisp_OOSDum ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
             origpcHisp + origLogInc,data=suspend,family = binomial)
stargazer(bm1, bm2, bm3, bm4, out = "../../Output/Regs/edu_OOSLogit_spec.tex", title="Effect of TV on Hispanic Out of School Suspension Dummy",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)') )

om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist, data=suspend)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=suspend)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=suspend)
om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=suspend)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_OOSIHS_spec.tex", title="Effect of TV on IHS(Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = 'IHS(\\# Hispanic Out of School Suspension)')
om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2, data=suspend)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=suspend)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=suspend)
om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=suspend)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_OOSIHS2_spec.tex", title="Effect of TV on IHS(Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec,
          dep.var.labels = 'IHS(\\# Hispanic Out of School Suspension)')
om1 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc, data=suspend)
om2 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=suspend)
om3 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=suspend)
om4 <- lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=suspend)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_OOSIHS2_spec2.tex", title="Effect of TV on IHS(Hispanic Out of School Suspension)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec2,
          dep.var.labels = 'IHS(\\# Hispanic Out of School Suspension)')


lm(ihs(sch_discwodis_singoos_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + origLogInc + origpcHisp, data=suspend)

##### HARASS #####
# harass <- cleanSchoolAll %>%
#   filter(!is.na(sch_hbreported_rac_hi)) %>%
#   mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
#   filter(origdist < 100000 ) %>%
#   mutate(origdist = origdist/1000, dist2 = origdist^2,
#          origLogPop = log(origpopulation), origLogInc = log(origincome))

harass <- cleanSchoolAll %>%
  filter(!is.na(sch_hbreported_rac_hi)) %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

# IHS


om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist ,data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + origLogPop +
            origpcHisp, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc,data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + origLogPop +
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT,data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School") )

om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS_spec.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')

om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS2_spec.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary','TV Dummy $\\times$ Distance2',
                               'Distance to Boundary (meters)', 'Distance2',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')

om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS2_spec2.tex", title="Effect of TV on IHS(Hispanic \\# Harassment Victims)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec2,
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')
om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS_spec3.tex", title="Effect of TV on IHS(Hispanic \\# Harassment Victims)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          #covariate.labels = label_spec2,
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')
om1 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + origLogPop, data=harass)
om2 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass)
om3 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + TV*dist2 +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass)
om4 <- lm(ihs(sch_hbreported_rac_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=harass)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhOLSIHS2_spec3.tex", title="Effect of TV on IHS(Hispanic \\# Harassment Victims)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          #covariate.labels = label_spec2,
          dep.var.labels = 'IHS(\\# Hispanic Victims of Harassment)')


om1 <- glm(hisp_harassVicRaceDum ~ TV*origdist + 
            origpcHisp + origLogInc + origLogPop, data=harass, family = binomial)
om2 <- glm(hisp_harassVicRaceDum ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=harass, family = binomial)
om3 <- glm(hisp_harassVicRaceDum ~ TV*origdist +
            origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=harass, family = binomial)
om4 <- glm(hisp_harassVicRaceDum ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=harass, family = binomial)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_harhLogit_spec3.tex", title="Effect of TV on Hispanic \\# Harassment Victim Dummy)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          #covariate.labels = label_spec2,
          dep.var.labels = 'IHS(\\# Hispanic Victim Dummy)')

# zero inflate
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

z1 <- zeroinfl(sch_hbreported_rac_hi ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students, data = harass, link = "logit", dist = "poisson")
z2 <- zeroinfl(sch_hbreported_rac_hi ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = harass, link = "logit", dist = "poisson")
z3 <- zeroinfl(sch_hbreported_rac_hi ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
                 origpcHisp + origLogInc, data = harass, link = "logit", dist = "poisson")
stargazer(z1,z2,z3, out = "../../Output/Regs/edu_harhZi_spec.tex", title="Effect of TV on Hispanic Victim Harassment Dummy, Zero-Inflated",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
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


##### APs ######
aps <- cleanSchoolAll %>%
  filter(!is.na(sch_apenr_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

z1 <- lm(ihs(sch_apenr_hi) ~ TV*origdist, data = aps)
z2 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc, data = aps)
z3 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + origLogPop +
                 origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = aps)
z4 <- felm(ihs(sch_apenr_hi) ~ TV*origdist + origLogPop +
           origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 
           | STATE | 0 | 0, data = aps)
stargazer(z1,z2,z3,z4, out = "../../Output/Regs/edu_aptIHS.tex", title="Effect of TV on APs Taken",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students', 'Contains Grade 1', 'Contains Grade 6',
                               'Contains Grade 9'),
          dep.var.labels = '\\# IHS(Hispanic Students Taking AP)')

# variation on spec
z1 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students, data = aps)
z2 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = aps)
z3 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 +
           origLogPop + origpcHisp + origLogInc, data = aps)
z4 <- felm(ihs(sch_apenr_hi) ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 
           | STATE | 0 | 0, data = aps)
stargazer(z1,z2,z3,z4, out = "../../Output/Regs/edu_aptIHS_spec.tex", title="Effect of TV on APs Taken",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6', 'Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = '\\# IHS(Hispanic Students Taking AP)')

aps <- cleanSchoolAll %>%
  filter(!is.na(sch_appass_oneormore_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

z1 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist, data = aps)
z2 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + origLogPop +
           origpcHisp + origLogInc, data = aps)
z3 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + origLogPop +
           origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = aps)
z4 <- felm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 
           | STATE | 0 | 0, data = aps)
stargazer(z1,z2,z3,z4, out = "../../Output/Regs/edu_appIHS.tex", title="Effect of TV on APs Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)',"\\# Teachers at School",
                               '\\# Hispanic Students', 'Total Students', 'Contains Grade 1', 'Contains Grade 6',
                               'Contains Grade 9'),
          dep.var.labels = '\\# IHS(Hispanic Students Passing AP)')

# variation on spec
z1 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students, data = aps)
z2 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data = aps)
z3 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
           total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 +
           origLogPop + origpcHisp + origLogInc, data = aps)
z4 <- felm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + origLogPop +
             origpcHisp + origLogInc +SCH_TEACHERS_CURR_TOT +  hisp_students  + 
             total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 
           | STATE | 0 | 0, data = aps)
stargazer(z1,z2,z3,z4, out = "../../Output/Regs/edu_appIHS_spec.tex", title="Effect of TV on APs Passed",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                              "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                              'Contains Grade 1', 'Contains Grade 6', 'Contains Grade 9',
                              'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = '\\# IHS(Hispanic Students Passing AP)')

om1 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc, data=aps)
om2 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=aps)
om3 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=aps)
om4 <- lm(ihs(sch_apenr_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=aps)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_aptIHS2_spec2.tex", title="Effect of TV on IHS(APs Taken)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec2,
          dep.var.labels = 'IHS(APs Taken by Hispanic Students)')
om1 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc, data=aps)
om2 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=aps)
om3 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=aps)
om4 <- lm(ihs(sch_appass_oneormore_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=aps)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_appIHS2_spec2.tex", title="Effect of TV on IHS(APs Passed)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec2,
          dep.var.labels = 'IHS(APs Passed by Hispanic Students)')



##### LEP - limited proficiency #####
lep <- cleanSchoolAll %>%
  filter(!is.na(sch_lepenr_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))


om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist, data=lep)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=lep)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=lep)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=lep)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_lepOLSIHS_spec.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = 'IHS(Hispanic \\# Limited English Proficiency)')

om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc, data=lep)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=lep)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=lep)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=lep)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_lepOLSIHS2_spec2.tex", title="Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec2,
          dep.var.labels = 'IHS(Hispanic \\# Limited English Proficiency)')
om1 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc, data=lep)
om2 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=lep)
om3 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=lep)
om4 <- lm(ihs(sch_lepenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=lep)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_lepOLSIHS_spec2.tex", title="Effect of TV on IHS(LEP)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label_spec2,
          dep.var.labels = 'IHS(Hispanic \\# Limited English Proficiency)')

# regular OLS - signs flip with controls
om1 <- lm(sch_lepenr_hi ~ TV*origdist, data=lep)
om2 <- lm(sch_lepenr_hi ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=lep)
om3 <- lm(sch_lepenr_hi ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=lep)
om4 <- lm(sch_lepenr_hi ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=lep)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_lepOLS_spec.tex", title="Effect of TV on Hispanic \\% Harassment Victims",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                               "\\# Teachers at School",'\\# Hispanic Students', 'Total Students',
                               'Contains Grade 1', 'Contains Grade 6','Contains Grade 9',
                               'Log(Population)','\\% County Hispanic', 'Log(Income)'),
          dep.var.labels = 'Hispanic \\# Limited English Proficiency')


#### GIFTED ####
gifted <- cleanSchoolAll %>%
  filter(!is.na(sch_gtenr_hi)) %>%
  mutate(TV = ifelse(origintersects == 1 & (origareaRatio > .95 | origdist > 0 ), 1, 0)) %>%
  filter(origdist < 100000 ) %>%
  mutate(origdist = origdist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

om1 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc, data=gifted)
om2 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=gifted)
om3 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + TV*dist2 + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=gifted)
om4 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + TV*dist2 + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=gifted)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_giftedOLSIHS2_spec2.tex", title="Effect of TV on IHS(Gifted)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label2_spec2,
          dep.var.labels = 'IHS(Hispanic \\# Gifted Students)')
om1 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc, data=gifted)
om2 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students, data=gifted)
om3 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + 
            origpcHisp + origLogInc + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09, data=gifted)
om4 <- lm(ihs(sch_gtenr_hi) ~ TV*origdist + SCH_TEACHERS_CURR_TOT +  hisp_students  + 
            total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 + origLogPop +
            origpcHisp + origLogInc ,data=gifted)
stargazer(om1, om2, om3, om4, out = "../../Output/Regs/edu_giftedOLSIHS_spec2.tex", title="Effect of TV on IHS(Gifted)",
          omit.stat = c('f','ser'), column.sep.width = '-2pt', notes.append = FALSE, omit = "Constant",
          order = ('TV' ), 
          covariate.labels = label_spec2,
          dep.var.labels = 'IHS(Hispanic \\# Gifted Students)')



#### FUNCTIONS ####
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















