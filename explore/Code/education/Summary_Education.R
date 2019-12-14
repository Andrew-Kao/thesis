###### Education Summary Stats ####

library(data.table)
library(rgdal)
library(sf)
library(raster)
library(rgeos)
library(dplyr)
library(stringr)
library(stargazer)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}

educ <- readRDS('LEAReadyRaster.Rdata')

lea <- educ@data %>%
  mutate(origdist = origdist/1000, logpop = log(origpopulation), loginc = log(origincome),
         origpcHisp = origpcHisp * 100) %>%
  select(origdist, origintersects, origpcHisp, logpop,
         loginc)
stargazer(lea, out = "../../Output/Summary/LEAInfo.tex", title="School-District Level Summary Statistics",
          covariate.labels = c('Distance to Boundary', 'SLTV Coverage Dummy', '\\% County Hispanic',
                               'Log(Population)', 'Log(Income)'),
          notes = "\\textit{Note:} Distance to SLTV Boundary measured in KM", summary = TRUE, digits = 3, digits.extra=0)

# reference Education.R for cleanSchoolAll
school <- cleanSchoolAll

schoolS <- school %>%
  select(total_students, hisp_students, SCH_GRADE_G01, SCH_GRADE_G06, SCH_GRADE_G09,
         hisp_OOSDum, sch_absent_hi, SCH_TEACHERS_CURR_TOT) %>%
  mutate(SCH_GRADE_G01 = ifelse(SCH_GRADE_G01 == "Yes", 1, 0), SCH_GRADE_G06 = ifelse(SCH_GRADE_G06 == "Yes", 1, 0),
         SCH_GRADE_G09 = ifelse(SCH_GRADE_G09 == "Yes", 1, 0), 
         SCH_TEACHERS_CURR_TOT = ifelse(SCH_TEACHERS_CURR_TOT < 1, NA, SCH_TEACHERS_CURR_TOT),
         total_students = ifelse(total_students < 1, NA, total_students),
         hisp_students = ifelse(hisp_students < 1, NA, hisp_students))
stargazer(schoolS, out = "../../Output/Summary/SchoolInfo.tex", title="School Level Summary Statistics",
          covariate.labels = c( 'Total Students', '\\# Hispanic Students','Contains Grade 1',
                                'Contains Grade 6', 'Contains Grade 9',
                                'Hispanic Suspension Dummy', 'Hispanic Chronic Absentees',
                                '\\# Teachers'),
          notes = "\\textit{Note:} Dummies indicate whether event occurred in the school over the past year",
          summary = TRUE, digits = 3, digits.extra=0)











