###### Education: McCrary, Bandwidth etc. #####

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
library(conflicted)
library(rdd)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}


### data built already in Education_Right.R
harass <- read.csv(file = "dda.csv")


leaContour <- readRDS('LEAContour.RData')
harass_contour <- harass %>%
  left_join(leaContour, by = c('LEAID'))


########### McCrary test ##############
hc_mini <- harass_contour %>%
  select(TV, origdist) %>%
  mutate(distance = ifelse(TV > 0, origdist, -origdist)) %>%
  filter(origdist != 0)

DCdensity(hc_mini$distance, verbose = TRUE)

hc_mini <- harass %>%
  select(TV, origdist) %>%
  mutate(distance = ifelse(TV > 0, origdist, -origdist)) %>%
  filter(origdist != 0)

DCdensity(hc_mini$distance, verbose = TRUE)


## main data

cleanSchoolAll <- readRDS("CleanSchlAll.Rdata")

cSA <- cleanSchoolAll %>%
  filter(minDist < 100000 ) %>%
  mutate(distance = ifelse(inside > 0, minDist, -minDist)) %>%
  mutate(distance = distance/1000) %>%
  filter(distance != 0)

DCdensity(cSA$distance, verbose = TRUE)

## somehow 2/4 draws here are still significant < .05 -- test too strong?
cSA <- cleanSchoolAll %>%
  filter(minDist < 100000 ) %>%
  mutate(rb = rbernoulli(nrow(.))) %>%
  mutate(distance = ifelse(rb == 1, origdist, -origdist)) %>%
  mutate(distance = distance/1000) %>%
  filter(distance != 0)

DCdensity(cSA$distance, verbose = TRUE, bin = 5)

rcSA <- cSA[sample(nrow(cSA), 2000), ]
DCdensity(rcSA$distance, verbose = TRUE)

# wrong
# cSA <- cleanSchoolAll %>%
#   filter(minDist < 100000 ) %>%
#   mutate(distance = (minDist - 40000)/1000)

DCdensity(cSA$distance, verbose = TRUE, bin = 2, bw = 6)


######## bandwidths ##########

harass_fix <- harass %>%
  mutate(distance = ifelse(TV > 0, origdist, -origdist))
IKbandwidth(harass_fix$distance, ihs(harass$sch_satact), cutpoint = 0, verbose = TRUE, kernel = "triangular")
IKbandwidth(harass_fix$distance, ihs(harass$sch_satact), cutpoint = 0, verbose = TRUE, kernel = "rectangular")


harass_as <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         origdist = ifelse(TV == 1,0,origdist),
         nontrad = ifelse(SCH_STATUS_MAGNET == 1 | SCH_STATUS_ALT == 1, 1, 0),
         CHARTER_SCH = SCH_STATUS_CHARTER,
         sch_ret_as = sch_ret_g09_as + sch_ret_g10_as + sch_ret_g11_as + sch_ret_g12_as) %>%
  select(TV, origdist, dist2, origpcHisp , origLogInc , origLogPop , SCH_TEACHERS_CURR_TOT , hisp_students,  
         asian_students, white_students, 
         total_students , SCH_GRADE_G01 , SCH_GRADE_G06 , SCH_GRADE_G09,
         sch_gtenr_as, sch_appass_oneormore_as, lea_gedcred_as, sch_absent_as, sch_discwodis_singoos_as,
         sch_hbreported_rac_as, sch_hbdisciplined_rac_as, sch_apenr_as, sch_lepenr_as, sch_algpass_g08_as,
         sch_apmathenr_as, sch_apscienr_as, sch_mathenr_advm_as,
         sch_mathenr_calc_as, sch_scienr_biol_as, sch_scienr_chem_as, sch_scienr_phys_as, sch_satact_as,
         sch_ret_g09_as,sch_ret_g10_as,sch_ret_g11_as,sch_ret_g12_as, sch_ret_as,
         sch_hbreported_sex_as, sch_ideaenr_as,
         lea_gedpart_as, LEAID, stateCounty, STATE, X, Y, SCHID, nontrad, CHARTER_SCH) %>%
  rename(sch_gtenr = sch_gtenr_as, sch_appass_oneormore = sch_appass_oneormore_as, lea_gedcred = lea_gedcred_as,
         sch_absent = sch_absent_as, sch_discwodis_singoos = sch_discwodis_singoos_as, 
         sch_hbreported_rac = sch_hbreported_rac_as, sch_hbdisciplined_rac = sch_hbdisciplined_rac_as,
         sch_apenr = sch_apenr_as, sch_lepenr = sch_lepenr_as, sch_algpass_g08 = sch_algpass_g08_as,
         sch_apmathenr = sch_apmathenr_as, sch_apscienr = sch_apscienr_as, sch_mathenr_advm = sch_mathenr_advm_as,
         sch_mathenr_calc = sch_mathenr_calc_as, sch_scienr_biol= sch_scienr_biol_as,
         sch_scienr_chem = sch_scienr_chem_as, sch_scienr_phys = sch_scienr_phys_as,
         sch_satact = sch_satact_as, lea_gedpart = lea_gedpart_as,
         sch_ret = sch_ret_as, sch_hbreported_sex = sch_hbreported_sex_as, sch_ideaenr = sch_ideaenr_as,
         sch_ret_g09 = sch_ret_g09_as, sch_ret_g10 = sch_ret_g10_as,
         sch_ret_g11 = sch_ret_g11_as, sch_ret_g12 = sch_ret_g12_as) %>%
  mutate(eth = 0, sweight = asian_students)


harass_hi <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome),
         origdist = ifelse(TV == 1,0,origdist),
         nontrad = ifelse(SCH_STATUS_MAGNET == 1 | SCH_STATUS_ALT == 1, 1, 0),
         CHARTER_SCH = SCH_STATUS_CHARTER,
         sch_ret_hi = sch_ret_g09_hi + sch_ret_g10_hi + sch_ret_g11_hi + sch_ret_g12_hi) %>%
  select(TV, origdist, dist2, origpcHisp , origLogInc , origLogPop , SCH_TEACHERS_CURR_TOT , hisp_students,  
         asian_students, white_students, 
         total_students , SCH_GRADE_G01 , SCH_GRADE_G06 , SCH_GRADE_G09,
         sch_gtenr_hi, sch_appass_oneormore_hi, lea_gedcred_hi, sch_absent_hi, sch_discwodis_singoos_hi,
         sch_hbreported_rac_hi, sch_hbdisciplined_rac_hi, sch_apenr_hi, sch_lepenr_hi, sch_algpass_g08_hi,
         sch_apmathenr_hi, sch_apscienr_hi, sch_mathenr_advm_hi,
         sch_mathenr_calc_hi, sch_scienr_biol_hi, sch_scienr_chem_hi, sch_scienr_phys_hi, sch_satact_hi,
         sch_ret_g09_hi,sch_ret_g10_hi,sch_ret_g11_hi,sch_ret_g12_hi, sch_ret_hi,
         sch_hbreported_sex_hi, sch_ideaenr_hi,
         lea_gedpart_hi, LEAID, stateCounty, STATE, X, Y, SCHID, nontrad, CHARTER_SCH) %>%
  rename(sch_gtenr = sch_gtenr_hi, sch_appass_oneormore = sch_appass_oneormore_hi, lea_gedcred = lea_gedcred_hi,
         sch_absent = sch_absent_hi, sch_discwodis_singoos = sch_discwodis_singoos_hi, 
         sch_hbreported_rac = sch_hbreported_rac_hi, sch_hbdisciplined_rac = sch_hbdisciplined_rac_hi,
         sch_apenr = sch_apenr_hi, sch_lepenr = sch_lepenr_hi, sch_algpass_g08 = sch_algpass_g08_hi,
         sch_apmathenr = sch_apmathenr_hi, sch_apscienr = sch_apscienr_hi, sch_mathenr_advm = sch_mathenr_advm_hi,
         sch_mathenr_calc = sch_mathenr_calc_hi, sch_scienr_biol= sch_scienr_biol_hi,
         sch_scienr_chem = sch_scienr_chem_hi, sch_scienr_phys = sch_scienr_phys_hi,
         sch_ret = sch_ret_hi, sch_hbreported_sex = sch_hbreported_sex_hi, sch_ideaenr = sch_ideaenr_hi,
         sch_ret_g09 = sch_ret_g09_hi, sch_ret_g10 = sch_ret_g10_hi,
         sch_ret_g11 = sch_ret_g11_hi, sch_ret_g12 = sch_ret_g12_hi,
         sch_satact = sch_satact_hi, lea_gedpart = lea_gedpart_hi) %>%
  mutate(eth = 1, sweight = hisp_students)

harass_fix <- harass_hi %>%
  rbind(harass_as) %>%
  mutate(schlea = paste0(LEAID,SCHID)) %>%
  mutate(distance = ifelse(TV > 0, origdist, -origdist))

IKbandwidth(harass_fix$distance, ihs(harass_fix$sch_satact), cutpoint = 0, verbose = TRUE, kernel = "triangular")
IKbandwidth(harass_fix$distance, ihs(harass_fix$sch_satact), cutpoint = 0, verbose = TRUE, kernel = "rectangular")

## time to kill the project?


harass <- cleanSchoolAll %>%
  mutate(TV = inside) %>%
  filter(minDist < 100000 ) %>%
  mutate(origdist = minDist/1000, dist2 = origdist^2,
         origLogPop = log(origpopulation), origLogInc = log(origincome))

label_spec3 <- c('TV dummy', 'TV Dummy $\\times$ Distance to Boundary', 'Distance to Boundary (meters)',
                 '\\# Hispanic Students')

etable_order <- c('TV $\times$ eth', 'TV dummy $\times$ Hispanic', 'TV:hispanic_d', 'TV','TV:origdist:eth','TV:origdist','origdist:eth','eth')
setFixest_dict(c('TV:eth' = "TV dummy $\\times$ Hispanic", TV = "TV dummy", 'TV:hispanic_d' = "TV dummy $\\times$ Hispanic",
                 'TV:eth:word_latin_mean' = 'TV dummy $\\times$ Hispanic $\\times$ \\% programs on identity',
                 'TV:eth:word_edu_mean' = 'TV dummy $\\times$ Hispanic $\\times$ \\% programs on education',
                 'TV:eth:word_rolemodel_mean' = 'TV dummy $\\times$ Hispanic $\\times$ \\% programs with role models'))
