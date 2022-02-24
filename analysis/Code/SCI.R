###### SCI regressions ####

library(dplyr)
library(texreg)
library(tidyverse)
library(stargazer)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/SCI') 
}


instrument <- readRDS("../instrument/countyInstrumentCovariate.Rdata") %>%
  mutate(TV = ifelse(intersects == 1 & (areaRatio > .95 | dist > 0),1,0)) %>%
  mutate(stateCounty = paste0(state, county)) %>%
  filter(dist < 100000)

sci_country <- readRDS("SCI_county_country.Rdata") %>%
  mutate(stateCounty = str_pad(stateCounty,5,side = "left", pad = "0"))

sciCounty = inner_join(sci_country, instrument, by = "stateCounty") %>%
  mutate(logPop = log(population), income = log(income))

m2 <- lm(latin_friends ~ TV + logPop, data=sciCounty) 
m3 <- lm(latin_friends ~ TV + logPop + pcHisp , data=sciCounty)
m4 <- lm(latin_friends ~ TV + logPop + pcHisp + income, data=sciCounty)
stargazer(m2,m3,m4, out = "../../Output/Regs/sci_latin.tex", title="Effect of TV connectedness with Latin America",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Connectedness to Latin American countries',
          notes ="", se = makeRobust3(m2,m3,m4))

m2 <- lm(brazil_friends ~ TV + logPop, data=sciCounty) 
m3 <- lm(brazil_friends ~ TV + logPop + pcHisp , data=sciCounty)
m4 <- lm(brazil_friends ~ TV + logPop + pcHisp + income, data=sciCounty)
stargazer(m2,m3,m4, out = "../../Output/Regs/sci_brazil.tex", title="Effect of TV connectedness with Brazil",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Connectedness to Brazil',
          notes ="", se = makeRobust3(m2,m3,m4))

m2 <- lm(nonlatin_friends ~ TV + logPop, data=sciCounty) 
m3 <- lm(nonlatin_friends ~ TV + logPop + pcHisp , data=sciCounty)
m4 <- lm(nonlatin_friends ~ TV + logPop + pcHisp + income, data=sciCounty)
stargazer(m2,m3,m4, out = "../../Output/Regs/sci_nonlatin.tex", title="Effect of TV connectedness with non-Latin America",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:hispanic_d','hispanic_d$','logPop'),
          covariate.labels = c('TV Dummy', 
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Connectedness to non-Latin America',
          notes ="", se = makeRobust3(m2,m3,m4))

latin_sci <- sciCounty %>%
  mutate(friends = latin_friends, latin = 1)
nonlatin_sci <- sciCounty %>%
  mutate(friends = nonlatin_friends, latin = 0)
brazil_sci <- sciCounty %>%
  mutate(friends = brazil_friends, latin = 0)

latin_nonlatin <- latin_sci %>%
  rbind(nonlatin_sci)
latin_brazil <- latin_sci %>%
  rbind(brazil_sci)

m2 <- lm(friends ~ TV*latin + logPop, data=latin_nonlatin) 
m3 <- lm(friends ~ TV*latin + logPop + pcHisp , data=latin_nonlatin)
m4 <- lm(friends ~ TV*latin + logPop + pcHisp + income, data=latin_nonlatin)
stargazer(m2,m3,m4, out = "../../Output/Regs/sci_dd_nonlatin.tex", title="Effect of TV connectedness with Latin America vs. rest of world",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:latin','latin','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Latin America',
                               'Latin America',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Connectedness to Latin America',
          notes ="", se = makeRobust3(m2,m3,m4))

m2 <- lm(friends ~ TV*latin + logPop, data=latin_brazil) 
m3 <- lm(friends ~ TV*latin + logPop + pcHisp , data=latin_brazil)
m4 <- lm(friends ~ TV*latin + logPop + pcHisp + income, data=latin_brazil)
stargazer(m2,m3,m4, out = "../../Output/Regs/sci_dd_brazil.tex", title="Effect of TV connectedness with Latin America vs. Brazil",
          omit.stat = c('f','ser'), column.sep.width = '-5pt',
          order = c('TV','TV:latin','latin','logPop'),
          covariate.labels = c('TV Dummy', 'TV Dummy $\\times$ Latin America',
                               'Latin America',
                               'Log(Population)','County \\% Hispanic','Log(Income)',
                               'Foregin-born','Foreign-born Hispanic'),
          omit = c('Constant','dist2','age','sexMale','sexNIU','cases'),
          dep.var.labels = 'Connectedness to Latin America',
          notes ="", se = makeRobust3(m2,m3,m4))

######
makeRobust3 <- function(m1,m2,m3) {
  om1r <- sqrt(diag(vcovHC(m1, type="HC1")))
  om2r <- sqrt(diag(vcovHC(m2, type="HC1")))
  om3r <- sqrt(diag(vcovHC(m3, type="HC1")))
  return(list(om1r,om2r,om3r))
}
