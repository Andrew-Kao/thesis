library(arm)
library(ordinal)
library(stargazer)

fake.clm <- function(x) {
  model.fake <- x
  model.fake$nobs <- x$dims$nobs
  model.fake$call[1] <- call("clm")
  model.fake
}

# load data and create consistent variable names
ldees <- read.csv("national_ldees_data.csv")
lns <- read.csv("national_lns_data.csv")
lns[,c("gender", "partisanship", "education", "bornus")] <- lns[,c("sex","pid7", "educ", "usborn")]

# estimate models

# LNS models
participation <- glmer(latino.participation ~ sum + income + education + gender + race + age + age2 + english + bornus + lifeinUS + ideo7 + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

identity <- clmm(factor(identity) ~ sum + income + education + gender + race + age + age2 + english + bornus + lifeinUS + ideo7 + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = lns)

knowledge <- glmer(pk ~ sum + income + education + gender + race + age + age2 + english + bornus + lifeinUS + ideo7 + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

# LDEES models
totalcontact <- glmer(contacted ~ sum + income + education + gender + age + age2 + english + bornus + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = ldees, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

gopcontact <- glmer(gopcontact ~ sum + income + education + gender + age + age2 + english + bornus + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = ldees, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

demcontact <- glmer(demcontact ~ sum + income + education + gender + age + age2 + english + bornus + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = ldees, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

# Table 1
stargazer(participation, fake.clm(identity), knowledge, totalcontact, gopcontact, demcontact, star.cutoffs = c(.05, .01, .001), single.row = TRUE)
