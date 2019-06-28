library(arm)
library(stargazer)

nsl <- read.csv("national_nsl_data.csv")
lns <- read.csv("national_lns_data.csv")
ldees <- read.csv("national_ldees_data.csv")

# estimate SLTV x citizenship/US born models

civiccit <- glmer(latino.participation ~ sum*citizen + income + educ + sex + race + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

civicborn <- glmer(latino.participation ~ sum*usborn + income + educ + sex + race + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

knowcitizen <- glmer(pk ~ sum*citizen + income + educ + sex + race + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

knowborn <- glmer(pk ~ sum*usborn + income + educ + sex + race + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

# Table J2

stargazer(civiccit, civicborn, knowcitizen, knowborn, no.space = TRUE, digits = 3, star.cutoffs = c(.05, .01, .001))

# estimate conditional LDEES models

hisp.vote <- glmer(vote2008 ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + pcthisp*sum + (1|county), data = ldees, family = binomial(link="logit"))

educ.vote <- glmer(vote2008 ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*education + (1|county), data = ldees, family = binomial(link="logit"))

inc.vote <- glmer(vote2008 ~  sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*income  + (1|county), data = ldees, family = binomial(link="logit"))

sex.vote <- glmer(vote2008 ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*gender + (1|county), data = ldees, family = binomial(link="logit"))

college.vote <- glmer(vote2008 ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*pctcollege + (1|county), data = ldees, family = binomial(link="logit"))

ancestry.vote <- glmer(vote2008 ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*cuban + sum*puerto + (1|county), data = ldees, family = binomial(link="logit"))

# Table J3
stargazer(hisp.vote, educ.vote, inc.vote, sex.vote, college.vote, ancestry.vote, single.row = TRUE, digits = 1, no.space = TRUE, star.cutoffs = c(.05, .01))


# estimate conditional NSL models

hisp.vote <- glmer(vote.plan ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + pcthisp*sum + (1|sample12), data = nsl, family = binomial(link="logit"))

educ.vote <- glmer(vote.plan ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*education + (1|sample12), data = nsl, family = binomial(link="logit"))

inc.vote <- glmer(vote.plan ~  sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*income  + (1|sample12), data = nsl, family = binomial(link="logit"))

sex.vote <- glmer(vote.plan ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*gender + (1|sample12), data = nsl, family = binomial(link="logit"))

college.vote <- glmer(vote.plan ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + sum*pctcollege + (1|sample12), data = nsl, family = binomial(link="logit"))

ancestry.vote <- glmer(vote.plan ~ sum + education + income +  gender + age + age2 + english + bornus + partisanship +  pcthisp + pctunemp + pctcollege + dominican  + cuban + salvadoran + puerto + sum*dominican + sum*cuban + sum*salvadoran + sum*puerto + (1|sample12), data = nsl, family = binomial(link="logit"))

# Table J4
stargazer(hisp.vote, educ.vote, inc.vote, sex.vote, college.vote, ancestry.vote, single.row = TRUE, digits = 1, no.space = TRUE)

# estimate conditional LNS models

hisp.vote <- glmer(latino.participation ~ sum + educ + income +  sex + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + dominican  + cuban + salvadoran + puerto + sum*pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

educ.vote <- glmer(latino.participation ~ sum + educ + income +  sex + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + dominican  + cuban + salvadoran + puerto + sum*educ + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

income.vote <- glmer(latino.participation ~ sum + educ + income +  sex + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + dominican  + cuban + salvadoran + puerto + sum*income + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

sex.vote <- glmer(latino.participation ~ sum + educ + income +  sex + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + dominican  + cuban + salvadoran + puerto + sum*sex + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

college.vote <- glmer(latino.participation ~ sum + educ + income +  sex + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + dominican  + cuban + salvadoran + puerto + sum*pctcollege + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

ancestry.vote <- glmer(latino.participation ~ sum + educ + income +  sex + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + dominican + cuban + salvadoran + puerto + sum*dominican + sum*cuban + sum*salvadoran + sum*puerto + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

# Table J5
stargazer(hisp.vote, educ.vote, income.vote, sex.vote, college.vote, ancestry.vote, single.row = TRUE, digits = 1, no.space = TRUE, star.cutoffs = c(.05, .01))

