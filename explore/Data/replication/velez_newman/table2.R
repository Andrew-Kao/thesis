# load LNS
lns <- read.csv("national_lns_data.csv")

# estimate models
hc_participation <- clmm(factor(part_home) ~ sum + income + educ + sex + race + age + age2 + english + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, subset = usborn == 0)

hc_vote <- glmer(voted_home ~ sum + income + educ + sex + race + age + age2 + english + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), subset = usborn == 0)

# Table 2
stargazer(fake.clm(hc_participation), hc_vote, covariate.labels = c("Spanish-Language Stations", "Income", "Education", "Gender", "Race", "Age", "Age Sq.","Born in US", "Life in US", "Ideology", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic"), no.space = TRUE, single.row = TRUE, digits = 3, star.cutoffs = c(.05,.01,.001))
