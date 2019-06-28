library(ggplot2)
library(mvtnorm)

nsl <- read.csv("national_nsl_data.csv")
ldees <- read.csv("national_ldees_data.csv")

# estimate model using NSL data
fit.nsl <- glmer(vote.plan ~ sum + income + education + gender + age + age2 + english + bornus + partisanship + pctunemp + pctcollege + pcthisp + (1|sample12), data = nsl, family = binomial(link="logit"))

# code for generating predicted probabilities
betas <- fixef(fit.nsl)
vcov <- as.matrix(summary(fit.nsl)$vcov)
coefsim <- rmvnorm(10000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "education", "gender", "age", "age2", 
           "english", "bornus", "partisanship", "pctunemp", "pctcollege", "pcthisp")
X <- nsl[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs <- {}
ses <- {}

for (i in 1:length(names)) {
  
  means0 <- cbind(1, min(X[names[1]], na.rm = T), 
                  rep.row(colMeans(X[,which(names != names[1])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[1]], na.rm = T), 
                  rep.row(colMeans(X[,which(names != names[1])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), 
                          which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  
}

# save coefs and ses for plotting
nsl.df <- data.frame(names, coefs, ses, survey = "Turnout Intention (2012)")

# estimate model using LDEES data
# use nAGQ = 0 for faster estimation; results are comparable with the default setting of nAGQ = 1
fit.ldees <- glmer(vote2008 ~ sum + income + education + gender + age + age2 + english + bornus + partisanship + pctunemp + pctcollege + pcthisp + (1|county), data = ldees, subset = as.Date(ldees$earliestregistrationdate, "%M/%d/%y") <= as.Date("11/5/08", "%M/%d/%y"), family = binomial(link = "logit"), nAGQ = 0)

# code for generating predicted probabilities
betas <- fixef(fit.ldees)
vcov <- as.matrix(summary(fit.ldees)$vcov)
coefsim <- rmvnorm(10000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "education", "gender", "age", "age2", "english", "bornus", "partisanship", "pctunemp", "pctcollege", "pcthisp")
X <- ldees[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs <- {}
ses <- {}

for (i in 1:length(names)) {
  
  means0 <- cbind(1, min(X[names[1]], na.rm = T), rep.row(colMeans(X[,which(names != names[1])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[1]], na.rm = T), rep.row(colMeans(X[,which(names != names[1])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  
}

ldees.df <- data.frame(names, coefs, ses, survey = "Validated Turnout (2008)")

nsl.ldees.coefs <- rbind(ldees.df, nsl.df)

limits <- aes(ymax = coefs + ses*1.96, ymin= coefs - ses*1.96)

nsl.ldees.coefs$names <- rep(c("Spanish-Language Stations", "Income", "Education", "Gender", "Age", "Age Sq.", "English", "Born in US", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic"), 2)

nsl.ldees.coefs$names <- factor(nsl.ldees.coefs$names, levels = rev(c("Spanish-Language Stations", "Income", "Education", "Gender", "Age", "Age Sq.","English", "Born in US", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic")))

nsl.ldees.coefs %>% ggplot(aes(y= coefs, x = names)) + geom_pointrange(limits, position=position_dodge(width=.5)) + facet_grid(. ~ survey) + theme_bw() + geom_hline(yintercept = 0) + labs(x = "", y = "First Differences", color = "Outcome", title = "") +  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() + theme(text = element_text(size=22))

ggsave("national_replication_test.pdf")