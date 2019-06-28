library(stargazer)
library(lme4)
library(arm)
library(ggplot2)
library(ordinal)
library(mvtnorm)
library(dplyr)

lns <- read.csv("national_lns_data.csv")

fake.clm <- function(x) {
  model.fake <- x
  model.fake$nobs <- x$dims$nobs
  model.fake$call[1] <- call("clm")
  model.fake
}

# estimate LNS model
fit.lns.1 <- glmer(latino.participation ~ sum + income + educ + sex + race + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

# code for plotting predicted probabilities
betas <- fixef(fit.lns.1)
vcov <- as.matrix(summary(fit.lns.1)$vcov)
coefsim <- rmvnorm(10000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "educ", "sex", "race", "age", "age2", "english", "usborn", "lifeinUS", "ideo7", "pid7", "pctunemp", "pctcollege", "pcthisp")
X <- lns[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs <- {}
ses <- {}

for (i in 1:length(names)) {
  means0 <- cbind(1, min(X[names[i]], na.rm = T), 
                  rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[i]], na.rm = T), 
                  rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), 
            which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
}

plot_df <- data.frame(names = names, coefs = coefs, ses = ses, variable = "Ethnic Civic Participation")
limits <- aes(ymax = coefs + ses*1.96, ymin= coefs - ses*1.96)

# estimate political knowledge model
fit.lns.2 <- glmer(pk ~ sum + income + educ + sex + race + age + age2 + english + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), control=glmerControl(optimizer="bobyqa"))

# code for plotting predicted probabilites
betas <- fixef(fit.lns.2)
vcov <- as.matrix(summary(fit.lns.2)$vcov)
coefsim <- rmvnorm(10000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "educ", "sex", "race", "age", "age2", "usborn","english","lifeinUS", "ideo7", "pid7", "pctunemp", "pctcollege", "pcthisp")
X <- lns[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs <- {}
ses <- {}


for (i in 1:length(names)) {
  
  means0 <- cbind(1, min(X[names[i]], na.rm = T), 
                  rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[i]], na.rm = T), 
                  rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), 
                          which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  
}

limits <- aes(ymax = coefs + ses*1.96, ymin= coefs - ses*1.96)

plot_df2 <- data.frame(names = names, coefs = coefs, ses = ses, variable = "Political Knowledge")

gg.data <- rbind(plot_df, plot_df2)

gg.data$names <- rep(c("Spanish-Language Stations", "Income", "Education", "Gender", "Race", "Age", "Age Sq.", "Born in US", "English", "Life in US", "Ideology", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic"), 2)

gg.data$names <- factor(gg.data$names, levels = rev(c("Spanish-Language Stations", "Income", "Education", "Gender", "Race", "Age", "Age Sq.", "Born in US", "English", "Life in US", "Ideology", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic")))

gg.data %>% ggplot(aes(y= coefs, x = names)) + facet_grid(. ~ variable, scales = "free") + geom_pointrange(limits, position=position_dodge(width=.25)) + theme_bw() + geom_hline(yintercept = 0) + labs(x = "", y = "First Differences", color = "Media Language Preference", title = "") +  theme(plot.title = element_text(hjust = 0.5)) + scale_color_grey(start = 0, end = .5) +theme(panel.spacing = unit(2, "lines")) + coord_flip() + theme(text = element_text(size=22))

ggsave("national_intermediate_outcomes.pdf")
