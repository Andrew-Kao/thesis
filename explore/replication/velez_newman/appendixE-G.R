# table E
# see figure5.R

# table F
# see figure6.R

# figure G
library(arm)
library(mvtnorm)
library(ggplot2)

lns <- read.csv("national_lns_data.csv")

fit1 <- glmer(latino.participation ~ sum + income + educ + sex + race + age + age2 + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), subset = english.media == 1)

betas <- fixef(fit1)
vcov <- as.matrix(summary(fit1)$vcov)
coefsim <- rmvnorm(10000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "educ", "sex", "race", "age", "age2", "usborn", "lifeinUS", "ideo7", "pid7", "pctunemp", "pctcollege", "pcthisp")
X <- lns[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs <- {}
ses <- {}

for (i in 1:length(names)) {
  
  means0 <- cbind(1, min(X[names[i]], na.rm = T), rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[i]], na.rm = T), rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  
}

fit2 <- glmer(latino.participation ~ sum + income + educ + sex + race + age + age2 + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), subset = spanish.media == 1)

betas <- fixef(fit2)
vcov <- as.matrix(summary(fit2)$vcov)
coefsim <- rmvnorm(100000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "educ", "sex", "race", "age", "age2", "usborn", "lifeinUS", "ideo7", "pid7", "pctunemp", "pctcollege", "pcthisp")
X <- lns[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs2 <- {}
ses2 <- {}

for (i in 1:length(names)) {
  
  means0 <- cbind(1, min(X[names[i]], na.rm = T), rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[i]], na.rm = T), rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs2[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses2[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  
}

fit3 <- glmer(latino.participation ~ sum + income + educ + sex + race + age + age2 + usborn + lifeinUS + ideo7 + pid7 + pctunemp + pctcollege + pcthisp + (1|county), data = lns, family = binomial(link="logit"), subset = bilingual.media == 1)

betas <- fixef(fit3)
vcov <- as.matrix(summary(fit3)$vcov)
coefsim <- rmvnorm(100000, betas, vcov)
k <- length(betas)
names <- c("sum", "income", "educ", "sex", "race", "age", "age2", "usborn", "lifeinUS", "ideo7", "pid7", "pctunemp", "pctcollege", "pcthisp")
X <- lns[,names]

rep.row<-function(x,n){
  matrix(rep(x,each=n),nrow=n)
}

coefs3 <- {}
ses3 <- {}

for (i in 1:length(names)) {
  
  means0 <- cbind(1, min(X[names[i]], na.rm = T), rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  means1 <- cbind(1, max(X[names[i]], na.rm = T), rep.row(colMeans(X[,which(names != names[i])], na.rm = T), 1))
  
  coefsim.m <- coefsim[,c(1, which(colnames(coefsim) == names[i]), which(colnames(coefsim) != names[i] & colnames(coefsim) != "(Intercept)"))]
  
  coefs3[i] <- mean(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  ses3[i] <- sd(invlogit(coefsim.m %*% t(means1)) - invlogit(coefsim.m %*% t(means0)))
  
}

coefs <- c(coefs, coefs2, coefs3)
ses <- c(ses, ses2, ses2)

gg.data <- data.frame(rep(names, 3), coefs, ses, rep(c("English", "Spanish", "Bilingual"), each = 14))
names(gg.data) <- c("names", "coefs", "ses", "language")

limits <- aes(ymax = coefs + ses*1.96, ymin= coefs - ses*1.96)

gg.data$names <- rep(c("Spanish-Language Stations", "Income", "Education", "Gender", "Race", "Age", "Age Sq.","Born in US", "Life in US", "Ideology", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic"), 3)

gg.data$names <- factor(gg.data$names, levels = rev(c("Spanish-Language Stations", "Income", "Education", "Gender", "Race", "Age", "Age Sq.","Born in US", "Life in US", "Ideology", "Partisanship", "Pct. Unemployed", "Pct. College", "Pct. Hispanic")))

gg.data$Language <- factor(gg.data$language, levels = unique(gg.data$language))

p <- ggplot(gg.data[c(1,15,29),], aes(y= coefs, x = names)) + geom_pointrange(limits, position=position_dodge(width=.25)) + theme_bw() + geom_hline(yintercept = 0) + labs(x = "", y = "First Differences", color = "Media Language Preference", title = "Ethnic Civic Participation") +  theme(plot.title = element_text(hjust = 0.5)) + scale_color_grey(start = 0, end = .5) + coord_flip() + facet_grid(Language ~ ., scale = "free")

p + theme(text = element_text(size=22))
