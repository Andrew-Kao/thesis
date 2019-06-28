library(ggplot2)
library(arm)

data <- read.csv("wuvc_nc_balance_data.csv")

sizes <- c(100, 200, 300, 400, 500, 750, 1000)

c.00 <- sapply(15:21, function(x) coef(lm(voted.2000 ~ treatment, data, subset = district == 1 & data[,x] == 1))[2])
s.00 <- sapply(15:21, function(x) se.coef(lm(voted.2000 ~ treatment, data, subset = district == 1 & data[,x] == 1))[2])

c.04 <- sapply(15:21, function(x) coef(lm(voted.2004 ~ treatment + voted.2000, data, subset = district == 1 & data[,x] == 1))[2])
s.04 <- sapply(15:21, function(x) se.coef(lm(voted.2004 ~ treatment + voted.2000, data, subset = district == 1 & data[,x] == 1))[2])

c.08 <- sapply(15:21, function(x) coef(lm(voted.2008 ~ treatment + voted.2000, data, subset = district == 1 & data[,x] == 1))[2])
s.08 <- sapply(15:21, function(x) se.coef(lm(voted.2008 ~ treatment + voted.2000, data, subset = district == 1 & data[,x] == 1))[2])

c.12 <- sapply(15:21, function(x) coef(lm(voted.2012 ~ treatment + voted.2000, data, subset = district == 1 & data[,x] == 1))[2])
s.12 <- sapply(15:21, function(x) se.coef(lm(voted.2012 ~ treatment + voted.2000, data, subset = district == 1 & data[,x] == 1))[2])

df <- data.frame(buffer = rep(sizes, 4), year = rep(c(2000, 2004, 2008, 2012), each = 7), coefs = rbind(cbind(c.00, s.00), cbind(c.04, s.04), cbind(c.08, s.08), cbind(c.12, s.12)))

names(df)[3:4] <- c("coefs","ses")

limits <- aes(ymax = coefs + 1.96*ses, ymin = coefs - 1.96*ses)

p <- ggplot(df, aes(y=coefs, x=year, color = factor(buffer))) + geom_pointrange(limits, position=position_dodge(width=1)) + theme_bw() + geom_hline(yintercept = 0) + labs(x = "Year", y = "Estimated Regression Coefficients", color = "Buffer Size") + scale_color_grey(start = .8, end = .4) + labs(title = "Robustness Check - Buffer Sizes") + theme(plot.title = element_text(hjust = 0.5))

p
