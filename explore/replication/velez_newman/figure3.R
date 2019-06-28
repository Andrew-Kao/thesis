library(arm)
library(ggplot2)
library(dplyr)

# load data (b = buffer size)
nc_data <- read.csv("wuvc_nc_voting_data.csv")

# extract coefficients and standard errors
coefs <- c(coef(lm(voted.2000 ~ treatment, nc_data, subset = district == 1 & b <= 500))[2],
           coef(lm(voted.2004 ~ treatment + voted.2000, nc_data, subset = district == 1 & b <= 500))[2],
           coef(lm(voted.2008 ~ treatment + voted.2000, nc_data, subset = district == 1 & b <= 500))[2],
           coef(lm(voted.2012 ~ treatment + voted.2000, nc_data, subset = district == 1 & b <= 500))[2])

ses <- c(se.coef(lm(voted.2000 ~ treatment, nc_data, subset = district == 1 & b <= 500))[2],
         se.coef(lm(voted.2004 ~ treatment + voted.2000, nc_data, subset = district == 1 & b <= 500))[2],
         se.coef(lm(voted.2008 ~ treatment + voted.2000, nc_data, subset = district == 1 & b <= 500))[2],
         se.coef(lm(voted.2012 ~ treatment + voted.2000, nc_data, subset = district == 1 & b <= 500))[2])

# provide labels for each election year
names <- c("2000", "2004", "2008", "2012")

plot_df <- data.frame(names, coefs, ses); limits <- aes(ymax = coefs + 1.96*ses, ymin = coefs - 1.96*ses)

plot_df %>% ggplot(aes(y=coefs, x=names)) + geom_pointrange(limits, position=position_dodge(width=.5)) + theme_bw() + geom_hline(yintercept = 0) + labs(x = "Election Year", y = "Estimated Regression Coefficients", color = "Demographics") + scale_color_grey(start = .8, end = .4) + theme(text = element_text(size=22)) 

ggsave("nc_grd.pdf")