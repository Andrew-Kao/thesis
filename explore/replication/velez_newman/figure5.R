library(ggplot2)
library(dplyr)

# create data frame to plot estimates
results <- list()
results[[1]] <- read.csv("wbwp_fl_estimates_v2000.csv")
results[[2]] <- read.csv("wbwp_fl_estimates_v2004.csv")
results[[3]] <- read.csv("wbwp_fl_estimates_v2008.csv")
results[[4]] <- read.csv("wbwp_fl_estimates_v2012.csv")

plot_df <- cbind(rep(c(2000, 2004, 2008, 2012), each = 7), do.call(rbind.data.frame, results))
names(plot_df)[1] <- "year"; plot_df$year <- as.factor(plot_df$year)
limits <- aes(ymax = Rob.CIu, ymin= Rob.CIl)

# plot estimates (without boundary points that failed placebo test)
plot_df %>% filter(Point < 6) %>% ggplot(aes(y=Estimate, x=Point)) + geom_pointrange(limits, position=position_dodge(width=.5)) + facet_grid(. ~ year) + geom_pointrange(limits, position=position_dodge(width=.5)) + geom_hline(yintercept = 0) + labs(x = "\n Boundary Point", y = "Estimate") + scale_color_grey(start = .8, end = .4) +  theme(plot.title = element_text(hjust = 0.5)) + scale_x_discrete(limits = 1:5) + theme_bw() + theme(text = element_text(size=22))

ggsave("fl_grd.pdf")

