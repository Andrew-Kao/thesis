library(ggplot2)
library(Rmisc)

results <- read.csv("placebo_fl.csv")

limits <- aes(ymax = Rob.CIu, ymin= Rob.CIl)

age <- ggplot(results[1:7,], aes(y=Estimate, x=Point)) + geom_pointrange(limits, position=position_dodge(width=.5)) + scale_y_continuous(limits = c(-10, 10)) + geom_hline(yintercept = 0, col = "red") + scale_x_discrete(limits = 1:7) + labs(x = "Boundary Point", y = "LATE", shape = "Year", title = "Balance Check (Age)") + theme(plot.title = element_text(hjust = 0.5))

age

gender <- ggplot(results[8:14,], aes(y=Estimate, x=Point)) + geom_pointrange(limits, position=position_dodge(width=.5)) + scale_y_continuous(limits = c(-.25, .25)) + geom_hline(yintercept = 0, col = "red") + scale_x_discrete(limits = 1:7) + labs(x = "Boundary Point", y = "LATE", shape = "Year", title = "Balance Check (Gender)") + theme(plot.title = element_text(hjust = 0.5))

gender

rep <- ggplot(results[15:21,], aes(y=Estimate, x=Point)) + geom_pointrange(limits, position=position_dodge(width=.5)) + scale_y_continuous(limits = c(-1, 1)) + geom_hline(yintercept = 0, col = "red") + scale_x_discrete(limits = 1:7) + labs(x = "Boundary Point", y = "LATE", shape = "Year", title = "Balance Check (Partisanship)") + theme(plot.title = element_text(hjust = 0.5))

rep

v2000 <- ggplot(results[22:28,], aes(y=Estimate, x=Point)) + geom_pointrange(limits, position=position_dodge(width=.5)) + scale_y_continuous(limits = c(-1, 1)) + geom_hline(yintercept = 0, col = "red") + scale_x_discrete(limits = 1:7) + labs(x = "Boundary Point", y = "LATE", shape = "Year", title = "Balance Check (2000 Vote)") + theme(plot.title = element_text(hjust = 0.5))

v2000

multiplot(age, gender, rep, v2000, cols=2)


