wuvc <- read.csv("wuvc_nc_aux_data.csv")

p_his <- sapply(seq(50, 1000, by = 50), function(x) coef(lm(p_his ~ t, wuvc, subset = st_distance < x))['t'])
p_his_se <- sapply(seq(50, 1000, by = 50), function(x) se.coef(lm(p_his ~ t, wuvc, subset = st_distance < x))['t'])

plot(seq(50, 1000, by = 50), p_his, type = "line", ylim = c(-.05,.05), 
     xlab = "Distance to Boundary", lwd = 2, 
     ylab = "Difference in Pr(Latino)", 
     main = "Distance to Boundary and Ethnicity Estimate")

lines(seq(50, 1000, by = 50), p_his + 1.96*p_his_se, ylim = c(-.05,.05), lty = 2)
lines(seq(50, 1000, by = 50), p_his - 1.96*p_his_se, ylim = c(-.05,.05), lty = 2)

abline(h = 0)

p_his <- sapply(seq(50, 1000, by = 50), 
                function(x) coef(lm(latino ~ t, wuvc, subset = st_distance < x & p_his > .90))['t'])

p_his_se <- sapply(seq(50, 1000, by = 50), 
                   function(x) se.coef(lm(latino ~ t, wuvc, subset = st_distance < x & p_his > .90))['t'])

plot(seq(50, 1000, by = 50), p_his, type = "line", ylim = c(-1,1), 
     xlab = "Distance to Boundary", lwd = 2, 
     ylab = "Difference in Latino Self-Identification", 
     main = "Estimated Pr(Latino) > .90")

lines(seq(50, 1000, by = 50), p_his + 1.96*p_his_se, ylim = c(-.05,.05), lty = 2)
lines(seq(50, 1000, by = 50), p_his - 1.96*p_his_se, ylim = c(-.05,.05), lty = 2)

abline(h = 0)

