library(ggplot2)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/analysis/Output/graphs') 
}

# Figures
# Donations
plotdf <- regDataT %>%
  mutate(donations = rawDonations*hispanicSum, logPop = log(population),
         donations_d = rawDonations * (hispanicDummy), # ceiling(dummy)
         distance = distance/1000, dist2 = distance^2,
         inout = ifelse(intersects == 1, 1, -1),
         donations_dum = ifelse(donations_d > 0, 1, 0)) %>%
  filter(distance < 50) %>%
  mutate(distance = distance * inout)

ggplot() + geom_smooth(data = plotdf, aes(distance,ihs(donations)),n = 9) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(# Hispanic Donations to Trump)") # do w/ 50
ggsave("hispanictrump.pdf")

# Firms
plotdf <- regDataF %>%
  mutate(inout = ifelse(intersects == 1, 1, -1),
         distance = distance/1000, dist2 = distance^2,
        dist = inout * distance,
        logPop = log(population),
        busn = busnCount * hispMajSum,
        hispFoodNameD = ifelse(hispFoodName * hispSum > 0, 1, 0)) %>%
  filter(distance < 100)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(pcHispanic)))
ggplot(data = plotdf, aes(x=cut(dist,5),ihs(busnCount))) + geom_boxplot()
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodNameD))
ggplot(data = plotdf, aes(dist,hispFoodName)) + geom_smooth(method = lm, formula = y ~ splines::bs(x, 10), se = FALSE)
m1 <- lm(busnCount ~ logPop + pcHispanic + income + dist2 , data=plotdf)
pred_busnCount <- predict(m1,plotdf)
m2 <- lm(hispFoodName ~ logPop + pcHispanic + income + dist2, data=plotdf)
pred_foodName <- predict(m2,plotdf)
plotdf <- plotdf %>%
  mutate(pred_busnCount = pred_busnCount,
         pred_foodName = pred_foodName)
ggplot() + geom_smooth(data = plotdf, aes(dist,pred_busnCount))
ggplot() + geom_smooth(data = plotdf, aes(dist,pred_foodName))
ggplot(data = plotdf, aes(dist,pred_foodName)) + geom_smooth(method = lm, formula = y ~ splines::bs(x, 5), se = FALSE)
ggplot(data = plotdf, aes(dist,pred_busnCount)) + geom_point() + geom_quantile()

plotdf <- busn2@data %>%
  mutate(inout = ifelse(inside == 1, 1, -1),
         distance = minDist/1000,
         dist = inout * distance,
         logPop = log(origpopulation),
         hispFoodNameD = ifelse(hispFoodName * hisp_sum > 0, 1,0),
         hispFoodName = hispFoodName * hisp_sum)  %>%
  filter(distance < 100)
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodNameD))
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodName))
m1 <- glm(hispFoodNameD ~ logPop + origpcHisp + origincome , data=plotdf, family = binomial)
pred_foodName <- predict(m1,plotdf)
plotdf <- plotdf %>%
  mutate(pred_foodName = pred_foodName)
ggplot() + geom_smooth(data = plotdf, aes(dist,pred_foodName), n = 12)


# Schools
# cut these all from -50 to 50??
plotdf <- cleanSchoolAll %>%
  mutate(inout = ifelse(inside == 1, 1, -1),
         distance = minDist/1000,
         dist = inout * distance)  %>%
  filter(distance < 27)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_lepenr_hi)), n = 9)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_apenr_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_discwodis_singoos_hi)), n = 8) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50
ggsave("hispanicsuspensions.pdf")
ggplot() + geom_boxplot(data = plotdf, aes(cut(dist,5),ihs(sch_discwodis_singoos_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_hbreported_rac_hi)), n = 9)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_absent_hi)),n=7) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(# Hispanic Students Chronically Absent)") # do w/ 27
ggsave("hispanic.pdf")

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

