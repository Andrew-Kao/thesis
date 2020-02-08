library(ggplot2)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/analysis/Output/graphs') 
}

# Figures

# Firms
plotdf <- regDataF %>%
  mutate(inout = ifelse(intersects == 1, 1, -1),
         distance = distance/1000, dist2 = distance^2,
        dist = inout * distance,
        logPop = log(population)) %>%
  filter(distance < 100)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(busnCount)))
ggplot(data = plotdf, aes(x=cut(dist,5),ihs(busnCount))) + geom_boxplot()
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodName))
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

plotdf <- busn2@data %>%
  mutate(inout = ifelse(inside == 1, 1, -1),
         distance = minDist/1000,
         dist = inout * distance)  %>%
  filter(distance < 100)
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodName))


# cut these all from -50 to 50??
plotdf <- cleanSchoolAll %>%
  mutate(inout = ifelse(inside == 1, 1, -1),
         distance = minDist/1000,
         dist = inout * distance)  %>%
  filter(distance < 50)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_lepenr_hi)), n = 9)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_apenr_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_discwodis_singoos_hi)), n = 8) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50
ggsave("hispanicsuspensions.pdf")
ggplot() + geom_boxplot(data = plotdf, aes(cut(dist,5),ihs(sch_discwodis_singoos_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_hbreported_rac_hi)), n = 9)

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

