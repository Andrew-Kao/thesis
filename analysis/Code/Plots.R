library(ggplot2)
library(dplyr)


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
m1 <- lm(busn ~ logPop + pcHispanic + income + dist2 , data=plotdf)
pred_busnCount <- predict(m1,plotdf)
m2 <- lm(hispFoodNameD ~ logPop + pcHispanic + income + dist2, data=plotdf)
pred_foodName <- predict(m2,plotdf)
plotdf <- plotdf %>%
  mutate(pred_busnCount = busn - pred_busnCount,
         pred_foodName = hispFoodNameD - pred_foodName)
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
  filter(distance < 50)
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodNameD))
ggplot() + geom_smooth(data = plotdf, aes(dist,hispFoodName))
m1 <- glm(hispFoodNameD ~ logPop + origpcHisp + origincome , data=plotdf, family = binomial)
pred_foodName <- predict(m1,plotdf)
plotdf <- plotdf %>%
  mutate(pred_foodName = hispFoodNameD - pred_foodName - 4.835) # subtract mean?
ggplot() + geom_smooth(data = plotdf, aes(dist,pred_foodName), n = 9) +
  labs(x = "Distance to Contour Boundary (KM)", y = "Dummy(Hispanic Businesses with Hispanic Name)") # do w/ 50
ggsave("hispanicbusnname.pdf")


# Schools
# cut these all from -50 to 50??
plotdf <- cleanSchoolAll %>%
  mutate(inout = ifelse(inside == 1, 1, -1),
         distance = minDist/1000,
         dist = inout * distance,
         origLogPop = log(origpopulation), origLogInc = log(origincome)) %>%
  filter(distance < 50)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_lepenr_hi)), n = 9)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_apenr_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_discwodis_singoos_hi)), n = 8) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50
ggsave("hispanicsuspensions.pdf")
m1 <- lm(ihs(sch_discwodis_singoos_hi) ~ origpcHisp + origLogInc + origLogPop + hisp_students, data=plotdf)
pred_suspend <- predict(m1,plotdf)
plotdf <- plotdf %>%
  mutate(pred_suspend = ihs(sch_discwodis_singoos_hi) - pred_suspend)
ggplot() + geom_smooth(data = plotdf, aes(dist,pred_suspend), n = 8) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50
ggplot() + geom_boxplot(data = plotdf, aes(cut(dist,5),ihs(sch_discwodis_singoos_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_hbreported_rac_hi)), n = 9)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_absent_hi)),n=7) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(# Hispanic Students Chronically Absent)") # do w/ 27
ggsave("hispanic.pdf")

## take harass from mech of education_right
mechdf <- harass %>%
  mutate(inout = ifelse(inside == 1, 1, -1),
         distance = minDist/1000,
         dist = inout * distance,
         word_edu_med = median(harass$word_edu_mean),
         word_latin_med = median(harass$word_latin_mean),
         word_rolemodel_med = median(harass$word_rolemodel_mean),
         above_edu_med = ifelse(word_edu_mean > word_edu_med, 1, 0),
         above_latin_med = ifelse(word_latin_mean > word_latin_med, 1, 0),
         above_rolemodel_med = ifelse(word_rolemodel_mean > word_rolemodel_med, 1, 0))

smallmechdf <- mechdf %>%
  filter(distance < 50)

colours <- c("above"="#00BFC4","below"="#F8766D")
atext <- "Above"
btext <- "Below"
  
ggplot() + geom_smooth(data = subset(smallmechdf,above_edu_med == 1), aes(dist,ihs(sch_absent_hi), colour = "above"), n = 8) +
  geom_smooth(data = subset(smallmechdf,above_edu_med == 0), aes(dist,ihs(sch_absent_hi), colour = "below"), n = 8) +
  scale_colour_manual(name="Median education programming",values=colours,labels=c(atext,btext)) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50
ggplot() + geom_smooth(data = subset(smallmechdf,above_latin_med == 1), aes(dist,ihs(sch_absent_hi), colour = "above"), n = 8) +
  geom_smooth(data = subset(smallmechdf,above_latin_med == 0), aes(dist,ihs(sch_absent_hi), colour = "below"), n = 8) +
  scale_colour_manual(name="Median Hispanic programming",values=colours,labels=c(atext,btext)) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50
ggplot() + geom_smooth(data = subset(smallmechdf,above_rolemodel_med == 1), aes(dist,ihs(sch_absent_hi), colour = "above"), n = 8) +
  geom_smooth(data = subset(smallmechdf,above_rolemodel_med == 0), aes(dist,ihs(sch_absent_hi), colour = "below"), n = 8) +
  scale_colour_manual(name="Median good role model programming",values=colours,labels=c(atext,btext)) +
  labs(x = "Distance to Contour Boundary (KM)", y = "IHS(Hispanic Students Suspended)") # do w/ 50


binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}



##### TIME USE
if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/TUS') 
}
instrument_all <- readRDS("../instrument/countyInstrumentCovariate.Rdata") %>%
  mutate(TV = ifelse(intersects == 1 & (areaRatio > .95 | dist > 0),1,0)) %>%
  mutate(stateCounty = paste0(state, county))

atus2015 <- read.csv('atus_indiv_tv_all.csv') %>%
  mutate(stateCounty = str_pad(county,5,side = "left", pad = "0"))

atusCounty = inner_join(atus2015, instrument_all, by = "stateCounty") %>%
  mutate(logPop = log(population), income = log(income)) %>%
  mutate(dist2 = dist*dist, hispanic_d = if_else(hispan != "Not Hispanic",1,0),
         age2 = age*age,
         foreign = if_else(citizen == "Foreign born, not a U.S. citizen" | citizen == "Foreign born, U.S. citizen by naturalization",
                           1, 0),
         inout = ifelse(intersects == 1, 1, -1),
         dist = dist * inout,
         dist = dist/1000) 



if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/analysis/Output/graphs') 
}

mdext <- mean(atusCounty$duration_ext)
m1 <- lm(duration_ext ~ income + logPop + pcHisp + age + sex + age2 , data=atusCounty)
pred_dext <- predict(m1,atusCounty)
atusCounty <- atusCounty %>%
  mutate(pdext = duration_ext - pred_dext + mdext)

ggplot() + geom_smooth(data = atusCounty[atusCounty$dist > -100 & atusCounty$hispanic_d == 1,], aes(dist,pdext, color = "blue")) +
 geom_smooth(data = atusCounty[atusCounty$dist > -100  & atusCounty$hispanic_d == 0,], aes(dist,pdext, color = "red")) +
  labs(x = "Distance to contour boundary (KM)", y = "Minutes of TV watched") +
  theme(legend.position = c(0.2, 0.8)) + scale_color_discrete(name = "Demographic",
                                                              labels = c("Hispanic", "Non-Hispanic"))
ggsave("atus.pdf")


atusPoint <- atusCounty %>%
  filter(dist != 0)

ggplot() + stat_binscatter(data = atusPoint[atusPoint$dist > -100  &  atusPoint$hispanic_d == 1,], aes(dist,pdext, color = "blue"), alpha = 0.5) +
  stat_binscatter(data = atusPoint[atusPoint$dist > -100  &  atusPoint$hispanic_d == 0,], aes(dist,pdext, color = "red"), alpha = 0.5) +
  geom_smooth(data = atusCounty[atusCounty$dist > 0 & atusCounty$hispanic_d == 1,], aes(dist,pdext, color = "blue")) +
  geom_smooth(data = atusCounty[atusCounty$dist > 0  & atusCounty$hispanic_d == 0,], aes(dist,pdext, color = "red")) +
  geom_smooth(data = atusCounty[atusCounty$dist > -100 & atusCounty$dist < 0 & atusCounty$hispanic_d == 1,], aes(dist,pdext, color = "blue")) +
  geom_smooth(data = atusCounty[atusCounty$dist > -100  & atusCounty$dist < 0 &  atusCounty$hispanic_d == 0,], aes(dist,pdext, color = "red")) +
  labs(x = "Distance to contour boundary (KM)", y = "Minutes of TV watched") +
  theme(legend.position = c(0.2, 0.8)) + scale_color_discrete(name = "Demographic",
                                                              labels = c("Hispanic", "Non-Hispanic")) + 
  coord_cartesian(xlim =c(-100, NA), ylim = c(150, 200))
ggsave("atus2.pdf")







StatBinscatter <- ggplot2::ggproto(
  "StatBinscatter", 
  Stat,
  compute_group = function(data, scales, bins = 25) {
    bins     <- min(floor(nrow(data)/10), bins)
    x_bin    <- ggplot2::cut_number(data$x + 1e-12*runif(nrow(data)), bins)
    x_means  <- stats::ave(data$x, x_bin, FUN = mean)
    y_means  <- stats::ave(data$y, x_bin, FUN = mean)
    y_se     <- stats::ave(data$y, x_bin, FUN = sd)
    y_obs    <- stats::ave(data$y, x_bin, FUN = length)
    result   <- data.frame(x    = x_means, 
                           y    = y_means, 
                           ymax = y_means + 1.96*y_se/sqrt(y_obs),
                           ymin = y_means - 1.96*y_se/sqrt(y_obs))
    result   <- unique(result)
    return(result)
  },
  required_aes = c("x", "y")
)

#' Binscatter
#' 
#' Group variable on the horizontal axis into equal-sized bins and calculate group means
#' for each bin
#' @param bins Number of bins (defaults to 10)
#' @param geom Which geom to plot (defaults to "point", other options include "pointrange" or 
#' "line")
#' @examples 
#' 
#' ggplot(mpg, aes(x = displ, y = hwy)) + 
#' geom_point(alpha = .1) + 
#' stat_binscatter(color = "red")
#' 
#' ggplot(mpg, aes(x = displ, y = hwy)) + 
#' geom_point(alpha = .1) + 
#' stat_binscatter(color = "red", geom = "pointrange")
#' 
#' ggplot(diamonds, aes(x = carat, y = price, color = cut)) + 
#' stat_binscatter(bins = 20, geom = "pointrange") +
#' stat_binscatter(bins = 20, geom = "line")
#' 
stat_binscatter <- function(mapping = NULL, data = NULL, geom = "point",
                            position = "identity", na.rm = FALSE, show.legend = NA, 
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(
    stat = StatBinscatter, data = data, mapping = mapping, geom = geom, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
