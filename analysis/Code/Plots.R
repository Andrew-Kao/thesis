library(ggplot2)


# Figures

# Firms
plotdf <- regDataF %>%
  mutate(inout = ifelse(intersects == 1, 1, -1),
         distance = distance/1000,
        dist = inout * distance) %>%
  filter(distance < 100)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(busnCount)))


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
  filter(distance < 100)
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_lepenr_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_apenr_hi)))
ggplot() + geom_smooth(data = plotdf, aes(dist,ihs(sch_discwodis_singoos_hi)))


