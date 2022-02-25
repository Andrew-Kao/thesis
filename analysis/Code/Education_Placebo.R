###### Education: placebo contours #####

library(stargazer)
library(dplyr)
library(purrr)
library(stringr)
library(sandwich)
library(lmtest)
library(lfe)
library(ggplot2)
library(DescTools)
library(glmnet)
library(hdm)
library(pscl)
library(spdep)
library(spatialreg)
library(missForest)
library(fixest)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("rename", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("arrange", "dplyr")


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/education') 
}


harass <- read.csv(file = "dda.csv")

# plan:
# 1. draw fake contour
# https://search.r-project.org/CRAN/refmans/sampSurf/html/spCircle.html 
# 2. classify schools as inside/outside by lat/long
# 3. run feols, store result
# 4. repeat 1-3 1000x
# 5. plot result



###### alternate placebos:
# randomly draw schools 

# initiate empty tibbles
set.seed(24601)
placebo_1 <- tibble(coeff = numeric(), se = numeric())
placebo_2 <- tibble(coeff = numeric(), se = numeric())
placebo_3 <- tibble(coeff = numeric(), se = numeric())

for (i in 1:1000) {
  # generate random TV indicators
  placebo_TV <- rbinom(n=nrow(harass)/2, size=1, prob=0.5)
  placebo_TV <- placebo_TV %>%
    append(placebo_TV)
  harass_temp <- harass %>%
    mutate(TV = placebo_TV)
  
  # run regressions and store results
  om1 <- feols(ihs(sch_satact) ~ TV*eth +
                 origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 | LEAID, cluster = c("LEAID"), data=harass_temp,
               subset = harass$origdist < 100)
  placebo_1 <- placebo_1 %>% 
    add_row(coeff = om1$coefficients['TV:eth'], se = om1$se['TV:eth'])
  
  om2 <- feols(ihs(sch_mathenr_calc) ~ TV*eth +
                 origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 | LEAID, cluster = c("LEAID"), data=harass_temp,
               subset = harass$origdist < 100)
  placebo_2 <- placebo_2 %>% 
    add_row(coeff = om2$coefficients['TV:eth'], se = om2$se['TV:eth'])
  
  om3 <- feols(ihs(sch_appass_oneormore) ~ TV*eth +
                 origpcHisp + origLogInc + origLogPop + SCH_TEACHERS_CURR_TOT +  hisp_students + asian_students  + 
                 total_students + SCH_GRADE_G01 + SCH_GRADE_G06 + SCH_GRADE_G09 | LEAID, cluster = c("LEAID"), data=harass_temp,
               subset = harass$origdist < 100)
  placebo_3 <- placebo_3 %>% 
    add_row(coeff = om3$coefficients['TV:eth'], se = om3$se['TV:eth'])
}

## make plots

placebo_1 <- placebo_1 %>%
  mutate(top = coeff + 1.96 * se, bottom = coeff - 1.96 * se) %>%
  arrange(coeff) %>%
  mutate(x = 1:nrow(placebo_1))

placebo_1_s <- placebo_1 %>%
  filter((top > 0 & bottom > 0) | (top < 0 & bottom < 0))
placebo_1_ns <- placebo_1 %>%
  filter((top > 0 & bottom < 0))
placebo_1_real <- tibble(coeff = 0.1598, top = 0.1598 + 1.96 * 0.0264, bottom = 0.1598 - 1.96 * 0.0264,
                         x = 1001:1002)

ggplot(placebo_1_ns, aes(x = x, y = coeff)) +
  geom_point() +
  geom_ribbon(data = placebo_1, aes(ymin = bottom, ymax = top),
              alpha = 0.1) +
  geom_point(data = placebo_1_s, aes(x = x, y = coeff, color = "red")) +
  geom_ribbon(data = placebo_1_real, aes(x=x,ymin = bottom, ymax = top)) +
  geom_point(data = placebo_1_real, aes(x = x, y = coeff, color = "blue")) +
  labs(y = "Coefficient", x = "IHS(SAT/ACTs taken)") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_manual(labels = c("actual","p < .05"), values = c("blue","red"))
ggsave("../../../analysis/Output/graphs/placebo_random_1.pdf")

placebo_2 <- placebo_2 %>%
  mutate(top = coeff + 1.96 * se, bottom = coeff - 1.96 * se) %>%
  arrange(coeff) %>%
  mutate(x = 1:nrow(placebo_2))

placebo_2_s <- placebo_2 %>%
  filter((top > 0 & bottom > 0) | (top < 0 & bottom < 0))
placebo_2_ns <- placebo_2 %>%
  filter((top > 0 & bottom < 0))
placebo_2_real <- tibble(coeff = 0.2718, top = 0.2718 + 1.96 * 0.0369, bottom = 0.2718 - 1.96 * 0.0369,
                         x = 1001:1002)

ggplot(placebo_2_ns, aes(x = x, y = coeff)) +
  geom_point() +
  geom_ribbon(data = placebo_2, aes(ymin = bottom, ymax = top),
              alpha = 0.1) +
  geom_point(data = placebo_2_s, aes(x = x, y = coeff, color = "red")) +
  geom_ribbon(data = placebo_2_real, aes(x=x,ymin = bottom, ymax = top)) +
  geom_point(data = placebo_2_real, aes(x = x, y = coeff, color = "blue")) +
  labs(y = "Coefficient", x = "IHS(calculus taken)") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_manual(labels = c("actual","p < .05"), values = c("blue","red"))
ggsave("../../../analysis/Output/graphs/placebo_random_2.pdf")

placebo_3 <- placebo_3 %>%
  mutate(top = coeff + 1.96 * se, bottom = coeff - 1.96 * se) %>%
  arrange(coeff) %>%
  mutate(x = 1:nrow(placebo_3))

placebo_3_s <- placebo_3 %>%
  filter((top > 0 & bottom > 0) | (top < 0 & bottom < 0))
placebo_3_ns <- placebo_3 %>%
  filter((top > 0 & bottom < 0))
placebo_3_real <- tibble(coeff = 0.0964, top = 0.0964 + 1.96 * 0.0346, bottom = 0.0964 - 1.96 * 0.0346,
                         x = 934:935)

ggplot(placebo_3_ns, aes(x = x, y = coeff)) +
  geom_point() +
  geom_ribbon(data = placebo_3, aes(ymin = bottom, ymax = top),
              alpha = 0.1) +
  geom_point(data = placebo_3_s, aes(x = x, y = coeff, color = "red")) +
  geom_ribbon(data = placebo_3_real, aes(x=x,ymin = bottom, ymax = top)) +
  geom_point(data = placebo_3_real, aes(x = x, y = coeff, color = "blue")) +
  labs(y = "Coefficient", x = "IHS(APs passed)") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  scale_color_manual(labels = c("actual","p < .05"), values = c("blue","red"))
ggsave("../../../analysis/Output/graphs/placebo_random_3.pdf")
