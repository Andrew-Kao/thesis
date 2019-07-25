##### MERGE ALL DONOR DATA ######

library(raster)
library(rgdal)
library(dplyr)

# Data sources
# 1. Cleaned donor data (direct)
# 2. Name classification data
# 3. Location data

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

# I think we want to try just using the base name, but also try keeping the Hispanic names

FL <- read.csv('names_predict_FL.csv') %>%
  group_by(race) %>%
  summarise(count = n())

census <- read.csv('names_predict_census.csv') %>%
  group_by(race) %>%
  summarise(count = n())

wiki <- read.csv('names_predict_wiki.csv') %>%
  group_by(race) %>%
  summarise(count = n())
