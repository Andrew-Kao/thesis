# This file is (currently) used for exploring the FCC contour data
# When completed, it will be used to produce SLTV contours.

# SETUP -------------------------------------------------------------------

library(dplyr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/instrument/all-cdbs-files') 
}

# Structure is cleaned with Haskell to drop extra fields
ownership_structure <- fread('ownership_structure_clean.dat',header = FALSE, sep = '|', fill=TRUE) %>%
  filter(V5 == "H" | V5 == "N") %>%
  rename(application_id = V2) %>%
  select(application_id, V5) %>%
  mutate(hispanic = ifelse(V5 == "H", 1, 0)) %>%
  group_by(application_id) %>%
  summarise(count = n(), hisp_sum = sum(hispanic)) %>%
  mutate(hisp_share = hisp_sum/count)

# try ownership_group next!
tv_owners <- fread('ownership_group.dat', header = FALSE, sep = '|') %>%
  rename(application_id = V2) %>%
  filter(V5 == "TV") %>%
  group_by(application_id) %>%
  right_join(ownership_structure, by = 'application_id') %>%
  filter(!is.na(V1))

# What's the match rate?
# sum(!is.na(ownership_group$V1)) 
