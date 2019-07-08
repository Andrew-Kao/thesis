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
  select(V2, V5) %>%
  mutate(hispanic = ifelse(V5 == "H", 1, 0)) %>%
  rename(application_id = V2) %>%
  group_by(application_id) %>%
  summarise(count = n(), hisp_sum = sum(hispanic)) %>%
  mutate(hisp_share = hisp_sum/count)

## join with altc group to get the facility id
# either V3 or V5 is the key, the other is the facility id
altc_group <- fread('altc_group.dat', header = FALSE, sep = '|') %>%
  rename(application_id = V3) %>%
  right_join(ownership_structure, by = 'application_id')

# checking with this for both V3/V5, we get nothing:
# sum(!is.na(altc_group$V1)) 
