# This file is used to turn the Trump Donations csv to a list of addresses for ArcGIS look-up


library(dplyr)
library(data.table)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/politics/trump_donations') 
}

donations <- fread('TrumpDonations.csv', data.table=TRUE) 

addresses <- donations %>%
  rename(street = contributor_street_1, city = contributor_city, state2 = contributor_state, zip = postcode) %>%
  dplyr::select(street, city, state2, zip) %>%
  distinct() %>%
  mutate(country = "USA") %>%
  write_csv('TrumpAddresses.csv')
