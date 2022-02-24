###### SCI data cleaning ####

library(tidyverse)


if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/SCI') 
}

## clean county to country data
sci_country <- read.table(file = 'us-counties-countries-fb-social-connectedness-index-october-2021.tsv',
                          sep = '\t', header = TRUE) %>%
  mutate(latin = ifelse(fr_loc == "MX" | fr_loc == "BO" | fr_loc == "CL" | fr_loc == "AR" | fr_loc == "VE" |
                          fr_loc == "BZ" | fr_loc == "CR" | fr_loc == "SV" | fr_loc == "GT" | fr_loc == "HN" |
                          fr_loc == "NI" | fr_loc == "PA" | fr_loc == "CO" | fr_loc == "EC" | fr_loc == "GY" |
                          fr_loc == "PY" | fr_loc == "PE" | fr_loc == "SR" | fr_loc == "UY" | fr_loc == "CU" |
                          fr_loc == "DO" | fr_loc == "HT" | fr_loc == "PR", 1, 0),
         latin_friends = latin * scaled_sci/(663543396 - 215079250)*1000000,
         nonlatin_friends = (1-latin) * scaled_sci/(7929246314 - 663543396)*1000000,
         brazil = ifelse(fr_loc == "BR",1,0),
         brazil_friends = brazil * scaled_sci/215079250*1000000,
         mexico = ifelse(fr_loc == "MX", 1, 0),
         mexico_friends = mexico * scaled_sci,
         japan = ifelse(fr_loc == "JP",1,0),
         japan_friends = japan * scaled_sci/125800000*1000000,
         creole = ifelse(fr_loc == "HT" | fr_loc == "JM", 1, 0),
         creole_friends = creole * scaled_sci/14300000*1000000) %>%
  filter(!is.na(latin)) %>%
  group_by(user_loc) %>%
  summarise_all(mean) %>%
  select(stateCounty = user_loc, latin_friends, nonlatin_friends, brazil_friends, japan_friends, creole_friends)
  
saveRDS(sci_country, "SCI_county_country.Rdata")


sci_ccountry <- read.table(file = 'us-counties-countries-fb-social-connectedness-index-october-2021.tsv',
           sep = '\t', header = TRUE) %>%
  mutate(latin = ifelse(fr_loc == "MX" | fr_loc == "BO" | fr_loc == "CL" | fr_loc == "AR" | fr_loc == "VE" |
                          fr_loc == "BZ" | fr_loc == "CR" | fr_loc == "SV" | fr_loc == "GT" | fr_loc == "HN" |
                          fr_loc == "NI" | fr_loc == "PA" | fr_loc == "CO" | fr_loc == "EC" | fr_loc == "GY" |
                          fr_loc == "PY" | fr_loc == "PE" | fr_loc == "SR" | fr_loc == "UY" | fr_loc == "CU" |
                          fr_loc == "DO" | fr_loc == "HT" | fr_loc == "PR", 1, 0),
         brazil = ifelse(fr_loc == "BR",1,0)) %>%
  mutate(stateCounty = user_loc)

saveRDS(sci_ccountry, "SCI_county_country_long.Rdata")
## population references
# Lat Am: https://www.worldometers.info/world-population/latin-america-and-the-caribbean-population/
# Brazil: https://www.worldometers.info/world-population/brazil-population/
# world:  https://www.worldometers.info/world-population/


# countries:
# latin +  mexico +  bolivia +  chile +  argentina + 
#   venezuela +  belize +  costa.rica +  salvador +  guatemala +  hondura +
#   nicaragua +  panama +  brazil +  colombia +  ecuador +  guyana +  paraguay +
#   peru +  suriname +  uruguay +  cuba +  dominican.republic +  haiti +  puerto




