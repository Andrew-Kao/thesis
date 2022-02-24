###### SCI data cleaning ####

library(dplyr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/SCI') 
}

sci_country <- read.table(file = 'us-counties-countries-fb-social-connectedness-index-october-2021.tsv',
                          sep = '\t', header = TRUE) %>%
  mutate(latin = ifelse(fr_loc == "MX" | fr_loc == "BO" | fr_loc == "CL" | fr_loc == "AR" | fr_loc == "VE" |
                          fr_loc == "BZ" | fr_loc == "CR" | fr_loc == "SV" | fr_loc == "GT" | fr_loc == "HN" |
                          fr_loc == "NI" | fr_loc == "PA" | fr_loc == "CO" | fr_loc == "EC" | fr_loc == "GY" |
                          fr_loc == "PY" | fr_loc == "PE" | fr_loc == "SR" | fr_loc == "UY" | fr_loc == "CU" |
                          fr_loc == "DO" | fr_loc == "HT" | fr_loc == "PR", 1, 0))
  



# latin +  mexico +  bolivia +  chile +  argentina + 
#   venezuela +  belize +  costa.rica +  salvador +  guatemala +  hondura +
#   nicaragua +  panama +  brazil +  colombia +  ecuador +  guyana +  paraguay +
#   peru +  suriname +  uruguay +  cuba +  dominican.republic +  haiti +  puerto




