# This file is used to interface with archive.org to get SLTV transcript data
# https://archive.org/details/tv?q=si&and%5B%5D=creator%3A%22ksts%22&and%5B%5D=creator%3A%22wzdc%22&page=2


# SETUP -------------------------------------------------------------------

library(dplyr)
library(httr)
library(jsonlite)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/transcripts') 
}


# approach 1:
# scrape individual station names, search for keywords and record hits within the relevant timeframe
# see https://archive.readme.io/docs
# just # of hits

##### read data from python


##### interpolate from univision, or telemundo, etc.
##### find proportion of total programming (DoF?)


##### more extreme measure: recollect Spanish language from Wikipedia (https://en.wikipedia.org/wiki/WMAR-TV Broadcast Maryland/DC category shows SLTV etc.)


### get list of Spanish stations -- RUN ONCE ONLY
stations <- readRDS('../instrument/TMS/spanishStations.Rdata')
write.csv(stations,'archive_station_word.csv')


# KSTS is telemundo
# WFDC, WUVP is univision

word_data <- read.csv('archive_station_word.csv') %>%  filter(callSign == "KSTS" | callSign == "WFDC" | callSign == "WUVP") %>%
  mutate(parent = ifelse(callSign == "KSTS","telemundo","univision"),
         education = educación + enseñanza + colegio + escuela + universidad + 
           estudio + estudiar + estudiante + alumna + alumno + profesora + profesor + 
           maestro + maestra +  clase + rango + grado + aprender + mates + matematicas,
         latin =  latin +  mexico +  bolivia +  chile +  argentina + 
           venezuela +  belize +  costa.rica +  salvador +  guatemala +  hondura +
           nicaragua +  panama +  brazil +  colombia +  ecuador +  guyana +  paraguay +
           peru +  suriname +  uruguay +  cuba +  dominican.republic +  haiti +  puerto +  hispanic ) %>%
  group_by(parent) %>%
  summarise(word_education = mean(education), word_latin = mean(latin))

## note others: Estrella, UniMas, PBS, MundoFox, TBN, Mega TV/SBS, 
## idea: use affiliation switches as test cases? but endogeneous... so maybe just drop
## difference between stations based in Mexico vs US?

station_word_data <- read.csv('archive_station_word.csv') %>%
  filter(callSign != "KBDI" & callSign != "KDIN" & callSign != "KETD" & callSign != "KFTH" &
           callSign != "KFWD" & callSign != "KMBH" & callSign != "KMPX" & callSign != "KPNZ" & 
           callSign != "KQCK" & callSign != "KSCE" & callSign != "KSTR" & callSign != "KTBU" &
           callSign != "KTFD" & callSign != "KTFN" & callSign != "KTFQ" & callSign != "KVAW" &
           callSign != "KYAZ" & callSign != "KZJL" & callSign != "WAPA" & callSign != "WFTT" &
           callSign != "WFUT" & callSign != "WGEN" & callSign != "WLNY" & callSign != "WOTF" &
           callSign != "WSBS" & callSign != "WUCF" & callSign != "WXFT" & callSign != "XEFE" &
           callSign != "XEJ" & callSign != "XEPM" & callSign != "XERV" & callSign != "XETV" &
           callSign != "XHAB" & callSign != "XHAMC" & callSign != "XHBR" & callSign != "XHCAW" &
           callSign != "XHCHW" & callSign != "XHCJE" & callSign != "XHCJH" & callSign != "XHHE" & 
           callSign != "XHHR" & callSign != "XHJCI" & callSign != "XHLAR" & callSign != "XHLAT" &
           callSign != "XHLNA" & callSign != "XHMTA" & callSign != "XHNAT" & callSign != "XHOCH" &
           callSign != "XHOR" & callSign != "XHPN" & callSign != "XHPNG" & callSign != "XHPNH" &
           callSign != "XHPNT" & callSign != "XHPNW" & callSign != "XHREY" & callSign != "XHRIO" & 
           callSign != "XHTAM" & callSign != "XHVTV" & callSign != "XHWDT") %>% # non Telemundo/Univsion no data
  mutate(parent = ifelse(callSign == "KSTS" | callSign == "KASA" | callSign == "KDEN" | callSign == "KKJB" | 
                           callSign == "KTDO" | callSign == "KTEL" | callSign == "KTLM" | callSign == "KTMD" |
                           callSign == "KTMW" | callSign == "KTUZ" | callSign == "KXTX" | callSign == "WNJU" |
                           callSign == "WSCV" | callSign == "WSNS" | callSign == "WWSI","telemundo","univision"))





# do English placebo at some point? English stations or English language


# API get - want json
url <- "https://archive.org/"

query_tv <- 'details/tv?q=si&&and[]=creator%3A"wtsp"'
query_default <- "services/search/v1/scrape?debug=false&xvar=production&total_only=false&output=json&count=10000&fields=identifier&q="
query <- '(si) AND creator:(WTSP)'
raw_output <- GET(url = url, path = paste0(query_default,query)) # query
# processing API output
text_output <- rawToChar(raw_output$content)
api_output <- fromJSON(text_output)

total_hits <- api_output$total


# data entry
panel[rowCall, 4+callNum] <- api_output$QueryResult$RecordsFound

##### search list

# role models (characters?)
search_list <- TERMS TO SEARCH

# direct education terms

# popular telenovellas



# restructure data to station, # of hits

# then merge to station dataset


# cannot use language; unreliable -- instead by station
# although looks like just random scanning, unclear how this works. 
# Telemundo also probably doesn't simultaneously broadcast the same thing to all channels? but maybe we can try

# search: https://openlibrary.org/dev/docs/api/search
# http://openlibrary.org/search.json?q=the+lord+of+the+rings&page=2


# approach 2:
# if possible, get text as well and maybe apply NLP etc. to uncover themes
# or download text corpus
# https://archive.org/advancedsearch.php
# http://blog.archive.org/2012/04/26/downloading-in-bulk-using-wget/





# to check
# https://guides.library.harvard.edu/c.php?g=310705&p=2164806
# see Gale Directory

# other data sources

# factiva transcripts (limited Spanish selection)
# https://global-factiva-com.ezp-prod1.hul.harvard.edu/sb/default.aspx?NAPC=S
# no data before 2021 for Telemundo, but transcripts look good otherwise

# lexis transcripts (very limited Spanish selection)
# https://advance-lexis-com.ezp-prod1.hul.harvard.edu/search/?pdmfid=1516831&crid=dcb07835-0c4e-4723-973a-6209824b9a52&pdsearchterms=si&pdstartin=hlct%3A1%3A1&pdtypeofsearch=searchboxclick&pdsearchtype=SearchBox&pdqttype=and&pdsf=&pdquerytemplateid=urn%3Aquerytemplate%3Aaf250bf4ef678aa67f4773a7d472c893~%5EAll%2520News%2520Transcripts&ecomp=bz-2k&earg=pdsf&prid=9592eaf6-a2bf-4c67-ba1e-5d23fbd473fe
