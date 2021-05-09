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



# cannot use language; unreliable -- instead by station
# although looks like just random scanning, unclear how this works. 
# Telemundo also probably doesn't simultaneously broadcast the same thing to all channels? but maybe we can try

# search: https://openlibrary.org/dev/docs/api/search
# http://openlibrary.org/search.json?q=the+lord+of+the+rings&page=2


# approach 2:
# if possible, get text as well and maybe apply NLP etc. to uncover themes
# or download text corpus
# https://archive.org/advancedsearch.php





# to check
# https://guides.library.harvard.edu/c.php?g=310705&p=2164806
# see Gale Directory

# other data sources
# factiva transcripts (limited Spanish selection)
# https://global-factiva-com.ezp-prod1.hul.harvard.edu/sb/default.aspx?NAPC=S
# lexis transcripts (very limited Spanish selection)
# https://advance-lexis-com.ezp-prod1.hul.harvard.edu/search/?pdmfid=1516831&crid=dcb07835-0c4e-4723-973a-6209824b9a52&pdsearchterms=si&pdstartin=hlct%3A1%3A1&pdtypeofsearch=searchboxclick&pdsearchtype=SearchBox&pdqttype=and&pdsf=&pdquerytemplateid=urn%3Aquerytemplate%3Aaf250bf4ef678aa67f4773a7d472c893~%5EAll%2520News%2520Transcripts&ecomp=bz-2k&earg=pdsf&prid=9592eaf6-a2bf-4c67-ba1e-5d23fbd473fe
