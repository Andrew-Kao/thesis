# Scraping the Floria corporation files on FTP
# 


library(stringr)
library(dplyr)
library(purrr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)

# cross year, month, day, and then do in order

yearList <- c(2011:2019)
monthList <- c(1:12) %>%
  str_pad(width=2,side="left",pad="0")
# original, 1:31... progress at starting num
dayList <- c(31:31) %>%
  str_pad(width=2,side="left",pad="0")
dates <- cross3(yearList,monthList,dayList)

calls <- tibble(dates = dates) %>%
  mutate(dates = sapply(dates,paste,collapse=''))

filelist <- list.files("scrapes")
completed <- lapply(filelist, str_extract, "[0-9]+")

for(date in calls$dates) {
  if (!(date %in% completed)) {
    ### SCRAPE
    url <- paste0('ftp://ftp.dos.state.fl.us/public/doc/cor/',date,'c.txt')
    try(saveRDS(readLines(url),paste0('scrapes/',date,'.Rdata')))
             #,error = saveError(date))
  }
}


#### testing
thepage <- readLines('ftp://ftp.dos.state.fl.us/public/doc/cor/20110103c.txt')
saveRDS(thepage,'scrapes/20110103.Rdata')
# ftp://ftp.dos.state.fl.us/public/doc/cor/20110103c.txt

test <- readRDS('scrapes/20110127.Rdata')

saveError <- function(date) {
  saveRDS("a",paste0('scrapes/',date,'.Rdata'))
}



