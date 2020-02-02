# Taking the raw scrape data and turning into tidy data

library(stringr)
library(dplyr)
library(purrr)
library(stringi)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)


# https://dos.myflorida.com/sunbiz/other-services/data-downloads/corporate-data-file/file-structure/
parseFL <- function(text) {
  tryCatch({df <- tibble(COR_REC = str_sub(text,1,1), COR_NUMBER = str_sub(text,2,12),
               COR_NAME = str_sub(text,13,204), COR_STATUS = str_sub(text,205,205),
               COR_FILING_TYPE = str_sub(text,206,220), COR_PRINC_ADD_1 = str_sub(text,221,262),
               COR_PRINC_ADD_2 = str_sub(text,263,304), COR_PRINC_CITY = str_sub(text,305,332),
               COR_PRINC_STATE = str_sub(text,333,334), BCOR_PRINC_ZIP = str_sub(text, 335,344),
               COR_PRINC_COUNTRY = str_sub(text,345,346), COR_MAIL_ADD_1 = str_sub(text,347,388),
               COR_MAIL_ADD_2 = str_sub(text,389,430), COR_MAIL_CITY = str_sub(text,431,458),
               COR_MAIL_STATE = str_sub(text,459,460), COR_MAIL_ZIP = str_sub(text,461,470),
               COR_MAIL_COUNTRY = str_sub(text,471,472), COR_FILE_DATE = str_sub(text,473,480),
               COR_FEI_NUMBER = str_sub(text,481,494), MORE_THAN_SIX_OFF_FLAG = str_sub(text,495,495),
               LAST_TRX_DATE = str_sub(text,496,503), STATE_COUNTRY = str_sub(text,504,505),
               REPORT_YEAR_1 = str_sub(text,506,509), HOUSE_FLAG_1 = str_sub(text,510,510),
               REPORT_DATE_1 = str_sub(text,511,518), REPORT_YEAR_2 = str_sub(text,519,522),
               HOUSE_FLAG_2 = str_sub(text,523,523), REPORT_DATE_2 = str_sub(text,524,531),
               REPORT_YEAR_3 = str_sub(text,532,535), HOUSE_FLAG_3 = str_sub(text,536,536),
               REPORT_DATE_3 = str_sub(text,537,544), RA_NAME = str_sub(text,545,586),
               RA_NAME_TYPE = str_sub(text,587,587), RA_ADD_1 = str_sub(text,588,629),
               RA_CITY = str_sub(text,630,657), RA_STATE = str_sub(text,658,659),
               RA_ZIP5 = str_sub(text,660,664), RA_ZIP4 = str_sub(text,665,668),
               PRINC_TITLE = str_sub(text,669,672), PRINC_NAME_TYPE = str_sub(text,673,673),
               PRINC_NAME = str_sub(text,674,715), PRINC_ADD_1 = str_sub(text,716,757),
               PRINC_CITY = str_sub(text,758,785), PRINC_STATE = str_sub(text,786,787),
               PRINC_ZIP5 = str_sub(text,788,793), PRINC_ZIP4 = str_sub(text,794,797)) %>%
    sapply(str_trim)},
    error = function(c) {
      df <- tibble(COR_REC = str_sub(text,1,1), COR_NUMBER = str_sub(text,2,12),
                   COR_NAME = str_sub(text,13,204), COR_STATUS = str_sub(text,205,205),
                   COR_FILING_TYPE = str_sub(text,206,220), COR_PRINC_ADD_1 = str_sub(text,221,262),
                   COR_PRINC_ADD_2 = str_sub(text,263,304), COR_PRINC_CITY = str_sub(text,305,332),
                   COR_PRINC_STATE = str_sub(text,333,334), BCOR_PRINC_ZIP = str_sub(text, 335,344),
                   COR_PRINC_COUNTRY = str_sub(text,345,346), COR_MAIL_ADD_1 = str_sub(text,347,388),
                   COR_MAIL_ADD_2 = str_sub(text,389,430), COR_MAIL_CITY = str_sub(text,431,458),
                   COR_MAIL_STATE = str_sub(text,459,460), COR_MAIL_ZIP = str_sub(text,461,470),
                   COR_MAIL_COUNTRY = str_sub(text,471,472), COR_FILE_DATE = str_sub(text,473,480),
                   COR_FEI_NUMBER = str_sub(text,481,494), MORE_THAN_SIX_OFF_FLAG = str_sub(text,495,495),
                   LAST_TRX_DATE = str_sub(text,496,503), STATE_COUNTRY = str_sub(text,504,505),
                   REPORT_YEAR_1 = str_sub(text,506,509), HOUSE_FLAG_1 = str_sub(text,510,510),
                   REPORT_DATE_1 = str_sub(text,511,518), REPORT_YEAR_2 = str_sub(text,519,522),
                   HOUSE_FLAG_2 = str_sub(text,523,523), REPORT_DATE_2 = str_sub(text,524,531),
                   REPORT_YEAR_3 = str_sub(text,532,535), HOUSE_FLAG_3 = str_sub(text,536,536),
                   REPORT_DATE_3 = str_sub(text,537,544), RA_NAME = str_sub(text,545,586),
                   RA_NAME_TYPE = str_sub(text,587,587), RA_ADD_1 = str_sub(text,588,629),
                   RA_CITY = str_sub(text,630,657), RA_STATE = str_sub(text,658,659),
                   RA_ZIP5 = str_sub(text,660,664), RA_ZIP4 = str_sub(text,665,668),
                   PRINC_TITLE = str_sub(text,669,672), PRINC_NAME_TYPE = str_sub(text,673,673),
                   PRINC_NAME = str_sub(text,674,715), PRINC_ADD_1 = str_sub(text,716,757),
                   PRINC_CITY = str_sub(text,758,785), PRINC_STATE = str_sub(text,786,787),
                   PRINC_ZIP5 = str_sub(text,788,793), PRINC_ZIP4 = str_sub(text,794,797)) %>%
        sapply(iconv,from="UTF-8",to="UTF-8",sub='') %>% # this slows it down lots, so only use if necessary
        sapply(str_trim)})
  
  
}

# given a DF in the right format and a filename, makes new DF with new entries
mergeFL <- function(merged, new_filename) {
  text <- readRDS(paste0('scrapes/',new_filename))
  new_df <- parseFL(text)
  print(new_filename)
  rbind(merged, new_df)
}


# time to purrr
filelist <- list.files("scrapes")
test <- readRDS(paste0('scrapes/',filelist[2])) # start at 2 bc 1 is a dud
base <- parseFL(test)
filelist <- filelist[-2]
i = 1
while (i < 23) {
  lower = (i-1)*100 + 1
  upper = i*100
  
  base <- reduce(filelist[lower:upper], mergeFL,
                 .init = base)
  saveRDS(base, 'tidy_merged.Rdata')
  i = i + 1
}

base <- readRDS('tidy_merged.Rdata')

# rebuild (faster than remerging)
tidy <- as_tibble(base)

# clean and move to csv
tidy2 <- tidy %>% 
  filter(PRINC_NAME != "") %>%
  filter(COR_STATUS == 'A') %>%
  filter(PRINC_STATE == 'FL') %>%
  filter(PRINC_NAME_TYPE == 'P')

write.csv(tidy2, "tidy_merged.csv")

addresses <- tidy2 %>%
  select(PRINC_ADD_1, PRINC_CITY, PRINC_STATE, PRINC_ZIP5) %>%
  distinct()

write.csv(addresses, "FLAddresses.csv")

# all at once
merged_FL <- reduce(filelist[-2], mergeFL,
       .init = base) 

# UTF error that necessitates tryCatch
test2 <- readRDS(paste0('scrapes/','20140407.Rdata')) # start at 2 bc 1 is a dud
base2 <- parseFL(test2)


# probably only want to keep people and not corps (RA/PRINC name type)
text <- readRDS('scrapes/20110127.Rdata')







