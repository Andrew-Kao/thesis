# Taking the raw scrape data and turning into tidy data

library(stringr)
library(dplyr)
library(purrr)

if (Sys.info()["user"] == "AndrewKao") {
  setwd('~/Documents/College/All/thesis/explore/Data/firms/florida') 
}

options(stringsAsFactors = FALSE)

test <- readRDS('scrapes/20110127.Rdata')

# https://dos.myflorida.com/sunbiz/other-services/data-downloads/corporate-data-file/file-structure/
df <- tibble(COR_REC = str_sub(test,1,1), COR_NUMBER = str_sub(test,2,12),
             COR_NAME = str_sub(test,13,204), COR_STATUS = str_sub(test,205,205),
             COR_FILING_TYPE = str_sub(test,206,220), COR_PRINC_ADD_1 = str_sub(test,221,262),
             COR_PRINC_ADD_2 = str_sub(test,263,304), COR_PRINC_CITY = str_sub(test,305,332),
             COR_PRINC_STATE = str_sub(test,333,334), BCOR_PRINC_ZIP = str_sub(test, 335,344),
             COR_PRINC_COUNTRY = str_sub(test,345,346), COR_MAIL_ADD_1 = str_sub(test,347,388),
             COR_MAIL_ADD_2 = str_sub(test,389,430), COR_MAIL_CITY = str_sub(test,431,458),
             COR_MAIL_STATE = str_sub(test,459,460), COR_MAIL_ZIP = str_sub(test,461,470),
             COR_MAIL_COUNTRY = str_sub(test,471,472), COR_FILE_DATE = str_sub(test,473,480),
             COR_FEI_NUMBER = str_sub(test,481,494), MORE_THAN_SIX_OFF_FLAG = str_sub(test,495,495),
             LAST_TRX_DATE = str_sub(test,496,503), STATE_COUNTRY = str_sub(test,504,505),
             REPORT_YEAR_1 = str_sub(test,506,509), HOUSE_FLAG_1 = str_sub(test,510,510),
             REPORT_DATE_1 = str_sub(test,511,518), REPORT_YEAR_2 = str_sub(test,519,522),
             HOUSE_FLAG_2 = str_sub(test,523,523), REPORT_DATE_2 = str_sub(test,524,531),
             REPORT_YEAR_3 = str_sub(test,532,535), HOUSE_FLAG_3 = str_sub(test,536,536),
             REPORT_DATE_3 = str_sub(test,537,544), RA_NAME = str_sub(test,545,586),
             RA_NAME_TYPE = str_sub(test,587,587), RA_ADD_1 = str_sub(test,588,629),
             RA_CITY = str_sub(test,630,657), RA_STATE = str_sub(test,658,659),
             RA_ZIP5 = str_sub(test,660,664), RA_ZIP4 = str_sub(test,665,668),
             PRINC_TITLE = str_sub(test,669,672), PRINC_NAME_TYPE = str_sub(test,673,673),
             PRINC_NAME = str_sub(test,674,715), PRINC_ADD_1 = str_sub(test,716,757),
             PRINC_CITY = str_sub(test,758,785), PRINC_STATE = str_sub(test,786,787),
             PRINC_ZIP5 = str_sub(test,788,793), PRINC_ZIP4 = str_sub(test,794,797)) %>%
  sapply(str_trim)


# probably only want to keep people and not corps (RA/PRINC name type)








