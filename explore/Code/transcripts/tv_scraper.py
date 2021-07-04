### get TV hit counts from archive.org

import requests
import shutil
import os
import json
import time
from bs4 import BeautifulSoup
import pdb
from requests import get
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
import time
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.action_chains import ActionChains
import re
import pandas as pd
import numpy as np


## read data file
df = pd.read_csv("../../Data/transcripts/archive_station_word.csv")


# list of words to scrape
keywords = ['test','test2','callSign']

# create browser
try:
	browser = webdriver.Chrome(executable_path="/Users/AndrewKao/Dropbox/creative-auctions/creative/chromedriver")
except: 
	try: 
		browser = webdriver.Firefox()#for running on the server
	except:
		pass

base_url = 'https://archive.org/details/tv?q=('

for word in keywords:
	try:
		df[word]
	except:
		# new column, need to populate
		print(word)
		df[word] = np.nan

		# iterate through rows
		for i in range(len(df)):

			# url
			logsite = base_url + word + ') AND ceator:(' + df['callSign'][i] + ')'
			browser.get(logsite)
			WebDriverWait(browser,1).until(element_present)

			# get # of search results = number of programs
			result_text = browser.find_element_by_xpath("//div[@class='results_count']").text
			df[word][i] = re.search("([0-9]+)", result_text).group()[0:]


		pd.write_csv(df,"../../Data/transcripts/archive_station_word.csv")


pdb.set_trace()
