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


## list of words to scrape

# relating to schools
edu_words_eng = ['education','school','study','student','teacher', 'class','grade','learn','math']
edu_words = ['educación ','enseñanza','colegio','escuela','universidad','estudio','estudiar','estudiante','alumna','alumno','profesora','profesor','maestro','maestra', 'clase','rango','grado','aprender','mates','matematicas']

# relating to Hispanic ID
# countries, direct ID. unclear if cultural components/salient
hispanic_words_eng = ['latin', 'mexico', 'bolivia', 'chile', 'argentina', 
                'venezuela', 'belize', 'costa rica', 'salvador', 'guatemala', 'hondura',
                'nicaragua', 'panama', 'brazil', 'colombia', 'ecuador', 'guyana', 'paraguay',
                'peru', 'suriname', 'uruguay', 'cuba', 'dominican republic', 'haiti', 'puerto', 'hispanic']
hispanic_words = ['latin', 'mexico', 'bolivia', 'chile', 'argentina', 
                'venezuela', 'belize', 'costa rica', 'salvador', 'guatemala', 'hondura',
                'nicaragua', 'panama', 'brazil', 'colombia', 'ecuador', 'guyana', 'paraguay',
                'peru', 'suriname', 'uruguay', 'cuba', 'dominican republic', 'haiti', 'puerto', 'hispanic']


# relating to 'good values'
values_words_eng = ['']
values_words = ['']

# strong role models, TV shows
role_model_words_eng = ['']
role_model_words = ['']

# append together
keywords = ['callSign'] + edu_words + hispanic_words + values_words + role_model_words



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
			logsite = base_url + word + ') AND creator:(' + df['callSign'][i] + ')'
			browser.get(logsite)

			try: 
				# get # of search results = number of programs
				result_text = browser.find_element_by_xpath("//div[@class='results_count']").text
				result_text = result_text.replace(',','')
				# WebDriverWait(browser,1).until(result_text) // assume no need to wait
				df.loc[i,word] = re.search("([0-9]+)", result_text).group()[0:]
			except:
				# handle search failure -- means no hits
				result_text = browser.find_element_by_xpath("//div[@id='search-fail']").text
				df.loc[i,word] = 0
	
		df.to_csv("../../Data/transcripts/archive_station_word.csv")


pdb.set_trace()
