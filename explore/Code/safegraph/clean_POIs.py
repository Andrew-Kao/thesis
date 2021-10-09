###### Safegraph data ####


import pandas as pd
import pdb
import numpy as np


# do for each of the coreplaces
df = pd.read_csv('~/Dropbox/safegraph/coreplaces/core_poi-part1.csv')

df = df[df['iso_country_code'] == 'US']

df['sector'] = np.floor(df['naics_code']/10000)


## sectors:
# placebo
# 51 - information
# 52 - finance and insurance
# outcomes
# 61 - educational services
# 71 - arts, entertainment, and recreation
# 72 - accomodation and food services

df = df[ (df['sector'] == 51) | (df['sector'] == 52) | (df['sector'] == 61) | (df['sector'] == 71) | (df['sector'] == 72)]


# descriptives
df[df['sector']==51]['top_category'].value_counts()
df[df['sector']==52]['top_category'].value_counts()
df[df['sector']==61]['top_category'].value_counts()
df[df['sector']==71]['top_category'].value_counts()
df[df['sector']==72]['top_category'].value_counts()


# Hispanic references

# restaurants
# see Language Schools


# safegraph_place_id
# parent_safegraph_place_id
# location_name
# safegraph_brand_ids
# brands
# top_category
# sub_category
# naics_code
# latitude
# longitude
# street_address
# city
# region
# postal_code
# iso_country_code
# phone_number
# open_hours
# category_tags

pdb.set_trace()


