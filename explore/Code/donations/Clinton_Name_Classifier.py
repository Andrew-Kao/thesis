# Classify Clinton Donation Names
# Potentially expand later.


############# ETHNICITY

import pandas as pd
from ethnicolr import pred_wiki_name, pred_census_ln, pred_fl_reg_name
import os

# my version of Tensor Flow can't use low-level CPU functions. Rip fast linalg
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

os.chdir('explore/Data/politics/clinton_donations')

# only need the first and last names
clinton_donations = pd.read_csv('ClintonDonations.csv', usecols=['contributor_first_name','contributor_last_name'])

# print(list(trump_donations))

clinton_donations = clinton_donations.drop_duplicates()

prediction = pred_fl_reg_name(clinton_donations,'contributor_last_name','contributor_first_name')
prediction.to_csv('names_predict_FL.csv')
prediction = pred_wiki_name(clinton_donations,'contributor_last_name','contributor_first_name')
prediction.to_csv('names_predict_wiki.csv')
prediction = pred_census_ln(clinton_donations,'contributor_last_name')
prediction.to_csv('names_predict_census.csv')



# Syntax: 
# names = [{'lname': 'smith','fname' : 'john'},
#         {'lname': 'zhang', 'fname' : 'tony'},
# 		{'lname': 'jackson', 'fname' : 'michael'}]

# df = pd.DataFrame(names)
# pred_fl_reg_name(also_has_first_name_df, 'lname', 'fname')

# How do you correct for the fact that people are statistically more likely to be white? (i.e. a threshold will sort on people who 'choose' to differentiate)
	# can try Monte Carlo
	# outcome could be name differences...


############ GENDER


# from chicksexer import predict_genders

# prediction = predict_genders(trump_donations)
# prediction.to_csv('gender_predict.csv')


