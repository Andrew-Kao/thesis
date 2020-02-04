# Classify Florida Business Names
# Potentially expand later.


############# ETHNICITY

import pdb
import pandas as pd
from ethnicolr import pred_wiki_name, pred_census_ln, pred_fl_reg_name
import os

# my version of Tensor Flow can't use low-level CPU functions. Rip fast linalg
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

os.chdir('explore/Data/firms/florida')

# only need the first and last names
florida_names = pd.read_csv('tidy_merged.csv', usecols=['PRINC_NAME'])

florida_names = florida_names.drop_duplicates()

names = florida_names['PRINC_NAME'].str.split(None,2,True)
florida_names['last_name'] = names[0]
florida_names['first_name'] = names[0]


# just do with FL (voter reg) training data for now to save time
prediction = pred_fl_reg_name(florida_names,'last_name','first_name')
prediction.to_csv('names_predict_FL.csv')
# prediction = pred_wiki_name(florida_names,'contributor_last_name','contributor_first_name')
# prediction.to_csv('names_predict_wiki.csv')
# prediction = pred_census_ln(florida_names,'contributor_last_name')
# prediction.to_csv('names_predict_census.csv')



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


