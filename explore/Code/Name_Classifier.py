# Classify Trump Donation Names
# Potentially expand later.


import pandas as pd
from ethnicolr import census_ln, pred_census_ln, pred_fl_reg_name
import os

# my version of Tensor Flow can't use low-level CPU functions. Rip fast linalg
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

os.chdir('explore/Data/politics/trump_donations')

# only need the first and last names
trump_donations = pd.read_csv('TrumpDonations.csv', usecols=['contributor_first_name','contributor_last_name'])

print(list(trump_donations))

trump_donations.drop_duplicates()

prediction = pred_fl_reg_name(trump_donations,'contributor_last_name','contributor_first_name')
prediction.to_csv('names_predict_FL.csv')


# Syntax: 
# names = [{'lname': 'smith','fname' : 'john'},
#         {'lname': 'zhang', 'fname' : 'tony'},
# 		{'lname': 'jackson', 'fname' : 'michael'}]

# df = pd.DataFrame(names)
# pred_fl_reg_name(also_has_first_name_df, 'lname', 'fname')

# How do you correct for the fact that people are statistically more likely to be white? (i.e. a threshold will sort on people who 'choose' to differentiate)
	# can try Monte Carlo
	# outcome could be name differences...