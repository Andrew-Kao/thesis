import pandas as pd
from ethnicolr import census_ln, pred_census_ln, pred_fl_reg_name
import os
os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'

names = [{'lname': 'smith','fname' : 'john'},
        {'lname': 'zhang', 'fname' : 'tony'},
		{'lname': 'jackson', 'fname' : 'michael'}]

df = pd.DataFrame(names)
# x = pred_census_ln(df, 'name')
x = pred_fl_reg_name(df,'lname','fname')
print(x)


# Syntax: 
# pred_fl_reg_name(also_has_first_name_df, 'lname', 'fname')

# How do you correct for the fact that people are statistically more likely to be white? (i.e. a threshold will sort on people who 'choose' to differentiate)
	# can try Monte Carlo
	# outcome could be name differences...