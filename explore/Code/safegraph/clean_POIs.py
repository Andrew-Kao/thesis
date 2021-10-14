###### Safegraph data ####
# run after R files clean_POIs.R and clean_census_geo.R


import pandas as pd
import pdb
import numpy as np
import json

# function from https://github.com/ryanfoxsquire/safegraph_demo_profile/blob/8128cacb4ae34664c8a7d2bbfc619aa9b54cf36d/demo_profile_functions/demo_profile_functions.py#L265
def vertically_explode_json(df, json_column='visitor_home_cbgs', index_column='safegraph_place_id', key_col_name='visitor_home_cbg', value_col_name='visitor_count'):
    # This function vertically explodes a JSON column in SafeGraph Patterns
    # The resulting dataframe has one row for every data element in the all the JSON of all the original rows

    # Inputs
    #    df -- a pandas.DataFrame(dataframe with at 2 columns: 
    #    1) index_column (default = safegraph_place_id), this is what the exploded json data will be joined to in the final result
    #    2) json_column -- each element of this column is a stringified json blog
    #    key_col_name -- arbitrary string, the name of the column in the output which contains the keys of the key:values of the JSON
    #    value_col_name -- arbitrary string, the name of the column in the output which contains the values of the key:values of the JSON
    # Outputs
    #    df -- a pandas.DataFrame with 3 columns
    #    1) index_column
    #    2) key_col_name
    #    3) value_col_name
    
    df = df.dropna(subset = [json_column]).copy() # Drop nan jsons 
    df[json_column+'_dict'] = [json.loads(cbg_json) for cbg_json in df[json_column]]

    # extract each key:value inside each visitor_home_cbg dict (2 nested loops) 
    all_sgpid_cbg_data = [] # each cbg data point will be one element in this list
    for index, row in df.iterrows():
        this_sgpid_cbg_data = [ {index_column : row[index_column], key_col_name:key, value_col_name:value} for key,value in row[json_column+'_dict'].items() ]
  
        # concat the lists
        all_sgpid_cbg_data = all_sgpid_cbg_data + this_sgpid_cbg_data

    return(pd.DataFrame(all_sgpid_cbg_data))

# Heckman correction for selection
def compute_adjust_factor(df, population_col, sample_col):
    adjust_factor = df[population_col] / df[population_col].sum() * df[sample_col].sum() / df[sample_col]
    return(adjust_factor)

# Hispanic in out
def get_poi_hispanic_inout(pois, cbgs):
	# make vertically explode
    poi = vertically_explode_json(pois)
    poi['visitor_home_cbg'] = pd.to_numeric(poi['visitor_home_cbg'])
	# merge with census block data
    poi = pd.merge(poi,cbgs, left_on='visitor_home_cbg', right_on='census_block_group').drop('visitor_home_cbg',axis='columns')

	# do adjustment (Heckman selection)
    poi['cbg_adjust_factor'] = compute_adjust_factor(poi,'B01001e1','number_devices_residing')
    poi['visitor_count'] = poi['visitor_count'] * poi['cbg_adjust_factor']

    # get mean number of visitors 
    # TODO: inside and outside
    demo_codes = ['B03003e2','B03003e3','hisp_inside','nonhisp_inside']
    for dc in demo_codes:
        poi['visitor_count_'+dc+'_D_adj'] = poi[dc+'_frac'] * poi['visitor_count'] 

    cols_to_keep = ['safegraph_place_id','visitor_count', 'visitor_count_B03003e2_D_adj',
                    'visitor_count_B03003e3_D_adj','visitor_count_hisp_inside_D_adj',
                    'visitor_count_nonhisp_inside_D_adj'] 
    poi = poi[cols_to_keep]
    poi = poi.groupby(['safegraph_place_id']).sum().reset_index()

    return(poi)



## sectors:
# placebo
# 51 - information
# 52 - finance and insurance
# outcomes
# 61 - educational services
# 71 - arts, entertainment, and recreation
# 72 - accomodation and food services


# load data
pois = pd.read_csv('../../Data/safegraph/POI/POI_cleaned.csv')
block_instr = pd.read_csv('../../Data/safegraph/safegraph_open_census_data_2010_to_2019_geometry/block_instr.csv')
cbg = pd.read_csv('../../Data/safegraph/safegraph_open_census_data_2019/data/cbg_b03.csv')
home_cbg = pd.read_csv('../../Data/safegraph/home_panel_summary.csv')
total_cbg = pd.read_csv('../../Data/safegraph/safegraph_open_census_data_2019/data/cbg_b01.csv')


# get only Hispanic subset for CBG
hispanic_demo_codes = ['B03003e2','B03003e3']
cbg = cbg[['census_block_group'] + hispanic_demo_codes]
demo_totals_hispanic = cbg[hispanic_demo_codes].sum(axis=1) # the sum of Hispanic and Not Hispanice for each CBG
for this_code in hispanic_demo_codes:
        cbg[this_code+"_frac"] = cbg[this_code] / demo_totals_hispanic
## merge instrument data to CBG
cbg = pd.merge(cbg,block_instr, on = ['census_block_group'])
## merge home data
home_cbg = home_cbg[['census_block_group','number_devices_residing']]
cbg = pd.merge(cbg,home_cbg, on = ['census_block_group'])
## merge total resident data
total_cbg = total_cbg[['census_block_group','B01001e1']]
cbg = pd.merge(cbg,total_cbg, on = ['census_block_group'])
cbg['hisp_inside_frac'] = cbg['B03003e3_frac'] * cbg['intersects']
cbg['nonhisp_inside_frac'] = cbg['B03003e2_frac'] * cbg['intersects']

## process: make it work for patterns-part-1.csv and then rsync in parts to the cluster and chunk? (or locally)
month = '01'
l2 = '1'

patterns = pd.read_csv('~/Dropbox/safegraph/2019/' + month + '/patterns-part' + l2 + '.csv')

# only do January for now, TODO: the rest (need acquire data past April)

# need to merge pois patterns
pois_id = pois['safegraph_place_id'].to_frame()
pattern_pois = pois_id.merge(patterns, on = ['safegraph_place_id'])

npois = get_poi_hispanic_inout(pattern_pois, cbg)

npois.to_csv('../../Data/safegraph/POI/POI_pattern_' + month + '_' + l2 + '.csv')

pdb.set_trace()



# descriptives
# df[df['sector']==51]['top_category'].value_counts()
# df[df['sector']==52]['top_category'].value_counts()
# df[df['sector']==61]['top_category'].value_counts()
# df[df['sector']==71]['top_category'].value_counts()
# df[df['sector']==72]['top_category'].value_counts()



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


