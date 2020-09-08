# google_feat_gen.py
# Generates features from all google sources at level 2
import pandas as pd
import numpy as np
import math
import os
from tqdm import tqdm
from functools import reduce

## NEEDED DATA
# google_index = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/index.csv")
# google_main = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/main.csv")
# google_demo = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/demographics.csv")
# google_econ = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/economy.csv") #country level only
# google_geo = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/geography.csv")
# google_health = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/health.csv") #country level only
# google_gov_resp = pd.read_csv("https://storage.googleapis.com/covid19-open-data/v2/oxford-government-response.csv")

## FUNCTIONS
def google_level_keys(google_index, countries, level=2):
	keys = (google_index
		.query("country_name in @countries", engine="python")
		.query("aggregation_level==@level", engine="python")
		)
	keys = list(set(keys.key))

	return keys

def google_demo_level_2(keys, google_demo):
	# keys: from google_level_2_keys()

	demo = (google_demo
		.query("key in @keys", engine="python")
		.query("population==population")
		.drop(columns=["rural_population", "urban_population", "largest_city_population", "clustered_population", 
			"population_density", "human_development_index"])
		)

	print("Demo: Filling missing information where needed")
	demo = extract_level_2(demo, google_demo)

	demo = (demo
	 .assign(prop_male = lambda d : d.population_male/(d.population_male + d.population_female))
	 .assign(prop_female = lambda d : 1-d.prop_male)
	 .assign(prop_young = lambda d : (d.population_age_00_09 + d.population_age_10_19) 
	         /(d.population_age_00_09 + d.population_age_10_19 + d.population_age_20_29 +
	                                  d.population_age_30_39 + d.population_age_40_49 + d.population_age_50_59 +
	                                  d.population_age_60_69 + d.population_age_70_79 + d.population_age_80_89 + 
	                                  d.population_age_90_99))
	 .assign(prop_adult = lambda d : (d.population_age_20_29 + d.population_age_30_39 + d.population_age_40_49) 
	         /(d.population_age_00_09 + d.population_age_10_19 + d.population_age_20_29 +
	                                  d.population_age_30_39 + d.population_age_40_49 + d.population_age_50_59 +
	                                  d.population_age_60_69 + d.population_age_70_79 + d.population_age_80_89 + 
	                                  d.population_age_90_99))
	 .assign(prop_old = lambda d : 1- (d.prop_young + d.prop_adult))
	 .sort_values("prop_old")
	 .loc[:,["key", "population", "prop_female", "prop_male", "prop_young", "prop_adult", "prop_old"]]
	)

	return demo

def google_geo_level_2(keys, google_geo):
	# Collects geo data for countries at subregion level 2
	# keys: from google_level_2_keys()

	geo = (google_geo
		.query("key in @keys", engine="python")
		.query("area==area")
		.loc[:,["key", "latitude", "longitude", "area"]]
		)

	print("Geo: Filling missing information where needed")
	geo = extract_level_2(geo, google_geo)

	return geo

def google_epi_level_2(keys, google_epi, need_deaths=True):

	epi = (google_epi
		.query("key in @keys", engine="python")
		.loc[:,["key", "date", "new_confirmed", "total_confirmed", "new_deceased", "total_deceased"]]
		.assign(date = lambda d: pd.to_datetime(d.date))
		)

	# Pre-nan clean up for valid values (Leading and trailing NANs)
	epi = (epi
		.sort_values(["key", "date"])
		.reset_index(drop=True)
		.assign(index_col = lambda d: d.index)
		.assign(
			last_valid = lambda d: d.groupby(["key"])
						.new_confirmed.transform(lambda e: e.last_valid_index())
			)
		.assign(
			first_valid = lambda d: d.groupby(["key"])
						.total_deceased.transform(lambda e: e.first_valid_index())
			)
		.query("last_valid==last_valid")
		.query("first_valid==first_valid")
		.query("index_col <= last_valid")
		.query("index_col >= first_valid")
		.query("total_confirmed>10") # Failsafe clean up step
		.drop(columns=["index_col", "last_valid", "first_valid"])
		)

	# Remove regions which have nan deceased
	if need_deaths==True:
		has_nas = (epi
		 .groupby("key")
		 .apply(lambda d : d.total_deceased.isnull().any())
		 .reset_index(name="has_na")
		 .query("has_na==True")
		 .key.tolist()
		)
		print(len(has_nas), "regions have nulls in deceased column") 

		epi = epi.query("key not in @has_nas", engine="python")

	print("Number of regions searched for: ", len(set(keys)))
	print("Number of regions found: ", len(set(epi.key)))
	# print("Regions not found: ",
	# 	set(keys).difference(set(epi.key))
	# 	)

	return epi

def google_country_static_cov(keys, google_demo, google_econ, google_health):
	# keys: from google_level_2_keys()

	all_keys = pd.DataFrame({"lvl2_key":keys,
		"key": [i.split("_")[0] for i in keys]})

	all_keys = (all_keys
		.merge(google_demo.loc[:,["key", "human_development_index"]], on="key")
		.merge(google_econ.loc[:,["key", "gdp_per_capita", "human_capital_index"]], on="key")
		.merge(google_health.drop(columns=["hospital_beds"]), on="key")
		.drop(columns=["key"])
		.rename(columns = {"lvl2_key":"key"})
		.drop_duplicates()
		)

	return all_keys

def google_country_movil_gov_resp(keys, google_gov_resp):
	# keys: from google_level_2_keys()

	all_keys = pd.DataFrame({"lvl2_key":keys,
		"key": [i.split("_")[0] for i in keys]})
	country_keys = list(set(all_keys.key))

	country_count = (google_gov_resp
		.query("key in @country_keys", engine="python")
		.groupby("key").size()
		.reset_index(name="n")
		)
	print("Number of countries in query: ", len(country_keys))
	print("Number of countries with government response found: ", len(set(country_count.key)))
	print(country_count)

	all_keys = (all_keys
		.merge(google_gov_resp.loc[:,["date", "key", 
         "school_closing", "workplace_closing", "cancel_public_events", "restrictions_on_gatherings",
        "stay_at_home_requirements", "income_support", "stringency_index"]], on="key")
		.drop(columns=["key"])
		.rename(columns = {"lvl2_key":"key"})
		.assign(date = lambda d: pd.to_datetime(d.date))
		.drop_duplicates()
		)

	return all_keys

def extract_level_2(df, google_table):
	# Uses level 2 keys found in df and collects pertinent features found in google table. When feature is not
	#  present, it reverts to level 1 feature for level 2 reion

	exclusion_columns = ["key", "country_code", "subregion1_code", "subregion2_code"]
	extract_columns = list(set(df.columns).difference(exclusion_columns))

	df_no_na = df.dropna().reset_index(drop=True)
	df_na    = df[df.isna().any(axis=1)].reset_index(drop=True)

	all_keys = list(set(df_na.key))

	main_table = []
	with tqdm(total=len(set(all_keys))) as pbar:
		for i in all_keys:
			i_table = df.query("key==@i", engine="python").reset_index(drop=True)
			level_1_key = "_".join(i.split("_")[:2])

			for j in extract_columns:

				if np.isnan(i_table[j].values[0]):
					i_table.loc[0, j] = google_table.query("key==@level_1_key", engine="python")[j].values[0]

			main_table.append(i_table)
			pbar.update(1)

	main_table = pd.concat(main_table)
	main_table = pd.concat([main_table, df_no_na])
	return main_table

def google_all_level_2_data(countries, google_index, google_demo, google_epi, google_econ, google_geo, google_health, google_gov_resp):
	# Combines all google sources ready for training

	# Get all keys for all countries at level 2
	lvl2_keys = google_level_2_keys(google_index, countries)

	# Parse all Google support data
	google_demo_lvl2 = google_demo_level_2(lvl2_keys, google_demo)
	google_geo_lvl2  = google_geo_level_2(lvl2_keys, google_geo)
	google_country_static = google_country_static_cov(lvl2_keys, google_demo, google_econ, google_health)
	google_country_movil  =  google_country_movil_gov_resp(lvl2_keys, google_gov_resp)

	# Epi data
	google_epi_lvl2 = google_epi_level_2(lvl2_keys, google_epi)

	# Merge all data
	google_merge = pd.merge(google_epi_lvl2, google_country_movil, on=["key", "date"])
	google_merge = reduce(lambda left,right: pd.merge(left, right, on='key'), 
		[google_merge, google_demo_lvl2, google_geo_lvl2, google_country_static])

	# Return
	return google_merge


def prep_for_train_google(df, train_window=14, min_zeroes=0, split_date="2020-07-15"):
    
    from datetime import datetime, timedelta
    split_date = pd.to_datetime(split_date)
    
    # Smoothing
    df = (df
    	.assign(new_confirmed = lambda d: d.groupby(["key"])
                  .new_confirmed.transform(lambda e: e.rolling(7, min_periods=1).mean()),
                 new_deceased = lambda d: d.groupby(["key"])
                  .new_deceased.transform(lambda e: e.rolling(7, min_periods=1).mean()),
                 total_confirmed = lambda d: d.groupby(["key"])
                  .total_confirmed.transform(lambda e: e.rolling(7, min_periods=1).mean()),
                 total_deceased = lambda d: d.groupby(["key"])
                  .total_deceased.transform(lambda e: e.rolling(7, min_periods=1).mean())
                 )
    	)

    # Define features and target
    time_features = ['new_confirmed', 'total_confirmed', 'new_deceased',
       'total_deceased', 'school_closing', 'workplace_closing',
       'cancel_public_events', 'restrictions_on_gatherings',
       'stay_at_home_requirements', 'stringency_index'] 

    nontime_features = ['population', 'prop_female', 'prop_male', 'prop_young', 'prop_adult',
       'prop_old', 'latitude', 'longitude', 'area', 'human_development_index',
       'gdp_per_capita', 'human_capital_index', 'life_expectancy',
       'smoking_prevalence', 'diabetes_prevalence', 'infant_mortality_rate',
       'adult_male_mortality_rate', 'adult_female_mortality_rate',
       'pollution_mortality_rate', 'comorbidity_mortality_rate', 'nurses',
       'physicians', 'health_expenditure', 'out_of_pocket_health_expenditure']
    
    target_variables = ["new_confirmed", "new_deceased"]

    # Make sure we have enough data to train per key
    valid_keys = list(df
     .groupby(["key"]).size()
     .reset_index(name="n")
     .query("n>@train_window", engine="python")
     .key
    )
    df = df.query("key in @valid_keys", engine="python")
    
    # PROCESS
    df = df.sort_values(["key", "date"]).reset_index()
    
    # Split data
    df_train   = df.query("date < @split_date", engine="python")
    df_valid   = df.query("date >= @split_date", engine="python")
    
     # Get feature per window
    temp_features = []
    fact_features = []
    target_var    = []
    removed_count = 0
    
    with tqdm(total=len(valid_keys)) as pbar:
        for i in valid_keys:
            
            temp_df   = df_train.query("key==@i", engine="python").reset_index()
            days      = temp_df.shape[0]
            
            for d in range(0, days):
                
                if (d+train_window <= (temp_df.shape[0] - 1)) and \
                    (np.sum([i>0 for i in list(temp_df.new_confirmed)[d:d+train_window]])>min_zeroes):
                    
                    temp_features.append(
                        np.array(temp_df.iloc[d:(d+train_window),:].loc[:,time_features])
                    )
                    
                    fact_features.append(
                        np.array(temp_df.loc[:,nontime_features].drop_duplicates())[0]
                    )
                    
                    target_var.append(
                        np.asarray([
                            list(temp_df[target_variables[0]])[d+train_window],
                            list(temp_df[target_variables[1]])[d+train_window]
                        ])
                    )
                
                else:
                    removed_count = removed_count+1
                    
            pbar.update(1)
    
    print("Removed time windows: {}".format(removed_count))
    temp_features = np.asarray(temp_features)
    fact_features = np.asarray(fact_features)
    target_var    = np.asarray(target_var)
    
    return temp_features, fact_features, target_var, time_features, nontime_features

def ml_prep_v2(time_feat, fixed_feat, target_var, temp_feats=14, norm_mob_feat=True):
    # Prepping data for ML
    
    # Get train/valid indexes
    train_perc = 0.75
    np.random.seed(1234)
    train_index = np.random.choice(range(time_feat.shape[0]), 
                                   round(time_feat.shape[0] * train_perc), 
                                   replace=False)
    valid_index = np.array(list(set(range(time_feat.shape[0])).difference(list(train_index))))
    
    # Get train/valid data
    x_train_time_feat = time_feat[train_index]
    x_valid_time_feat = time_feat[valid_index]

    x_train_fixed_feat = fixed_feat[train_index]
    x_valid_fixed_feat = fixed_feat[valid_index]

    y_train = target_var[train_index]
    y_valid = target_var[valid_index]
    
    # Normalize data if needed
    time_feat_max  = x_train_time_feat.reshape(-1, temp_feats).max(axis=0)
    time_feat_min  = x_train_time_feat.reshape(-1, temp_feats).min(axis=0)

    fixed_feat_max = x_train_fixed_feat.max(axis=0)
    fixed_feat_min = x_train_fixed_feat.min(axis=0)
    
    if norm_mob_feat==False:
        time_feat_max[-5:] = 1
        time_feat_min[-5:] = 0
        
    print(time_feat_max)
    print(time_feat_min)
    print(fixed_feat_max)
    print(fixed_feat_min)
    x_train_time_feat = (x_train_time_feat - time_feat_min) / (time_feat_max - time_feat_min)
    x_valid_time_feat = (x_valid_time_feat - time_feat_min) / (time_feat_max - time_feat_min)

    x_train_fixed_feat = (x_train_fixed_feat - fixed_feat_min) / (fixed_feat_max - fixed_feat_min)
    x_valid_fixed_feat = (x_valid_fixed_feat - fixed_feat_min) / (fixed_feat_max - fixed_feat_min)
    
    return x_train_time_feat, x_valid_time_feat, x_train_fixed_feat, x_valid_fixed_feat, y_train, y_valid, \
           time_feat_max, time_feat_min, fixed_feat_max, fixed_feat_min



## EXECUTE



## PLOT PREPPING FUNCTIONS
def prep_incidence(df_epi, google_demo, google_index, assign_names=False):
	#df_epi: obtained from google_epi_level_2()
	#assign_names: if True then region and country names assigned based on keys

	# Obtain annualized and cumulative incidence
	df_epi = (df_epi
	 .merge(google_demo.loc[:,["key", "population"]], on="key")
	 .sort_values(['key', "date"])
	 .assign(
	     annual_inc = lambda d: d.groupby(["key"])
	                  .new_confirmed.transform(lambda e: e.rolling(7, min_periods=1).sum())
	 )
	 .assign(annual_inc = lambda d : (d.annual_inc/d.population)*52)
	 .assign(cum_inc = lambda d : d.total_confirmed/d.population)

	 .assign(std_inc = lambda d: d.groupby("key") # Standardize incidence
	 	.annual_inc.transform(lambda e: (e - e.min())/(e.max() - e.min()) ))
	 .assign(std_last = lambda d: d.groupby("key") # Get last standardized annual value
	    .std_inc.transform(lambda e: list(e)[-1] ))

	 .assign(annual_inc_last = lambda d: d.groupby("key") # Get latest annual value
	    .annual_inc.transform(lambda e: list(e)[-1] ))
	 .assign(cum_inc_last = lambda d: d.groupby("key") # Get latest cummulative value
	    .cum_inc.transform(lambda e: list(e)[-1] ))
	 .sort_values(["key", "std_last"], ascending=False)
	)

	# Assign country/region names if needed
	if assign_names==True:
		df_epi = (df_epi
			.merge(google_index
	        	  .query("aggregation_level==1")
	              .loc[:,["key", "country_name", "subregion1_name"]], on="key")
			)

	return df_epi