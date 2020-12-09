# -*- coding: utf-8 -*-
"""
Created on Wed Dec  2 10:03:25 2020

@author: Steven
"""

import pandas as pd

import numpy as np

import math

import matplotlib.pyplot as plt



path = "Y:/shared/data/wp-5/data_linkage/release/datasets/clinical/002_crf_20200904_topline_cleaned_20200921_161000.csv"

# Create list of columns of interests

demographics = ['subjid', 'age', 'sex','ethnicity','temp_vsorres','rr_vsorres','oxy_vsorres','hr_vsorres','sysbp_vsorres','admission_diabp_vsorres','infect_cmtrt','onset2admission']
clinical = ['daily_neutro_lborres', 'daily_lymp_lborres', 'daily_plt_lborres', 'daily_crp_lborres', 'daily_creat_lborres', 'daily_bun_lborres']
morbidities = ['chrincard', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn', 'modliv', 'diabetescom_mhyn', 'diabetes_mhyn', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn']

columns = demographics + clinical + morbidities



data = pd.read_csv(path, usecols = columns)

# Ranges contains acceptable variable ranges.

Ranges = pd.read_excel(r'Ranges.xlsx')


# Drop any rows or columns that have all missing values

data = data.dropna(axis=1, how='all')

data = data.dropna(axis=0, how = 'all')



# Do the following replacements:
# no, No, NO, Unchecked -> 0
# yes, Yes, YES, Checked -> 1
# Unknown, Not specified -> nan
#
# White -> 0
# Other -> 1
# Arab -> 2
# Black -> 3
# West Asian -> 4
# South Asian -> 5
# East Asian -> 6
# Latin American -> 7
# Aboriginal/First Nations -> 8



data = data.replace(to_replace=['no', 'No', 'NO', 'yes', 'Yes', 'YES' ], value=[0,0,0,1,1,1])

data = data.replace(to_replace=['Unknown', 'Not specified', ''], value=['nan', 'nan', 'nan'])

data = data.replace(to_replace=['Unchecked', 'Checked'], value=[0,1])

data = data.replace(to_replace=['Male', 'Female'], value=[0,1])



ethnic_values = data.ethnicity.unique()

# Remove nan 

ethnic_values  = ethnic_values[~pd.isnull(ethnic_values)]


data['ethnicity'] = data['ethnicity'].replace(to_replace=ethnic_values, value = range(len(ethnic_values)) )




# catVars is a list of variables that are categorical
# nonCatVars is a list of variables that are not categorical 

catVars= ['subjid', 'sex', 'ethnicity', 'infect_cmtrt', 'chrincard', 'chronicpul_mhyn', 
          'asthma_mhyn', 'renal_mhyn', 'modliv', 'diabetescom_mhyn', 'diabetes_mhyn', 
          'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn']

nonCatVars = list( set(data.columns) - set(catVars) )



data.loc[:, data.columns != 'subjid'] =  data.loc[:, data.columns != 'subjid'].astype(float)

# The function explore print value counts for categorical variables, and
# plots important ranges for non categorical variables.


def explore(var):
    
    if var in catVars:
        print(data.loc[:, var].value_counts() )
        
    else:
        
        upper = float(Ranges.loc[1, var])
        lower = float(Ranges.loc[2, var])  
        
        print('Lower:', lower)
        print('Upper:', upper)
        
        print('Max: ', data[var].max() )
        print('Min: ', data[var].min() )
        
        
        
        halfRange = (upper - lower)/2
        
        data[(data[var] < lower) & (data[var] >  lower - halfRange) ].hist(var) 
        
        data[(data[var] < upper + halfRange) & (data[var] > upper)].hist(var)
        
        data[(data[var] > -upper) & (data[var] < -lower)].hist(var)
        
        data[(data[var] < halfRange/5) & (data[var] > -halfRange/5)].hist(var)
        
    return
        
            
# This function 'corrects' the data.
# Values between hard limits and soft limits get set to the soft limit.
# Negative values whose magnitude is in the acceptable range gets multiplied by -1.
# Everything else gets set to nan.

def correct(data):
    
    for col in nonCatVars:
        
        print(col)
        
        hardUpper = float(Ranges.loc[0, col ])
        softUpper = float(Ranges.loc[1, col ])
        
        hardLower = float(Ranges.loc[2, col ])
        softLower = float(Ranges.loc[3, col ])
        
        
        data[col][  (data[col] >= softUpper) & (data[col] <= hardUpper) ] = softUpper
        
        data[col][  (data[col] >= hardLower) & (data[col] <= softLower) ] = softLower
        
        data[col][   (data[col] >= -softUpper) & (data[col] <= -softLower)] *= -1
        
        data[col][  (data[col] > hardUpper) | (data[col] < hardLower) ] = 'nan'
    
    data.loc[:, data.columns != 'subjid'] =  data.loc[:, data.columns != 'subjid'].astype(float)
        
    return data        
        
        
data = correct(data)        
        
# Save data as csv        

data.to_csv( 'Y:/stevenkerr/processedData.csv', index = False)













