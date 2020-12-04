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



data = pd.read_csv(path)




demographics = data[['age', 'sex','ethnicity','temp_vsorres','rr_vsorres','oxy_vsorres','hr_vsorres','sysbp_vsorres','admission_diabp_vsorres','infect_cmtrt','onset2admission']]
clinical = data[['daily_neutro_lborres', 'daily_lymp_lborres', 'daily_plt_lborres', 'daily_crp_lborres', 'daily_creat_lborres', 'daily_bun_lborres']]
morbidities = data[['chrincard', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn', 'modliv', 'diabetescom_mhyn', 'diabetes_mhyn', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn']]

variables = pd.concat([demographics, clinical, morbidities], axis=1)





# Do the following replacements:
# no, No, NO, Unchecked -> 0
# yes, Yes, YES, Checked -> 1
# Unknown -> NaN
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



variables = variables.replace(to_replace=['no', 'No', 'NO', 'yes', 'Yes', 'YES', ], value=[0,0,0,1,1,1])

variables = variables.replace(to_replace=['Unknown', 'Not specified', ''], value=['nan', 'nan', 'nan'])

variables = variables.replace(to_replace=['Unchecked', 'Checked'], value=[0,1])

variables = variables.replace(to_replace=['Male', 'Female'], value=[0,1])



ethnic_values = variables.ethnicity.unique()

# Remove nan 

ethnic_values = np.delete(ethnic_values, 2)


variables['ethnicity'] = variables['ethnicity'].replace(to_replace=ethnic_values, value = range(len(ethnic_values)) )








Ranges = pd.DataFrame( columns = variables.columns, index = ['lower', 'upper']  )

Ranges['age'] = [0, 110]

Ranges['temp_vsorres'] = [34,44]
Ranges['rr_vsorres'] = [10,50]
Ranges['oxy_vsorres'] = [60,100]
Ranges['hr_vsorres'] = [20,200]
Ranges['sysbp_vsorres'] = [50,300]
Ranges['admission_diabp_vsorres'] = [20,200]
Ranges['onset2admission'] = [-100,100]
Ranges['daily_neutro_lborres'] = [0,50]
Ranges['admission_diabp_vsorres'] = [20,200]
Ranges['daily_lymp_lborres'] = [0,20]
Ranges['daily_plt_lborres'] = [0,50]
Ranges['daily_crp_lborres'] = [5,700]
Ranges['daily_creat_lborres'] = [50,1000]
Ranges['daily_bun_lborres'] = [0,50]







def explore(var):
    
    
    
    if Ranges[var].isnull().values.any():
        print(variables.loc[:, var].value_counts() )
        
    else:
        
        print('Lower:', Ranges.loc['lower', var])
        print('Upper:', Ranges.loc['upper', var])
        
        print('Max: ', variables[var].max() )
        print('Min: ', variables[var].min() )
        
        upper = Ranges.loc['upper', var]
        lower = Ranges.loc['lower', var]  
        
        halfRange = (upper - lower)/2
        
        variables[(variables[var] < lower) & (variables[var] >  lower - halfRange) ].hist(var) 
        
        variables[(variables[var] < upper + halfRange) & (variables[var] > upper)].hist(var)
        
        variables[(variables[var] > -upper) & (variables[var] < -lower)].hist(var)
        
        variables[(variables[var] < halfRange/5) & (variables[var] > -halfRange/5)].hist(var)
        
    return
        
            






variables[(variables['oxy_vsorres'] <-60) & (variables['oxy_vsorres'] >-100)] *=-1


variables['temp_vsorres'][(variables['temp_vsorres'] >-44) & (variables['temp_vsorres'] < -34)] *= -1







# Drop any rows or columns that have all missing values

variables = variables.dropna(axis=1, how='all')

variables = variables.dropna(axis=0, how = 'all')


# Find data types in each column

types = variables.dtypes


# Find value counts in a given column

variables.iloc[:, 3].value_counts()

variables.loc[:, 'chronicpul_mhyn'].value_counts()






# Find the number of NaN's in each column


missingValues = variables.isna().sum()

criteria = missingValues < 50000














