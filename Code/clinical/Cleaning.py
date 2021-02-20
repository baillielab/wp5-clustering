###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code selects a subset of variables from the topline clinical data
# and cleans them

###################################################################### 

import pandas as pd
import numpy as np
import os

########################## CURATE VARIABLE SELECTOIN #################

# catVars is a list of variables that are categorical
catVars= ['subjid', 'sex', 'ethnicity', 'infect_cmtrt', 'chrincard', 'chronicpul_mhyn', 
          'asthma_mhyn', 'renal_mhyn', 'modliv', 'diabetes_type_mhyn','diabetescom_mhyn',
          'diabetes_mhyn', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn', 'oxy_vsorresu']

# realVars is a list of variables that take values in the real numbers
realVars = ['age', 'onset2admission', 'hr_vsorres', 'sysbp_vsorres','temp_vsorres'
            ,'rr_vsorres', 'admission_diabp_vsorres', 'daily_crp_lborres',
            'daily_bun_lborres','daily_creat_lborres','daily_neutro_lborres',
            'daily_lymp_lborres','daily_plt_lborres', 'daily_fio2c_lborres']

# percentVars is a list of variables that are percentages
percentVars = ['oxy_vsorres', 'daily_sao2_lborres']

# Int1Vars is a list of variables that take values in [0,1]
Int1Vars = ['daily_fio2_lborres' , 'daily_fio2b_lborres']

columns = catVars + realVars + percentVars + Int1Vars

############################ IMPORT DATA ############################

# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True

ultra = False
if ultra == True:
    root = '/home/u034/'
else:
    root = 'Y:/'

path = root + 'shared/data/wp-5/data_linkage/release/datasets/clinical/002_crf_20200904_topline_cleaned_20200921_161000.csv'

workingDir = root + 'stevenkerr/Git/wp5-clustering/Code/clinical'

immunopath = root + 'stevenkerr/Git/ccp_medication/subset_extracts/imm_supp_02022021.csv'

os.chdir(workingDir)


data = pd.read_csv(path, usecols = columns)
immuno = pd.read_csv(immunopath)

data = data.merge(immuno, how='left')

######################### GENERAL CLEANING #############################

# Drop any rows or columns that have all missing values
data = data.dropna(axis=1, how='all')

data = data.dropna(axis=0, how = 'all')

# Do the following replacements:
# no, No, NO, Unchecked, N/K -> 0
# yes, Yes, YES, Checked -> 1
# Unknown, Not specified -> nan
# Male -> 0
# Female -> 1
data = data.replace(to_replace=['no', 'No', 'NO', 'yes', 'Yes', 'YES' ], value=[0,0,0,1,1,1])

data = data.replace(to_replace=['Unknown', 'Not specified', '', 'N/K'], value=[np.nan, np.nan, np.nan, np.nan])

data = data.replace(to_replace=['Unchecked', 'Checked'], value=[0,1])

################### EXPLORE VARIABLES ##############################

# Ranges contains acceptable variable ranges.
Ranges = pd.read_excel(r'Ranges.xlsx')

Ranges.iloc[0:4, 1:] = Ranges.iloc[0:4, 1:].astype(float)

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

######################## CLEANING FUNCTIONS ##########################


# If appropriate, multiply negative values in a column by -1
def fixNeg(data):
    
    Vars = Ranges.T.loc[ Ranges.T.iloc[:, 4] == 'Yes'  ].index 

    data[Vars] = data[Vars].abs()
    return data

# If a percentage variable has been recorded as in [0,1], multiply by 100
# if it has been recorded as in [100, 1000], divide by 10
def cleanPercent(data):
    for var in percentVars:
        
        data[var][ (data[var]>0 ) & (data[var] <=1)  ] *= 100
        
        data[var][ (data[var]>100 ) & (data[var] <=1000)  ] /= 10
    return(data)

# If a variable in [0,1] has been recorded as a percentage, divide it by 100
def cleanInt1(data):
    for var in Int1Vars:
         data[var][ (data[var]>1 ) & (data[var] <=100)  ] /= 100
    return data
                      
# This function squeezes variables.
# Values between hard limits and soft limits get set to the soft limit.
# Everything else gets set to nan.
def squeeze(data):
    for var in (realVars + percentVars + Int1Vars):
    
        hardUpper = Ranges.loc[0, var ]
        softUpper = Ranges.loc[1, var ]
        
        hardLower = Ranges.loc[2, var ]
        softLower = Ranges.loc[3, var ]
        
        
        data[var][  (data[var] >= softUpper) & (data[var] <= hardUpper) ] = softUpper
        
        data[var][  (data[var] >= hardLower) & (data[var] <= softLower) ] = softLower
        
        data[var][  (data[var] > hardUpper) | (data[var] < hardLower) ] = float('nan')
    return data   
 
# This functions adds a derived SFR column to the data
def addSFR(data):
    
    #Create column sao2
    
    data['sao2'] = data['daily_sao2_lborres']/100
    
    indices = data.index[ (data['daily_sao2_lborres'].isnull() ) & ( ~data['oxy_vsorres'].isnull() ) ].tolist()
    
    data.loc[ indices, 'sao2'] = data.loc[ indices, 'oxy_vsorres' ]/100
    
    # Create column derived fio2
    data['fio2'] = data['daily_fio2_lborres']
    
    indices2 = data.index[ (data['fio2'].isnull())  & (~data['daily_fio2b_lborres'].isnull()   )].tolist()
    
    data.loc[ indices2,  'fio2'] = data.loc[ indices2, 'daily_fio2b_lborres']
    
    indices3 = data.index[ (data['fio2'].isnull())  & (~data['daily_fio2c_lborres'].isnull()   )].tolist()
    
    # daily_fio2c_lborres is the amount of oxygen in L/min patient receives
    # in addition to atmospheric oxygen, assumed to be 21% of atmosphere.
    # Each L/min of additional oxygen implies an increase in FiO2 of 0.04%
    data.loc[ indices3,  'fio2'] = 0.04* data.loc[ indices3, 'daily_fio2c_lborres'] + 0.21
    
    indices4 = data.index[ (data['fio2'].isnull())  & (data['oxy_vsorresu'] == 'Room air') ].tolist()  
    
    data.loc[ indices4, 'fio2'] = 0.21
        
    # Create column SFR
    data['sfr'] = data['sao2']/data['fio2']
    
    return data


def addDiabetes(data):
    
    positive = (data['diabetes_type_mhyn'] == 1) | (data['diabetes_type_mhyn'] == 2) \
                      |( data['diabetescom_mhyn'] == 1)  | ( data['diabetes_mhyn'] == 1)
    
    negative =  (data['diabetes_type_mhyn'] == 0) | ( data['diabetescom_mhyn'] == 0) \
             | ( data['diabetes_mhyn'] == 0)                   
    
    data['diabetes'] = np.nan   
    
    data['diabetes'][negative]  = 0
    
    data['diabetes'][positive]  = 1 
    
    return data

############################# CLEANING ########################################

# 'daily_fio2c_lborres' is is oxygen received in litres per minute.
# We assume that if greater than 20, they actually wrote FiO2
# Each additional 4% FiO2 is equivalent to 1 L/min
# daily_fio2c_lborres needs to be converted to a number in [0,1]

data['daily_fio2c_lborres'][data['daily_fio2c_lborres'] >= 20] = \
  (data['daily_fio2c_lborres'][data['daily_fio2c_lborres'] >= 20] - 20)/4   

data = fixNeg(data)    

data = cleanPercent(data)

data = cleanInt1(data)

data = squeeze(data)


data = addSFR(data)

data = addDiabetes(data)

# Drop columns that aren't of interest
data = data.drop( percentVars + Int1Vars + ['daily_fio2c_lborres', 'diabetes_type_mhyn', \
                             'diabetescom_mhyn', 'diabetes_mhyn', 'oxy_vsorresu'], axis=1 )

############################# WRITE DATA ########################################
    
# Save data as csv  .
data.to_csv( root + 'shared/data/wp-5/clinical_imputation/cleanData.csv', index = False)



