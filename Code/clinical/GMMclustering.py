# -*- coding: utf-8 -*-
"""
Created on Sun Dec 27 16:34:18 2020

@author: Steven
"""

import numpy as np

import pandas as pd

from sklearn.mixture import GaussianMixture

from scipy.stats import zscore

import plotly.express as px

import plotly.io as pio

import pickle


# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True

ultra = True

if ultra == True:
    
    root = '/home/u034/'
    
else:
    
    root = 'Y:/'
    
    

path = root + 'shared/data/wp-5/clinical_imputation/imputed_datasets/imputed_dataset_1.csv'

data = pd.read_csv(path, index_col=0)

# Make subjid the index, and drop subid as a column

data.index = data['subjid']

data = data.drop('subjid', axis=1)

# Get the mean and standard deviation of each column. 
# Define a function that is the inverse of zscore

moments= pd.DataFrame( [ data.mean() , data.std()], columns = data.columns, index = ['mean', 'std'] )


def zscoreInverse(df, moments):
    
    original = df.dot( np.diag( moments.loc['std', : ] ) ) +  \
        np.array( [moments.loc['mean', : ] , ]*  data.shape[0] )
        
    original = original.round(5) 
        
    return original


def Save(Object, filename):
    
    file = open( root + 'stevenkerr/Git/wp5-clustering/Code/clinical/GMM clustering/' + filename + ".pkl", "wb")
    
    pickle.dump(Object, file)

    file.close()
    
    
def Open(filename):

    file = open( root + 'stevenkerr/Git/wp5-clustering/Code/clinical/GMM clustering/' + filename + ".pkl", "rb")
    
    return pickle.load(file)    


# z-normalise the data.

normData = pd.DataFrame(  zscore( data ), columns = data.columns, index = data.index)



# Choose variables to be included in clustering

variables = normData.columns.drop(['sao2', 'fio2' ])



summaryStats = pd.DataFrame(columns = ['BIC', 'Likelihood'] )


for components in range(1,21):
    
    # Fit GMM model
    
    gmm = GaussianMixture(n_components=components, covariance_type='diag', random_state=0).fit( normData[variables] )
    
    
    # means and covariances of the GMM components
    
    means = pd.DataFrame( gmm.means_, columns = normData[variables].columns).T
    
    #covs = gmm.covariances_, 
    
    
    
    # Save a plot of the means
    
    figure = px.line(means, x=means.index, y= means.columns, template = "simple_white" )
      
    savepath = root + 'stevenkerr/Git/wp5-clustering/Code/clinical/GMM clustering/' + str(components) + '.html'
      
    pio.write_html(figure, file=savepath, auto_open=False)


     # bic and score for the model
    
    summaryStats.loc[components, :] = [gmm.bic(normData[variables]), gmm.score(normData[variables])]
    




Save(summaryStats, 'summaryStats')


summaryStats = Open('summaryStats')


# Predict probability of labels, and labels.

probs = gmm.predict_proba( normData[variables] )

labels = gmm.predict( normData[variables] )




