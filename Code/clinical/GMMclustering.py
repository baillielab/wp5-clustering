###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code carries out GMM clustering on clinical topline data
# that has been cleaned and then imputed.

###################################################################### 

import numpy as np
import pandas as pd
from sklearn.mixture import GaussianMixture
from scipy.stats import zscore
import plotly.express as px
import plotly.io as pio

############################### IMPORT DATA ##########################

# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True

ultra = True

if ultra == True:
    root = '/home/u034/'
else:
    root = 'Y:/'
    
    
path = root + 'shared/data/wp-5/clinical_imputation/imputed_datasets/imputed_dataset_1.csv'

data = pd.read_csv(path, index_col=0)

# Make subjid the index, and drop subjid as a column
#data.index = data['subjid']

#data = data.drop('subjid', axis=1)

########################## FUNCTIONS #################################

# Get the mean and standard deviation of each column. 
# Define a function that is the inverse of zscore
moments= pd.DataFrame( [ data.mean() , data.std()], columns = data.columns, index = ['mean', 'std'] )

def zscoreInverse(df, moments):
    
    original = df.dot( np.diag( moments.loc['std', : ] ) ) +  \
        np.array( [moments.loc['mean', : ] , ]*  data.shape[0] )
        
    original = original.round(5) 
        
    return original

############################# CLUSTERING #######################################

oneHot = pd.get_dummies(data['ethnicity'])

data = pd.concat( [data, oneHot], axis=1)

data = data.drop(['ethnicity'], axis=1)

data['sex'] = data['sex'].replace(to_replace=['Female', 'Male' ], value=[0,1])

# z-normalise the data. This is mainly for visulisation purposes.
normData = pd.DataFrame( zscore(data.drop(['subjid'], axis = 1)),       \
                        columns = data.columns.drop('subjid'), index = data.index)
    
normData.insert(0, 'subjid', data['subjid']  )  

# Choose multiple feature subsets, for which clustering will be carried out
allVars = normData.columns.drop(['sao2', 'fio2', 'subjid' ])

featureDict = {'allVars': allVars }

  
def cluster(key, maxClusters):    
    
    summaryStats = pd.DataFrame(columns = ['BIC', 'Likelihood'] )
        
    predictions = pd.DataFrame(normData['subjid'])
    
    variables = featureDict[key]
    
    for components in range(1, maxClusters +1):
        
        # Fit GMM model
        gmm = GaussianMixture(n_components=components, covariance_type='diag', random_state=0).fit( normData[variables] )
        
        # means and covariances of the GMM components
        means = pd.DataFrame( gmm.means_, columns = normData[variables].columns).T
        
        #covs = gmm.covariances_, 
        
        # Create a plot of the means
        figure = px.line(means, x=means.index, y= means.columns, template = "simple_white" )
        
        savepath = root + 'stevenkerr/Git/wp5-clustering/Code/clinical/GMM_clustering_' + key 
        
        means.to_csv( savepath + '/means' + str(components) + '.csv')
        
        pio.write_html(figure, file= savepath + '/' + str(components) + '.html', auto_open= False) 
    
        # bic and score for the model
        summaryStats.loc[components, :] = [gmm.bic(normData[variables]), gmm.score(normData[variables])]
     
        predictions['clinical' + str(components)] =  gmm.predict(normData[variables])
        
    return(summaryStats, predictions)
    
    
    
def multiCluster(maxClusters):

    for key in featureDict: 
        
        savepath = root + 'stevenkerr/Git/wp5-clustering/Code/clinical/GMM_clustering_' + key 
        
        (summaryStats, predictions) = cluster(key, maxClusters)
        
        predictions.to_csv(savepath + '/predictions.csv', index = False)
        
        summaryStats.to_csv(savepath + '/summaryStats.csv', index = False)   
    
    
multiCluster(20)
