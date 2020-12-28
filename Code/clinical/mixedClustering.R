library(dplyr)

library(data.table)

library(MixAll)

# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True

ultra <- TRUE

if (ultra == TRUE){
  
  root <- '/home/u034/'
} else {
  
  root <- 'Y:/'
}



data <- fread( paste( root, 'shared/data/wp-5/clinical_imputation/cleanData.csv', sep=''), data.table=FALSE)

# catVars is a list of variables that are categorical.

catVars <- c('sex', 'ethnicity', 'infect_cmtrt', 'chrincard', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn', 'modliv',           
             'diabetescom_mhyn', 'diabetes_mhyn', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn' )

# Rename rows by subjid, and drop subjid column

rownames(data) <- data$subjid

data$subjid <- NULL

# z- score normalise data

data <- as.data.frame(scale(data) )



# Format categorical varaiables as factors

data[catVars] <- lapply(data[catVars], factor )

sapply(data, class)

# Separate dataset into continuous and categorical variables. Needed for mixed clustering methods

dataCat <- data[catVars]

dataCont <- data[!colnames(data) %in% catVars]


# Create parameters for clustering

dataList = list(dataCat, dataCont)

models = c("categorical_pk_pjk","gaussian_pk_sjk")


mixedClustering <- clusterMixedData( dataList, models, nbCluster=5, strategy = clusterSemiSEMStrategy(), criterion = 'BIC' )

# Save the clustering object

save(mixedClustering, file = paste( root,'stevenkerr/Git/wp5-clustering/Code/clinical/clustering.RData', sep='') )















