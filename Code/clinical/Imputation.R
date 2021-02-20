###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code does multiple imputation by chained equations for cleaned 
# topline clinical data

###################################################################### 

library(dplyr)
library(ggplot2)
library(mice)
library(data.table)

########################### READ DATA ###############################################

# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True
ultra <- TRUE
if (ultra == TRUE){
  root <- '/home/u034/'
} else {
  
  root <- 'Y:/'
}


data <- fread( paste( root, 'shared/data/wp-5/clinical_imputation/cleanData.csv', sep=''), data.table=FALSE)

# nan from python in a column that has strings is read as "", so need to replace with NA.
data[ data[, 'ethnicity'] == "",  'ethnicity'] <- NA

# catVars is a list of variables that are categorical.
catVars <- c('sex', 'ethnicity', 'infect_cmtrt', 'chrincard', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn', 'modliv',           
             'diabetes', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn', 'immunosuppressed_yn' )

# mice needs categorical varaiables to be factors
data[catVars] <- lapply(data[catVars], factor )

########################### PLOT MISSINGNESS ###############################################

# Create plots for number of NA in rows and columns
columnNA <- as.data.frame(colSums(is.na(data)) )

colnames(columnNA) <- 'Number of NA'

rowNA <- as.data.frame(rowSums(is.na(data)) )

colnames(rowNA) <- 'Number of NA'


columnNAPlot <-ggplot(columnNA, aes(x= reorder( rownames(columnNA), `Number of NA` ) , y=`Number of NA` )) + geom_bar(stat="identity", fill="steelblue") + coord_flip()

show(columnNAPlot)

rowNAPlot <-ggplot(rowNA, aes(x=`Number of NA` ) ) + geom_histogram(bins=dim(data)[2], fill="steelblue") + 
  scale_x_continuous(breaks = seq(0, dim(data)[2], 1) ) +
  scale_y_continuous(breaks = seq(0, 37000, 5000) )

show(rowNAPlot)

# Take a sample of data
#sample <- sample_n(data, 500)

################################ IMPUTATION ###############################################

# impExclude is a vector of variables that are not to be imputed
impExclude <- c('subjid', 'sfr')

# mice normally automatically detects data types in each column and selects an appropriate imputation method for them
# However, since we are excluding some variables from the imputation, we have to set up the method vector manually.

createMethod <- function(impExclude){
  method <- as.data.frame( sapply(data, nlevels) )
  
  method[ method[,1] > 2, ] <- 'polyreg'
  
  method[ impExclude, ] <- ''
  
  method[ method[,1] ==0 , ] <- 'pmm'
  
  method[ method[,1] ==2 , ] <- 'logreg'
  
  method <- dplyr::pull(method, colnames(method) )
}

method <- createMethod(impExclude)

# Carry out imputation
Imputation <- mice(data,m=1,maxit=100, predictorMatrix = quickpred(data, exclude = impExclude), method = method  )

# Save the mids objection 
save(Imputation, file = paste(root,'shared/data/wp-5/clinical_imputation/mids.RData', sep='') )

####################################### WRITE IMPUTED DATASETS ###########################################

for (m in 1:Imputation$m ){
  name <- paste(root, 'shared/data/wp-5/clinical_imputation/imputed_datasets/imputed_dataset_', sep="")
  name <- paste(name, m, sep="")
  name <- paste(name, '.csv', sep="")
  
  Complete <- complete(Imputation, m)
  
  Complete['sfr'] <- Complete['sao2']/Complete['fio2']
  
  write.csv(Complete , name )
}






