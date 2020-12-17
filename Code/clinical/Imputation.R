library(dplyr)

library(ggplot2)

library(mice)

library(data.table)

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

# mice needs categorical varaiables to be factors

data[catVars] <- lapply(data[catVars], factor )

sapply(data, class)


# Create plots for number of NA in rows and columns


columnNA <- as.data.frame(colSums(is.na(data)) )

colnames(columnNA) <- 'Number of NA'

rowNA <- as.data.frame(rowSums(is.na(data)) )

colnames(rowNA) <- 'Number of NA'



columnNAPlot <-ggplot(columnNA, aes(x= reorder( rownames(columnNA), `Number of NA` ) , y=`Number of NA` )) + geom_bar(stat="identity", fill="steelblue") + coord_flip()

show(columnNAPlot)


rowNAPlot <-ggplot(rowNA, aes(x=`Number of NA` ) ) + geom_histogram(bins=28, fill="steelblue") + 
  scale_x_continuous(breaks = seq(0, 28, 1) ) +
  scale_y_continuous(breaks = seq(0, 37000, 5000) )

show(rowNAPlot)



# Take a sample of data

# sample <- sample_n(data, 500)

# The variables to be imputed are everything except subjid.

imputationVars <- colnames(data)[colnames(data) != 'subjid' ]

# Carry out imputation

Imputation <- mice(sample,m=3,maxit=5, predictorMatrix = quickpred(sample, exclude = 'subjid')  )

# Save the mids objection 

save(Imputation, file = paste( root,'shared/data/wp-5/clinical_imputation/mids.RData', sep='') )

# Write complete imputed datasets to csv

for (m in 1:Imputation$m ){
  
  name <- paste(root, 'shared/data/wp-5/clinical_imputation/imputed_datasets/imputed_dataset_', sep="")
  name <- paste(name, m, sep="")
  name <- paste(name, '.csv', sep="")
  
  write.csv( complete(Imputation, m), name )
}






