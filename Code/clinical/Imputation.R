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

Imputation <- mice(data,m=5,maxit=50, predictorMatrix = quickpred(data, exclude = 'subjid')  )

# Save the mids objection 

save(Imputation, file = paste( root,'shared/data/wp-5/clinical_imputation/mids.RData', sep='') )

# Write complete imputed datasets to csv

for (m in 1:Imputation$m ){
  
  name <- paste(root, 'shared/data/wp-5/clinical_imputation/imputed_datasets/imputed_dataset_', sep="")
  name <- paste(name, m, sep="")
  name <- paste(name, '.csv', sep="")
  
  write.csv( complete(Imputation, m), name )
}



# Imputation diagnostics

# freqTab prints frequency tables for the imputations and the real data against each other for the categorical variables
# to allow comparison.


freqTab <- function(Imputation){
  
  for (var in catVars){
    
    print(var)
    
    real <- table(data[var])
    
    nlevels <- length(real)
    
    freqTable <- data.frame( matrix(ncol = Imputation$m +1, nrow = nlevels) )
    
    colnames(freqTable) <- append( seq(1, Imputation$m, 1), 'real')
    
    rownames(freqTable) <- seq(0, nlevels-1, 1)
    
    
    for (m in 1:Imputation$m ){
      
      tab <- table(as.data.frame(Imputation$imp[var])[,m] )
      
      freqTable[, m] <- tab / sum(tab)
      
      freqTable[, 'real'] <- table(data[var]) / sum( table(data[var]) )
      
    }
    print(var)
    
    print(freqTable)
    
  }
}


# Load mids object


load(file = paste(root, 'shared/data/wp-5/clinical_imputation/mids.RData', sep='') )  



densityplot(Imputation)

plot(Imputation)

freqTab(Imputation)


