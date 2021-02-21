###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code can be used to examine the results of the imputation, with graphs
# and frequency tables.

###################################################################### 
library(mice)
library(data.table)


# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True

ultra <- FALSE

if (ultra == TRUE){
  
  root <- '/home/u034/'
} else {
  
  root <- 'Y:/'
}


# catVars is a list of variables that are categorical.
catVars <- c('sex', 'ethnicity', 'infect_cmtrt', 'chrincard', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn', 'modliv',           
             'diabetes', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn', 'immunosuppressed_yn' )

contVars <- c('temp_vsorres', 'hr_vsorres', 'rr_vsorres', 'sysbp_vsorres', 'admission_diabp_vsorres', 'daily_lymp_lborres', 
              'daily_neutro_lborres', 'daily_plt_lborres', 'daily_bun_lborres', 'daily_creat_lborres', 'daily_crp_lborres',      
           'onset2admission', 'age', 'sao2', 'fio2')

# freqTab prints frequency tables for the imputations and the real data against each other for the categorical variables
# to allow comparison.
freqTab <- function(Imputation){
  
  for (var in catVars){
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
# Load data
data <- fread( paste( root, 'shared/data/wp-5/clinical_imputation/cleanData.csv', sep=''), data.table=FALSE)

# nan from python in a column that has strings is read as "", so need to replace with NA.
data[, c('sex', 'ethnicity')][ data[, c('sex', 'ethnicity')] == ""] <- NA

densityplot(Imputation, data ~ temp_vsorres + hr_vsorres + rr_vsorres + sysbp_vsorres + admission_diabp_vsorres + daily_lymp_lborres +
      daily_neutro_lborres + daily_plt_lborres + daily_bun_lborres + daily_creat_lborres + daily_crp_lborres + onset2admission + age + sao2 + fio2 )

plot(Imputation)

freqTab(Imputation)




