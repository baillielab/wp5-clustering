


# Imputation diagnostics

# freqTab prints frequency tables for the imputations and the real data against each other for the categorical variables
# to allow comparison.


freqTab <- function(Imputation){
  
  for (var in catVars){
    
    real <- table(data[var])
    
    nlevels <- length(real)
    
    freqTable <- data.frame( matrix(ncol = Imputation$m +1, nrow = nlevels) )
    
    colnames(freqTable) <- append( seq(1, Imputation$m, 1), 'real')
    
    rownames(freqTable) <- seq(0, nlevels-1, 1)
    
    
    for (m in seq(1,Imputation$m, 1) ){
      
      tab <- table(as.data.frame(Imputation$imp[var])[,m] )
      
      freqTable[, m] <- tab / sum(tab)
      
      freqTable[, 'real'] <- table(data[var]) / sum( table(data[var]) )
      
    }
    print(var)
    
    print(freqTable)
    
  }
}


# Load mids object

Imputation <- load(file = 'Y:/shared/data/wp-5/clinical_imputation/mids.RData')



densityplot(Imputation)

plot(Imputation)

freqTab(Imputation)
