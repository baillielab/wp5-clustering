library(ggplot2)

library(reshape2)

library(dplyr)

library(data.table)

# This takes care of changing path names depending on whether working on ultra or not.
# If working on ultra, set ultra==True

ultra <- FALSE

if (ultra == TRUE){
  
  root <- '/home/u034/'
} else {
  
  root <- 'Y:/'
}


load(file = paste( root,'stevenkerr/Git/wp5-clustering/Code/clinical/clustering2.RData', sep='') )


catVars <- c('sex', 'ethnicity', 'infect_cmtrt', 'chrincard', 'chronicpul_mhyn', 'asthma_mhyn', 'renal_mhyn', 'modliv',           
             'diabetescom_mhyn', 'diabetes_mhyn', 'dementia_mhyn', 'malignantneo_mhyn', 'obesity_mhyn' )

catData <-as.data.frame(mixedClustering@lcomponent[[1]]@data)

contData <- as.data.frame(mixedClustering@lcomponent[[2]]@data)

contData$V1 <- NULL

# Get the complete dataset. Missing values are imputed as part of the clustering.

completeData <- bind_cols(  catData, contData )

contVars <- setdiff(colnames(completeData), catVars)

# probs is a matrix of predicted probabilities of cluster membership

probs <- mixedClustering@tik

label <- mixedClustering@zi

# Get the means of the clusters

gaussianMeans <- as.data.frame( t(mixedClustering@lcomponent[[2]]@mean) )[-1 ,]

gaussianMeans$name <- contVars

catMeans <-  mixedClustering@lcomponent[[1]]@plkj 






# Plot the cluster means for the continuous variables

meltedMeans <- melt(as.data.table(gaussianMeans), id.vars = 'name' )

ggplot(meltedMeans, aes(x=name, y=value, color=variable)) + geom_line(aes(group=variable)) + coord_flip()


# Plot the cluster means for the binary variables

# ethPosition is the index of catVars that corresponds to ethnicity.
# Need to do this because MixAll doesn't keep column names

ethPosition = match('ethnicity', catVars)


binMeans <- as.data.frame( t(catMeans[1, , -ethPosition]) )

binMeans$name <- catVars[-ethPosition]


meltedCatMeans <- melt(as.data.table(binMeans), id.vars = 'name' )

ggplot(meltedCatMeans, aes(x=name, y=value, color=variable)) + geom_line(aes(group=variable)) + coord_flip()






# Plot the cluster means for ethnicity

ethMeans <- as.data.frame(catMeans[ , ,ethPosition])

ethMeans$name <- rownames(ethMeans)


meltedEthMeans <- melt(as.data.table(ethMeans), id.vars = 'name' )

ggplot(meltedEthMeans, aes(x=name, y=value, color=variable)) + geom_line(aes(group=variable)) + coord_flip()








# plot cluster means for everything except ethnicity together


meltedAllMeans <- rbind(meltedMeans, meltedCatMeans)

ggplot(meltedAllMeans, aes(x=name, y=value, color=variable)) + geom_line(aes(group=variable)) + coord_flip()

