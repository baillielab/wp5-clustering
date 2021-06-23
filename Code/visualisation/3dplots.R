###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code creates 3d plots of pseuduotime, sfr/94 and serology/cytokine measure

###################################################################### 

library(dplyr)
library(ggplot2)
library(data.table)

########################### READ DATA ################################


ccp <- fread('Y:/shared/data/wp-4/clinical-data/008_crf_20210616/ccp_data.csv', data.table=FALSE)

pseudotime <- fread('Y:/s1611423/pseudotime-Kristian-21-06-21.csv', data.table=FALSE)


