###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code creates 3d plots with pseuduotime, sfr/94 and serology/cytokine measure on the axes

###################################################################### 

library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
library(stringr)

########################### LOAD DATA ################################

timestamp <- '2021-06-22_1715'

ccp <- readRDS( paste0('~/Data/ccp_subset_derived_', timestamp, '.rds'))

pseudotime <- fread('~/Data/pseudotime-Kristian-21-06-21.csv', select = 2:4, data.table=FALSE)

sero <- fread('~/Data/wp7_serology_serum_cleaned_20210622_185732.csv', data.table=FALSE)

# Day 1 with reaosnably complete data and deduplicated
#cyto <- fread('~/Data/ISARIC_plasma_mediators_final.csv', data.table=FALSE)

# raw cytokine data
cyto <- fread('~/Data/wp6_plasma_cytokines_cleaned_20210705_175519.csv', data.table=FALSE)

antibodies <- fread('~/Data/2021-04-20_serology_neutralisation_titres.csv', data.table=FALSE)

########################## FUNCTIONS ################################

# Tells you the number of rows that have values for all of vars
number_overlap <-function(vars){
  return( select(df, vars) %>% na.omit() %>% nrow() )
}


# This makes and saves a 3d plot with psuedotime, sfr, and value of cytokine assays on the axes.
make_cyto_fig <- function(assays){

  subset <- filter(df, Assay %in% assays & !is.na(pseudotime) & !is.na(sfr) )
  
  # If there are 3 or fewer data points, an error is thrown
  if( nrow(subset) <= 3  ){
    return()
  } else{
  
  fig <- plot_ly(subset, x = ~pseudotime, y = ~sfr, z = ~Value, color = ~DABA_S.CO, colors = c('#BF382A', '#0C4B8E'))
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'Pseudotime'),
                                     yaxis = list(title = 'sfr'),
                                     zaxis = list(title = assays)),
                        yaxis = list(range=c(0,20))
  )
  
  htmlwidgets::saveWidget(as_widget(fig), paste0("./Outputs/3dplots/pseudotime_sfr_", assays, ".html"))
  
  return(fig)
  }
}

# This makes and saves a 3d plot with psuedotime, sfr, and value of cytokine assays on the axes.
make_var_fig <- function(var){
  
  subset <- select(df, as.name(var), pseudotime, sfr) %>% na.omit()
  
  subset <- filter(df, !is.na(var) & !is.na(pseudotime) & !is.na(sfr) )
  
  # If there are 3 or fewer data points, an error is thrown
  if( nrow(subset) <= 3  ){
    return()
  } else{
    
    fig <- plot_ly(subset, x = ~pseudotime, y = ~sfr, z = ~as.name(var))
    fig <- fig %>% add_markers()
    fig <- fig %>% layout(scene = list(xaxis = list(title = 'Pseudotime'),
                                       yaxis = list(title = 'sfr'),
                                       zaxis = list(title = var)),
                          yaxis = list(range=c(0,20))
    )
    
    htmlwidgets::saveWidget(as_widget(fig), paste0("./Outputs/3dplots/pseudotime_sfr_", assays, ".html"))
    
    return(fig)
  }
}

########################## MAKE PLOTS ################################

sero <- mutate(sero, time = case_when( Time_Point %in% c('', 'Convalescent') ~ NA_real_,
                                          TRUE ~ as.numeric( substr(Time_Point, 4, 5))  ))

pseudotime$date_taken <- as.Date(pseudotime$date_taken)

ccp <- mutate_at(ccp, 'days_since_admission', as.numeric)

cyto <- mutate_at(cyto, 'sample_collected', as.Date)

antibodies <- mutate_at(antibodies, 'Date Taken', as.Date)

# Join data
df <- full_join( select(ccp, subjid, sfr, sf94, daily_dsstdat, days_since_admission), 
                 pseudotime, c('subjid' = 'subjid', 'daily_dsstdat' = 'date_taken'))

df <- full_join(df, select(sero, Patient_ID, DABA_S.CO, time), c('subjid' = 'Patient_ID', 'days_since_admission' = 'time') )

df <- full_join(df, select(cyto, `Patient ID`, sample_collected, Assay, Value ), c('subjid' = 'Patient ID', 'daily_dsstdat' = 'sample_collected'))

df <- full_join(df, select(antibodies, starts_with('Serum'), `Patient ID`, `Date Taken`), by = c('subjid' = 'Patient ID', 'daily_dsstdat' = 'Date Taken'  ))

# List of all assays
cyto_assays <- unique(cyto$Assay)

# Make DABA plot
fig <- plot_ly(df, x = ~pseudotime, y = ~sfr, z = ~DABA_S.CO)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Pseudotime'),
                                     yaxis = list(title = 'sfr'),
                                     zaxis = list(title = 'DABA_S.CO')),
                        yaxis = list(range=c(0,20)) )

htmlwidgets::saveWidget(as_widget(fig), "./Outputs/3dplots/pseudotime_sfr_daba.html")

# Make all cytokine assay plots
for (assay in cyto_assays){
  print(assay)
  make_cyto_fig(assay)
} 


# There is no overlap between patients with antibody data and patients with sfr readings
number_overlap(c('sfr', 'pseudotime', 'Serum IgG IC70'))

