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

########################### READ DATA ################################

ccp <- readRDS('~/Data/ccp_subset_derived.rds')

pseudotime <- fread('~/Data/pseudotime-Kristian-21-06-21.csv', select = 2:4, data.table=FALSE)

sero <- fread('~/Data/wp7_serology_serum_cleaned_20210622_185732.csv', data.table=FALSE)

cyto <- fread('~/Data/ISARIC_plasma_mediators_final.csv', data.table=FALSE)

########################## FUNCTIONS ################################

# This makes and saves a 3d plot with psuedotime, sfr, and value of cytokine assays on the axes.
make_cyto_fig <- function(assays){
  subset <- filter(df, Assay %in% assays)
  
  fig <- plot_ly(subset, x = ~pseudotime, y = ~sfr, z = ~Value, color = ~DABA_OD, colors = c('#BF382A', '#0C4B8E'))
  fig <- fig %>% add_markers()
  fig <- fig %>% layout(scene = list(xaxis = list(title = 'Pseudotime'),
                                     yaxis = list(title = 'sfr'),
                                     zaxis = list(title = assays)),
                        yaxis = list(range=c(0,20))
  )
  
  htmlwidgets::saveWidget(as_widget(fig), paste0("./Outputs/3dplots/pseudotime_sfr_", assays, ".html"))
  
  return(fig)
}

########################## MAKE PLOTS ################################

sero <- mutate(sero, time = case_when( Time_Point %in% c('', 'Convalescent') ~ NA_real_,
                                          TRUE ~ as.numeric( substr(Time_Point, 4, 5))  ))

pseudotime$date_taken <- as.Date(pseudotime$date_taken)

ccp <- mutate_at(ccp, 'days_since_admission', as.numeric)

cyto <- mutate_at(cyto, 'Visit Day', as.numeric)

# Join data
df <- full_join( select(ccp, subjid, sfr, sf94, daily_dsstdat, days_since_admission), 
                 pseudotime, c('subjid' = 'subjid', 'daily_dsstdat' = 'date_taken'))

df <- full_join(df, select(sero, Patient_ID, DABA_OD, time), c('subjid' = 'Patient_ID', 'days_since_admission' = 'time') )

df <- full_join(df, select(cyto, ID, `Visit Day`, Assay, Value ), c('subjid' = 'ID', 'days_since_admission' = 'Visit Day'))

# List of all assays
cyto_assays <- unique(cyto$Assay)

# Make DABA plot
fig <- plot_ly(df, x = ~pseudotime, y = ~sfr, z = ~DABA_OD)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Pseudotime'),
                                     yaxis = list(title = 'sfr'),
                                     zaxis = list(title = 'DABA_OD')),
                        yaxis = list(range=c(0,20)) )

htmlwidgets::saveWidget(as_widget(fig), "./Outputs/3dplots/pseudotime_sfr_daba.html")

# Make all cytokine assay plots
for (assay in cyto_assays){
  make_cyto_fig(assay)
} 

