# Get all necessary packages across data prep and analysis scripts 
# add any packages your scripts require here. Keep in alphabetical order.

loadpacks <- c(
  'BAMMtools',
  'dplyr', 
  'ggplot2',
  'leaflet', 
  'readr',
  'rgeos',
  'rlist',
  'sf', 
  'sp',
  'tidyverse', 
  'tidycensus', 
  'viridis'
  )

for(i in loadpacks){
  if(!i %in% (.packages(all.available=T))) install.packages(i, dependencies =TRUE)
  }
rm(i, loadpacks)
