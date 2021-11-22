#===========
#MWCOG_EEA.R
#===========
#===========
# Description: 
#     This script will use downloaded census data to create MWCOG equity emphasis areas (EEAs)
#     MWCOG methodology: https://www.mwcog.org/assets/1/6/methodology.pdf
#     MWCOG map of output: https://gis.mwcog.org/webmaps/tpb/clrp/ej/
#===========

source("Data Prep/config.R")
source("Equity/download_Census_data.R")


# Install/Load libraries --------------
source("Data Prep/get_packages.R")

library(dplyr)
library(tidyverse)
library(tidycensus)
library(viridis)
library(leaflet)
library(readr)
library(sp)
library(sf)
library(rgeos)
library(rlist)


# MWCOG requirements --------------
# Source: https://www.mwcog.org/assets/1/6/methodology.pdf
#     income/poverty: B06012 (hhs_below150poverty = B06012_002 +B06012_003)
#     race: B02001 - need to break this out to Asian and African American
#     African American: B02001_003
#     Asian: B02001_005
#     ethnicity (Hispanic or Latino): B03001
#     total population: B00001


mwcog_census_table_tract <- full_census_table_TAZ %>% 
  mutate(
    hhs_below150poverty = B06012_002 +B06012_003,
    hhs_white = B02001_002,
    hhs_africanamerican = B02001_003,
    hhs_asian = B02001_005,
    hhs_hispanic = B03001_003,
    hhs_total = B01001_001
  ) %>%
  select("Bzone",hhs_total,hhs_hispanic,hhs_white,hhs_asian,hhs_africanamerican,hhs_below150poverty)





##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############
# under construction

bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
#vtrans_final_table_geo <-st_set_geometry(vtrans_final_table, bzone_geometry_reordered$geometry) 

#plot(vtrans_final_table_geo['EEA'],
#     main = 'Bzone - Equity Emphasis Areas')
