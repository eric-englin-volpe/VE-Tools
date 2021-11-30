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
  select("Bzone",hhs_total,hhs_hispanic,hhs_white,hhs_asian,hhs_africanamerican,hhs_below150poverty) %>%
  mutate(
  percent_below150poverty = hhs_below150poverty/hhs_total,
  percent_africanamerican = hhs_africanamerican/hhs_total,
  percent_asian = hhs_asian/hhs_total,
  percent_hispanic = hhs_hispanic/hhs_total
) %>% 
  mutate(
    low_income_ratio = percent_below150poverty/mean(percent_below150poverty),
    africanamerican_ratio = percent_africanamerican/mean(percent_africanamerican),
    hispanic_ratio = percent_hispanic/mean(percent_hispanic),
    asian_ratio = percent_asian/mean(percent_asian)
  )


mwcog_final_table <- mwcog_census_table_tract %>% select(Bzone,hispanic_ratio,low_income_ratio,africanamerican_ratio,asian_ratio)


mwcog_final_table$low_income_index <- ifelse(mwcog_final_table$low_income_ratio>3,9,
                                             ifelse(mwcog_final_table$low_income_ratio>1.5, mwcog_final_table$low_income_ratio*3,
                                                    ifelse(mwcog_final_table$low_income_ratio>1,mwcog_final_table$low_income_ratio,0)))

mwcog_final_table$hispanic_index <- ifelse(mwcog_final_table$hispanic_ratio>3,3,
                                           ifelse(mwcog_final_table$hispanic_ratio>1.5,mwcog_final_table$hispanic_ratio,0))
mwcog_final_table$africanamerican_index <- ifelse(mwcog_final_table$africanamerican_ratio>3,3,
                                                  ifelse(mwcog_final_table$africanamerican_ratio>1.5,mwcog_final_table$africanamerican_ratio,0))
mwcog_final_table$asian_index <- ifelse(mwcog_final_table$asian_ratio>3,3,
                                        ifelse(mwcog_final_table$asian_ratio>1.5,mwcog_final_table$asian_ratio,0))


                                        
mwcog_final_table$total_index <- mwcog_final_table$low_income_index+mwcog_final_table$asian_index+mwcog_final_table$hispanic_index+mwcog_final_table$africanamerican_index
                                                                        
mwcog_final_table$EEA <- ifelse(mwcog_final_table$total_index>4, 1,0)


##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############
# under construction

bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
mwcog_final_tablegeo <-st_set_geometry(mwcog_final_table, bzone_geometry_reordered$geometry) 

plot(mwcog_final_tablegeo['total_index'],
     main = 'Bzone - MWCOG Equity Index')

plot(mwcog_final_tablegeo['EEA'],
     main = 'Bzone - Equity Emphasis Area')

