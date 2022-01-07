#===========
#PBOT_EquityMatrix.R
#===========
#===========
# Description: 
#     This script will use downloaded census data to create the PBOT Equity Matrix (equity scores 2-10)
#     PBOT methodology: https://www.portland.gov/transportation/justice/pbot-equity-matrix
#===========

if(!exists('full_census_table_TAZ')){
  source("Data Prep/config.R")
  source("Equity/download_Census_data.R")
}


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

library(BAMMtools)

# Vtrans requirements --------------

# Source: https://www.portland.gov/transportation/justice/pbot-equity-matrix
#     income/poverty: B06012
#     race: B02001
#     ethnicity (Hispanic or Latino): B03001
#     total population: B00001


PBOT_overall <- full_census_table_TAZ %>% 
  mutate(
    hhs_below150poverty = B06012_002 +B06012_003,
    hhs_nonwhite = B01001_001-B02001_002+B03001_003, # this method gives you non-white percent over 100 percent (double counting likely happening between race and ethnicity)
    hhs_nonwhite2 = B01001_001-B02001_002,
    hhs_hispanic = B03001_003,
    hhs_total = B01001_001,
    percent_below150poverty = hhs_below150poverty/hhs_total,
    percent_nonwhite2 = hhs_nonwhite2/hhs_total,
    percent_nonwhite = hhs_nonwhite/hhs_total,
    percent_hispanic = hhs_hispanic/hhs_total
  ) %>%
  select(Bzone,hhs_total,hhs_hispanic,hhs_nonwhite2, hhs_nonwhite,hhs_below150poverty,
         percent_below150poverty, percent_hispanic,percent_nonwhite,percent_nonwhite2)


# Create 5 natural break jenks for each of these variables (income, race)
income_jenks <- getJenksBreaks(PBOT_overall$percent_below150poverty, 6, subset = NULL)
race_jenks <- getJenksBreaks(PBOT_overall$percent_nonwhite2, 6, subset = NULL)

PBOT_overall$income_index <- ifelse(PBOT_overall$percent_below150poverty>income_jenks[5],5,
                                    ifelse(PBOT_overall$percent_below150poverty>income_jenks[4],4,
                                    ifelse(PBOT_overall$percent_below150poverty>income_jenks[3],3,
                                    ifelse(PBOT_overall$percent_below150poverty>income_jenks[2],2,1))))

PBOT_overall$race_index <- ifelse(PBOT_overall$percent_nonwhite2>race_jenks[5],5,
                                    ifelse(PBOT_overall$percent_nonwhite2>race_jenks[4],4,
                                           ifelse(PBOT_overall$percent_nonwhite2>race_jenks[3],3,
                                                  ifelse(PBOT_overall$percent_nonwhite2>race_jenks[2],2,1))))

PBOT_overall$PBOT_index <- PBOT_overall$income_index + PBOT_overall$race_index


##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############

bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
PBOT_overall_geo <- st_set_geometry(PBOT_overall, bzone_geometry_reordered$geometry) 

plot(PBOT_overall_geo['PBOT_index'],
     main = 'PBOT Bzone - PBOT Index')

write.csv(PBOT_overall, file.path(working_dir, 'PBOT_Equity_Bzones.csv'), row.names = F)

