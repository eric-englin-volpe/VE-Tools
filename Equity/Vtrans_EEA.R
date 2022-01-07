#===========
#Vtrans_EEA.R
#===========
#===========
# Description: 
#     This script will use downloaded census data to create Vtrans equity emphasis areas (EEAs)
#     Vtrans methodology: https://icfbiometrics.blob.core.windows.net/vtrans/assets/docs/2020_VTrans_Mid-term_Needs_DRAFT_Technical_Guide.pdf
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


# Vtrans requirements --------------

# Source: https://icfbiometrics.blob.core.windows.net/vtrans/assets/docs/2020_VTrans_Mid-term_Needs_DRAFT_Technical_Guide.pdf
#     income: B19001
#     age: B01001
#     race: B02001
#     ethnicity (Hispanic or Latino): B03001
#     English proficiency: B06007
#     disability: B18101_001E
#     total population: B00001


vtrans_census_table_tract <- full_census_table_TAZ %>% 
  mutate(
    hhs_below150poverty = B06012_002 +B06012_003,
    hhs_75older = B01001_023+B01001_024+B01001_025+B01001_047+B01001_048+B01001_049,
    hhs_NonEnglish = B06007_005+B06007_008,
    hhs_disability = B18101_004+B18101_007+B18101_010+B18101_013+B18101_016+B18101_019+B18101_023+B18101_026+B18101_029+B18101_032+B18101_035+B18101_038,
    hhs_white = B02001_002,
    hhs_hispanic = B03001_003,
    hhs_total = B01001_001
  ) %>%
  select("Bzone",hhs_total,hhs_hispanic,hhs_white,hhs_disability,hhs_NonEnglish,hhs_75older,hhs_below150poverty)  %>%
  mutate(
    low_income_percent = hhs_below150poverty/hhs_total,
    racial_minority_percent = (hhs_total-hhs_white)/hhs_total,
    hispanic_percent = hhs_hispanic/hhs_total,
    age75older_percent = hhs_75older/hhs_total,
    nonEnglish_percent = hhs_NonEnglish/hhs_total,
    disability_percent = hhs_disability/hhs_total
  ) %>% 
  mutate(
    low_income_index = low_income_percent/mean(low_income_percent),
    racial_minority_index = racial_minority_percent/mean(racial_minority_percent),
    hispanic_index = hispanic_percent/mean(hispanic_percent),
    age75older_index = age75older_percent/mean(age75older_percent),
    nonEnglish_index = nonEnglish_percent/mean(nonEnglish_percent),
    disability_index = disability_percent/mean(disability_percent)
  )



vtrans_final_table <- vtrans_census_table_tract %>% select(Bzone,hispanic_index,low_income_index,racial_minority_index,age75older_index,nonEnglish_index,disability_index)
vtrans_final_table$low_income_index <- ifelse(vtrans_final_table$low_income_index>3,3,vtrans_final_table$low_income_index)
vtrans_final_table$low_income_index <- ifelse(vtrans_final_table$low_income_index<1,0,vtrans_final_table$low_income_index)
vtrans_final_table$hispanic_index <- ifelse(vtrans_final_table$hispanic_index>3,3,vtrans_final_table$hispanic_index)
vtrans_final_table$hispanic_index <- ifelse(vtrans_final_table$hispanic_index<1.5,0,vtrans_final_table$hispanic_index)
vtrans_final_table$racial_minority_index <- ifelse(vtrans_final_table$racial_minority_index>3,3,vtrans_final_table$racial_minority_index)
vtrans_final_table$racial_minority_index <- ifelse(vtrans_final_table$racial_minority_index<1.5,0,vtrans_final_table$racial_minority_index)
vtrans_final_table$age75older_index <- ifelse(vtrans_final_table$age75older_index>3,3,vtrans_final_table$age75older_index)
vtrans_final_table$age75older_index <- ifelse(vtrans_final_table$age75older_index<1.5,0,vtrans_final_table$age75older_index)
vtrans_final_table$nonEnglish_index <- ifelse(vtrans_final_table$nonEnglish_index>3,3,vtrans_final_table$nonEnglish_index)
vtrans_final_table$nonEnglish_index <- ifelse(vtrans_final_table$nonEnglish_index<1.5,0,vtrans_final_table$nonEnglish_index)
vtrans_final_table$disability_index <- ifelse(vtrans_final_table$disability_index>3,3,vtrans_final_table$disability_index)
vtrans_final_table$disability_index <- ifelse(vtrans_final_table$disability_index<1.5,0,vtrans_final_table$disability_index)

vtrans_final_table <- vtrans_final_table %>% 
  mutate(index = low_income_index+hispanic_index+racial_minority_index+age75older_index+nonEnglish_index)

vtrans_final_table$EEA <- ifelse(vtrans_final_table$index>2,
                                 ifelse(vtrans_final_table$low_income_index >1 | vtrans_final_table$disability_index>1,1,0),0)







##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############


bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
vtrans_final_table_geo <- st_set_geometry(vtrans_final_table, bzone_geometry_reordered$geometry) 

plot(vtrans_final_table_geo['EEA'],
     main = 'VTrans Bzone - Equity Emphasis Areas')


write.csv(vtrans_final_table, file.path(working_dir, 'VTrans_Equity_Bzones.csv'), row.names = F)

