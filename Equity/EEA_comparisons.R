#===========
#EEA_comparison.R
#===========
#===========
# Description: 
#     This script will compare Vtrans and MWCOG equity emphasis areas (EEAs)
#     Vtrans methodology: https://icfbiometrics.blob.core.windows.net/vtrans/assets/docs/2020_VTrans_Mid-term_Needs_DRAFT_Technical_Guide.pdf
#     MWCOG methodology: https://www.mwcog.org/assets/1/6/methodology.pdf

#===========


if(!exists('full_census_table_TAZ')){
  source("Data Prep/config.R")
  source("Equity/download_Census_data.R")
}

# Install/Load libraries --------------
source("Data Prep/get_packages.R")
source("Equity/Vtrans_EEA.R")
source("Equity/MWCOG_EEA.R")

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
library(ggplot2)
library(RColorBrewer)

# Join Datasets --------------


comp_table <- vtrans_final_table %>%
  mutate(Vtrans_EEA = EEA * 10) %>% 
  select(Bzone, Vtrans_EEA) %>%
  merge(mwcog_final_table) %>%
  mutate(MWCOG_EEA = EEA) %>%
  select(Bzone, Vtrans_EEA, MWCOG_EEA) %>%
  mutate(EEA_comp = MWCOG_EEA + Vtrans_EEA) %>%
  mutate(EEA_Comparison = case_when(
    EEA_comp == 0 ~ "Non-EEA",
    EEA_comp == 1 ~ "Vtrans EEA Only",
    EEA_comp == 10 ~ "MWCOG EEA only", 
    EEA_comp == 11 ~ "EEA in both", 
    TRUE ~ "N/A"
  ))







##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############


bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
comp_table_geo <- st_set_geometry(comp_table, bzone_geometry_reordered$geometry) 

p <- ggplot()+ ggtitle("Equity Emphasis Areas Comparison") + 
  geom_sf(data = comp_table_geo, mapping = aes(fill = EEA_Comparison), show.legend = TRUE) + 
  coord_sf() 

p +  scale_color_brewer(palette = "Spectral")
