#===========
#Scenario_Comparisons.R
#===========
#===========
# Description: 
#     This script will use extracted bzone-level data to compare outcomes across equity indices
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

# Load data

base_df <- read.csv("Example Data/VDOT_HH_2019_Base_Scenario.csv")
future_df <- read.csv("Example Data/VDOT_HH_2045_Base_Scenario.csv")
future_telework_df <- read.csv("Example Data/VDOT_HH_2045_Telework_Scenario.csv")


base_df_bzone <- base_df %>% 
  group_by(Bzone) %>% 
  summarise(DailyGGE = sum(DailyGGE),
            DailyCO2e = sum(DailyCO2e),
            DVMT = sum(Dvmt), 
            WalkTrips = sum(WalkTrips),
            BikeTrips = sum(BikeTrips),
            TransitTrips = sum(TransitTrips), 
            VehicleTrips = sum(VehicleTrips), 
            Population = sum(HhSize)
  )


future_df_bzone <- future_df %>% 
  group_by(Bzone) %>% 
  summarise(DailyGGE = sum(DailyGGE),
            DailyCO2e = sum(DailyCO2e),
            DVMT = sum(Dvmt), 
            WalkTrips = sum(WalkTrips),
            BikeTrips = sum(BikeTrips),
            TransitTrips = sum(TransitTrips), 
            VehicleTrips = sum(VehicleTrips), 
            Population = sum(HhSize)
  )

future_telework_df_bzone <- future_telework_df %>% 
  group_by(Bzone) %>% 
  summarise(DailyGGE = sum(DailyGGE),
            DailyCO2e = sum(DailyCO2e),
            DVMT = sum(Dvmt), 
            WalkTrips = sum(WalkTrips),
            BikeTrips = sum(BikeTrips),
            TransitTrips = sum(TransitTrips), 
            VehicleTrips = sum(VehicleTrips), 
            Population = sum(HhSize)
  )








