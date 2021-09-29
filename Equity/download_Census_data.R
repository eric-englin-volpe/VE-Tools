#===========
#download_Census_data.R
#===========
#===========
# Description: 
#     This script will download all relevant census data
#     API data tables found at this location: https://api.census.gov/data/2016/acs/acs1/variables.html
#     Note: we could find more updated census information -- would likely be helpful to match with model base year.  
#===========

source("Data Prep/config.R")


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

# Load Census Data --------------
# add in census api key
fileName <- 'census_api.txt'

# check if census_api.txt is in the working directory
if(!file.exists(file.path(working_dir,fileName))){ 
  stop(paste('Census API Key needed in as a plain text file in /n', file.path(working_dir), '/n go to https://api.census.gov/data/key_signup.html /n and save the API key as `census_api.txt`'))
}

mystring <- read_file(file.path(working_dir, fileName))
api_key <- gsub('/r/n', '', mystring) #clean up in case text file has breaks

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron") # will check R environment for past api keys

# Vtrans requirements --------------

# Source: https://icfbiometrics.blob.core.windows.net/vtrans/assets/docs/2020_VTrans_Mid-term_Needs_DRAFT_Technical_Guide.pdf
#     income: B19001
#     age: B01001
#     race: B02001
#     ethnicity (Hispanic or Latino): B03001
#     English proficiency: B06007
#     disability: B18101_001E
#     total population: B00001


# Other relevant census data:
#     Poverty status: B17001
#     Gini index: B19083_001E


# Download Census table
hh_income_county <- get_acs(geography = "county", table = "B19001",
                            state = state, geometry = FALSE) %>% filter(GEOID %in% counties_geoids)





# Vtrans method:
#       -Find # residents whose income below 150% of poverty level
#       -Find # residents 75 or older
#       -Find # residents who belong to racial minority (non-white)
#       -Find # residents who are Hispanic/Latino
#       -Find # residents who do not speak English "very well"

