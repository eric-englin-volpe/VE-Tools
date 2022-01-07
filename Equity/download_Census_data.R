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

# Load Census API Info --------------
# add in census api key
fileName <- 'census_api.txt'

# check if census_api.txt is in the working directory
if(!file.exists(file.path(working_dir, fileName))){ 
  stop(paste('Census API Key needed in as a plain text file in /n', file.path(working_dir), '\n go to https://api.census.gov/data/key_signup.html \n and save the API key as `census_api.txt`'))
}

mystring <- read_file(file.path(working_dir, fileName))
api_key <- gsub('/r/n', '', mystring) #clean up in case text file has breaks

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
try(census_api_key(api_key, install = TRUE))
try(readRenviron("~/.Renviron")) # will check R environment for past api keys


# Pull Major Census Data --------------

# Other relevant census data:
#     Poverty status: B17001, B06012
#     Gini index: B19083_001E


pull_census_data <- function(state, counties, table, var_list){
  census_table_pull <- get_acs(geography = "tract", table = table,
                               state = state, county = counties, geometry = FALSE) 
  counter<-0
  for (var in var_list){
    print(var)
    if (counter == 0){
      counter<-1
      census_table_df <- census_table_pull %>% 
        filter(variable == var) %>% 
        select(GEOID, NAME, estimate)
      
    }
    else {
      census_table_df <- census_table_pull %>% 
        filter(variable == var)%>% 
        select(GEOID, NAME, estimate) %>% 
        merge(census_table_df, by = c("GEOID","NAME")) 
      this_county_census <- census_table_pull %>% 
        filter(variable == var)
      print(this_county_census$estimate[1])
    }
  }
  colnames(census_table_df) <- c("GEOID","NAME", rev(var_list))
  return(census_table_df)
}


var_list <- c("B06012_001", "B06012_002", "B06012_003", "B06012_004")
poverty_census <- pull_census_data(state, counties, "B06012", var_list)


var_list <- c("B01001_001","B01001_023", "B01001_024", "B01001_025", 
              "B01001_047", "B01001_048", "B01001_049")
older_age_census <- pull_census_data(state, counties, "B01001", var_list)

var_list <- c("B02001_002",'B02001_003','B02001_004','B02001_005','B02001_006','B02001_007', 'B02001_008')
race_census <- pull_census_data(state, counties, "B02001", var_list)


var_list <- c("B03001_003")
hispanic_census <- pull_census_data(state, counties, "B03001", var_list)


var_list <- c("B06007_001","B06007_005","B06007_008")
english_speaking_census <- pull_census_data(state, counties, "B06007", var_list)


var_list <- c("B18101_001","B18101_004","B18101_007",
              "B18101_010","B18101_013","B18101_016",
              "B18101_019","B18101_023","B18101_026",
              "B18101_029","B18101_032","B18101_035","B18101_038")
disability_census <- pull_census_data(state, counties, "B18101", var_list)


full_census_table <- poverty_census %>% 
  merge(older_age_census, by = c("GEOID","NAME")) %>%
  merge(race_census, by = c("GEOID","NAME")) %>%
  merge(hispanic_census, by = c("GEOID","NAME")) %>%
  merge(english_speaking_census, by = c("GEOID","NAME")) %>%
  merge(disability_census, by = c("GEOID","NAME")) 

#Pull geometry
tract_table_geo <- get_acs(geography = "tract", table = "B19001",
                           state = state, county = counties, geometry = TRUE) %>% 
  filter(variable == "B19001_001") %>%
  select(GEOID)

#Add geometry to our vtrans table
full_census_table_tract_geo <- tract_table_geo %>% merge(full_census_table, by= "GEOID") 


#############################################

# Clean tract and bzone geometries --------------
bzone_geometry_sp <- as(bzone_geometry, Class = "Spatial")  #make TAZ df into sp 
full_census_table_tract_geo_sp = as_Spatial(full_census_table_tract_geo)

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
bzone_geometry_sp_newproj <- spTransform(bzone_geometry_sp, CRS = proj.USGS)
full_census_table_tract_geo_sp_newproj <- spTransform(full_census_table_tract_geo_sp, CRS = proj.USGS)


# Find intersection between bzone and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(bzone_geometry_sp_newproj, full_census_table_tract_geo_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
n<-names(gI) #grab the ids/names from the gIntersection object
n<-data.frame(t(data.frame(strsplit(n," ",fixed=TRUE)))) #ids are combined so split into separate cells
colnames(n)[1:2]<-c("id_bzone","id_tract") #add id names to differentiate


#find the overlapping area for all the bzone-Tract objects
n$area<-sapply(gI@polygons, function(x) x@area) 
a<-data.frame(id=row.names(bzone_geometry_sp_newproj), Bzone = bzone_geometry_sp_newproj$Bzone)#subset bzone dataset so only joining bzone ids
df<-merge(n,a,by.x = "id_bzone", by.y = "id", all.x=TRUE) #merge the bzone ids into our dataset


#find the total area of every census tract
df <- df %>%   group_by(id_tract)%>%
  summarise(shape_area = sum(area))%>%
  right_join(df, by = "id_tract") 


full_census_table$id_tract <- seq.int(nrow(full_census_table)) #make column so we can join census tract df with intersection df
df2<- merge(df, full_census_table, by = "id_tract", by.y = "id_tract", all.x=TRUE)

# Finalize dataframe -------------------------
full_census_table_TAZ <- df2 %>% mutate(share.area = area/shape_area, #calculate % of tract in each bzone
                      B06012_004_this_area = B06012_004 * share.area,
                      B06012_003_this_area = B06012_003 * share.area, # multiply to get value in each intersected polygon
                      B06012_002_this_area = B06012_002 * share.area,
                      B06012_001_this_area = B06012_001 * share.area,
                      B01001_049_this_area = B01001_049 * share.area,
                      B01001_048_this_area = B01001_048 * share.area,
                      B01001_047_this_area = B01001_047 * share.area,
                      B01001_025_this_area = B01001_025 * share.area,
                      B01001_024_this_area = B01001_024 * share.area,
                      B01001_023_this_area = B01001_023 * share.area,
                      B01001_001_this_area = B01001_001 * share.area,
                      B02001_008_this_area = B02001_008 * share.area,
                      B02001_007_this_area = B02001_007 * share.area,
                      B02001_006_this_area = B02001_006 * share.area,
                      B02001_005_this_area = B02001_005 * share.area,
                      B02001_004_this_area = B02001_004 * share.area,
                      B02001_003_this_area = B02001_003 * share.area,
                      B02001_002_this_area = B02001_002 * share.area,
                      B03001_003_this_area = B03001_003 * share.area,
                      B06007_008_this_area = B06007_008 * share.area,
                      B06007_005_this_area = B06007_005 * share.area,
                      B06007_001_this_area = B06007_001 * share.area,
                      B18101_038_this_area = B18101_038 * share.area,
                      B18101_035_this_area = B18101_035 * share.area,
                      B18101_032_this_area = B18101_032 * share.area,
                      B18101_029_this_area = B18101_029 * share.area,
                      B18101_026_this_area = B18101_026 * share.area,
                      B18101_023_this_area = B18101_023 * share.area,
                      B18101_019_this_area = B18101_019 * share.area,
                      B18101_016_this_area = B18101_016 * share.area,
                      B18101_013_this_area = B18101_013 * share.area,
                      B18101_010_this_area = B18101_010 * share.area,
                      B18101_007_this_area = B18101_007 * share.area,
                      B18101_004_this_area = B18101_004 * share.area,
                      B18101_001_this_area = B18101_001 * share.area
                      ) %>% 
  group_by(Bzone)%>%
  summarise(n = n(),
            B06012_004 = sum(B06012_004_this_area),
            B06012_003 = sum(B06012_003_this_area), 
            B06012_002 = sum(B06012_002_this_area),
            B06012_001 = sum(B06012_001_this_area),
            B01001_049 = sum(B01001_049_this_area),
            B01001_048 = sum(B01001_048_this_area),
            B01001_047 = sum(B01001_047_this_area),
            B01001_025 = sum(B01001_025_this_area), 
            B01001_024 = sum(B01001_024_this_area),
            B01001_023 = sum(B01001_023_this_area),
            B01001_001 = sum(B01001_001_this_area),
            B02001_008 = sum(B02001_008_this_area),
            B02001_007 = sum(B02001_007_this_area),
            B02001_006 = sum(B02001_006_this_area), 
            B02001_005 = sum(B02001_005_this_area),
            B02001_004 = sum(B02001_004_this_area),
            B02001_003 = sum(B02001_003_this_area),
            B02001_002 = sum(B02001_002_this_area),
            B03001_003 = sum(B03001_003_this_area),
            B06007_008 = sum(B06007_008_this_area), 
            B06007_005 = sum(B06007_005_this_area),
            B06007_001 = sum(B06007_001_this_area),
            B18101_038 = sum(B18101_038_this_area),
            B18101_035 = sum(B18101_035_this_area),
            B18101_032 = sum(B18101_032_this_area),
            B18101_029 = sum(B18101_029_this_area), 
            B18101_026 = sum(B18101_026_this_area),
            B18101_023 = sum(B18101_023_this_area),
            B18101_019 = sum(B18101_019_this_area),
            B18101_016 = sum(B18101_016_this_area),
            B18101_013 = sum(B18101_013_this_area),
            B18101_010 = sum(B18101_010_this_area), 
            B18101_007 = sum(B18101_007_this_area),
            B18101_004 = sum(B18101_004_this_area),
            B18101_001 = sum(B18101_001_this_area)
          ) 
  

