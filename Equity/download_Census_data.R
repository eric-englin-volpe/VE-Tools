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
if(!file.exists(file.path(working_dir,fileName))){ 
  stop(paste('Census API Key needed in as a plain text file in /n', file.path(working_dir), '/n go to https://api.census.gov/data/key_signup.html /n and save the API key as `census_api.txt`'))
}

mystring <- read_file(file.path(working_dir, fileName))
api_key <- gsub('/r/n', '', mystring) #clean up in case text file has breaks

# load census api key (get one here: https://api.census.gov/data/key_signup.html)
census_api_key(api_key, install = TRUE)
readRenviron("~/.Renviron") # will check R environment for past api keys


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


var_list <- c("B01001_023", "B01001_024", "B01001_025", 
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


# Vtrans requirements --------------

# Source: https://icfbiometrics.blob.core.windows.net/vtrans/assets/docs/2020_VTrans_Mid-term_Needs_DRAFT_Technical_Guide.pdf
#     income: B19001
#     age: B01001
#     race: B02001
#     ethnicity (Hispanic or Latino): B03001
#     English proficiency: B06007
#     disability: B18101_001E
#     total population: B00001


vtrans_census_table_tract <- full_census_table %>% 
  mutate(
    hhs_below150poverty = B06012_002 +B06012_003,
    hhs_75older = B01001_023+B01001_024+B01001_025+B01001_047+B01001_048+B01001_049,
    hhs_NonEnglish = B06007_005+B06007_008,
    hhs_disability = B18101_004+B18101_007+B18101_010+B18101_013+B18101_016+B18101_019+B18101_023+B18101_026+B18101_029+B18101_032+B18101_035+B18101_038,
    hhs_white = B02001_002,
    hhs_hispanic = B03001_003,
    hhs_total = B06012_001
    ) %>%
  select("GEOID","NAME",hhs_total,hhs_hispanic,hhs_white,hhs_disability,hhs_NonEnglish,hhs_75older,hhs_below150poverty)
  

tract_table_geo <- get_acs(geography = "tract", table = "B19001",
                         state = state, county = counties, geometry = TRUE) %>% 
  filter(variable == "B19001_001") %>%
  select(GEOID)

vtrans_census_table_tract_geo <- tract_table_geo %>% merge(vtrans_census_table_tract, by= "GEOID") 


#############################################

# Clean tract and bzone geometries --------------
bzone_geometry_sp <- as(bzone_geometry, Class = "Spatial")  #make TAZ df into sp 
vtrans_census_table_tract_geo_sp = as_Spatial(vtrans_census_table_tract_geo)

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
bzone_geometry_sp_newproj <- spTransform(bzone_geometry_sp, CRS = proj.USGS)
vtrans_census_table_tract_geo_sp_newproj <- spTransform(vtrans_census_table_tract_geo_sp, CRS = proj.USGS)


# Find intersection between bzone and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(bzone_geometry_sp_newproj, vtrans_census_table_tract_geo_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
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


vtrans_census_table_tract$id_tract <- seq.int(nrow(vtrans_census_table_tract)) #make column so we can join census tract df with intersection df
df2<- merge(df, vtrans_census_table_tract, by = "id_tract", by.y = "id_tract", all.x=TRUE)

# Finalize dataframe -------------------------
df3 <- df2 %>% mutate(share.area = area/shape_area, #calculate % of tract in each bzone
                      hhs_total_this_area = hhs_total * share.area,
                      hhs_hispanic_this_area = hhs_hispanic * share.area, # multiply to get value in each intersected polygon
                      hhs_white_this_area = hhs_white * share.area,
                      hhs_disability_this_area = hhs_disability * share.area,
                      hhs_75older_this_area = hhs_75older * share.area,
                      hhs_below150poverty_this_area = hhs_below150poverty * share.area,
                      hhs_NonEnglish_this_area = hhs_NonEnglish * share.area) %>% 
  group_by(Bzone)%>%
  summarise(n = n(),
            hhs_total = sum(hhs_total_this_area),
            hhs_hispanic = sum(hhs_hispanic_this_area), 
            hhs_white = sum(hhs_white_this_area),
            hhs_disability = sum(hhs_disability_this_area),
            hhs_75older = sum(hhs_75older_this_area),
            hhs_below150poverty = sum(hhs_below150poverty_this_area),
            hhs_NonEnglish = sum(hhs_NonEnglish_this_area)) %>%
  mutate(Geo = Bzone) 


hhs_disability
hhs_NonEnglish
hhs_75older
hhs_below150poverty

# quality check that total households are same in TAZ and census tract files
identical(sum(df3$tot_hhs), sum(hh_income_raw$total_hh_list)) 











census_table_pull <- get_acs(geography = "tract", table = 'B18101',
                             state = state, county = c('059','600', '610'), geometry = FALSE) 

census_table_pull2 <- get_acs(geography = "block group", table = 'B01001A',
                             state = state, county = c( '610'), geometry = FALSE) 

# Vtrans method:
#       -Find # residents whose income below 150% of poverty level
#       -Find # residents 75 or older
#       -Find # residents who belong to racial minority (non-white)
#       -Find # residents who are Hispanic/Latino
#       -Find # residents who do not speak English "very well"

