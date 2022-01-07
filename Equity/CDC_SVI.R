#===========
#CDC_SVI.R
#===========
#===========
# Description: 
#     This script will use downloaded CDC data on Social Vulnerability Index
#     CDC SVI download: https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html
#     Download by State, csv format, for latest year

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


# CDC SVI requirements --------------

cdc_svi_table_virginia <- read_csv(file.path(working_dir, 'Virginia.csv'))
cdc_svi_table_virginia['GEOID']<-cdc_svi_table_virginia['FIPS']
cdc_svi_table_virginia <- as.data.frame(cdc_svi_table_virginia)

cdc_svi_tract <- merge(x = full_census_table, y = cdc_svi_table_virginia, by = "GEOID", all.x = TRUE)
cdc_svi_tract <- cdc_svi_tract %>% select('GEOID','RPL_THEME1','RPL_THEME2','RPL_THEME3','RPL_THEME4','RPL_THEMES')



#############################################

#Add geometry to our vtrans table
cdc_svi_tract_geo <- tract_table_geo %>% merge(cdc_svi_tract, by= "GEOID") 


# Clean tract and bzone geometries --------------
bzone_geometry_sp <- as(bzone_geometry, Class = "Spatial")  #make TAZ df into sp 
cdc_svi_tract_geo_sp = as_Spatial(cdc_svi_tract_geo)

#change all geometries to USGS project for continuity
proj.USGS <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
bzone_geometry_sp_newproj <- spTransform(bzone_geometry_sp, CRS = proj.USGS)
cdc_svi_tract_geo_sp_newproj <- spTransform(cdc_svi_tract_geo_sp, CRS = proj.USGS)


# Find intersection between bzone and Census Tract polygons --------------

# create and clean up intersect object
gI <- gIntersection(bzone_geometry_sp_newproj, cdc_svi_tract_geo_sp_newproj, byid=TRUE, drop_lower_td=TRUE) # gIntersection
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

#find the total area of every census tract
df <- df %>%   group_by(id_bzone)%>%
  summarise(shape_area_bzone = sum(area)) %>%
  right_join(df, by = "id_bzone") 


cdc_svi_tract$id_tract <- seq.int(nrow(cdc_svi_tract)) #make column so we can join census tract df with intersection df
df2<- merge(df, cdc_svi_tract, by = "id_tract", by.y = "id_tract", all.x=TRUE) %>% 
  mutate(share.area = area/shape_area_bzone,
         RPL_THEME1_this_area = RPL_THEME1 * share.area,
         RPL_THEME2_this_area = RPL_THEME2 * share.area, # multiply to get value in each intersected polygon
         RPL_THEME3_this_area = RPL_THEME3 * share.area,
         RPL_THEME4_this_area = RPL_THEME4 * share.area,
         RPL_THEMES_this_area = RPL_THEMES * share.area) #calculate % of tract in each bzone

write.csv(df2,file.path(proj_dir, "df2.csv"))

# Finalize dataframe -------------------------
cdc_svi_TAZ <- df2 %>%  
  group_by(Bzone)%>%
  summarise(n = n(),
            Bzone_area = sum(area),
            RPL_THEME1 = sum(RPL_THEME1_this_area),
            RPL_THEME2 = sum(RPL_THEME2_this_area), 
            RPL_THEME3 = sum(RPL_THEME3_this_area),
            RPL_THEME4 = sum(RPL_THEME4_this_area),
            RPL_THEMES = sum(RPL_THEMES_this_area)
  ) 

cdc_svi_TAZ['RPL_THEMES_clean'] <- ifelse(cdc_svi_TAZ$RPL_THEMES>0, cdc_svi_TAZ$RPL_THEMES, 0 )

##################################################################################
#### add-on script to save shapefiles, make plots for final output ###############


bzone_geometry_reordered <- bzone_geometry[order(bzone_geometry$Bzone),]
cdc_svi_TAZ_geo <-st_set_geometry(cdc_svi_TAZ, bzone_geometry_reordered$geometry) 

plot(cdc_svi_TAZ_geo['RPL_THEMES_clean'],
     main = 'CDC SVI Bzone - RPL Themes')

write.csv(cdc_svi_TAZ, file.path(working_dir, 'SVI_Equity_Bzones.csv'), row.names = F)
