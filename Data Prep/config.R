library(foreign)
library(tidyverse)
library(sf)

proj_dir <- 'C:/Users/eric.englin/Desktop/VisionEval/VDOT/'
working_dir <- file.path(proj_dir, 'working/') #may be needed

years <- c(2019, 2045)
Marea <- "NVTA"
bzone_geometry <- st_read(file.path(working_dir, "FFXsubzone/FFX_Subzone.shp")) #load TAZ dataset
bzone_names <- bzone_geometry %>% st_set_geometry(NULL) %>% select(TAZ_N)#remove geometry field, select name column
azone_names <- c("Fairfax", "Fairfax City", "Falls Church")
czone<-NA

geo = bzone_geometry %>%
  st_set_geometry(NULL) %>%
  select(NAME, TAZ_N) %>%
  rename(Azone = NAME,
         Bzone = TAZ_N) %>%
  mutate(Czone = NA,
         Marea = "NVTA")
