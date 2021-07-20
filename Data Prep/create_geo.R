# Create geo.csv for VDOT work

source("Data Prep/config.R")

write.csv(geo,
          file = file.path(proj_dir, 'defs/geo.csv'),
          row.names = F)


