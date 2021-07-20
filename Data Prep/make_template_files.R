

source("Data Prep/config.R")

# Make file templates

years = c(2019, 2045)


azone_template = expand.grid(unique(geo$Azone), years)
colnames(azone_template) = c('Geo', 'Year')

write.csv(azone_template, 
          file = file.path(working_dir, 'azone_template.csv'),
          row.names = F)

bzone_template = expand.grid(unique(geo$Bzone), years)
colnames(bzone_template) = c('Geo', 'Year')

write.csv(bzone_template, 
          file = file.path(working_dir, 'bzone_template.csv'),
          row.names = F)

