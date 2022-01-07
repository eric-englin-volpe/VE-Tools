# VERSPM Equity Analysis
library(tidyverse)
# 1. Join metrics with VERSPM households by bzones. Extract with VERSPM_SCenarios_Extract_For_Equity.R and then move the results to the workign dir here.

# 2. Produce initial figures and plots

if(!exists('geo')){
  source("Data Prep/config.R")
}

# Read in equity measures

equity_meas <- dir(working_dir)[grep('_Equity_Bzones.csv', dir(working_dir))]

equity_meas_compile <- vector()

for(e in equity_meas){
  ex <- read.csv(file.path(working_dir, e))
  
  if(e == equity_meas[1]){
    equity_meas_compile = ex
  } else {
    equity_meas_compile = full_join(equity_meas_compile, ex)
  }
  
}
head(equity_meas_compile)

# join to households



