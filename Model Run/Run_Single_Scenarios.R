
# run the models created by Create_NVTA_Model_Scenarios_All.R script
# utilize Scenario_Status.csv file for model name and path

# read in csv which contains models to be run
csvpath <- file.path(ve.runtime,"models","Scenario_Status.csv")
data <- read.csv(csvpath)

#Iterate through and run each model in the CSV
for(i in 1:nrow(data)){
  
  name <- data[i, "name"]
  
  cat('Running', name, '\n\n')
  
  model <- openModel(name)
  model$run()

  # Update status when complete
  data[i, 'status'] <- model$status

}

# Update all model status
write.csv(data, csvpath, row.names = F)

