
# Run the models created by Create_Single_Scenarios.R script
# utilize Single_Scenarios_Status.csv file for model name and path

# read in csv which contains models to be run
csvpath <- file.path(ve.runtime,"models","VERSPM_Scenarios","Single_Scenarios_Status.csv")
data <- read.csv(csvpath)

#Iterate through and run each model in the CSV
for(i in 1:nrow(data)){
  
  name <- data[i, "name"]
  modelpath <- data[i,"location"]
  
  cat('Running', name, '\n\n')
  
  model <- openModel(modelpath)
  model$run()
  
  # Update status when complete
  data[i, 'status'] <- model$status
  
}

# Update all model status
write.csv(data, csvpath, row.names = F)

