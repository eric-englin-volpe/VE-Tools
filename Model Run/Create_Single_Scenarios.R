library(tidyverse)
library(tools)


# User Input Requirements#

## scenario_inputs_path: path where scenarios are stored i.e. B-V 
scenario_inputs_path <- file.path("models","VERSPM_Scenarios","scenario_inputs");

## base_model_path: path where base model is stored i.e. VERSPM_base_model
base_model_path <- file.path("models", "VERSPM_Scenarios", "VERSPM_base_model")


# Little to no changes should be required below

#get the list of all scenarios to be created
scenario_directories <- list.dirs(scenario_inputs_path,full.names = FALSE,
                                  recursive = FALSE)


# iterate through all scenarios
# Create a dataframe of scenario names
modelNames <- vector()

for (scenario in scenario_directories){
  cat('Preparing Scenario', scenario, '\n\n')
  scenario_path <- file.path(scenario_inputs_path,scenario)
  print(scenario_path)
  single_scenario_levels <- list.files(scenario_path, full.names = FALSE, recursive = FALSE,
                                       pattern ="[2-9]")
  
  
  # run through each case excluding the 1 case
  # changing pattern regex above specifies what levels the script should 
  #   build folders for
  
  for (level in single_scenario_levels){
    cat('\tPreparing level', level, '\t')
    
    # create the model name
    base <- openModel(base_model_path)
    modelName <- paste0('VERSPM_',scenario,level)
    toChange <- list.files(file.path(scenario_inputs_path,scenario,level),full.names = FALSE)
    cat('Modifying file(s):\n', paste(toChange, collapse = '\n'), '\n')
    
    #create the model run folder
    if(!dir.exists(file.path(ve.runtime, "models", "VERSPM_Scenarios", modelName))){
      runningModel <- base$copy(modelName)
    }
    
    # prepare to copy over user changed input files
    # Use ve.runtime to locate where VisionEval is installed
    
    for (f in toChange){
      #fix this from line
      from <- file.path(ve.runtime, scenario_inputs_path, scenario, level, f)
      print(from)
      
      #fix this to line
      to <- file.path(ve.runtime, 'models', "VERSPM_Scenarios",modelName, "inputs", f)
      print(to)
      
      # # Check to see if the 'to' exists; some inputs are optional
      # if(file.exists(to)){
      #   # verify the to and from files are different
      #   stopifnot(md5sum(from) != md5sum(to))
      # }
      
      # Save to 'msg' to suppress the printing of the result ('TRUE')
      msg <- file.copy(from, to, overwrite = TRUE)
    }
    
    
    modelNames <- rbind(modelNames, data.frame(name = modelName,
                                               files = paste(toChange, collapse = ", "),
                                               location = runningModel$modelPath,
                                               status = runningModel$status))
  }
  
  
  # Write records to Single_Scenarios_Status.csv file
  
  write.csv(modelNames, file.path(ve.runtime, 'models', 'VERSPM_Scenarios','Single_Scenarios_Status.csv'), row.names = F)
}

