# VE-Tools

## Folders:
1. <b>Data Prep</b>: This folder contains files and scripts that help with the data preparation. 
2. <b>Model Run</b>: This folder will support running the VERSPM module. 

## Data Prep

### [VERSPM File Summary Tracker.xlsx](https://github.com/eric-englin-volpe/VE-Tools/raw/main/Data%20Prep/VERSPM%20File%20Summary%20Tracker.xlsx)

This excel file has information on all 68 input files needed to run VisionEval. It also contains information on the run order and specific module that each input is used by. 

Typically, this excel is used while preparing the input files and shared across a team that is working together to compile all files. The file contains a status folder thata can be used to track this progress. 

### Config.txt

Users should start by changing the initial settings in this text file. Other data prep scripts will reference the variables and file locations in this text file. 



## Model Run

For the NVTA case study, different scenarios were created and run by modifying the base VERSPM model.
The key scripts developed and used are as follows:

- Create_NVTA_Model_Scenarios_All.R

Constructs model run folders based on base level scenario. The script copies the base level model and inserts the user
changed files into each respective scenario folder. Details about each constructed scenario is saved to 
the Scenario Status CSV including files modified and model run status.

In this implementation, /models directory contained a folder with folder for each scenario. Each scenario folder
contained a folder for a different iteration of the same scenario. The individual iteration folder contained the
user changed files to be swapped into the base model. All other files not included remained the same.


- Run_NVTA_Model_Scenarios_All.R

Runs the models created by the Create_NVTA_Model_Scenarios_All.R script. Updates Scenario Status CSV after model are
completed running. May take a while to complete running all models.


- Extract_Scenario_Metrics_All.R

Extracts metrics from each completed scenario by calling extract scenario metrics on each model run folder. The
output is stored in two files - one at the household level and one at the marea level. The script compiles
all results from the various scenarios into these two files.
