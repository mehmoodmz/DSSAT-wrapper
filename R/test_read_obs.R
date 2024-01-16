################################################################################
# This script illustrates the use of the read_obs function
# This function read DSSAT observations files (both time series and end-of-season files),
# and returns a corresponding observation list in CroptimizR/CroPlotR format.
#
# WARNING: model_options content may have to be adapted in the script depending on the DSSAT version you use.
#

## Installing/Loading the necessary packages and functions

if(!require("CroptimizR")){   # Needed to check the format of the observation list
  if(!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
source(list.files(recursive = TRUE)[grep("read_obs.R",list.files(recursive = TRUE))[1]])

## Setting the required information
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"

## Defining the list of situations (EXPERIMENT_TRNO) for which observations must be read
situation_name<- c("KSAS8101_1") 

## Reading the corresponding observation files and printing the returned observation list
obs_list <- read_obs(model_options, situation_name)
print(obs_list)

## Checking that they are in CroptimizR/CroPlotR format
cat(paste("Is the obs_list in CroptimizR/CroPlotR format?",CroptimizR:::is.obs(obs_list),"!"))

## Let's imagine you want to use only observations of LAID variable: 
## You can filter the observation list using the CroptimizR::filter_obs function
filtered_obs_list <- CroptimizR::filter_obs(obs_list = obs_list,var = "LAID",include = TRUE)
print(filtered_obs_list)
