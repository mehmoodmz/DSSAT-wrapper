library(DSSAT)
library(dplyr)
library(CroptimizR)
source("read_obs.R")

model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT47'
model_options$DSSAT_exe <-  'DSCSM047.EXE'
model_options$Crop <- "Wheat"
model_options$experiment_file <- "SWSW7501.WHX"

obs_list <- read_obs(model_options)
CroptimizR:::is.obs(obs_list)
