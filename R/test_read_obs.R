library(DSSAT)
library(dplyr)
library(CroptimizR)
source("R/read_obs.R")

model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"

situation_name<- c("AQTB1101_2", "AQTB1101_10", "AQTB1201_5", "AQTB1201_15") 

obs_list <- read_obs(model_options, situation_name)
CroptimizR:::is.obs(obs_list)
