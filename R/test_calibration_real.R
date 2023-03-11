################################################################################
#
# This script runs a simple parameter estimation on real data provided with DSSAT
# 
#

## Installing/Loading the necessary packages and functions

if(!require("CroptimizR")){   
  if(!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
if(!require("CroPlotR")){
  if(!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("SticsRPacks/CroPlotR@*release")
  library("CroPlotR")
}
source(list.files(recursive = TRUE)[grep("DSSAT_wrapper.R",list.files(recursive = TRUE))[1]])
source(list.files(recursive = TRUE)[grep("read_obs.R",list.files(recursive = TRUE))[1]])

## Setting the required information for the wrapper
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"
model_options$ecotype <-  "DFAULT"
model_options$cultivar <- "ASAR01"

## we select the situations on which performing the calibration 
# situation_name<- c(paste0("AQTB1101","_",seq(1,10)),  
#                    paste0("AQTB1201","_",seq(1,10))) 
situation_name<- c("AQTB1101_1","AQTB1201_1","KARA1201_1") 

## We read the associated observations
obs_list <- read_obs(model_options, situation_name)

## Let's print the names of the observed variables
print(names(bind_rows(obs_list)))

## Only keep RSTD and HWAD variables in the observations for the calibration
var_name <- c("RSTD", "HWAD")
obs_list <- filter_obs(obs_list, var = var_name, include = TRUE)

## we simulate the same variable but using the default value of the parameters
## we use here the argument sit_var_dates_mask so that if simulated maturity 
## happens before the last date of observations, the values of all the simulated 
## variables will be kept constant up to that date, so that they can be compared
## to observations 
sim_default <- DSSAT_wrapper(model_options = model_options, 
                             situation=situation_name,
                             var = var_name, sit_var_dates_mask = obs_list)

## We try now to estimate several parameters 
param_info <- list(
  lb = c(P1 = 100., G2=10.),
  ub = c(P1 = 500., G2=80.)
)

## Define the path were to put the results
optim_options <- list(xtol_rel=1e-2, 
                      out_dir = "results_calibration_real")

## Do the calibration
res <- estim_param(
  obs_list = obs_list,
  model_function = DSSAT_wrapper,
  model_options = model_options,
  optim_options = optim_options,
  param_info = param_info
)

## we compute simulations using the estimated values of the parameters 
sim_estim <- DSSAT_wrapper(param_values = res$final_values, 
                           model_options = model_options, 
                           situation=situation_name,
                           var = var_name, sit_var_dates_mask = obs_list)

## Now let's check the results obtained on the situations used for calibration
## Plots are stored in out_dir
p1 <- plot(sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list)
print(p1)
save_plot_pdf(plot=p1, out_dir = optim_options$out_dir, file_name = "Dynamic_plots.pdf")

p2 <- plot(sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list,
     type="scatter")
print(p2)
save_plot_pdf(plot=p2, out_dir = optim_options$out_dir, file_name = "Scatter_plots.pdf")

stats <- summary(sim_default=sim_default$sim_list,
                 sim_estim=sim_estim$sim_list,
                 obs=obs_list, stats=c("rRMSE","EF","MAPE"))
p3 <- plot(stats)
print(p3)
