################################################################################
#
# This script runs a simple parameter estimation on data generated from simulations
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
if(!require("truncnorm")){   # This one is used to generate the synthetic observations
  install.packages("truncnorm")
  library("truncnorm")
}
source(list.files(recursive = TRUE)[grep("DSSAT_wrapper.R",list.files(recursive = TRUE))[1]])

## Setting the required information for the wrapper
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"
model_options$ecotype <-  "USWH01"
model_options$cultivar <- "NEWTON"

## Situations used in the calibration (format: EXPERIMENT_TRNO)
situation_name<- c("KSAS8101_1") 

## We set the true value of a given parameter
param_true_values <- c(P1=350)

## We select the variables on which the calibration will be done
var_name <- c("CWAD", "LWAD")

## We simulate these variables using the true value of the parameter
sim_true <- DSSAT_wrapper(param_values = param_true_values, 
                          model_options = model_options, 
                          situation=situation_name,
                          var = var_name)

## We define synthetic observations from true simulated values by selecting 
## some dates and adding truncated Gaussian noise of standard deviation 
## noise_sd*100 percent of the values.
noise_sd <- 0.2
obs_df <- CroPlotR::bind_rows(sim_true$sim_list) %>% 
  dplyr::slice(c(seq(from=1, to=nrow(.), by=20),nrow(.))) %>%
  dplyr::mutate(across(all_of(var_name), ~ .x + .x * truncnorm::rtruncnorm(length(.x), a=-3*noise_sd , b=3*noise_sd, sd=noise_sd)))
obs_list <- split(obs_df, f = obs_df$situation, lex.order = TRUE)
obs_list <- lapply(obs_list, function(x) { dplyr::select(x,-situation)}) # remove column situation
# obs_list <- lapply(obs_list, function(x) { x$HWAD[1:(nrow(x)-1)] <- NA; return(x)}) # only keep last date for Yield


## we simulate the same variable but using the default value of the parameters
## we use here the argument sit_var_dates_mask so that if simulated maturity 
## happens before the last date of observations, the values of all the simulated 
## variables will be kept constant up to that date, so that they can be compared
## to observations 
sim_default <- DSSAT_wrapper(model_options = model_options, 
                             situation=situation_name,
                             var = var_name, sit_var_dates_mask = obs_list)

## We try now to retrieve parameter and simulated true values from the observations 

## Let's define the information on the parameter to estimate (lower and upper bound)
param_info <- list(
  lb = c(P1 = 100.),
  ub = c(P1 = 500.)
)

## Let's define the options for the minimization 
optim_options <- list(xtol_rel=1e-2, out_dir="results_calibration_synth")

## and do the calibration
res <- estim_param(
  obs_list = obs_list,
  model_function = DSSAT_wrapper,
  model_options = model_options,
  optim_options = optim_options,
  param_info = param_info
)

## We now compute simulations using the estimated values of the parameters and compare
## with default, true values and observations.
sim_estim <- DSSAT_wrapper(param_values = res$final_values, 
                          model_options = model_options, 
                          situation=situation_name,
                          var = var_name, sit_var_dates_mask = obs_list)

p1 <- plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list)
print(p1)
save_plot_pdf(plot=p1, out_dir = optim_options$out_dir, file_name = "Dynamic_plots.pdf")

p2 <- plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list,
     type="scatter")
print(p2)
save_plot_pdf(plot=p2, out_dir = optim_options$out_dir, file_name = "Scatter_plots.pdf")

stats <- summary(sim_default=sim_default$sim_list,
                 sim_estim=sim_estim$sim_list,
                 obs=obs_list, stats=c("rRMSE","EF","MAPE"))
p3 <- plot(stats)
print(p3)
