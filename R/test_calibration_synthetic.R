################################################################################
#
# This script runs a simple parameter estimation (1 parameter estimated) on data 
# generated from simulations (for 2 variables).
#  
# WARNING: model_options content may have to be adapted in the script depending on the DSSAT version you use.
#
# The idea here is to:
#  i) set a "true" value for a given parameter, that is different from the "default" 
#  value defined in the DSSAT input files, 
#  ii) simulate some DSSAT output variables, sensitive to the selected parameter,
#  using this true value of the parameter,
#  iii) define "synthetic" observations by selecting some simulated 
#  values of the considered variables and adding random noise to them,
#  iv) estimate the value of the considered parameter using these "synthetic" 
#  observations,
#  v) compare the simulations obtained using the "true" value, the "default" value,
#  and the "estimated" value of the considered parameter. The simulations obtained 
#  with the "estimated" value of the parameter should be closer to those obtained
#  with the "true" value, than those obtained with the "default" value.
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

## Define the "true" value of a given selected parameter of the eco file ("P1" in this case)
## For that, we read the values of the considered parameter for all ecotypes in the 
## eco file, and compute a value within the min and max values over the ecotypes,
## that is significantly different from the value set for the selected ecotype 
## (in this example, +/- 60% of the difference between the parameter value and the min or max value 
## over the ecotypes) 
if (file.exists(file.path(model_options$DSSAT_path, model_options$Crop,
                          model_options$ecotype_filename))) {
  eco_values <- DSSAT::read_eco(file.path(model_options$DSSAT_path, model_options$Crop,
                                          model_options$ecotype_filename))
} else {
  eco_values <- DSSAT::read_eco(file.path(model_options$DSSAT_path, "Genotype",
                                          model_options$ecotype_filename))
}
default_value <- as.numeric(eco_values[["P1"]][which(eco_values$`ECO#`==model_options$ecotype)])
min_value <- min(as.numeric(eco_values[["P1"]]))
max_value <- max(as.numeric(eco_values[["P1"]]))
if (min_value==max_value) {
  min_value <- default_value - 0.5*default_value
  max_value <- default_value + 0.5*default_value
}
offset <- c(- 0.6*(default_value-min_value), 0.6*(max_value-default_value))
true_value <- setNames(default_value + offset[which.max(abs(offset))], 
                       nm="P1")

## We select the variables on which the calibration will be done
var_name <- c("CWAD", "LWAD")

## We simulate these variables using the "true" value of the parameter
sim_true <- DSSAT_wrapper(param_values = true_value, 
                          model_options = model_options, 
                          situation=situation_name,
                          var = var_name)

## We define "synthetic" observations from "true" simulated values by selecting 
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
  lb = c(P1 = min_value),
  ub = c(P1 = max_value)
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

# print default value, true value and estimated value of the parameter
print(paste("Default value of the parameter:",default_value))
print(paste("True value of the parameter:",true_value))
print(paste("Estimated value of the parameter:",res$final_values))

