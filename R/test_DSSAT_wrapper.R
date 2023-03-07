#
# This script test the DSSAT wrapper following the advice given at https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html
# Printed sums of differences should be different from 0. 
# IT IS JUST A TEMPLATE : some information must be filled before running it (see below)
# 

if(!require("DSSAT")){
  install.packages("DSSAT")
  library("DSSAT")
}

source("DSSAT_wrapper.R")
library(tidyr)

########## TO BE ADAPTED TO YOUR CASE ....

model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT47'
model_options$DSSAT_exe <-  'DSCSM047.EXE'
model_options$Crop <- "Wheat"
model_options$Genotype <- "Genotype" 
model_options$ecotype_filename <- "WHCER047.ECO"
model_options$cultivar_filename <- "WHCER047.CUL"

# Adapt to Australian or French dataset case ...
model_options$experiment_filename <-  'CSIR1066.WHX'
model_options$ecotype <-  "AUWH01"
model_options$cultivar <- "CSIR01"


param_names <- c("P1","P3")    # set the name of one or several model input parameters in a vector
param_lb<-c(100,100)       # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
param_ub<-c(500,500)       # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
var_name<-"GSTD"       # give the name of an output variable sensitive to this (or these) parameter(s)

situation_name<- as.character (seq(1, 2, by=1)) # give the name of the situation to simulate 

############ A simple test

param_values_min <- setNames(param_lb, param_names)
param_values_max <- setNames(param_ub, param_names)
sim_min       <- DSSAT_wrapper(param_values = param_values_min, model_options = model_options, 
                         situation=situation_name, var=var_name)
sim_max       <- DSSAT_wrapper(param_values = param_values_max, model_options = model_options, 
                               situation=situation_name, var=var_name)


for (sit in situation_name) {
  
  min_nrow <- min(nrow(sim_min$sim_list[[sit]]), nrow(sim_max$sim_list[[sit]]))
  
  print(paste("Sum of differences, variable",var_name,", situation",sit," = ",
              
              sum(abs(sim_max$sim_list[[sit]][1:min_nrow,var_name] - sim_min$sim_list[[sit]][1:min_nrow,var_name])
                  
                  ,na.rm=TRUE)))
}


options(tibble.print_max = Inf)
sim_min$sim_list
sim_max$sim_list


############ using test_wrapper function from CroptimizR

library(CroptimizR)

test_wrapper(model_function = DSSAT_wrapper, model_options = model_options, 
             param_values = setNames(param_lb, param_names), 
             situation = situation_name)


############ testing a simple optimization on a synthetic experiment 

library(CroPlotR)
library(CroptimizR)

## we set the true value of a given parameter
param_true_values <- c(P1=150)

## we choose a few situations and a variable
situation_name<- as.character (seq(1, 2, by=1))
var_name <- "LAID"

## we simulate a given variable using the true value of the parameter
sim_true <- DSSAT_wrapper(param_values = param_true_values, 
                          model_options = model_options, 
                          situation=situation_name,
                          var = var_name)

## we simulate the same variable but using the default value of the parameters
sim_default <- DSSAT_wrapper(model_options = model_options, 
                             situation=situation_name,
                             var = var_name)

## we compare the simulated values of the variable obtained with true and default value of the parameter
p_before <- plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list)

## we define synthetic observations by from true simulated values by selecting values and adding noise
noise_sd <- 0.2
obs_df <- bind_rows(sim_true$sim_list) %>% 
  dplyr::slice(seq(from=1, to=nrow(.), by=10)) %>%
  dplyr::mutate(across(all_of(var_name), ~ .x + .x * truncnorm::rtruncnorm(length(.x), a=-3*noise_sd , b=3*noise_sd, sd=noise_sd)))
obs_list <- split(obs_df, f = obs_df$situation, lex.order = TRUE)
obs_list <- lapply(obs_list, function(x) { dplyr::select(x,-situation)}) # remove column situation

plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list, obs=obs_list)

## We try to retrieve parameter and simulated true values from the observations 
## starting from default value of the parameters
param_info <- list(
  lb = c(P1 = 100.),
  ub = c(P1 = 500.)
)

optim_options <- list(maxeval = 100, xtol_rel=1e-2)
res <- estim_param(
  obs_list = obs_list,
  model_function = DSSAT_wrapper,
  model_options = model_options,
  optim_options = optim_options,
  param_info = param_info
)

## we compute simulations using the estimated values of the parameters and compare
## with default, true values and observations.
sim_estim <- DSSAT_wrapper(param_values = res$final_values, 
                          model_options = model_options, 
                          situation=situation_name,
                          var = var_name)

plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list)

