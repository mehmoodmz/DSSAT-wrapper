#
# This script tests the DSSAT wrapper in differetn ways:
#   - 
# 
#
# The first part of the script must be executed in any case.
#
# Then, scroll up to the test you want to execute, in case you do not want to 
# run all tests
#

### Set-up Section: to run in any case

# The wrapper and read_obs function used here must be in the following folder
setwd("C:\\DSSAT48\\Tools\\CroptimizR")

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}
if(!require("lubridate")){
  install.packages("lubridate")
  library("lubridate")
}
if(!require("wrapr")){
  install.packages("wrapr")
  library("wrapr")
}
if(!require("DSSAT")){
  install.packages("DSSAT")
  library("DSSAT")
}
if(!require("truncnorm")){
  install.packages("truncnorm")
  library("truncnorm")
}
library(CroptimizR)
library(CroPlotR)


source("DSSAT_wrapper.R")
source("read_obs.R")
library(tidyr)

### End of Set-up Section


############ A simple test
# 
# This test just runs the wrapper 2 times with different parameters values
# and check that the results obtained are different for a given variable, by 
# printing the sum of the differences between the results obtained.
#
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"

model_options$ecotype <-  "DFAULT"
model_options$cultivar <- "ASAR01"


param_names <- c("P1","P3")    # set the name of one or several model input parameters in a vector
param_lb<-c(100,100)       # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
param_ub<-c(500,500)       # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
var_name<-"GSTD"       # give the name of an output variable sensitive to this (or these) parameter(s)

situation_name<- paste0("AQTB1101","_",as.character(seq(1, 2, by=1))) # give the name of the situations to simulate 

# Runs the wrapper with lower bounds and then uppers bounds of the parameters.
param_values_min <- setNames(param_lb, param_names)
param_values_max <- setNames(param_ub, param_names)
sim_min       <- DSSAT_wrapper(param_values = param_values_min, model_options = model_options, 
                         situation=situation_name, var=var_name)
sim_max       <- DSSAT_wrapper(param_values = param_values_max, model_options = model_options, 
                               situation=situation_name, var=var_name)

# Print sum of differences between the results obtained
for (sit in situation_name) {
  
  min_nrow <- min(nrow(sim_min$sim_list[[sit]]), nrow(sim_max$sim_list[[sit]]))
  
  print(paste("Sum of differences, variable",var_name,", situation",sit," = ",
              
              sum(abs(sim_max$sim_list[[sit]][1:min_nrow,var_name] - sim_min$sim_list[[sit]][1:min_nrow,var_name])
                  
                  ,na.rm=TRUE)))
}

# Print the results returned by the wrapper
options(tibble.print_max = Inf)
sim_min$sim_list
sim_max$sim_list


############ Testing the wrapper using the test_wrapper function from CroptimizR
#
# This test runs the test_wrapper function, following the advice given at 
# https://sticsrpacks.github.io/CroptimizR/articles/Designing_a_model_wrapper.html
#
# The function prints if the tests are OK (green) or not (red).

model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"

model_options$ecotype <-  "DFAULT"
model_options$cultivar <- "ASAR01"

test_wrapper(model_function = DSSAT_wrapper, model_options = model_options, 
             param_values = setNames(param_lb, param_names), 
             situation = situation_name)

############ Test run on different experiments
#
# Just a simple test to run the wrapper on different experiments
#
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"

model_options$ecotype <-  "DFAULT"

## Situations to run (format: EXEPERIMENT_TRNO)
situation_name<- c("AQTB1101_2", "AQTB1101_10", "AQTB1201_5", "AQTB1201_15") 

sim <- DSSAT_wrapper(model_options = model_options, 
                     situation=situation_name, var=var_name)


############ Testing a simple optimization on a synthetic experiment 
#
# This test runs a simple parameter estimation on data generated from simulations
# 
#
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"

model_options$ecotype <-  "DFAULT"
model_options$cultivar <- "ASAR01"

## Situations used in the calibration (format: EXEPERIMENT_TRNO)
situation_name<- c("AQTB1101_2", "AQTB1201_5") 

## We set the true value of a given parameter
param_true_values <- c(P1=350)

## We select the variables on which the calibration will be done
var_name <- c("CWAD", "HWAD")

## We simulate these variables using the true value of the parameter
sim_true <- DSSAT_wrapper(param_values = param_true_values, 
                          model_options = model_options, 
                          situation=situation_name,
                          var = var_name)

## We simulate the same variable but using the default value of the parameters
sim_default <- DSSAT_wrapper(model_options = model_options, 
                             situation=situation_name,
                             var = var_name)

## We define synthetic observations from true simulated values by selecting 
## some dates and adding truncated Gaussian noise of standard deviation 
## noise_sd percent of the values.
noise_sd <- 0.2
obs_df <- CroPlotR::bind_rows(sim_true$sim_list) %>% 
  dplyr::slice(c(seq(from=1, to=nrow(.), by=20),nrow(.))) %>%
  dplyr::mutate(across(all_of(var_name), ~ .x + .x * truncnorm::rtruncnorm(length(.x), a=-3*noise_sd , b=3*noise_sd, sd=noise_sd)))
obs_list <- split(obs_df, f = obs_df$situation, lex.order = TRUE)
obs_list <- lapply(obs_list, function(x) { dplyr::select(x,-situation)}) # remove column situation
obs_list <- lapply(obs_list, function(x) { x$HWAD[1:(nrow(x)-1)] <- NA; return(x)}) # only keep last date for Yield

## Let's plot the true simulated values, the default one and the observations
plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list, obs=obs_list)

## We try now to retrieve parameter and simulated true values from the observations 

## Let's define the information on the parameter to estimate (lower and upper bound)
param_info <- list(
  lb = c(P1 = 100.),
  ub = c(P1 = 500.)
)

## Let's define the options for the minimization 
optim_options <- list(xtol_rel=1e-2)

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
                          var = var_name)

plot(sim_true=sim_true$sim_list, sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list)

############ Test optimization on real data
#
# This test runs a simple parameter estimation on real data provided with DSSAT
#

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

## Only keep RSTD and HWAD variables in the observations
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
out_dir <- "Calibration_2params"
optim_options <- list(xtol_rel=1e-2, 
                      out_dir = file.path(getwd(), out_dir))

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
save_plot_pdf(plot=p1, out_dir = out_dir, file_name = "Dynamic_plots.pdf")

p2 <- plot(sim_default=sim_default$sim_list, 
     sim_estim=sim_estim$sim_list, obs=obs_list,
     type="scatter")
print(p2)
save_plot_pdf(plot=p2, out_dir = out_dir, file_name = "Scatter_plots.pdf")

stats <- summary(sim_default=sim_default$sim_list,
                 sim_estim=sim_estim$sim_list,
                 obs=obs_list, stats=c("rRMSE","EF","MAPE"))
p3 <- plot(stats)
print(p3)
