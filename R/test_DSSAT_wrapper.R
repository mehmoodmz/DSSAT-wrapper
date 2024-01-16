################################################################################
# This script tests the DSSAT wrapper 
# 
# WARNING: model_options content may have to be adapted in the script depending on the DSSAT version you use.
#

## Installing/Loading the necessary packages and functions

if(!require("CroptimizR")){   # Needed to use the test_wrapper function
  if(!require("devtools")){
    install.packages("devtools")
  }
  devtools::install_github("SticsRPacks/CroptimizR@*release")
  library("CroptimizR")
}
source(list.files(recursive = TRUE)[grep("DSSAT_wrapper.R",list.files(recursive = TRUE))[1]])

############ A simple test
# 
# This test just runs the wrapper 2 times with different parameters values
# and check that the results obtained are different for a given variable, by 
# printing the sum of the differences between the results obtained.
#

## Setting the required information
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"

model_options$ecotype <-  "USWH01"
model_options$cultivar <- "NEWTON"

param_names <- c("P1","P3")  # set the name of one or several model input parameters in a vector
param_lb<-c(100,100)         # set the lower bounds of these parameters in a vector (no Inf or -Inf ...)
param_ub<-c(500,500)         # set the upper bounds of these parameters in a vector (no Inf or -Inf ...)
var_name<-"GSTD"             # give the name of an output variable sensitive to this (or these) parameter(s)

# Set the names of one or several situations to simulate (EXPERIMENT_TRNO)
situation_name<- c("KSAS8101_1") 

# Runs the wrapper with lower bounds and then uppers bounds of the parameters.
param_values_min <- setNames(param_lb, param_names)
param_values_max <- setNames(param_ub, param_names)
sim_min       <- DSSAT_wrapper(param_values = param_values_min, model_options = model_options, 
                         situation=situation_name, var=var_name)
sim_max       <- DSSAT_wrapper(param_values = param_values_max, model_options = model_options, 
                               situation=situation_name, var=var_name)

# Print the sum of differences between the results obtained
for (sit in situation_name) {
  
  min_nrow <- min(nrow(sim_min$sim_list[[sit]]), nrow(sim_max$sim_list[[sit]]))
  
  cat(paste("\nSum of differences, variable",var_name,", situation",sit," = ",
              
              sum(abs(sim_max$sim_list[[sit]][1:min_nrow,var_name] - sim_min$sim_list[[sit]][1:min_nrow,var_name])
                  
                  ,na.rm=TRUE), "(should be different from 0)"))
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

## Setting the required information
model_options <- vector("list")
model_options$DSSAT_path <- 'C:\\DSSAT48'
model_options$DSSAT_exe <-  'DSCSM048.EXE'
model_options$Crop <- "Wheat"
model_options$ecotype_filename <- "WHCER048.ECO"
model_options$cultivar_filename <- "WHCER048.CUL"

model_options$ecotype <-  "USWH01"
model_options$cultivar <- "NEWTON"

param_values <- c(P1=200, P3=250)   # give values for at least two parameters that impact model results

# Set the names of one or several situations to simulate (EXPERIMENT_TRNO)
situation_name<- c("KSAS8101_1") 

test_wrapper(model_function = DSSAT_wrapper, model_options = model_options, 
             param_values = param_values, 
             situation = situation_name)
