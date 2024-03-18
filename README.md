# DSSAT-wrapper

This repository aims to provide a DSSAT wrapper for the [CroptimizR](https://sticsrpacks.github.io/CroptimizR/) and [CroPlotR](https://sticsrpacks.github.io/CroPlotR/) packages.
It makes it possible to apply to the models implemented in DSSAT the calibration and evaluation methods these packages include.

This wrapper is an evolution of the one created for AgMIP Calibration Phase III protocol (see [Wallach et al., 2022](https://www.biorxiv.org/content/biorxiv/early/2022/08/29/2022.06.08.495355.full.pdf)). This new version of the DSSAT wrapper aims to be more generic and efficient than the first version. It makes it possible to work with any type of variable observations provided in the time series file \*.\*\*T, with any parameter among cultivar and/or ecotype ones, and with multiple experiment files and treatments.

## What does it require?

To apply to DSSAT the calibration and evaluation methods implemented in CroptimizR and CroPlotR, you must:

* have DSSAT installed (see https://dssat.net/), 
* have several R packages installed (the [DSSAT R package](https://cran.r-project.org/web/packages/DSSAT/index.html), the [CroptimizR](https://sticsrpacks.github.io/CroptimizR/) and [CroPlotR](https://sticsrpacks.github.io/CroPlotR/) packages, and a few other packages the DSSAT wrapper depends on). The example scripts provided in this repos. include instructions that automatically install these R packages if they are not yet installed.

## What does this repos. contain?

* DSSAT-wrapper.Rproj, the R project associated to this repos. 
* R folder: 

  * DSSAT_wrapper.R: the DSSAT wrapper function,
  * read_obs.R: a function that reads DSSAT observation files (both times series \*.\*\*T files, and end-of-season \*.\*\*A files) and returns them in CroptimizR/CroPlotR format,
  * test_read_obs.R: a little script to show how to use the read_obs function,
  * test_DSSAT_wrapper.R: a script for testing the DSSAT wrapper,
  * test_calibration_synthetic.R: a script showing a simple example of calibration of a model included in DSSAT (CERES-WHEAT), using synthetic data,
  * test_calibration_real.R: a script showing a simple example of calibration of a model included in DSSAT (CERES-WHEAT), using real data.
  
You will find comments at the beginning of each function and script file.   
  
## How to proceed?

* First, install DSSAT if you haven't already (see https://get.dssat.net/request/?sft=4),
* Then, install the DSSAT R Package if you haven't already (just by running `install.packages("DSSAT")` in R/Rstudio),
* Then, install [CroptimizR](https://sticsrpacks.github.io/CroptimizR/) and [CroPlotR](https://sticsrpacks.github.io/CroPlotR/) (see section [What does it require?]), if you haven't already,
* Then, download (or clone/fork) this repository (green button "Code"),
* Open the DSSAT-wrapper.Rproj in RStudio, or, if you do not want to use RStudio, go, in R, to the main folder of your local copy of the repository,
* The test_***.R scripts provided use by default DSSAT version 4.8, installed in C: drive, and the input files provided with it. If you have a different version of DSSAT or installed DSSAT in a different folder, you must modify the associated information in these scripts before running them.
* Just play by running the scripts and adapting them to your own case!

You will find a comprehensive documentation on the methods and features available for the calibration and evaluation of all crop models in [CroptimizR](https://sticsrpacks.github.io/CroptimizR/) and [CroPlotR](https://sticsrpacks.github.io/CroPlotR/) websites.

## Special features of the DSSAT wrapper

* The names of the simulated situations are set to EXPERIMENT_TRNO, where EXPERIMENT is the experiment name, and TRNO the treatment number. These names are used in the results returned by the wrapper to associate simulated results to the corresponding situation, and are also useful to specify the list of situations to simulate, using the "situation" argument of the wrapper.
* This wrapper generates additional variables wrt to what is read in DSSAT OUT files. The julian day, from 1st jan. of sowing year, of each Zadok stage is given in Zadok1 to Zadok100 variables, as interpolated from GSTD variable. If the corresponding Zadok stage is not reached, the value is set to the last day of the last simulated year. These variables are useful to estimate parameters from observed julian days of phenological stages.

More details about the wrapper inputs and outputs are provided as comments at the beginning of the R/DSSAT_wrapper.R file.

## Getting help

If you have any question or suggestion or if you want to report a bug, please do it via the GitHub [issues](https://github.com/sbuis/DSSAT-wrapper/issues).

## Citation 

If you have used the CroptimizR/CroPlotR packages for a study that led to a publication or report, please cite the authors. To get the suggested citation, run `citation("CroptimizR")` and `citation("CroPlotR")` in R.
