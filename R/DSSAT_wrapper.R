#' @title DSSAT wrapper for CroptimizR
#'
#' @description This function runs DSSAT with given ecotype and/or cultivar parameter 
#' values and returns its results in a shape compatible with CroptimizR and CroPlotR R packages.
#' (see https://sticsrpacks.github.io/CroptimizR/ and https://sticsrpacks.github.io/CroPlotR/).
#'
#' @param param_values (optional, NULL by default, in that case default values of 
#' the parameters as read in DSSAT inputs files are used) a named vector that contains values
#' of DSSAT input parameters to use in the simulations. Should have one named column per
#' parameter. If param_values is not provided or set to NULL, the simulations will
#' be performed using the parameters values defined in the DSSAT input files referenced
#' in model_options argument.
#'
#' @param situation Vector of situation names (EXPERIMENT_NAME_TRNO) for which results
#' must be returned.
#'
#' @param model_options List containing the information needed by the model.
#'   `DSSAT_path`: path to DSSAT directory
#'   `DSSAT_exe`: name of the model executable
#'   `Crop`: name of the crop (i.e. of subdirectory in DSSAT path)
#'   `ecotype`: name of the ecotype
#'   `cultivar`: name of the cultivar
#'   `ecotype_filename`: name of the ecotype filename
#'   `cultivar_filename`: name of the cultivar filename
#'   `out_files`: names of the (time-series only) output files to read 
#'   (by default, PlantGro.OUT, and PlantGR2.OUT for Wheat, are read)
#'   `suppress_output`: a logical value indicating whether to suppress DSSAT-CSM output 
#'   from being printed to the console (TRUE by default)
#'
#' @param var (optional) vector of variables names for which results
#' must be returned. If not provided, the function will return results for all simulated
#' variables.
#'
#' @param sit_var_dates_mask (optional) A named list
#' containing a mask for variables and dates for which simulated values
#' should be returned. Typically a list containing the observations to which
#' simulations should be compared. The wrapper use this mask to provide simulated values 
#' after maturity date if required (see details).
#'
#' @return A list containing simulated values. It include 2 elements:
#'   - `sim_list`: a named list of tibbles, one tibble per situation, each tibble contains a 
#'                 column Date storing the dates in Date or POSIXct format, plus one column 
#'                 per simulated variable storing the simulated values for each date.
#'                 The names of the elements of the list are the situation names (EXPERIMENT_TRNO).
#'                 See Details section for information about the variables included in sim_list.
#'   - `error`: an error code indicating if at least one simulation ended 
#'              with an error (TRUE) or if all simulations went OK (FALSE).
#' 
#' @details This wrapper run the DSSAT model for an ensemble of situations (EXPERIMENT X Treatment number)
#' and return associated results in cropr format.
#'
#' If argument sit_var_dates_mask is provided (which is the case during parameter estimation
#' using the CroptimizR::estim_param function), and if maturity is reached before 
#' the last date included in this mask for a given situation, then the simulated 
#' results obtained at maturity are replicated up to this date.
#' This guarantees the comparison of simulated and observed results regardless the 
#' maturity date (which may evolve during the calibration process).
#' 
#' This wrapper generates additional variable wrt to what is read in DSSAT OUT files.
#' The julian day, from 1st jan. of sowing year, of each Zadok stage is given in 
#' Zadok1 to Zadok100 variables, as interpolated from GSTD variable. If the corresponding
#' Zadok stage is not reached, the value is set to the last day of the last simulated year.
#' 
#' The very first version of this wrapper has been initiated by Jing Qi, Amir Souissi 
#' and Samuel Buis for AgMIP Calibration project Phase III exercise.
#' 
#' @importFrom wrapr seqi
#' @importFrom lubridate year
#' @import tidyr
#' @importFrom dplyr bind_rows mutate select relocate left_join filter slice bind_cols
#' 

# Installation and loading of the packages used by the DSSAT wrapper for CroptimizR

if(!require("DSSAT")){          
  install.packages("DSSAT")
  library("DSSAT")
}
if(!require("tidyr")){        
  install.packages("tidyr")
  library("tidyr")
}
if(!require("dplyr")){        
  install.packages("dplyr")
  library("dplyr")
}
if(!require("wrapr")){        
  install.packages("wrapr")
  library("wrapr")
}
if(!require("lubridate")){    
  install.packages("lubridate")
  library("lubridate")
}

DSSAT_wrapper <- function(param_values=NULL, situation, model_options, var=NULL, 
                          sit_var_dates_mask = NULL, ...) {
  
  on.exit({

    # come back to initial working directory
    setwd(ini_wd)
    
    # set the parameters file to their original values
    if (flag_eco_param) {
      res <- file.rename(from=file.path(ecotype_path,paste0(ecotype_filename,"_tmp")),to=file.path(ecotype_path,ecotype_filename))
      if (!res) {
        results$error <- TRUE
        warning(paste("Unable to rename ",file.path(ecotype_path,paste0(ecotype_filename,"_tmp")),"in ",file.path(ecotype_path,ecotype_filename),
                   ".\n This may alter the following results. Please allow write permissions on",file.path(ecotype_path,ecotype_filename)))
      }
    }
    if (flag_cul_param) {
      res <- file.rename(from=file.path(cultivar_path,paste0(cultivar_filename,"_tmp")),to=file.path(cultivar_path,cultivar_filename))
      if (!res) {
        results$error <- TRUE
        warning(paste("Unable to rename ",file.path(cultivar_path,paste0(cultivar_filename,"_tmp")),"in ",file.path(cultivar_path,cultivar_filename),
                   ".\n This may alter the following results. Please allow write permissions on",file.path(cultivar_path,cultivar_filename)))
      }
    }
    return(results)
  })

  # Initializations
  ini_wd <- getwd()
  flag_eco_param = FALSE; flag_cul_param = FALSE
  param_names <- names(param_values)
  results <- list(sim_list = setNames(vector("list",length(situation)), nm = situation), error=FALSE)
  
  options(DSSAT.CSM = file.path(model_options$DSSAT_path,model_options$DSSAT_exe))
  crop <- model_options$Crop
  project_path <- file.path(model_options$DSSAT_path,model_options$Crop)
  genotype_path <- file.path(model_options$DSSAT_path,"Genotype")
  ecotype_filename <- model_options$ecotype_filename
  # By default, DSSAT use ecotype and cultivar files that are in the project folder, 
  # If not there, use these in the Genotype folder
  if (file.exists(file.path(project_path, ecotype_filename))) {
    ecotype_path <- project_path
  } else {
    ecotype_path <- genotype_path
  }
  cultivar_filename <- model_options$cultivar_filename
  if (file.exists(file.path(project_path, cultivar_filename))) {
    cultivar_path <- project_path
  } else {
    cultivar_path <- genotype_path
  }
  ecotype <- model_options$ecotype
  cultivar <- model_options$cultivar
  if (is.null(model_options$suppress_output)) {
    suppress_output <- TRUE
  } else {
    suppress_output <- model_options$suppress_output
  }
  ## Retrieve the crop code
  ## Currently deduced from ecotype filename but oculd be from Crop by reading DETAIL.CDE 
  ## file once at first run and storing in environment
  crop_code <- substr(model_options$ecotype_filename,1,2)

  # Argument checks
  if (!is.null(param_values)) {
    if (is.null(names(param_values))) {
      results$error <- TRUE
      warning("Argument param_values must be a NAMED vector. The names must be the names of DSSAT parameters.")
    }
  }
  
  # check prams are listed in eco and cul
  # check eco and cul are in the files
  
  
  # Force ecotype parameters if provided in param_values
  if (!is.null(param_values)) {
    eco <- read_eco(file.path(ecotype_path,ecotype_filename))	  # read ecotype DSSAT file => put results in eco data.frame
    eco_params <- names(eco)
    if (any (param_names %in% eco_params)) {   # if some parameters in param_values are ecotype parameters
      res <- file.copy(from=file.path(ecotype_path,ecotype_filename),
                       to=file.path(ecotype_path,paste0(ecotype_filename,"_tmp")),
                       overwrite = TRUE)
      if (!res) {
        results$error <- TRUE
        warning(paste("Unable to copy ",file.path(ecotype_path,ecotype_filename)))
      }
      flag_eco_param = TRUE
      eco_paramNames <- intersect(param_names, eco_params)
      idx <- which(eco$`ECO#`==ecotype)
      if (length(idx)==0) {
        results$error <- TRUE
        warning(paste("Ecotype",ecotype,
                   "is not part of the list of ecotypes described in ecotype file",
                   file.path(ecotype_path,ecotype_filename)))
      }
      for (param in eco_paramNames) {   # modify their values in the eco data.frame
        eco[[param]][idx] <- param_values[param] 
      }      
      attr(eco, "comments") <- NULL # to prevent a bug in v0.0.7 of DSSAT package
      write_eco(eco,file.path(ecotype_path,ecotype_filename))  # write the ecotype DSSAT file from the modified eco data.frame
    }
  }
  
  # Force cultivar parameters if provided in param_values, same as for ecotype parameters but for cultivar ones
  if (!is.null(param_values)) {
    cul <- read_cul(file.path(cultivar_path,cultivar_filename)) 
    cul_params <- names(cul)
    if (any (param_names %in% cul_params)) {
      res <- file.copy(from=file.path(cultivar_path,cultivar_filename),to=file.path(cultivar_path,paste0(cultivar_filename,"_tmp")),overwrite = TRUE)
      if (!res) {
        results$error <- TRUE
        warning(paste("Unable to copy ",file.path(cultivar_path,cultivar_filename)))
      }
      flag_cul_param = TRUE
      cul_paramNames <- intersect(param_names, cul_params)
      idx <- which(cul$`VAR-NAME`==cultivar)
      if (length(idx)==0) {
        results$error <- TRUE
        warning(paste("Cultivar",cultivar,
                   "is not part of the list of cultivars described in cultivar file",
                   file.path(cultivar_path,cultivar_filename)))
      }
      for (param in cul_paramNames) {
        if (is.character(cul[[param]])) {
          cul[[param]][idx]  <- as.character(round(param_values[[param]],digits = 2)) 
        } else {
          cul[[param]][idx]  <- param_values[param] 
        }
      }
      write_cul(cul,file.path(cultivar_path,cultivar_filename))
    }
  }
  
  # Check all params where taken into account
  if (!is.null(param_values)) {
    if (length(setdiff(names(param_values), c(eco_params, cul_params)))>0) {
      results$error <- TRUE
      warning(paste("Parameter(s)",
                 paste(setdiff(names(param_values), c(eco_params, cul_params)), 
                       collapse = ","),
                 "in argument param_values is(are) not part of the ecotype or cultivar files",
                 file.path(ecotype_path,ecotype_filename),"and",
                 file.path(cultivar_path,cultivar_filename)),
           "\n Please check their spelling.")
    }
  }
  
  # Run the model
  setwd(project_path)
  
  ## Build batch file description tibble
  tmp <- t(dplyr::bind_rows(sapply(situation,FUN = strsplit, "_")))
  experiment_files <- paste0(tmp[,1],".",crop_code,"X")
  TRNO <- as.integer(tmp[,2])
  batch <- data.frame(FILEX=experiment_files,TRTNO=TRNO,RP=1,SQ=0,OP=0,CO=0)
  
  write_dssbatch(x=batch) # Generate a DSSAT batch file with function arguments

  run_dssat(suppress_output=suppress_output) # Run DSSAT-CSM

  setwd(ini_wd)
  
  # Catch error in case of
  if (file.exists(file.path(project_path,"ERROR.OUT"))) {
    results$error <- TRUE
    warning(paste("An error occured in DSSAT simulation. Please look at",file.path(project_path,"ERROR.OUT"),"file."))
  }
  
  # Read its outputs and store them in CroptimizR format
  if (file.exists(file.path(project_path,"PlantGro.OUT"))) {
    pgroTot <- as.data.frame(read_output(file.path(project_path,"PlantGro.OUT"))) %>% dplyr::mutate(Date=DATE) %>% 
      dplyr::select(-DATE) %>% dplyr::relocate(Date)

    # Add variables included in PlantGr2.OUT if Crop is Wheat 
    flag_pgr2 <- FALSE
    if (model_options$Crop=="Wheat" & file.exists(file.path(project_path,"PlantGr2.OUT"))) {
      pgr2 <- as.data.frame(read_output(file.path(project_path,"PlantGr2.OUT"))) %>% dplyr::mutate(Date=DATE) %>% 
        dplyr::select(-DATE) %>% dplyr::relocate(Date)
      pgroTot <- dplyr::left_join(pgroTot, 
                                  pgr2[,c("Date","EXPERIMENT","TRNO",
                                          setdiff(names(pgr2), names(pgroTot)))], 
                                  by= c("Date","EXPERIMENT","TRNO"))
      flag_pgr2 <- TRUE
    }
   
    for (out_file in setdiff(model_options$out_files,c("PlantGro.OUT","PlantGr2.OUT"))) {
      if (file.exists(file.path(project_path,out_file))) {
        pgr_tmp <- as.data.frame(read_output(file.path(project_path,out_file))) %>% dplyr::mutate(Date=DATE) %>% 
          dplyr::select(-DATE) %>% dplyr::relocate(Date)
        pgroTot <- dplyr::left_join(pgroTot, 
                                    pgr_tmp[,c("Date","EXPERIMENT","TRNO",
                                               setdiff(names(pgr_tmp), names(pgroTot)))], 
                                    by= c("Date","EXPERIMENT","TRNO"))
      }
    }
     
    if (nrow(pgroTot)==0) {
      results$error <- TRUE
      warning(paste("Error reading DSSAT output files. No results found."))
    }
    
    for (sit in situation) {

      experiment <- strsplit(sit,split="_")[[1]][1]
      trno <- as.integer(strsplit(sit,split="_")[[1]][2])
            
      pgro <- dplyr::filter(pgroTot, TRNO==trno, EXPERIMENT==experiment)
      if (nrow(pgro)==0) {
        results$error <- TRUE
        warning(paste("TRNO",trno,"and/or EXPERIMENT",experiment,
                   "is/are not part of the DSSAT output files.",
                   "\n Please check the situation names provided to the DSSAT wrapper (should be EXPERIMENT_NAME_TRNO)."))
      }
      
      pgro <- pgro[!duplicated(pgro$Date),]  # DSSAT sometimes include replicated lines in the .out file ...
      results$sim_list[[sit]] <- pgro
      
      if ( is.null(var) | any(grepl("Zadok",var)) ) {
        
        # Create variables that contain Zadok phenological stages in julian days (computed from the 1st of Jan of the sowing Year)
        zadok_df <- getIntGSTD(pgro$GSTD)
        zadok_df$dates <- pgro$Date[zadok_df$firstIndex]
        zadok_df$julDay <- julian(zadok_df$dates,origin=as.Date(paste(lubridate::year(zadok_df$dates[1]),"01","01",sep = "-"))) + 1
        
        ZlistNonFloored <- data.frame(Zadok=pgro$GSTD, julDay=julian(pgro$Date,origin=as.Date(paste(lubridate::year(zadok_df$dates[1]),"01","01",sep = "-")))+1)
        
        # interpolate julian days for non simulated zadok stages between first and last zadok simulated
        missingZadok <- setdiff(zadok_df$Zadok[1]:zadok_df$Zadok[length(zadok_df$Zadok)],zadok_df$Zadok)
        interpJulDays <- sapply(missingZadok, function(x) interp(x,ZlistNonFloored))
        zadok_df$firstIndex <- NULL
        tmp <- data.frame(Zadok=missingZadok, julDay=interpJulDays, dates=NA)
        
        zadok_df <- rbind(zadok_df,tmp)
        zadok_df <- zadok_df[order(zadok_df$Zadok),]
        
        # Extrapolate julian days for Zadok stages posterior to these simulated (to take them into account if they are observed but not simulated ...)
        missingZadok <- wrapr::seqi(max(zadok_df$Zadok)+1,100)
        
        end_DOY <- as.POSIXct(paste(lubridate::year(pgro$Date[nrow(pgro)]),"12","31",sep = "-"),format="%Y-%m-%d", tz="UTC")
        
        if (length(missingZadok)>0) {
          julianEndOfYear <- julian(as.Date(end_DOY), 
                                    origin=as.Date(paste(lubridate::year(zadok_df$dates[1]),"01","01",sep = "-"))) + 1
          
          dftmp <- data.frame(Zadok=missingZadok, julDay=julianEndOfYear[1], dates=NA)
          zadok_df <- rbind(zadok_df,dftmp)
        }
        
        df=as.data.frame(as.list(setNames(as.numeric(zadok_df$julDay),paste0("Zadok",zadok_df$Zadok))))
        
        results$sim_list[[sit]] <- 
          dplyr::bind_cols(results$sim_list[[sit]],df)
        
      }

      if (!is.null(sit_var_dates_mask) & sit %in% names(sit_var_dates_mask)) {
        end_date <- sit_var_dates_mask[[sit]]$Date[nrow(sit_var_dates_mask[[sit]])]
        if (!(end_date %in% results$sim_list[[sit]]$Date)) {
          dates_to_add <- seq(from = results$sim_list[[sit]]$Date[nrow(results$sim_list[[sit]])], 
                              to=end_date, by = "days")[-1]
          extension <- results$sim_list[[sit]] %>% 
            dplyr::slice(rep(nrow(results$sim_list[[sit]]),each=length(dates_to_add)))
          extension$Date <- dates_to_add
          results$sim_list[[sit]] <- bind_rows(results$sim_list[[sit]], extension)
        }
      }

      # Select variables for which results are required      
      if (!is.null(var)) {
        results$sim_list[[sit]] <- results$sim_list[[sit]] %>% dplyr::select(c("Date",all_of(var)))
        if (nrow(results$sim_list[[sit]])==0) {
          results$error <- TRUE
          warning(paste("No results found for variable(s)",var,
                     "\n Please check the spelling of the variables listed in the var argument of the DSSAT wrapper."))
        }
      }
      
    }
    
    attr(results$sim_list, "class")= "cropr_simulation"
    
    # Remove PlantGro.OUT file to be able to check if it is created for next model runs.
    file.rename(from=file.path(project_path,"PlantGro.OUT"), to=file.path(project_path,"PlantGro_saved.OUT"))
    if (flag_pgr2) file.rename(from=file.path(project_path,"PlantGr2.OUT"), to=file.path(project_path,"PlantGr2_saved.OUT"))
    for (out_file in setdiff(model_options$out_files,c("PlantGro.OUT","PlantGr2.OUT"))) {
      if (file.exists(out_file)) {
        file.rename(from=file.path(project_path,out_file), to=file.path(project_path,paste0(strsplit(out_file,"[.]")[[1]][1],"_saved.OUT")))
      }
    }  
    
  } else {
    
    results$error <- TRUE
    warning("DSSAT output file PlantGro.OUT has not been generated.")
    
  }
  
}


# Extract rounded values for GSTD and their index in the GSTD list
getIntGSTD <- function(GSTD) { u <- unique(floor(GSTD)) ; data.frame(Zadok=u, firstIndex=match(u, floor(GSTD)))}

interp <- function(ZadokStage,ZList) {
  # Interpolate the julian day of a Zadok stage from julian days of other Zadok stages
  # ZadokStage is the ZadokSateg to interpolate
  # ZList is a list containing a vector named Zadok and a vector named julDay, containing the Zadok stages and their associated julian days
  inf <- which(ZadokStage > ZList$Zadok)
  sup <- which(ZadokStage < ZList$Zadok)
  tmp <- (ZList$julDay[sup[1]]-ZList$julDay[inf[length(inf)]]) * (ZadokStage - ZList$Zadok[inf[length(inf)]]) /
    (ZList$Zadok[sup[1]]-ZList$Zadok[inf[length(inf)]])+ZList$julDay[inf[length(inf)]]
  return(tmp)
}

