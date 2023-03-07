# Function for reading DSSAT formatted observation and transforming into CroptimizR formatted ones

read_obs <- function(model_options) {
#' @description This function read DSSAT observations files file_name.WHT and file_name.WHA,
#' and return an observation list in CroptimizR format.
#'
#' @param model_options List of options similar to the one given to DSSAT_wrapper.
#' THe following fields are mandatory to use this function:
#'   `DSSAT_path`: path to DSSAT directory
#'   `DSSAT_exe`: name of the model executable
#'   `Crop`: name of the crop (i.e. of subdirectory in DSSAT path)
#'   `experiment_file`: name of the .**X experiment file               
#'
#' @return List of observations in CroptimizR format (named list of data.frame, 
#' see obs_list argument of CroitilizR estim_param function)  
#' 
#' @importFrom stringr str_sub
#' @importFrom dplyr full_join relocate mutate select
#' @import tidyr 
#' 
#'
#' @examples
#'
#' model_options <- vector("list")
#' model_options$DSSAT_path <- "C:\\DSSAT47"
#' model_options$DSSAT_exe <-  "DSCSM047.EXE"
#' model_options$Crop <- "Wheat"
#' model_options$experiment_file <- "SWSW7501.WHX"
#'
#' obs_list <- read_obs(model_options)
#' CroptimizR:::is.obs(obs_list)
#'
  
  if (is.null(model_options$experiment_file)) {
    stop("Please provide experiment file name in experiment_file field of model_options argument.")
  }
    
  tmp <- strsplit(x = model_options$experiment_file, split = "\\.")[[1]]
  file_name <- tmp[1]
  crop_code <- stringr::str_sub(tmp[2], start = 1, end = 2)
  
  file_name_a <- file.path(model_options$DSSAT_path,model_options$Crop,paste0(file_name,".",crop_code,"A"))
  file_name_t <- file.path(model_options$DSSAT_path,model_options$Crop,paste0(file_name,".",crop_code,"T"))

  if ( !file.exists(file_name_a) & !file.exists(file_name_t) ) {
    stop(paste("Files",file_name_a,"and",file_name_t,"do not exist.\n Please check file_name argument"))
  }
  
  # Handle time series data
  in_season_obs_df <- NULL
  if (file.exists(file_name_t)) {
    in_season_obs_df <- read_filet(file_name_t, na_strings = NA)
    in_season_obs_df <- in_season_obs_df %>% dplyr::mutate(Date=DATE) %>% 
      dplyr::select(-DATE) %>% dplyr::relocate(Date)
  }
  
  # Handle final data
  end_season_obs_df <- NULL
  if (file.exists(file_name_t)) {
    end_season_obs_df <- read_filea(file_name_a, na_strings = NA)
    if ( "MDAT" %in% names(end_season_obs_df) ) {
      end_season_obs_df <- end_season_obs_df %>% dplyr::mutate(Date=MDAT) %>% 
        dplyr::select(-MDAT) %>% dplyr::relocate(Date)
    } else if ( "HDAT" %in% names(end_season_obs_df) ) {
      end_season_obs_df <- end_season_obs_df %>% dplyr::mutate(Date=HDAT) %>% 
        dplyr::select(-HDAT) %>% dplyr::relocate(Date)
    } else {
      stop(paste("HDAP, HDAT, MDAT or MDAP not included in file",file_name_t,
      "\nThis function can not handle this case."))
    }
  }

  # Join in-season and final data in a single df
  obs_df <- dplyr::full_join(in_season_obs_df, end_season_obs_df, by=c("TRNO", "Date"))
  
  # Transform into CroptimizR format
  obs_list <- split(obs_df, f = obs_df$TRNO, lex.order = TRUE)
  obs_list <- lapply(obs_list, function(x) { dplyr::select(x,-TRNO)}) # remove column TRNO
    
  return(obs_list)
  
}
