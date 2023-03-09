# Function for reading DSSAT formatted observation and transforming into CroptimizR formatted ones

read_obs <- function(model_options, situation) {
#' @description This function read DSSAT observations files (both time series and end-of-season files),
#' and return a corresponding observation list in CroptimizR format.
#'
#' @param model_options List of options similar to the one given to DSSAT_wrapper.
#' THe following fields are mandatory to use this function:
#'   `DSSAT_path`: path to DSSAT directory
#'   `DSSAT_exe`: name of the model executable
#'   `Crop`: name of the crop (i.e. of subdirectory in DSSAT path)
#'   `ecotype_filename`: name of the ecotype filename
#'
#' @param situation Vector of situation names (EXPERIMENT_NAME_TRNO) for which 
#' observations must be returned.
#'
#' @importFrom stringr str_sub
#' @importFrom dplyr full_join relocate mutate select
#' @import tidyr 
#' 
#'
#' @examples
#'
#' model_options <- vector("list")
#' model_options$DSSAT_path <- "C:\\DSSAT48"
#' model_options$DSSAT_exe <-  "DSCSM048.EXE"
#' model_options$Crop <- "Wheat"
#' situation <- c("AQTB1101_2", "AQTB1101_10", "AQTB1201_5", "AQTB1201_15") 
#'
#' obs_list <- read_obs(model_options, situation <- situation)
#' CroptimizR:::is.obs(obs_list)
#'

  crop_code <- substr(model_options$ecotype_filename,1,2)
  situation_df <- setNames(as.data.frame(t(dplyr::bind_rows(sapply(situation,
                                                                   FUN = strsplit, 
                                                                   "_")))),
                             c("EXPERIMENT","TRNO"))
  obs_df <- NULL
  
  for (experiment in unique(situation_df$EXPERIMENT)) {
    
    filtered_situation_df <- filter(situation_df,EXPERIMENT==experiment)
    trno <- as.integer(filtered_situation_df$TRNO)
    
    file_name_a <- file.path(model_options$DSSAT_path,model_options$Crop,paste0(experiment,".",crop_code,"A"))
    file_name_t <- file.path(model_options$DSSAT_path,model_options$Crop,paste0(experiment,".",crop_code,"T"))
    
    if ( !file.exists(file_name_a) & !file.exists(file_name_t) ) {
      stop(paste("Files",file_name_a,"and",file_name_t,"do not exist.\n Please check file_name argument"))
    }
    
    # Handle time series data
    in_season_obs_df <- NULL
    if (file.exists(file_name_t)) {
      in_season_obs_df <- read_filet(file_name_t, na_strings = NA)
      in_season_obs_df <- in_season_obs_df %>% dplyr::mutate(Date=DATE) %>% 
        dplyr::select(-DATE) %>% dplyr::relocate(Date) %>% filter(TRNO %in% trno)
    }
    
    # Handle final data
    end_season_obs_df <- NULL
    if (file.exists(file_name_a)) {
      end_season_obs_df <- read_filea(file_name_a, na_strings = NA)
      end_season_obs_df <- filter(end_season_obs_df, TRNO %in% trno)
      # Set the Date of end-of-season obs to Mat Date or Harvest Date depending on which date is provided
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
    obs_df_tmp <- dplyr::full_join(in_season_obs_df, end_season_obs_df, by=c("TRNO", "Date"))
    
    # Add situation column and remove TRNO
    obs_df_tmp <- dplyr::mutate(obs_df_tmp, situation=paste0(experiment,"_",TRNO)) %>%
      dplyr::select(-TRNO)
    
    obs_df <- dplyr::bind_rows(obs_df, obs_df_tmp)
    
  }

  # Transform into CroptimizR format
  obs_list <- split(obs_df, f = obs_df$situation)
  obs_list <- lapply(obs_list, function(x) { dplyr::select(x,-situation)}) # remove column TRNO
  obs_list <- obs_list[situation]
  
  return(obs_list)
    
}
