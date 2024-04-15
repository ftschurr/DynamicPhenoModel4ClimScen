# Author: Flavian Tschurr
# Project: KP030
# Date: 16.03.2023
# Purpose: dymenvmodel: apply to CH2018 data helper functions
################################################################################

envpredutils.period_date_creator <- function(start_date, period_length){
  return(seq(as.Date(start_date), (as.Date(start_date)+period_length), by = "days"))
}


envpredutils.env_period_cutter <- function(start_date, env_variables, env_data, period_length= 100){
  out_list <- list()
  out_list[["timestamps"]] <- envpredutils.period_date_creator(start_date,period_length )
  for(env_vari in env_variables){
    timestamp_vect <- as.Date(env_data[[env_vari]]$DATE)
    out_list[[env_vari]] <- env_data[[env_vari]]$VALUE[which(timestamp_vect >= as.Date(start_date) & timestamp_vect <= (as.Date(start_date) + period_length))]

    if(length( out_list[["timestamps"]]) != length( out_list[[env_vari]] )) {
      rm(out_list)
      out_list <- NA
      next
    }
  }
  return(out_list)

}



envpredutils.cumulative_dose_response_pred_helper <- function(env_data_vector, .response_function., parameters){
  #'@description wrapper function to apply the response_prediction function in a "weighted" manner and do some gap filling of the env data

  if(length(which(is.na(env_data_vector)))>=1){
    return(rep(NA, length(env_data_vector)))
  }else{
    return(cumsum(as.numeric(unlist(lapply(env_data_vector,.response_function.,parameters)))))
  }

}

# envpredutils.GLM_prediction_df_creator <- function(resposne_predictions, timestamp_vect){
# 
#   one_df <- matrix(data=NA, ncol = (length(names(resposne_predictions))+1), nrow = length(timestamp_vect))
#   counter <- 1
# 
#   for(env_vari in names(resposne_predictions)){
# 
#     one_df[,counter] <- resposne_predictions[[env_vari]][["growth_cumulative"]]
#     counter <- counter +1
#   }
#   one_df[,counter] <- as.character(timestamp_vect)
# 
#   colnames(one_df) <- c(names(resposne_predictions),"timestamp")
# 
#   out_df <-as.data.frame(one_df)
# 
#   for(r in which(names(out_df) %in% names(resposne_predictions))){
#     out_df[,r] <- as.numeric(out_df[,r])
#   }
#   browser()
#   out_df$timestamp <- as.Date(out_df$timestamp)
# 
#   return(out_df)
# }


envpredutils.GLM_prediction_df_creator <- function(resposne_predictions, timestamp_vect){
  require(lubridate)
  # Extract growth_cumulative values from response_predictions
  data_matrix <- sapply(resposne_predictions, function(x) x$growth_cumulative)

  # Create a dataframe with growth_cumulative values and timestamp
  out_df <- data.frame(data_matrix, timestamp = as.character(timestamp_vect))

  # Convert columns to numeric except for the timestamp column
  out_df[, -ncol(out_df)] <- unlist(lapply(out_df[, -ncol(out_df)], as.numeric))

  # Convert timestamp column to Date class using lubridate::ymd()
  out_df$timestamp <- lubridate::ymd(out_df$timestamp)
  return(out_df)
}



#################################################################################

envpredutils.pheno_phase_prediction_glm_model <- function( env_data_pheno_phase, pheno_phase, model_run, NrEnvVariables ){

  # source("scripts/functions/FUN_response_multi_env.R")
  source("scripts/functions/FUN_unknown_environment_prediction_utils.R")
  source("scripts/functions/FUN_dose_response_curves.R")
  ##############################################################################
  # create correct model input file name
  ##############################################################################
  input_path_dir <- file.path(getwd(),"output/best_multi_env_model",model_run)
  file_in_name <- list.files(input_path_dir)[grep(paste("dymenv",pheno_phase,"NrEnvVariables",NrEnvVariables,sep="_"), list.files(input_path_dir))]
  ##############################################################################
  # load correct  model
  ##############################################################################
  model <- readRDS(paste(input_path_dir, file_in_name,sep="/" ))
  ####################################################################################
  # get needed variables
  ####################################################################################
  env_variables_model <- names(model$coefficients$env_parameters)[-1]
  predictions <- list()

  # browser()
  # for(env_vari_model in env_variables_model){
  iterator_fun <- function(env_vari_model){
    out_list <- list()
    ####################################################################################
    # load .response_functions.
    ####################################################################################
    granularity = "daily"
    path_best_curve <- file.path(getwd(),"output","paramter_best_response_curve",model_run)
    file_name_best <- paste0(env_vari_model,"_",granularity,"_",pheno_phase,"_best_response_curve_type.rds")
    best_curve <- readRDS(paste(path_best_curve,file_name_best,sep="/"))


    ## add selection of just trainings data maybe

    .response_function. <- get(paste0(best_curve$response_curve_type,"_prediction"))
    parameters <- best_curve$parameters$curve

    ####################################################################################
    # subset data
    ####################################################################################

    if(length(which(is.na(env_data_pheno_phase[[env_vari_model]])) ==T) > 1){
      return(NA)
    }else{
      # predictions[[env_vari_model]][["growth_cumulative"]]<- envpredutils.cumulative_dose_response_pred_helper(env_data_pheno_phase[[env_vari_model]],.response_function., parameters)
      out_list[["growth_cumulative"]] <- envpredutils.cumulative_dose_response_pred_helper(env_data_pheno_phase[[env_vari_model]],.response_function., parameters)
      return(out_list)
      
      }


  }
  
  predictions <- lapply(env_variables_model, iterator_fun)
  names(predictions)<- env_variables_model
  if(any(is.na(predictions))){
    return(NA)
  }

  combined_envs <- envpredutils.GLM_prediction_df_creator(resposne_predictions = predictions,
                                                          timestamp_vect = env_data_pheno_phase[["timestamps"]])

  # predict, handing in the fitted glm  and using the predict function
  combined_envs$prediction <- predict.glm(model$fitted_glm, combined_envs)
  combined_envs$stage_prediction <- ifelse(combined_envs$prediction <= as.numeric(model$coefficients$threshold_phase),0,1)
  

  # find end date
  end_date <- as.character(combined_envs$timestamp[max(which(combined_envs$stage_prediction == 0))])
  # return end date
  return(end_date)

}




#################################################################################

envpredutils.pheno_phase_prediction_glm_model_fixed_covariates <- function( env_data_pheno_phase, pheno_phase, model_run, Used_Env_Variables ){
  
  source("scripts/functions/FUN_unknown_environment_prediction_utils.R")
  source("scripts/functions/FUN_dose_response_curves.R")
  require(fs)
  ##############################################################################
  # create correct model input file name
  ##############################################################################
  input_path_dir <- file.path(getwd(),"output/parameter_multi_env_model",model_run)
  NrEnvVariables <- length(unlist(strsplit(gsub("global_radiation","GR",Used_Env_Variables),split="_")))
  # Define the pattern to match file names
  pattern <- paste("dymenv_glm_params", pheno_phase, "NrEnvVariables", NrEnvVariables, Used_Env_Variables, sep = "_")

  # List files matching the pattern
  file_in_name <- list.files(input_path_dir, pattern = pattern, full.names = FALSE)


  # file_in_name <- dir_ls(input_path_dir, regex = pattern)

  
  ##
  # input_path_dir <- file.path(getwd(),"output/parameter_multi_env_model",model_run)
  # NrEnvVariables <- length(unlist(strsplit(as.character(Used_Env_Variables),split = "_")))
  # if(grepl("global_radiation",Used_Env_Variables)){
  #   NrEnvVariables = NrEnvVariables-1
  # }
  # file_in_name <- list.files(input_path_dir)[grep(paste("dymenv_glm_params",pheno_phase,"NrEnvVariables",NrEnvVariables,Used_Env_Variables,sep="_"), list.files(input_path_dir))]
  ##############################################################################
  # load correct  model
  temp_dir <- file.path(getwd(),"output","temp",paste0(format(Sys.time(),"%Y-%m-%d_%H-%M-%S"),"_",paste(sample(1:100, 3),collapse="_")))
  dir.create(temp_dir, recursive = T,showWarnings = F)
  file.copy(from=file.path(input_path_dir, file_in_name),
            to=file.path(temp_dir,file_in_name))
  
 
  ##############################################################################
  # model <- readRDS(paste(input_path_dir, file_in_name,sep="/" ))
  model <- readRDS(paste(temp_dir, file_in_name,sep="/" ))
  

  ####################################################################################
  # get needed variables
  ####################################################################################
  env_variables_model <- names(model$coefficients$env_parameters)[-1]
  predictions <- list()
  iterator_fun <- function(env_vari_model){
    out_list <- list()
  # for(env_vari_model in env_variables_model){
    ####################################################################################
    # load .response_functions.
    ####################################################################################
    granularity = "daily"
    path_best_curve <- file.path(getwd(),"output","paramter_best_response_curve",model_run)
    file_name_best <- paste0(env_vari_model,"_",granularity,"_",pheno_phase,"_best_response_curve_type.rds")
    
    file.copy(from=file.path(path_best_curve, file_name_best),
              to=file.path(temp_dir,file_name_best))
    
    # best_curve <- readRDS(paste(path_best_curve,file_name_best,sep="/"))
    best_curve <- readRDS(paste(temp_dir,file_name_best,sep="/"))
    
    
    ## add selection of just trainings data maybe
    
    .response_function. <- get(paste0(best_curve$response_curve_type,"_prediction"))
    parameters <- best_curve$parameters$curve
    
    ####################################################################################
    # subset data
    ####################################################################################
    
    if(length(which(is.na(env_data_pheno_phase[[env_vari_model]])) ==T) > 1){
      return(NA)
    }else{
      # predictions[[env_vari_model]][["growth_cumulative"]]<- envpredutils.cumulative_dose_response_pred_helper(env_data_pheno_phase[[env_vari_model]],.response_function., parameters)
      out_list[["growth_cumulative"]] <- envpredutils.cumulative_dose_response_pred_helper(env_data_pheno_phase[[env_vari_model]],.response_function., parameters)
      return(out_list)
    }
      
    
    
  }
  predictions <- lapply(env_variables_model, iterator_fun)
  names(predictions)<- env_variables_model
  if(any(is.na(predictions))){
    return(NA)
  }

  
  unlink(temp_dir,recursive = T)
  combined_envs <- envpredutils.GLM_prediction_df_creator(resposne_predictions = predictions,
                                                          timestamp_vect = env_data_pheno_phase[["timestamps"]])
  
  
  
  
  # predict, handing in the fitted glm  and using the predict function
  combined_envs$prediction <- predict.glm(model$fitted_glm, combined_envs)
  combined_envs$stage_prediction <- ifelse(combined_envs$prediction <= as.numeric(model$coefficients$threshold_phase),0,1)

  
  # find end date
  end_date <- as.character(combined_envs$timestamp[max(which(combined_envs$stage_prediction == 0))])
  # return end date
  return(end_date)
  
}

