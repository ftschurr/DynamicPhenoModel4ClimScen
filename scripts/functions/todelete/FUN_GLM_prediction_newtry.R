# Author: Flavian Tschurr
# Project: KP030
# Date: 28.07.2023
# Purpose: glm phase prediction helper functions
################################################################################


glmpred.data_cleaner <- function(pheno_list,granularity){
  source("scripts/functions/FUN_utils.R")
  
  if(granularity=="hourly"){
    gapfilling_length <- 110
  }else if(granularity =="daily"){
    gapfilling_length <- 4
    
  }
  
  to_delete <- NULL
  counter <- 1
  for(i in 1:length(pheno_list)){
    one_pheno_list <- pheno_list[[i]]
    one_pheno_list$data <- utils.gap_filling_linear(one_pheno_list$data,max_gap_length = gapfilling_length)
    if(length(which(is.na(one_pheno_list$data)==T))>0){
      browser()
      to_delete[counter] <- names(pheno_list)[i]
      counter <- counter + 1
      
    }
  }
  pheno_list <- pheno_list[names(pheno_list) %in% to_delete == F]
  return(pheno_list)
}


glmpred.prediction_lapplier <- function(one_pheno_list,.response_function.,parameters){
  
  return(as.numeric(unlist(lapply(one_pheno_list$data,.response_function.,parameters))))
  
}


glmpred.timestamp_extractor <- function(one_pheno_list){
  return(one_pheno_list$timestamp)
}




glmpred.prediction_matching_envs_finder <- function(predictions){
  predictions_common_envs <- list()
  
  target_envs <- glmpred.target_envs_finder(predictions = predictions)
  
  for(pred in names(predictions)){
    wanted <- which(predictions[[pred]]$env_names %in% target_envs)
    predictions_common_envs[[pred]]$modelled <- predictions[[pred]]$modelled[wanted]
    predictions_common_envs[[pred]]$measured <- predictions[[pred]]$measured[wanted]
    predictions_common_envs[[pred]]$response_curve <- predictions[[pred]]$response_curve
    predictions_common_envs[[pred]]$env_names <- predictions[[pred]]$env_names[wanted]
    predictions_common_envs[[pred]]$env_variable <- pred
    
  }
  return(predictions_common_envs)
}



glmpred.target_envs_finder <- function(predictions){
  #put all envs in a vector, count there occurences and take the ones which are in every variable, return the names of them
  all_envs <- NULL
  for (pred in names(predictions)) {
    all_envs <- c(all_envs,as.character(predictions[[pred]]$env_names))
  }
  
  occurences <- table(all_envs)
  target_envs <- names(which(occurences == length(names(predictions))))
  return(target_envs)
  
}

glmpred.pheno_phase_start_stop_extractor <- function(one_pheno_list,pheno_phase_ID,pheno_phase){
  start_id <- pheno_phase_ID$start_id[which(pheno_phase_ID$phenological_phase == pheno_phase)]
  end_id <- pheno_phase_ID$end_id[which(pheno_phase_ID$phenological_phase == pheno_phase)]
  
  start_date <- one_pheno_list$meta$phase_information$Eintrittsdatum[which(one_pheno_list$meta$phase_information$Phase_id == start_id)]
  end_date <- one_pheno_list$meta$phase_information$Eintrittsdatum[which(one_pheno_list$meta$phase_information$Phase_id == end_id)]
  out <- data.frame("phase_start_date"=start_date,"phase_end_date"=end_date)
  return(out)
}



glmpred.prediction_cumulative_growth <- function(common_predictions_env, timestamps_list, pheno_phase_dates){
  
  
  out_list <- list()
  for (env in common_predictions_env$env_names) {
    one_env <- common_predictions_env[["modelled"]][[env]]
    
    out_list[[env]][["growth_modelled"]] <- one_env
    out_list[[env]][["growth_cumulative"]] <- cumsum(one_env)
    out_list[[env]][["timestamp"]] <- timestamps_list[[common_predictions_env$env_variable]][[env]]
    out_list[[env]][["phenophase_start_stop"]] <- pheno_phase_dates[[env]]
    
  }
  return(out_list)
}



glmpred.prediction_matching_envs_finder <- function(predictions){
  predictions_common_envs <- list()
  
  target_envs <- glmpred.target_envs_finder(predictions = predictions)
  
  for(pred in names(predictions)){
    wanted <- which(predictions[[pred]]$env_names %in% target_envs)
    predictions_common_envs[[pred]]$modelled <- predictions[[pred]]$modelled[wanted]
    predictions_common_envs[[pred]]$measured <- predictions[[pred]]$measured[wanted]
    predictions_common_envs[[pred]]$response_curve <- predictions[[pred]]$response_curve
    predictions_common_envs[[pred]]$env_names <- predictions[[pred]]$env_names[wanted]
    predictions_common_envs[[pred]]$env_variable <- pred
    
  }
  return(predictions_common_envs)
  
  
}



glmpred.prediction_cumulative_growth <- function(common_predictions_env, timestamps_list, pheno_phase_dates){
  
  
  out_list <- list()
  for (env in common_predictions_env$env_names) {
    one_env <- common_predictions_env[["modelled"]][[env]]
    
    out_list[[env]][["growth_modelled"]] <- one_env
    out_list[[env]][["growth_cumulative"]] <- cumsum(one_env)
    out_list[[env]][["timestamp"]] <- timestamps_list[[common_predictions_env$env_variable]][[env]]
    out_list[[env]][["phenophase_start_stop"]] <- pheno_phase_dates[[env]]
    
  }
  return(out_list)
}




glmpred.data_frame_creator <- function(common_predictions,pheno_phase_dates){
  common_in_df <- list()
  for(env in names(common_predictions[[1]])){
    one_df <- matrix(data=NA, ncol = (length(names(common_predictions))+5), nrow = length(common_predictions[[1]][[env]]$timestamp)) 
    if(length(pheno_phase_dates[[env]]$phase_end_date[1])== 0){
      next
    }
    stage <- ifelse(common_predictions[[1]][[env]]$timestamp <= pheno_phase_dates[[env]]$phase_end_date[1], 0,1)
    one_df[,1] <- stage
    checker <- NULL
    check_counter <- 1
    for(env_vari in names(common_predictions)){
      checker[check_counter] <- length(common_predictions[[env_vari]][[env]][["growth_cumulative"]])
      check_counter <- check_counter +1
    }
    
    if(length(unique(checker))!= 1){
      next
    }
    counter <- 2
    for(env_vari in names(common_predictions)){
      one_df[,counter] <- common_predictions[[env_vari]][[env]][["growth_cumulative"]]
      counter <- counter +1
    }
    one_df[,(counter )] <- as.character(common_predictions[[1]][[env]]$timestamp)
    one_df[,(counter + 1)] <- unlist(strsplit(env, "_"))[1]
    one_df[,(counter + 2)] <- unlist(strsplit(env, "_"))[2]
    one_df[,(counter + 3)] <- c(1:length(as.character(common_predictions[[1]][[env]]$timestamp)))
    one_df <- as.data.frame(one_df)
    colnames(one_df) <- c("response",names(common_predictions),"timestamp", "location.ID","harvest.year","phase_index")  
    
    
    common_in_df[[env]] <- one_df
  }
  
  combined_envs <- do.call("rbind",common_in_df)
  # for(r in c(1:8,12)){
  for(r in which(names(combined_envs) %in% c("response",names(common_predictions),"phase_index"))){
    combined_envs[,r] <- as.numeric(combined_envs[,r])
  }
  # for(r in c(10:11)){
  for(r in which(names(combined_envs) %in% c("location.ID","harvest.year"))){
    combined_envs[,r] <- as.factor(combined_envs[,r])
  }
  
  combined_envs$env <- as.factor(paste(combined_envs$location.ID,combined_envs$harvest.year,sep="_"))
  
  return(combined_envs)
}




