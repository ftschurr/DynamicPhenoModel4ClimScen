# Author: Flavian Tschurr
# Project: KP030
# Date: 10.07.2023
# Purpose: multi env model:trait prediction functions
################################################################################



weightutils.pheno_phase_start_stop_extractor <- function(one_pheno_list,pheno_phase_ID,pheno_phase){
  start_id <- pheno_phase_ID$start_id[which(pheno_phase_ID$phenological_phase == pheno_phase)]
  end_id <- pheno_phase_ID$end_id[which(pheno_phase_ID$phenological_phase == pheno_phase)]
  
  start_date <- one_pheno_list$meta$phase_information$Eintrittsdatum[which(one_pheno_list$meta$phase_information$Phase_id == start_id)]
  end_date <- one_pheno_list$meta$phase_information$Eintrittsdatum[which(one_pheno_list$meta$phase_information$Phase_id == end_id)]
  out <- data.frame("phase_start_date"=start_date,"phase_end_date"=end_date)
  return(out)
}

# get correct parameters_method
weightutils.parameters_loader <- function(env_variable, pheno_phase,response_curve_type, granularity, crop_abbrev){
  #'@param env_variable name of the environmental variable (e.b. tas_2m)
  #'@param pheno_phase name of the phenological pahse
  #'@param response_curve_typename of the wanted response curve type e.g. "negative_quadratic"
  #'@param granularity environment measurements granularity
  #'@param crop_abbrev crop abbreviation

  # create output paths
  
  output_path_base <- file.path(getwd(),"output","parameter_model",response_curve_type, crop_abbrev)
  out_file_name <- paste0(response_curve_type,"_parameter_",pheno_phase,"_" , env_variable,"_",granularity,".rds")

  params = readRDS(file= file.path(output_path_base,out_file_name))
  
  params_out <- params$median_output[[paste(env_variable,response_curve_type,sep="-")]]
  names(params_out) <- params$median_output$meta$parameter_list

  
  
  return(params_out)
  
}



weightutils.pred_helper <- function(one_pheno_list, .response_function., parameters,weight_parameter , granularity){
  #'@description wrapper function to apply the response_prediction function in a "weighted" manner and do some gap filling of the env data
  source("scripts/functions/FUN_utils.R")
  if(granularity=="hourly"){
    gapfilling_length <- 110
  }else if(granularity =="daily"){
    gapfilling_length <- 4
    
  }
  one_pheno_list$data <- utils.gap_filling_linear(one_pheno_list$data,max_gap_length = gapfilling_length)
  
  if(length(which(is.na(one_pheno_list$data)))>=1){
    return(rep(NA, length(one_pheno_list$data)))
  }else{
    return(as.numeric(unlist(lapply(one_pheno_list$data,.response_function.,parameters)))*weight_parameter)
  }
}



weightutils.timestamp_extractor <- function(one_pheno_list){
  return(one_pheno_list$timestamp)
}

  

weightutils.prediction_cumulative_growth <- function(common_predictions_env, timestamps_list, pheno_phase_dates){
  
  # take the envs
  # write each env into a matrix
  # add
  # write back into list
  
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



weightutils.pheno_phase_length_determiner <- function(one_combined_prediction){
  phase_end_reached = 100
  # get the length until phase end is reached from the modelled (100)
  
  length_modelled <- which.min(abs(one_combined_prediction$growth_cumulative - phase_end_reached))
  length_actual <- which(one_combined_prediction$timestamp == one_combined_prediction$phenophase_start_stop$phase_end_date[1])
  if(length(length_actual)!=1){
    print("timestamp vector too short!")
    
    out_df = data.frame("phase_length_modelled"= NA,"phase_length_measured" = NA)
    
  }else if(length(which(is.na(one_combined_prediction$growth_cumulative)))> 15){
    print(paste0(length(which(is.na(one_combined_prediction$growth_cumulative))), " are NA's in the prediction!"))
    out_df = data.frame("phase_length_modelled"= NA,"phase_length_measured" = NA)
    
  }else{
    out_df = data.frame("phase_length_modelled"= length_modelled,"phase_length_measured" = length_actual)
    
  }
  
  return(out_df)
  
}



weightutils.highest_correlation_selection <- function(multi_env_skillscore){
  
  RMSEs <- NULL
  cors <- NULL
  MLLs <- NULL
  for(vari in 1:length(names(multi_env_skillscore))){
    skill_scores <- multi_env_skillscore[[vari]]$skill_scores
    RMSEs[vari] <- skill_scores$RMSE
    cors[vari] <- skill_scores$cor
    MLLs[vari] <- skill_scores$MLL
  }

  best_model <- names(multi_env_skillscore)[which.max(rank(cors,na.last=F))]
  
  return(best_model)
}



weightutils.prediction_skills_calculator_phenostage_cor <- function(measured,modelled){
  source("scripts/functions/FUN_skillscores.R")
  # calculate RMSE
  RMSE <- calc_RMSE(measured = measured, modelled = modelled)
  # calculate correlation
  cor <- calc_cor(measured = measured ,modelled = modelled)
  logLikeli <- calc_SumLogLikelihood(measured = measured, modelled = modelled)
  # calculate mean absolute error
  MAE <- calc_MAE(measured = measured, modelled = modelled )
  
  skill_scores <- list()
  skill_scores$RMSE <- RMSE
  skill_scores$cor <- cor
  skill_scores$MLL <- logLikeli
  skill_scores$MAE <- MAE
  # one_multi_env_prediction$skill_scores <- skill_scores
  
  
  return(skill_scores)
  
}





weightutils.visualize_response_cruve_fit_cor <- function(pheno_phase,env_variable,model_run,skillscores_resp_curves){
  # create output path and file name
  output_path_fig <- file.path(getwd(),"output","plots","response_curve_fit",model_run,pheno_phase)
  dir.create(output_path_fig,recursive = T, showWarnings = F)
  
  file_name <- paste0(env_variable,"_",pheno_phase,"_response_cruve_fit_",model_run,".png")
  
  # extract data
  skills <- data.frame(response_curves = NA, RMSE =NA, cor=NA ,MLL=NA, MAE=NA)
  
  for (i in 1:length(skillscores_resp_curves)) {

    skills[i,] <- c(names(skillscores_resp_curves)[i],as.numeric(skillscores_resp_curves[[i]]$skill_scores))
  }
  

  skills$response_curves <- as.factor(skills$response_curves)
  for(i in 2:length(names(skills))){
    skills[[i]] <- as.numeric(skills[[i]])
  }
  
  best_curve_nr <- which(levels(skills$response_curves) == weightutils.highest_correlation_selection(skillscores_resp_curves))
  
  
  
  png(file= paste(output_path_fig,file_name,sep="/"),units = "cm",width=20,height= 22,res=480)
  
  y_max <- max(skills[,2:4])

  layout(matrix(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5), nrow = 14, ncol = 1, byrow = TRUE))
  plot(skills$response_curves,skills$RMSE,type="l",main=paste0("phenological phase: ",pheno_phase, " | variable: ", env_variable), ylab="RMSE", xlab="",xaxt="n")
  abline(v=best_curve_nr)
  axis(labels=FALSE,side = 1)

  plot(skills$response_curves,skills$MAE,type="l", ylab="MAE", xlab="",xaxt="n")
  abline(v=best_curve_nr)
  axis(labels=FALSE,side = 1)

  plot(skills$response_curves,skills$MLL,type="l", ylab="Maximum Log Likelihood", xlab="",xaxt="n")
  abline(v=best_curve_nr)
  axis(labels=FALSE,side = 1)
  
  plot(skills$response_curves,skills$cor,type="l", ylab="correlation", xlab="",xaxt="n")
  abline(v=best_curve_nr)
  axis(labels=FALSE,side = 1)

  plot(as.numeric(skills$response_curves),skills$RMSE,xaxt="n",yaxt="n",bty ="n",xlab="",ylab="",col="white")
  axis(3, at = skills$response_curves,
       labels = as.character(skills$response_curves),
       lty="blank", cex.axis=0.9, las=2, line= -3)
  
  dev.off()
  
  
}


