# Author: Flavian Tschurr
# Project: KP030
# Date: 04.09.2023
# Purpose: phase prediction
################################################################################
euler_p <-"/nfs/nas12/fs1202/green_groups_kp_public/"
windows_p <- "P:"
# set paths
euler = F

if(euler == T){
  setwd(file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
}else{
  setwd(file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
  
}

rdsnx = F
################################################################################
# load packages & functions
################################################################################
library(parallel)
library(ggplot2)
source("scripts/00_variable_settings.R")
source("scripts/functions/FUN_dose_response_curves.R")
source("scripts/functions/FUN_GLM_prediction.R")

################################################################################
# allocate cores / core number
if(rdsnx == T){
  cores_percent = 0.75
}else{
  cores_percent = 0.5
}
numCores <- round(detectCores()*cores_percent)
if(euler ==T){
  numCores <- 25
}

################################################################################
################################################################################
################################################################################
for(pheno_phase in pheno_phases){
  print(pheno_phase)
  
  # initaite empty lists
  predictions <- list()
  timestamps_list <- list()
  
  
  # load meta inforamtion for phenological pahses 
  pheno_phase_ID <- read.csv(file.path(getwd(),meta_data_path,"phenological_phases_IDs_DWD.csv"))
  
  
  cl <- makePSOCKcluster(numCores)
  
  for(env_variable in env_variables){
    print(env_variable)
    
    path_best_curve <- file.path(getwd(),"output","paramter_best_response_curve",model_run)
    file_name_best <- paste0(env_variable,"_",granularity,"_",pheno_phase,"_best_response_curve_type.rds")
    best_curve <- readRDS(file.path(path_best_curve,file_name_best))
    
    # create file name
    data_file_name <- paste0(paste(env_variable,granularity,"validation_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")
    
    # laod file
    pheno_list <- readRDS(file.path(data_path_validation,data_file_name))
    # clean envs with too many NA's
    pheno_list <- glmpred.data_cleaner(pheno_list = pheno_list, granularity = granularity)
    
    .response_function. <- get(paste0(best_curve$response_curve_type,"_prediction"))
    parameters <- best_curve$parameters$curve
    
    
    predictions[[env_variable]][["modelled"]]<- parLapplyLB(cl,pheno_list, glmpred.prediction_lapplier, .response_function., parameters)
    
    predictions[[env_variable]][["measured"]] <- rep(100,length(pheno_list))
    predictions[[env_variable]][["response_curve"]][["parameters"]] <- parameters
    predictions[[env_variable]][["response_curve"]][["type"]] <- best_curve$response_curve_type
    predictions[[env_variable]][["env_names"]] <- names(pheno_list)
    
    # extract timestamps
    timestamps_list[[env_variable]] <- lapply(pheno_list, glmpred.timestamp_extractor)
    
  }
  
  print("DRCs calcualted")

  pheno_phase_dates <- list()
  pheno_phase_dates <- parLapplyLB(cl,pheno_list, glmpred.pheno_phase_start_stop_extractor,pheno_phase_ID = pheno_phase_ID,pheno_phase = pheno_phase)
  print("dates extracted")

  stopCluster(cl)
  
  
  ####################################################################################
  ####################################################################################
  # calculate glm one to every variable us all combinations
  ####################################################################################
  ####################################################################################
  
  # create list with all potential models to fit
  model_fit_list <- list()
  counter <- 1
  for(i in 1:length(env_variables)){
    combos <- combn(env_variables,i,unique=T)
    for(cols in 1:dim(combos)[2]){
      model_fit_list[[counter]] <- combos[,cols]
      counter <- counter + 1
    }
  }
  
  ##################
  ############
  ######

  pheno_phase_prediction_glm <- function(one_variables_list, predictions, pheno_phase, model_run, timestamps_list, pheno_phase_dates ){
    source("scripts/functions/FUN_skillscores.R")
    # source("scripts/functions/FUN_LassoHelper.R")
    source("scripts/functions/FUN_GLM_prediction.R")
    
    
    ##############################################################################
    # create correct model input filename
    ##############################################################################
    input_path_dir <- paste(getwd(),"output/parameter_multi_env_model",model_run,sep="/" )
    all_files <- list.files(input_path_dir)
    # select correct file
    patterns <- c("dymenv_glm_params_",pheno_phase,paste0("NrEnvVariables_",length(unlist(one_variables_list))), model_run,paste0(one_variables_list,"_") )
    for(pat in patterns){
      all_files <- all_files[grep(pattern = pat,all_files)]
    }
    file_in_name <- all_files
    
    ####################################################################################
    # subset data
    ####################################################################################
    predictions_sub <- predictions[which(names(predictions) %in% unlist(one_variables_list))]
    common_predictions <- glmpred.prediction_matching_envs_finder(predictions = predictions_sub)
    
    common_predictions <- lapply(common_predictions, 
                                 glmpred.prediction_cumulative_growth,
                                 timestamps_list = timestamps_list,
                                 pheno_phase_dates = pheno_phase_dates)
    
    combined_envs <- glmpred.data_frame_creator(common_predictions = common_predictions,
                                                  pheno_phase_dates = pheno_phase_dates)
    
    
    
    ##############################################################################
    # load correct  model
    ##############################################################################
    model <- readRDS(paste(input_path_dir, file_in_name,sep="/" ))
    
    
    # predict, handing in the fitted glm  and using the predict function
    combined_envs$prediction <- predict.glm(model$fitted_glm, combined_envs)
    combined_envs$stage_prediction <- ifelse( combined_envs$prediction <= as.numeric(model$coefficients$threshold_phase),0,1)
    
    
    combined_env_correlation <- data.frame("env"=NA, "measurement" = NA, "prediction" = NA)
    counter <- 1
    for(en in unique(combined_envs$env)){
      one_env <- subset(combined_envs, env == en)
      length_measurement <- length(which(one_env$response == 0))
      length_mod <- length(which(one_env$stage_prediction == 0))
      combined_env_correlation[counter,] <- c(en, length_measurement, length_mod)
      counter <- counter + 1
      
    }
    combined_env_correlation$measurement <- as.numeric(combined_env_correlation$measurement)
    combined_env_correlation$prediction <- as.numeric(combined_env_correlation$prediction)

    
    COR <- calc_cor(combined_env_correlation$measurement, combined_env_correlation$prediction)
    RMSE <- calc_RMSE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
    MAE <- calc_MAE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
    LML <- calc_SumLogLikelihood(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
    RRMSE <- calc_RRMSE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
  
    skillscores <- data.frame("cor"= COR,
                              "RMSE" = RMSE,
                              "RRMSE" = RRMSE,
                              "MAE" = MAE,
                              "SumLogLikelihood" = LML,
                              "AIC" = model$fitted_glm$aic,
                              "nr_of_variables"=  length(unlist(one_variables_list)),
                              "env_variables" = paste(one_variables_list,collapse="_"),
                              "used_envs"= length(combined_env_correlation$prediction))

    
    return(skillscores)
    
    
  }
  
  
  ####################################################################################
  # calculate validation
  ####################################################################################
  
  start_time <- Sys.time()
  cl <- makePSOCKcluster(numCores)
  glm_skillcores_validation <- list()
  glm_skillcores_validation <- parLapplyLB(cl,
                                           model_fit_list,
                                           pheno_phase_prediction_glm,
                                           predictions, 
                                           pheno_phase, 
                                           model_run,
                                           timestamps_list,
                                           pheno_phase_dates)
  
  stopCluster(cl)
  
  print(Sys.time()-start_time)
  glm_skillcores_df <-as.data.frame(do.call("rbind", glm_skillcores_validation))
  
  for(i in c(1:6)){
    glm_skillcores_df[,i] <- as.numeric(glm_skillcores_df[,i])
  }
  
  ####################################################################################
  # save validation scores file
  ####################################################################################
  out_path_validation_csv <- file.path(getwd(),"output/validation",model_run)
  dir.create(out_path_validation_csv, recursive = T, showWarnings = F)
  file_name_csv <- paste0(paste("dymenv_validation_skillscores",pheno_phase,model_run,sep="_"),".csv")
  write.csv(glm_skillcores_df, file.path(out_path_validation_csv,file_name_csv),sep=",", row.names = F)
  
  ####################################################################################
  # select overall best model
  ####################################################################################
  used_skillscores = c("RMSE")
  
  best <- glmpred.skillscore_ranker(df_skillscores = glm_skillcores_df, names_skillscores = used_skillscores)
  
  env_variables_best <- unlist(strsplit(best$env_variables,"_"))
  if(length(grep(pattern = "global",env_variables_best))==1){
    env_variables_best <- env_variables_best[-c(grep(pattern = "global",env_variables_best),(grep(pattern = "global",env_variables_best)+1))]
    env_variables_best <- c(env_variables_best, "global_radiation")
  }
  skillscores_validation = best[,used_skillscores]
  glmpred.model_save(skills_validation = skillscores_validation,
                       model_run = model_run, 
                       env_variables = env_variables_best, 
                       pheno_phase = pheno_phase, 
                       model_type="BestModel")
  ####################################################################################
  # get overview
  ####################################################################################
  # all_best_skillscores <- glmpred.skillscore_ranker(df_skillscores = glm_skillcores_df, names_skillscores = c("cor","RMSE","MAE","AIC","SumLogLikelihood"))
  all_best_skillscores <- glmpred.skillscore_ranker(df_skillscores = glm_skillcores_df, names_skillscores = used_skillscores)
  
  out_path_plot <- file.path(getwd(),"output/plots/validation_overview",model_run)
  dir.create(out_path_plot, recursive = T, showWarnings = F)
  plot_name <- paste("dymenv_validaiton_overview",pheno_phase,model_run,sep="_")
  
  pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 7, height =7)
  overview_plot <- glmpred.skillscore_visualizer(glm_skillcores_df,pheno_phase, all_best_skillscores, best$nr_of_variables)
  dev.off()
  
  png(file.path(out_path_plot,paste0(plot_name,".png")),width= 7, height =7, res = 450, units = "in")
  overview_plot <- glmpred.skillscore_visualizer(glm_skillcores_df,pheno_phase,all_best_skillscores,best$nr_of_variables)
  dev.off()
  
  print(glmpred.skillscore_visualizer(glm_skillcores_df = glm_skillcores_df,
                                      pheno_phase = pheno_phase,
                                      skillscores_best = all_best_skillscores, 
                                      nr_of_variables_best = best$nr_of_variables))
  ####################################################################################
  # select per complexity best model
  ####################################################################################
  
  for(compl in 1:length(env_variables)){
    oneComplexity_skillscores <- subset(glm_skillcores_df, nr_of_variables == compl)
    bestOneComplexity <- glmpred.skillscore_ranker(df_skillscores = oneComplexity_skillscores, names_skillscores = used_skillscores)
    print(bestOneComplexity)
    env_variables_best <- unlist(strsplit(bestOneComplexity$env_variables,"_"))
    if(length(grep(pattern = "global",env_variables_best))==1){
      env_variables_best <- env_variables_best[-c(grep(pattern = "global",env_variables_best),(grep(pattern = "global",env_variables_best)+1))]
      env_variables_best <- c(env_variables_best, "global_radiation")
    }
    skillscores_validation = bestOneComplexity[,used_skillscores]
    glmpred.model_save(skills_validation = skillscores_validation,
                         model_run = model_run, 
                         env_variables = env_variables_best, 
                         pheno_phase = pheno_phase, 
                         model_type="NrEnvVariables")
    
  }
  
  
}

