# Author: Flavian Tschurr
# Project: KP030
# Date: 20.06.2022
# Purpose: dymenvmodel: predict phenology and optimize combination using multiple environmental covariates and tack them to one compelte model and optimize weight
################################################################################
euler_p <-"/nfs/nas12/fs1202/green_groups_kp_public/"
windows_p <- "P:"
# set paths
euler = F
if(euler== T){
  setwd(file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
}else{
  setwd(file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
  
}



  
# data_path_prediction <- file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_training_with_JRC")
#   
# 
# meta_data_path <- "meta/phenological_phases"

################################################################################
# load packages & functions
################################################################################
library(lubridate)
library(parallel)
source("scripts/00_variable_settings.R")
source("scripts/functions/FUN_utils.R")
################################################################################
# set parameter
################################################################################

# pheno_phases <- c("sowing-emergence","emergence-booting",  "booting-heading","heading-senescence")
# 
# 
# # environmental covariates
# env_variable_DRCs_inputs <- list("tas" = c("WangEngels"),
#                                  "tasmin" = c("WangEngels"),
#                                  "tasmax" = c("WangEngels"),
#                                  "RH" = c("reg_linear","non_linear", "asymptotic","WangEngels"),
#                                  "global_radiation" =  c("WangEngels"), # too high global radiation may lead to damage
#                                  "SPI"=c("reg_linear","non_linear", "asymptotic","WangEngels"),
#                                  "VPD" =  c("reg_linear","non_linear", "asymptotic","WangEngels") )
# 
# env_variables <- names(env_variable_DRCs_inputs)

combinations_list <- utils.pheno_phase_env_vari_DRC_list_combiner(pheno_phases= pheno_phases,
                                                                  env_variable_DRCs_inputs = env_variable_DRCs_inputs,
                                                                  resample_vector = c(1:20)
)

comb_parallel_list <- utils.pheno_phase_env_vari_list_combiner(pheno_phases = pheno_phases,
                                                               env_variable_DRCs_inputs = env_variable_DRCs_inputs)

# granularity <- "daily"
# crop_abbrev <- "WW"
# 
# # name of the model_run
# model_run <- "run_WW1"

numCores <- min(detectCores(),length(comb_parallel_list))


#################################################################################

response_curve_selecter <- function(one_comb, combinations_list,data_path, meta_data_path,granularity,crop_abbrev,model_run){
  source("scripts/functions/FUN_parameter_weighting_utils.R")
  source("scripts/functions/FUN_dose_response_curves.R")
  
  
  combinations_df <- do.call("rbind",combinations_list)
  env_variable <- one_comb$env_variable
  pheno_phase <- one_comb$pheno_phase
  response_curve_types <-   unique(subset(combinations_df, env_variable == one_comb$env_variable & pheno_phase == one_comb$pheno_phase)$response_curve_type)

  #load meta data
  pheno_phase_ID <- read.csv(paste(getwd(),meta_data_path,"phenological_phases_IDs_DWD.csv",sep="/"))
  
  # create file name
  data_file_name <- paste0(paste(env_variable,granularity,"training_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")
  
  # load file
  pheno_list <- readRDS(paste(data_path,data_file_name,sep="/"))
  
  pheno_phase_dates <- list()
  pheno_phase_dates <- lapply(pheno_list, weightutils.pheno_phase_start_stop_extractor,pheno_phase_ID = pheno_phase_ID,pheno_phase = pheno_phase)
  
  predictions <- list()
  timestamps_list <- list()
  common_predictions <- list()
  multi_env_skillscore <- list()
  ##
  for(response_curve_type in response_curve_types){
    print(paste(response_curve_type,env_variable,sep=" "))
    
    # catch response function
    .response_function. <- get(paste0(response_curve_type,"_prediction"))
    
    
    parameters <-  weightutils.parameters_loader(env_variable = env_variable,
                                     pheno_phase =  pheno_phase,
                                     response_curve_type = response_curve_type,
                                     granularity = granularity,
                                     crop_abbrev = crop_abbrev)
    
  
    predictions[[response_curve_type]][["modelled"]]<- lapply(pheno_list,
                                                              weightutils.pred_helper,
                                                                   .response_function.=.response_function.,
                                                                   parameters = parameters,
                                                                   granularity = granularity,
                                                                   weight_parameter=1)
    print("prediction done")

    
    predictions[[response_curve_type]][["env_names"]] <- names(pheno_list)
    predictions[[response_curve_type]][["env_variable"]] <- response_curve_type
    timestamps_list[[response_curve_type]] <- lapply(pheno_list, weightutils.timestamp_extractor)
    common_predictions[[response_curve_type]] <- weightutils.prediction_cumulative_growth(predictions[[response_curve_type]],
                                                                              timestamps_list = timestamps_list,
                                                                              pheno_phase_dates = pheno_phase_dates)
    print("cumulative response calcualted")
    
    phase_lengths <- lapply(common_predictions[[response_curve_type]], weightutils.pheno_phase_length_determiner)
    
 
    phase_lengths_measured <- na.omit(as.numeric(unlist(lapply(phase_lengths, '[[',"phase_length_measured"))))
    phase_lengths_modelled <-as.numeric(unlist(lapply(phase_lengths, '[[',"phase_length_modelled")))
    
    print("phase length calculated")
    
    ##
    # approximate growing phase if too short time period was used for the scaling factor --> estimate linearly in days when 100 is reached
    max_length_reached_counter <- 1
    counter <- 1
    # for(i in 1:length(common_predictions[[response_curve_type]])){
    for(i in names(common_predictions[[response_curve_type]])){
      
      if(i %in% names(phase_lengths)){
        if(!is.na(phase_lengths[[i]]$phase_length_measured)){
          if(phase_lengths[[i]]$phase_length_modelled == length(common_predictions[[response_curve_type]][[i]]$growth_cumulative)){
            phase_lengths_modelled[[counter]] <- phase_lengths_modelled[[counter]] + phase_lengths_modelled[[counter]]*((100-(max(common_predictions[[response_curve_type]][[i]]$growth_cumulative)))/100)
            max_length_reached_counter <- max_length_reached_counter + 1
          }
        }
        
        counter <- counter + 1
      }
    }
    
    # phase_lengths_measured <- na.omit(phase_lengths_measured)
    phase_lengths_modelled <- na.omit(phase_lengths_modelled)
    
    
    print("storing results into list")
    ##################################################################################################
    
    
    common_predictions[[response_curve_type]][["phase_length_measured"]] <- phase_lengths_measured
    common_predictions[[response_curve_type]][["phase_length_modelled"]] <- phase_lengths_modelled
    common_predictions[[response_curve_type]][["parameters"]][["curve"]] <- parameters
    common_predictions[[response_curve_type]][["parameters"]][["weight"]] <- 1
    common_predictions[[response_curve_type]][["response_curve_type"]] <- response_curve_type
    
    print("written to list")
    
    multi_env_skillscore[[response_curve_type]][["skill_scores"]] <- weightutils.prediction_skills_calculator_phenostage_cor(measured = phase_lengths_measured,
                                                                                                                 modelled = phase_lengths_modelled)
    print("skill score calculated")
    
    common_predictions[[response_curve_type]][["skill_scores"]] <-  multi_env_skillscore[[response_curve_type]][["skill_scores"]]
    common_predictions[[response_curve_type]][["model_run"]] <- model_run
  }
  
  # now select best model combo
  best_curve <- weightutils.highest_correlation_selection(multi_env_skillscore)
  
  output <- list()
  output[["measurements"]] <-  common_predictions[[best_curve]][["phase_length_measured"]]
  output[["modelled"]] <-  common_predictions[[best_curve]][["phase_length_modelled"]]
  output[["skill_scores"]] <- multi_env_skillscore[[best_curve]][["skill_scores"]]
  output[["parameters"]] <- common_predictions[[best_curve]][["parameters"]]
  output[["model_run"]] <- model_run
  output[["response_curve_type"]]<-  common_predictions[[best_curve]][["response_curve_type"]]
  
  
  ####################################################################################################
  
  out_path_best_curve <- file.path(getwd(),"output","paramter_best_response_curve",model_run)
  file_name_out_best <- paste0(env_variable,"_",granularity,"_",pheno_phase,"_best_response_curve_type.rds")
  dir.create(out_path_best_curve,recursive = T, showWarnings = F)
  # save output as .rds
  saveRDS( output, file= file.path(out_path_best_curve,file_name_out_best))
  
  out_path_whole  <- file.path(getwd(),"output","paramter_all_response_curve",model_run)
  file_name_out_out <- paste0(env_variable,"_",granularity,"_",pheno_phase,"_all_response_curve_type.rds")
  dir.create(out_path_whole,recursive = T, showWarnings = F)
  saveRDS( common_predictions, file= file.path(out_path_whole,file_name_out_out))
  
  # 
  weightutils.visualize_response_cruve_fit_cor(pheno_phase = pheno_phase,
                                               env_variable = env_variable,
                                               model_run = model_run,
                                               skillscores_resp_curves = multi_env_skillscore)
  
  
}


cl <- makePSOCKcluster(numCores)


start_time <- Sys.time()

output_curves <- parallel::parLapplyLB(cl,
                                comb_parallel_list,
                                response_curve_selecter,
                                combinations_list = combinations_list,
                                data_path = data_path_prediction,
                                meta_data_path = meta_data_path,
                                granularity = granularity,
                                crop_abbrev = crop_abbrev,
                                model_run = model_run)


stopCluster(cl)


end_time <- Sys.time()
print(end_time - start_time)




  


