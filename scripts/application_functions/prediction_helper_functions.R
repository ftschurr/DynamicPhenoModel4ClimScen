# Author: Flavian Tschurr
# Project: KP030
# Date: 02.10.2023
# Purpose: get best model for your data
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



your_environmental_covariates <- c("tas","RH","global_radiation","tasmin")
your_environmental_covariates <- c("tas","tasmin","tasmax","SPI")

your_pheno_phases <- c( "emergence-booting",  "booting-heading" )

test2 <- predhelp.get_best_model(your_environmental_covariates=your_environmental_covariates,
                                your_pheno_phases = your_pheno_phases,
                                skillscore_to_select="RMSE")


predhelp.get_best_model <- function(your_environmental_covariates,your_pheno_phases, skillscore_to_select = "RMSE", return_just_best=T){
  # load settings
  source("scripts/00_variable_settings.R")
  source("scripts/application_functions/utils_prediction_helper_functions.R")
  
  # create path
  out_path_validation_csv <- file.path(getwd(),"output/validation",model_run)
  # read data
  all_skillscores <- read.csv(file.path(out_path_validation_csv,"all_models_selected_with_RMSE.csv"))
  
  # get all potential combinations to select in the validation data.frame
  your_environmental_covariates_permutations <-predhelputils.get_all_permutations(your_environmental_covariates = your_environmental_covariates)
  all_available_models <- list()
  best_available_model <- list()
  
  for(pheno_ph in your_pheno_phases){
    one_pheno_skillscores <- subset(all_skillscores, pheno_phase == pheno_ph)
    all_available_models[[pheno_ph]] <- one_pheno_skillscores[which(one_pheno_skillscores$env_variables %in% your_environmental_covariates_permutations),]
    if(skillscore_to_select == "cor"){
      best_available_model[[pheno_ph]] <- all_available_models[[pheno_ph]][which.max(all_available_models[[pheno_ph]][[skillscore_to_select]]),]
      
    }else{
      best_available_model[[pheno_ph]] <- all_available_models[[pheno_ph]][which.min(all_available_models[[pheno_ph]][[skillscore_to_select]]),]
    }
  }
  
  if(return_just_best ==T){
    return(do.call("rbind",best_available_model))
  }else{
    return(do.call("rbind",all_available_models))
    
  }

}


