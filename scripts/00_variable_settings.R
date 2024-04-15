# Author: Flavian Tschurr
# Project: KP030
# Date: 08.03.2022
# Purpose: dymenvmodel: set parameters
################################################################################



euler_p <-"/nfs/nas12/fs1202/green_groups_kp_public/"
windows_p <- "P:"
# set paths
euler = F
if(euler== T){
  setwd(file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
  
  data_path_fitting <- file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_DWD_and_JRC")
  data_path_prediction <- file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_training_with_JRC")
  data_path_validation <- file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_validation_with_JRC")
  
}else{
  setwd(file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
  
  data_path_fitting <- file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_DWD_and_JRC")
  data_path_prediction <- file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_training_with_JRC")
  data_path_validation <- file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_validation_with_JRC")
  
}




meta_data_path <- "meta/phenological_phases"

################################################################################
################################################################################
# set parameters and constants
################################################################################
################################################################################

pheno_phases <- c("sowing-emergence","emergence-booting",  "booting-heading","heading-senescence")


# environmental covariates
env_variable_DRCs_inputs <- list("tas" = c("WangEngels"),
                                 "tasmin" = c("WangEngels"),
                                 "tasmax" = c("WangEngels"),
                                 "RH" = c("reg_linear","non_linear", "asymptotic","WangEngels"),
                                 "global_radiation" =  c("WangEngels"), # too high global radiation may lead to damage
                                 "SPI"=c("reg_linear","non_linear", "asymptotic","WangEngels"),
                                 "VPD" =  c("reg_linear","non_linear", "asymptotic","WangEngels") )


env_variables <- names(env_variable_DRCs_inputs)

granularity <- "daily"
crop_abbrev <- "WW"

# name of the model_run
model_run <- "run_WW1"

################################################################################
# set colors for plots

# pheno_phase_colors <- c("#A66969","#3D5BA3","#49662A","#DC482B")
# names(pheno_phase_colors) <- pheno_phases
# 
# pheno_phase_colors_df <- data.frame(Value = pheno_phase_colors,
#                   pheno_phase = pheno_phases)


pheno_phase_colors_vect <- c("sowing-emergence" =  "#A66969",
                             "emergence-booting"= "#3D5BA3",
                             "booting-heading"  ="#49662A",
                             "heading-senescence" = "#DC482B")

DRC_colors_vect <- c("WangEngels" = "#003f5c",
                          "asymptotic" = "#7a5195",
                          "reg_linear" = "#ef5675",
                          "non_linear" = "#ffa600")

DRC_name_vect <- c("WangEngels" = "Wang Engels",
                     "asymptotic" = "asymptotic",
                     "reg_linear" = "linear",
                     "non_linear" = "non linear")

