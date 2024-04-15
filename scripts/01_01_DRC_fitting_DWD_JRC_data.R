# Author: Flavian Tschurr
# Project: KP030
# Date: 08.03.2022
# Purpose: dymenvmodel: wrapper script with scaled data
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




library(parallel)

source("scripts/00_variable_settings.R")
source("scripts/functions/FUN_utils.R")

################################################################################
################################################################################
# set parameters and constants
################################################################################
################################################################################

# # phenology phases
# pheno_phases <- c("sowing-emergence","emergence-booting",  "booting-heading","heading-senescence")
# pheno_phases <- c(  "booting-heading","heading-senescence","sowing-emergence","emergence-booting")
# # pheno_phases <- c(  "emergence-booting")
# 
# # environmental covariates
# env_variable_DRCs_inputs <- list("tas" = c("WangEngels"),
#                       "tasmin" = c("WangEngels"),
#                       "tasmax" = c("WangEngels"),
#                       "RH" = c("reg_linear","non_linear", "asymptotic","WangEngels"),
#                       "global_radiation" =  c("WangEngels"), # too high global radiation may lead to damage
#                       "SPI"=c("reg_linear","non_linear", "asymptotic","WangEngels"),
#                       "VPD" =  c("reg_linear","non_linear", "asymptotic","WangEngels") )
# 
# 
env_variable_DRCs_inputs <- list(
                                  # "tas" = c("WangEngels"),
                                 # "tasmin" = c("WangEngels"),
                                 # "tasmax" = c("WangEngels"),
                                 "RH" = c( "asymptotic"),
                                 # "global_radiation" =  c("WangEngels"), # too high global radiation may lead to damage
                                 "SPI"=c( "asymptotic"),
                                 "VPD" =  c( "asymptotic")
                                 )
#
# # env_variables <- names(env_variable_DRCs_inputs)

combinations_list <- utils.pheno_phase_env_vari_DRC_list_combiner(pheno_phases= pheno_phases,
                                                                  env_variable_DRCs_inputs = env_variable_DRCs_inputs,
                                                                  resample_vector = c(1:20)
                                                                  )

# granularity <- "daily"
# crop_abbrev <- "WW"

numCores <- min(round(detectCores()*0.85),length(combinations_list))
if(euler ==T){
  numCores <- 48
}
################################################################################
################################################################################
# function to iterate over
################################################################################
################################################################################

DRC_fit_looper_function <- function(one_combination, 
                                    granularity,
                                    data_path,
                                    crop_abbrev,
                                    random_sample_size = 0.8
                                 ){
  # load libraries   
  require(nloptr)
  # load functions
  source("scripts/functions/FUN_utils.R")
  source("scripts/functions/FUN_dose_response_curves.R")
  source("scripts/functions/FUN_start_parameter_estimation.R")
  ##############################################################################
  # DO YOU WANT TO RETRAIN AND OVERWRITE OLD FILES?
  overwrite=TRUE
  ##############################################################################
  
  ##############################################################################
  # define variables
  ##############################################################################
  pheno_phase = one_combination$pheno_phase
  env_variable = one_combination$env_variable
  response_curve_type = one_combination$response_curve_type
  ##############################################################################  
  #* check if already calculated *#
  ##############################################################################
  # create output paths
  output_path_base <- file.path(getwd(),"output","parameter_model",response_curve_type,crop_abbrev)
  out_file_name <- paste0(response_curve_type,"_parameter_",pheno_phase,"_" , env_variable,"_",granularity,".rds")

  out_file_name_temp <- paste0("TEMP_",response_curve_type,"_parameter_",pheno_phase,"_" , env_variable,"_",granularity,"_","iteration",one_combination$resample_number,".rds")
  output_path_base_temp <- file.path(getwd(),"output","parameter_model","temp",crop_abbrev)
  
  if(overwrite == F){
    if(file.exists(paste(output_path_base,out_file_name,sep="/"))){
      return("existing")
    }
    if(file.exists(paste(output_path_base_temp,out_file_name_temp,sep="/"))){
      print("skipped")
      return("existing")
    }
    
  }
  ##############################################################################
  # start computation
  ##############################################################################
  # browser()
  # create file name
  data_file_name <- paste0(paste(env_variable,granularity,"training_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")
  # load file
  pheno_list <- readRDS(file.path(data_path,data_file_name))
  # catch response function
  .response_function. <- get(paste0(response_curve_type,"_response_loop"))
  # get starting parameter function
  .starting_parameter_estimation_function. <- get(paste0("estimate_starting_params_",response_curve_type,"_model"))
  # get starting parameters
  parameter_list <- .starting_parameter_estimation_function.(env_vect = pheno_list)
  
  # get constraint function
  .constraint_function. <- try(get(paste0(response_curve_type,"_constraint")),silent = T)
  if(class(.constraint_function.)== "try-error"){
    .constraint_function. = NULL
  }
  
  
  # extract parameters from parameter list
  lower_bounds <- lapply(parameter_list, "[[",1)
  starting_params <- lapply(parameter_list, "[[",2)
  upper_bounds <- lapply(parameter_list, "[[",3)
  names_params <- names(starting_params)
  
  # Set the seed based on the current time
  set.seed(as.integer(Sys.time()) + sample(1:1000, 1))
  
  random_sample <- sample(x=c(1:length(pheno_list)), size = ceiling(length(pheno_list)*random_sample_size))
  measurement_list <- pheno_list[random_sample]
  measurement_list <- lapply(measurement_list, function(x) x[!is.na(x)])
  
  # create control data --> 100 is aim to reach
  control_data <- rep(100,length(names(measurement_list)))
  
  ##############################################################################
  # coarse optimization
  ##############################################################################
  
  first_opt_df <- nloptr::auglag(x0 = as.numeric(starting_params),
                           fn = fit_dose_response_model_course,
                           env_data = measurement_list,
                           control_data = control_data,
                           names_params = names_params,
                           .response_function. = .response_function.,
                           .constraint_function. = .constraint_function.,
                           localsolver = c("COBYLA"),
                           localtol = 1e-24,
                           lower = as.numeric(lower_bounds),
                           upper = as.numeric(upper_bounds),
                           control = list(
                             xtol_rel = 1e-2,  # Adjusted to a larger value for coarseness
                             ftol_rel = 1e-24,
                             ftol_abs = 1e-24,
                             maxeval = 500  # Adjusted to a higher value for more function evaluations
                           )
  )
  
  # take this parameters to make next evaluation
  
  next_params <- utils.get_parameter_boundaries(first_opt_df$par,deviation_ratio = 1)
  
  lower_bounds <- next_params$lower
  starting_params <- next_params$start
  upper_bounds <- next_params$upper
  
  ##############################################################################
  # fine optimization
  ##############################################################################
  
  xtol_rel <- 1e-08 # Tolerance criterion between two iterations
  # (threshold for the relative difference of
  # parameter values between the 2 previous
  # iterations)
  maxeval <- 1000*length(starting_params)  # Maximum number of evaluations of the minimized criteria
  
  opt_df <- nloptr::auglag(x0 = as.numeric(starting_params),
                           fn = fit_dose_response_model_course,
                           env_data = measurement_list,
                           control_data = control_data,
                           names_params = names_params,
                           .response_function. = .response_function.,
                           .constraint_function. = .constraint_function.,
                           localsolver = c("COBYLA"),
                           localtol =  1e-24,
                           lower = as.numeric(lower_bounds),
                           upper= as.numeric(upper_bounds),
                           control = list(
                             xtol_rel = xtol_rel,
                             ftol_rel=  1e-24,
                             ftol_abs =  1e-24,
                             maxeval = maxeval
                           ))
  
  ##############################################################################
  # add mean estimation
  opt_df[[paste0("mean_",env_variable)]] <- as.numeric(unlist(lapply(measurement_list, mean)))
  # add parameter names
  names(opt_df$par) <- names_params
  # predict growth
  opt_df$growth_modelled <- as.numeric(unlist(lapply(measurement_list, .response_function., opt_df$par)))
  # add control data
  opt_df$growth_measured <- control_data
  
  # delete parameter estimation which did not converged!
  if(opt_df$convergence >= 4){
    opt_df$par <- rep(NA,length(starting_params))
  }
  
  ##############################################################################
  # save intermediate result
  # create directory and file names
  out_file_name_temp <- paste0("TEMP_",response_curve_type,"_parameter_",pheno_phase,"_" , env_variable,"_",granularity,"_","iteration",one_combination$resample_number,".rds")
  output_path_base_temp <- file.path(getwd(),"output","parameter_model","temp",crop_abbrev)
  # crate directory
  dir.create(output_path_base_temp,recursive = T, showWarnings = F)
  # save output as .rds
  saveRDS(opt_df, file= file.path(output_path_base_temp,out_file_name_temp))
  ##############################################################################
  
  
  return(opt_df)
  
}



################################################################################
################################################################################
# parallel function
################################################################################
################################################################################

# numCores <- min(detectCores(),length(combinations_list))
cl <- makePSOCKcluster(numCores)


start_time <- Sys.time()

output <- parallel::parLapplyLB(cl,
                                combinations_list,
                                DRC_fit_looper_function,
                                granularity = granularity,
                                data_path = data_path_fitting,
                                crop_abbrev = crop_abbrev,
                                random_sample_size = 0.8)

# output <- lapply(
#                                 combinations_list,
#                                 DRC_fit_looper_function,
#                                 granularity = granularity,
#                                 data_path = data_path,
#                                 crop_abbrev = crop_abbrev,
#                                 random_sample_size = 0.8)



stopCluster(cl)


end_time <- Sys.time()
print("modelfitting  done for")
print(end_time - start_time)

output_reload <- lapply(combinations_list,
                        utils.output_temp_file_loader,
                        granularity = granularity,
                        crop_abbrev = crop_abbrev)


not_existing_files <-sum(unlist(lapply(output_reload, utils.existing_checker)))
if(not_existing_files == 0){
  
  # save outputs
  utils.combine_and_save_fitting_output(combinations_list = combinations_list,
                                        output = output_reload,
                                        crop_abbrev = crop_abbrev)
}


