# Authot: Flavian Tschurr
# Project: KP030
# Date: 03.07.2023
# Purpose: Dymenv:utility functions
################################################################################

utils.get_median_of_parameters <- function(one_output){
  #'@param one_output output list of the optimized paramters
  #'@description calculated the median of the given input paramters and returns it 
  if(length(one_output[[1]])==1){
    # browser()
    return(NA)
  }
  params_list <- list()
  for(param in c(1:length(one_output[[1]]$par))){
    out_vect <- NULL
    for (i in 1:length(one_output)) {
      out_vect[i] <- one_output[[i]]$par[param] 
      
    }
    
    params_list[[param]] <- median(out_vect,na.rm = T)
  }
  returner <- unlist(params_list)
  return(unlist(params_list))
  
}


utils.get_min_mean_median_max_of_parameters <- function(one_output){
  #'@param one_output output list of the optimized paramters
  #'@description calculated the median of the given input paramters and returns it 
  
  if(length(one_output[[1]])==1){
    return(NA)
  }
  params_list <- list()
  for(param in c(1:length(one_output[[1]]$par))){
    out_vect <- NULL
    for (i in 1:length(one_output)) {
      out_vect[i] <- one_output[[i]]$par[param] 
      
    }
    params_list[[param]] <- c('min' = min(out_vect,na.rm=T), 'mean' = mean(out_vect,na.rm=T) , 'median' = median(out_vect,na.rm=T) , 'max' = max(out_vect,na.rm=T))
  }
  returner <- unlist(params_list)
  return(unlist(params_list))
  
}


utils.gap_filling_linear <- function(data_vect,max_gap_length){
  #'@param  data_vect input data vector
  #'@param max_gap_length maximal length of a NA gap in the data until which interpolation via spline will be done
  #'@description gapfilling routine, using base spline function go fill gaps. If a NA gap is too long (over max_gap_length), NA will by introduced again
  #'

  filled <- approx(data_vect,n=length(data_vect))
  
  ture_NA <- ifelse(is.na(data_vect),TRUE,FALSE)
  repetitions <- rle(ture_NA)
  # na_introductions <- which(repetitions[["lengths"]][which(repetitions[["values"]]==TRUE)]>=max_gap_length)
  na_introductions_test <- which(repetitions[["lengths"]]>=max_gap_length)
  na_introduction <- ifelse(repetitions[["values"]][na_introductions_test]==TRUE,na_introductions_test,NA)
  na_introduction <-na.omit(na_introduction)
  
  for(na_intro in na_introduction){
    start <-sum(repetitions[["lengths"]][1:(na_intro-1)])
    stop <-sum(repetitions[["lengths"]][1:(na_intro)])
    filled$y[start:stop] <- NA
  }
  return(filled$y)
}

utils.pheno_phase_env_vari_DRC_list_combiner <- function(pheno_phases, env_variable_DRCs_inputs,resample_vector){
  #'@param pheno_phases vector with the phenological phases
  #'@param env_variable_DRCs_inputs list per environmental covariate which DRCs should be fitted
  #'@param resample_vector numeric vector with the number of resample which should be done per pheno_phase x env_variable x DRC combination
  
  out_list <- list()
  counter <- 1
  for(pheno_phase in pheno_phases){
    for(env_variable in names(env_variable_DRCs_inputs)){
      for(DRC in env_variable_DRCs_inputs[[env_variable]]){
        for(sample in resample_vector){
          
          out_list[[paste(pheno_phase,env_variable,DRC, sample, sep=":")]] <- data.frame(pheno_phase = pheno_phase, env_variable = env_variable, response_curve_type = DRC, resample_number = sample)
          counter <- counter + 1
        }
        
      }
    }
    
  }
  return(out_list)  
}

utils.pheno_phase_env_vari_list_combiner <- function(pheno_phases, env_variable_DRCs_inputs,resample_vector){
  #'@param pheno_phases vector with the phenological phases
  #'@param env_variable_DRCs_inputs list per environmental covariate which DRCs should be fitted
  #'@param resample_vector numeric vector with the number of resample which should be done per pheno_phase x env_variable x DRC combination
  
  out_list <- list()
  counter <- 1
  for(pheno_phase in pheno_phases){
    for(env_variable in names(env_variable_DRCs_inputs)){
      # for(DRC in env_variable_DRCs_inputs[[env_variable]]){
      #   for(sample in resample_vector){
      #     
          out_list[[paste(pheno_phase,env_variable, sep=":")]] <- data.frame(pheno_phase = pheno_phase, env_variable = env_variable)
          counter <- counter + 1
      #   }
      #   
      # }
    }
    
  }
  return(out_list)  
}





utils.get_parameter_boundaries <- function(parameters_numeric, deviation_ratio){
  lower <- NULL
  upper <- NULL
  for(par in 1 :length(parameters_numeric)){
    deviation <- abs(parameters_numeric[par]) * (deviation_ratio)
    lower[par] <- parameters_numeric[par] - deviation
    upper[par] <- parameters_numeric[par] + deviation
  }
  return(list(lower= lower, start = parameters_numeric, upper = upper ))
}


#

utils.combine_and_save_fitting_output <- function(combinations_list, output, crop_abbrev){
  #'@description saves the output in the correct format with some meta info
  combinations_df <- do.call("rbind",combinations_list )
  for(pheno_ph in unique(combinations_df$pheno_phase)){
    combinations_sub <- subset(combinations_df, pheno_phase == pheno_ph)

    for(response_type in unique(combinations_sub$response_curve_type)){
      combinations_sub_sub <-   subset(combinations_sub, response_curve_type == response_type)

      for(env_vari in unique(combinations_sub_sub$env_variable)){
        one_combo <-   subset(combinations_sub_sub, env_variable == env_vari)
        print(paste(pheno_ph,env_vari,response_type,sep=" : "))
        list_names <- rownames(one_combo)
        
        ####
        # create meta information before saving
        meta_info <- list()
        meta_info[["response_curve_type"]] <- response_type
        meta_info[["env_variable"]] <- env_vari
        meta_info[["crop"]] <- crop_abbrev
        meta_info[["parameter_list"]] <- names(output[[list_names[1]]]$par)
        
        # calculate median
        median_output<- list()
        median_output[[paste(env_vari,response_type,sep="-")]] <- utils.get_median_of_parameters(output[list_names])
        median_output[["meta"]] <- meta_info
        
        all_statistics_output <- list()
        all_statistics_output[[paste(env_vari,response_type,sep="-")]] <- utils.get_min_mean_median_max_of_parameters(output[list_names])
        all_statistics_output[["meta"]] <- meta_info
        
        
        combined_output <- list()
        combined_output[["median_output"]] <- median_output
        combined_output[["complete_output"]] <- output[list_names]
        combined_output[["parameter_range_output"]] <- all_statistics_output
        
        # create directory and file names
        output_path_base <- file.path(getwd(),"output","parameter_model",response_type,crop_abbrev)
        out_file_name <- paste0(response_type,"_parameter_",pheno_ph,"_" , env_vari,"_",granularity,".rds")
        # crate directory
        dir.create(output_path_base,recursive = T, showWarnings = F)
        # save output as .rds
        saveRDS(combined_output, file= file.path(output_path_base,out_file_name))
      
      
      }
    }
  }
  
}

utils.output_temp_file_loader <- function(one_combination,granularity,crop_abbrev){
  #'@description reads in the temporal saved output files after optimization. all combinations from the combinationslist (us lapply to apply :D)
  in_file_name_temp <- paste0("TEMP_",one_combination$response_curve_type,"_parameter_",one_combination$pheno_phase,"_" , one_combination$env_variable,"_",granularity,"_","iteration",one_combination$resample_number,".rds")
  output_path_base_temp <- file.path(getwd(),"output","parameter_model","temp",crop_abbrev)
  if(file.exists(file.path(output_path_base_temp,in_file_name_temp))){
    # read .rds
    in_file <- readRDS(file= file.path(output_path_base_temp,in_file_name_temp))
    return(in_file)
    
  }else{
    return("not_existing")
  }
}

utils.existing_checker <- function(one_output_reload){
  if(typeof(one_output_reload) == "character"){
    return(1)
  }else{return(0)}
}


