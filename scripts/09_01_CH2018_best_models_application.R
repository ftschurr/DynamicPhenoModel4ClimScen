# Author: Flavian Tschurr
# Project: KP030
# Date: 16.09.2023
# Purpose: dymenvmodel: apply to CH2018 data
################################################################################
Sys.setenv(LANG = "en")

git_base <- "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"
setwd(git_base)
script_base <- "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/scripts"
data_path <- "O:/Projects/KP0030_ftschurr/data/CH2018_data"
data_path_stations <- paste0(data_path,"/QMstations")
meta_path <-  "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/meta/CH2018"
output_base_path <-"O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output"

#vectors with the used stations


stations <- c('TAE','AIG','ALT','RAG','BAS','BER','BUS','CHU','ELM','FAH','GVE','GLA','GUT',
              'INT','OTL','LUG','LUZ','MAG','MER','NEU','CGI','PAY','PIO','PUY','RUE','SHA',
              'SIO','STG','SBO','VAD','VIS','WYN','WAE','REH','SMA','KLO')


# used scenarios
scenarios <- c("RCP26","RCP45","RCP85")


env_variables<- c("tas","tasmin","tasmax","global_radiation","SPI","VPD", "RH")
# either dynamic sowing date or fixed sowing date
sowing_date_determination_method ="dynamic_sowing_date"
sowing_date_determination_method = "fixed_sowing_date"
# if fixed <- sa the day: ("MM-DD")
fix_sowing_day <- "10-15"
# # fixed start dates of the phase to compare ("MM-DD") in a list with specifying the phase
# all_phase_start_fixed <- list("emergence-booting"="10-27",
#                                "booting-heading"="05-05",
#                               "heading-senescence" = "06-09")
all_phase_start_fixed <- FALSE



################################## functions ###################################

source("scripts/00_variable_settings.R")

source(paste0(script_base,"/CH2018_functions/utility/FUN_gapfilling.R"))
source(paste0(script_base,"/CH2018_functions/utility/FUN_covariate_converting.R"))
source(paste0(script_base,"/CH2018_functions/crop_specific/FUN_sowing_date_determination.R"))



library(parallel)
################################## helper tables ##################################
modelchain_table <- read.csv(paste(meta_path,"modelchains/modelchains_CH2018_climate_suitability_rsds.csv",sep="/"),header=TRUE,skip = 1,sep=";")
model_selection <- read.csv(file.path(getwd(),"output","plots","agr_vs_ch2018_best_model","best_models_agro_ch2018_combination.csv"))
model_selection <- subset(model_selection, type =="Opt")

################################## start routine ################################## 
lapplier_list <- list()
for(station in stations){
  for(scenario in scenarios){
    modelchains <- subset(modelchain_table,rcp==scenario)
    
    for(modelchain in  modelchains$chain_name){
      to_loop <- data.frame(st=station, scen=scenario, mod = modelchain)
      lapplier_list[[paste(station,scenario,modelchain,sep=".")]] <- to_loop
    }
    
  }
}

lapplier_list <- rev(lapplier_list)

################################## looper function ################################## 

CH2018_dymenv_model_looper <- function(station_scenarion_model_list, 
                                       env_variables,
                                       sowing_date_determination_method, 
                                       fix_sowing_day, 
                                       model_run, 
                                       data_path_stations, 
                                       script_base,
                                       output_base_path,
                                       model_selection =model_selection,
                                       all_phase_start_fixed=FALSE){
  
  source(paste0(script_base,"/CH2018_functions/utility/FUN_gapfilling.R"))
  source(paste0(script_base,"/CH2018_functions/utility/FUN_covariate_converting.R"))
  source(paste0(script_base,"/CH2018_functions/crop_specific/FUN_sowing_date_determination.R"))

  source("scripts/functions/FUN_unknown_environment_prediction_utils.R")
  
  ################################## load data ################################## 
  station = station_scenarion_model_list$st
  scenario = station_scenarion_model_list$scen
  modelchain = station_scenarion_model_list$mod
  
  overwrite = FALSE
  ###
  out_folder <- file.path(output_base_path,"CH2018","model_application_best_model_selection_opt",model_run,sowing_date_determination_method)
  # out_folder <- file.path(output_base_path,"CH2018","model_application_best_model_selection",model_run,sowing_date_determination_method)
  
  dir.create(out_folder, recursive = T, showWarnings = F)
  if(any(all_phase_start_fixed == FALSE, na.rm=T) != TRUE){
    file_out <- paste0(paste("dymenv_model_selection",station,modelchain,model_run,sowing_date_determination_method, "fixed-phase-start",sep="_"),".csv")
    
  }else{
    file_out <- paste0(paste("dymenv_model_selection",station,modelchain,model_run,sowing_date_determination_method, sep="_"),".csv")
    
  }
  
  if(overwrite== FALSE){
    if(file.exists(file.path(out_folder,file_out))){
      return(read.csv(file.path(out_folder,file_out)))
      
    }
  }
  ##
  env_data <- list()
  # copy data into a folder
  env_variables_to_copy <- gsub("RH","hurs",env_variables)
  env_variables_to_copy <- gsub("global_radiation","rsds",env_variables_to_copy)
  # browser()
  temp_folder <- file.path(output_base_path,"temp",paste0(paste(unlist(station_scenarion_model_list),collapse="_"),"_",format(Sys.time(),"%Y-%m-%d_%H-%M-%S")))
  dir.create(temp_folder,recursive = T,showWarnings = F)
  copy_list <- list(
  "files_to_copy" = file.path(
                             paste("CH2018",env_variables_to_copy,modelchain,"QMstations_1981-2099_csv",sep="_"),
                             paste0(paste("CH2018",env_variables_to_copy,modelchain,"QMstations_1981-2099",station,sep="_"),".csv")),
  "files_destination" = file.path(paste0(paste("CH2018",env_variables_to_copy,modelchain,"QMstations_1981-2099",station,sep="_"),".csv"))
  
  )
  file.copy(from = file.path(data_path_stations,env_variables_to_copy,copy_list$files_to_copy), 
            to = file.path(temp_folder,copy_list$files_destination))
  # read and delete afterwards again

  for(variable in env_variables){
    
    if(variable == "global_radiation"){
      # folder_name <- paste("CH2018","rsds",modelchain,"QMstations_1981-2099_csv",sep="_")
      folder_name = temp_folder
      file_name <- paste0(paste("CH2018","rsds",modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
      # variable = "rsds"
      # if(file.exists(paste(data_path_stations,"rsds",folder_name,file_name,sep="/"))){
      if(file.exists(paste(folder_name,file_name,sep="/"))){
        
        
        # env_data[[variable]] <- read.csv(paste(data_path_stations,"rsds",folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
        env_data[[variable]] <- read.csv(paste(folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
        
      } else{
        unlink(temp_folder,recursive = T)
        
        return(NA)
      }
      
    } else if(variable == "RH"){
      # folder_name <- paste("CH2018","hurs",modelchain,"QMstations_1981-2099_csv",sep="_")
      folder_name = temp_folder
      
      file_name <- paste0(paste("CH2018","hurs",modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
      
      # if(file.exists(paste(data_path_stations,"hurs",folder_name,file_name,sep="/"))){
      if(file.exists(paste(folder_name,file_name,sep="/"))){
        
        
        # env_data[[variable]] <- read.csv(paste(data_path_stations,"hurs",folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
        env_data[[variable]] <- read.csv(paste(folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
        
      } else{
        unlink(temp_folder,recursive = T)
        
        return(NA)
      }
      
      
    } else{
      # folder_name <- paste("CH2018",variable,modelchain,"QMstations_1981-2099_csv",sep="_")
      folder_name = temp_folder
      
      file_name <- paste0(paste("CH2018",variable,modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
      
      # if(file.exists(paste(data_path_stations,variable,folder_name,file_name,sep="/"))){
      if(file.exists(paste(folder_name,file_name,sep="/"))){
        
        
        # env_data[[variable]] <- read.csv(paste(data_path_stations,variable,folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
        env_data[[variable]] <- read.csv(paste(folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
        
      }else{
        unlink(temp_folder,recursive = T)
        
        return(NA)
      }
      
    }
    
    
    env_data[[variable]]$DATE <- as.Date(env_data[[variable]]$DATE)
    env_data[[variable]]$VALUE <- gapfilling(as.numeric(env_data[[variable]]$VALUE),as.Date(env_data[[variable]]$DATE))
    if(variable == "global_radiation" ){
      env_data[[variable]]$VALUE <- CH2018Utils_W_per_sqm_to_J_per_sqcm(env_data[[variable]]$VALUE)
    }
  } # end env_variables loop
  # browser()
  
  # delete temp folder
  unlink(temp_folder,recursive = T)
  
  ################################## determine sowing date ################################## 
  
  if(sowing_date_determination_method =="dynamic_sowing_date"){
    env_sowing_date <- list()
    env_sowing_date[["tas"]] <- env_data[["tas"]]
    
    folder_name <- paste("CH2018","pr",modelchain,"QMstations_1981-2099_csv",sep="_")
    file_name <- paste0(paste("CH2018","pr",modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
    if(file.exists(paste(data_path_stations,"pr",folder_name,file_name,sep="/"))){
      env_sowing_date[["pr"]] <- read.csv(paste(data_path_stations,"pr",folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
    }else{
      return(NA)
    }
    env_sowing_date[["pr"]]$DATE <- as.Date(env_sowing_date[["pr"]]$DATE)
    env_sowing_date[["pr"]]$VALUE <- gapfilling(as.numeric(env_sowing_date[["pr"]]$VALUE),as.Date(env_sowing_date[["pr"]]$DATE))
    
    sowing_dates <- sowing_date_determination(env_sowing_date)
    sowing_dates <- sowing_dates[which(sowing_dates <= as.Date("2098-01-01"))]
    
  }else if( sowing_date_determination_method =="fixed_sowing_date"){
    sowing_dates<- as.Date(paste( unique(lubridate::year(env_data[[1]]$DATE)), fix_sowing_day, sep="-"))
    sowing_dates <- sowing_dates[which(sowing_dates <= as.Date("2098-01-01"))]
    
  }
  sowing_years <- lubridate::year(as.Date(sowing_dates))
  
  overview_complexities <- list()
  
  # model_selection <- read.csv(file.path(getwd(),"output","plots","agr_vs_ch2018_best_model","best_models_agro_ch2018_combination.csv"))

  
  ################################## apply to different model complexities ################################## 
  for(model_type in unique(model_selection$type)){
    start_time <- Sys.time()
    ################################## sowing to emergence ################################## 
    # phase name
    pheno_phase = "sowing-emergence"
    # cut env data from period start on
    period_length_sowing_to_emergence = 100 
    env_sowing_emergence <- list()
    env_sowing_emergence<-  lapply(sowing_dates, envpredutils.env_period_cutter,
                                   env_variables = env_variables,
                                   env_data = env_data,
                                   period_length  = period_length_sowing_to_emergence)
    

    # select correct model
    current_model <- model_selection[which(model_selection$type==model_type),]
    current_model <- current_model[which(current_model$pheno_phase_ch2018==pheno_phase),]
    
    # predict period end
    end_sowing_emergence <- lapply(env_sowing_emergence, envpredutils.pheno_phase_prediction_glm_model_fixed_covariates,
                                   pheno_phase = pheno_phase,
                                   model_run = model_run,
                                   Used_Env_Variables = current_model$env_variables)
    # write into table
    results_sowing_emergence <- data.frame(pheno_phase=pheno_phase,
                                           start_date = sowing_dates,
                                           end_date = as.character(unlist(end_sowing_emergence)),
                                           duration = as.numeric(as.Date(as.character(unlist(end_sowing_emergence)))- as.Date(sowing_dates)),
                                           sowing.year = sowing_years,
                                           station = station,
                                           model_complexity = current_model$xintercept,
                                           model_type = model_type,
                                           env_variables  = current_model$env_variables,
                                           scenario = scenario, 
                                           modelchain= modelchain)
    
    end_time <- Sys.time()
    print(paste0("calculation: ", pheno_phase, " with the model complexity: ",current_model$xintercept, " took: "))
    print(end_time - start_time)
    start_time <- Sys.time()
    ################################## emergence to booting ################################## 
    # phase name
    pheno_phase = "emergence-booting"
    
    # cut env data from period start on
    period_length_emergence_to_booting = 250 
    # use fix start day if chosen above
    if(any(all_phase_start_fixed == FALSE, na.rm=T) != TRUE){
      phase_start_date <- all_phase_start_fixed[[pheno_phase]]
      end_sowing_emergence<- as.Date(paste( unique(lubridate::year(env_data[[1]]$DATE)), phase_start_date, sep="-"))
      end_sowing_emergence <- end_sowing_emergence[which(end_sowing_emergence <= as.Date("2098-01-01"))]
    }
    
    env_emergence_booting <- list()
    
    env_emergence_booting<-  lapply(end_sowing_emergence, envpredutils.env_period_cutter,
                                    env_variables = env_variables,
                                    env_data = env_data,
                                    period_length  = period_length_emergence_to_booting)
    
    # select correct model
    current_model <- model_selection[which(model_selection$type==model_type),]
    current_model <- current_model[which(current_model$pheno_phase_ch2018==pheno_phase),]
    
    # predict period end
    end_emergence_booting <- lapply(env_emergence_booting, envpredutils.pheno_phase_prediction_glm_model_fixed_covariates,
                                   pheno_phase = pheno_phase,
                                   model_run = model_run,
                                   Used_Env_Variables = current_model$env_variables)
    # write into table
    results_emergence_booting <- data.frame(pheno_phase=pheno_phase,
                                            start_date = as.character(unlist(end_sowing_emergence)),
                                            end_date = as.character(unlist(end_emergence_booting)),
                                            duration = as.numeric(as.Date(as.character(unlist(end_emergence_booting)))- as.Date(as.character(unlist(end_sowing_emergence)))),
                                            sowing.year = sowing_years,
                                            station = station,
                                            model_complexity = current_model$xintercept,
                                            model_type = model_type,                                             
                                            env_variables  = current_model$env_variables,
                                            scenario = scenario, 
                                            modelchain= modelchain)
    
    end_time <- Sys.time()
    print(paste0("calculation: ", pheno_phase, " with the model complexity: ",current_model$xintercept, " took: "))
    print(end_time - start_time)
    start_time <- Sys.time()
    
    ################################## booting to heading ################################## 
    
    # phase name
    pheno_phase = "booting-heading"
    
    # cut env data from period start on
    period_length_booting_to_heading = 180 
    
    # use fix start day if chosen above
    if(any(all_phase_start_fixed == FALSE, na.rm=T) != TRUE){
      phase_start_date <- all_phase_start_fixed[[pheno_phase]]
      end_emergence_booting<- as.Date(paste( unique(lubridate::year(env_data[[1]]$DATE)), phase_start_date, sep="-"))
      end_emergence_booting <- end_emergence_booting[which(end_emergence_booting <= as.Date("2098-01-01"))]
    }
    
    env_booting_heading <- list()
    
    env_booting_heading<-  lapply(end_emergence_booting, envpredutils.env_period_cutter,
                                  env_variables = env_variables,
                                  env_data = env_data,
                                  period_length  = period_length_booting_to_heading)
    
    
    
    # select correct model
    current_model <- model_selection[which(model_selection$type==model_type),]
    current_model <- current_model[which(current_model$pheno_phase_ch2018==pheno_phase),]
    
    # predict period end
    end_booting_heading <- lapply(env_booting_heading, envpredutils.pheno_phase_prediction_glm_model_fixed_covariates,
                                   pheno_phase = pheno_phase,
                                   model_run = model_run,
                                   Used_Env_Variables = current_model$env_variables)
    # write into table
    results_booting_heading <- data.frame(pheno_phase=pheno_phase,
                                          start_date = as.character(unlist(end_emergence_booting)),
                                          end_date = as.character(unlist(end_booting_heading)),
                                          duration = as.numeric(as.Date(as.character(unlist(end_booting_heading)))- as.Date(as.character(unlist(end_emergence_booting)))),
                                          sowing.year = sowing_years,
                                          station = station,
                                          model_complexity = current_model$xintercept,
                                          model_type = model_type,
                                          env_variables  = current_model$env_variables,
                                          scenario = scenario, 
                                          modelchain= modelchain)
    
    end_time <- Sys.time()
    print(paste0("calculation: ", pheno_phase, " with the model complexity: ",current_model$xintercept, " took: "))
    print(end_time - start_time)
    start_time <- Sys.time()
    
    ################################## heading to senescence ################################## 
    
    # phase name
    pheno_phase = "heading-senescence"
    
    # cut env data from period start on
    period_length_heading_to_senescence = 100 
    
    # use fix start day if chosen above
    if(any(all_phase_start_fixed == FALSE, na.rm=T) != TRUE){
      phase_start_date <- all_phase_start_fixed[[pheno_phase]]
      end_booting_heading<- as.Date(paste( unique(lubridate::year(env_data[[1]]$DATE)), phase_start_date, sep="-"))
      end_booting_heading <- end_booting_heading[which(end_booting_heading <= as.Date("2098-01-01"))]
    }
    
    env_heading_senescence <- list()
    
    env_heading_senescence<-  lapply(end_booting_heading, envpredutils.env_period_cutter,
                                     env_variables = env_variables,
                                     env_data = env_data,
                                     period_length  = period_length_heading_to_senescence)
    
    
    # select correct model
    current_model <- model_selection[which(model_selection$type==model_type),]
    current_model <- current_model[which(current_model$pheno_phase_ch2018==pheno_phase),]
    
    # predict period end
    end_heading_senescence <- lapply(env_heading_senescence, envpredutils.pheno_phase_prediction_glm_model_fixed_covariates,
                                   pheno_phase = pheno_phase,
                                   model_run = model_run,
                                   Used_Env_Variables = current_model$env_variables)
    # write into table
    results_heading_senescence <- data.frame(pheno_phase=pheno_phase,
                                             start_date = as.character(unlist(end_booting_heading)),
                                             end_date = as.character(unlist(end_heading_senescence)),
                                             duration = as.numeric(as.Date(as.character(unlist(end_heading_senescence)))- as.Date(as.character(unlist(end_booting_heading)))),
                                             sowing.year = sowing_years,
                                             station = station,
                                             model_complexity = current_model$xintercept,
                                             model_type = model_type,
                                             env_variables  = current_model$env_variables,
                                             scenario = scenario, 
                                             modelchain= modelchain)
    end_time <- Sys.time()
    print(paste0("calculation: ", pheno_phase, " with the model complexity: ",current_model$xintercept, " took: "))
    print(end_time - start_time)
    start_time <- Sys.time()
    ################################## end phases modeled ################################## 
    
    ################################## bind output to one DF ################################## 
    overview_complexities[[as.character(model_type)]] <- rbind(results_sowing_emergence,results_emergence_booting,results_booting_heading,results_heading_senescence)
    # browser()
    
  } # complexities
  
  one_modelchain <- do.call("rbind",overview_complexities)
  
  # out_folder <- file.path(output_base_path,"CH2018","model_application",model_run,sowing_date_determination_method)
  # dir.create(out_folder, recursive = T, showWarnings = F)
  # if(any(all_phase_start_fixed == FALSE, na.rm=T) != TRUE){
  #   file_out <- paste0(paste("dymenv_all_complexities",station,modelchain,model_run,sowing_date_determination_method, "fixed-phase-start",sep="_"),".csv")
  #   
  # }else{
  #   file_out <- paste0(paste("dymenv_all_complexities",station,modelchain,model_run,sowing_date_determination_method, sep="_"),".csv")
  #   
  # }
  # browser()
  write.table(one_modelchain, paste(out_folder,file_out,sep="/"), sep=",", row.names = F)
  
  return(one_modelchain)
  
} # end function 


all_modelchains <- list()
# all_modelchains <- lapply(
#                                lapplier_list,
#                                CH2018_dymenv_model_looper,
#                                env_variables = env_variables,
#                                sowing_date_determination_method = sowing_date_determination_method,
#                                fix_sowing_day = fix_sowing_day,
#                                model_run = model_run,
#                                data_path_stations = data_path_stations,
#                                script_base = script_base,
#                                output_base_path = output_base_path,
#                                   model_selection=model_selection,
#                                all_phase_start_fixed = all_phase_start_fixed
# )


numCores <- detectCores()
cl <- makePSOCKcluster(numCores)
all_modelchains <- parLapply(cl,
                               lapplier_list,
                               CH2018_dymenv_model_looper,
                               env_variables = env_variables,
                               sowing_date_determination_method = sowing_date_determination_method,
                               fix_sowing_day = fix_sowing_day,
                               model_run = model_run,
                               data_path_stations = data_path_stations,
                               script_base = script_base,
                               output_base_path = output_base_path,
                               model_selection=model_selection,
                               all_phase_start_fixed = all_phase_start_fixed

)
  stopCluster(cl)


all_modelchains_df <- do.call("rbind",all_modelchains)


if(any(all_phase_start_fixed == FALSE, na.rm=T) != TRUE){
  write.csv(all_modelchains_df, paste0(paste(output_base_path,"CH2018/model_application_best_model_selection",model_run,sowing_date_determination_method,"combined",sep="/"),"/all_modelchains_",model_run ,"_fixed-phase-start",".csv"))

}else{
  write.csv(all_modelchains_df, paste0(paste(output_base_path,"CH2018/model_application_best_model_selection_opt",model_run,sowing_date_determination_method,"combined",sep="/"),"/all_modelchains_",model_run ,".csv"))
  
  # write.csv(all_modelchains_df, paste0(paste(output_base_path,"CH2018/model_application_best_model_selection",model_run,sowing_date_determination_method,"combined",sep="/"),"/all_modelchains_",model_run ,".csv"))
}


# all_model_chains_old <- read.csv( paste0(paste(output_base_path,"CH2018/model_application_best_model_selection",model_run,"fixed_sowing_date_old","combined",sep="/"),"/all_modelchains_",model_run ,".csv"))
# 
# 
# all_modelchains_df <- subset(all_modelchains_df, model_type=="Opt")
# 
# all_model_chains_old <- subset(all_model_chains_old, model_type!="Opt")
# all_model_chains_old <- all_model_chains_old[,c(2:12)]
# all_modelchains_df <- rbind(all_model_chains_old,all_modelchains_df)

