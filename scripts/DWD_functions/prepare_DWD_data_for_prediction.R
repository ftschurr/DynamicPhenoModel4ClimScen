# Authot: Flavian Tschurr
# Project: KP030
# Date: 01.07.2022
# Purpose: dymenvmodel: get timeseries for predictions out of data
################################################################################

# set paths
setwd("P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel")

################################################################################

library(parallel)
# data path
path_data <- "O:/Projects/KP0030_ftschurr/data/DWD/historic"

data_path_training <- "P:/Evaluation/Projects/KP0030_ftschurr/data/DWD/historic/climate_phenology_new"

source("P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/scripts/functions/FUN_utils.R")
library(stringr)

# variables <- c("tas","global_radiation","SPI","VPD","RH")
variables <- c("tas","global_radiation","tasmin","tasmax","SPI","VPD","RH")

# hourly variables
# variables <- c("tas","global_radiation","SPI","VPD","RH")

# granularity <- "hourly"
granularity <- "daily"



pheno_phases <- c("sowing-emergence","emergence-booting","booting-heading","heading-senescence")
# pheno_phases <- c("booting-heading","heading-senescence")
# pheno_phases <- c("emergence-booting")

for(pheno_phase in pheno_phases){
  year_envs_reps <- readRDS(paste(path_data,paste0("year_envs_reps_wheat_phenology_DWD_",pheno_phase,".rds"),sep="/"))
  if(pheno_phase =="sowing-emergence" ){
    phase_start_id <-10
    phase_end_id <- 12
  } else if(pheno_phase =="emergence-booting" ){
    phase_start_id <-12
    phase_end_id <- 15
  }else if(pheno_phase =="booting-heading" ){
    phase_start_id <-15
    phase_end_id <- 18
  }else if(pheno_phase =="heading-senescence" ){
    phase_start_id <-18
    phase_end_id <- 22
  }
  
  for(variable in variables){
    # browser()
    one_variable_list <- list()
    
    path_variable <- paste(path_data,"climate",granularity,variable,sep="/")
    available_stations <- na.omit(as.numeric(unlist(str_split(pattern="_", list.files(path_variable))),suppressWarnings=T))
    
    pheno_stations <- as.numeric(substr(names(year_envs_reps),start=1, stop=nchar(names(year_envs_reps))-5))
    subset_pheno <- pheno_stations[which(pheno_stations %in% available_stations)]
    data_combiner <- function(station, variable,granularity,path_variable, year_envs_reps, phase_start_id, phase_end_id,phase_buffer_days=100){
      # browser()
      
      one_station_list <- list()
      path_env_file <- paste0(path_variable,"/station_",station,"_",variable,"_",granularity,".csv")
      one_env <- read.csv(path_env_file)
      one_env$timestamp <- as.POSIXct(one_env$timestamp)
      
      if(granularity=="hourly"){
        gapfilling_length <- 110
      }else if(granularity =="daily"){
        gapfilling_length <- 4
        
      }
      
      # one_env[[variable]] <- gap_filling_linear(one_env[[variable]], max_gap_length = gapfilling_length)
      
      year_reps <- which(substr(names(year_envs_reps), start = 1, stop=nchar(names(year_envs_reps))-5) == station)
      for(yr in year_reps){
        if(granularity == "daily"){
          start_date <- as.Date(year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_start_id),"Eintrittsdatum"])
          end_date <- as.Date(year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_end_id),"Eintrittsdatum"])+ phase_buffer_days
          one_env$timestamp <- as.Date(one_env$timestamp)
          timespan <- which(one_env$timestamp >= start_date & one_env$timestamp<=end_date)
          time_vect <- one_env$timestamp[timespan]
          
        }else if(granularity =="hourly"){
          start_date <- year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_start_id),"Eintrittsdatum"]
          end_date <- year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_end_id),"Eintrittsdatum"] + phase_buffer_days
          timespan <- which(one_env$timestamp >= start_date & one_env$timestamp<=end_date)
          time_vect <- one_env$timestamp[timespan]
        }
        
        if(length(which(is.na(one_env[[variable]][timespan]))== FALSE)<= length(timespan)/2 & length(timespan)>0){
          # if(length(which(is.na(one_env[[variable]][timespan]))) >= length(one_env[[variable]][timespan])-1){browser()}
          
          gapfilled <- gap_filling_linear(one_env[[variable]][timespan], max_gap_length = gapfilling_length)
          # one_station_list[[names(year_envs_reps)[yr]]] <-gapfilled
          
          one_station_list[[names(year_envs_reps)[yr]]][["timestamp"]] <- time_vect
          one_station_list[[names(year_envs_reps)[yr]]][["data"]] <- gapfilled
          one_station_list[[names(year_envs_reps)[yr]]][["meta"]][["phase_information"]] <- year_envs_reps[[yr]]
          one_station_list[[names(year_envs_reps)[yr]]][["meta"]][["variable"]] <- variable
          
          # one_station_list[[names(year_envs_reps)[yr]]] <- one_env[[variable]]
          
        }
      }
      # browser()
      return(one_station_list)
      # }
    }
    
    
    # stats <- c(3,853)
    # numCores <- detectCores()
    # numCores <- min(numCores,length(unique(subset_pheno)))
    # cl <- makePSOCKcluster(numCores)
    # start_time <- Sys.time()
    # 
    # one_variable_list <- parallel::parLapply(cl, 
    #                                          # unique(subset_pheno), 
    #                                          stats,
    #                                         data_combiner,
    #                                         variable = variable,
    #                                         granularity = granularity, 
    #                                         path_variable = path_variable, 
    #                                         year_envs_reps = year_envs_reps,
    #                                         phase_start_id = phase_start_id,
    #                                         phase_end_id = phase_end_id)
    # 
    # end_time <- Sys.time()
    # print(end_time - start_time)
    # stopCluster(cl)
    start_time <- Sys.time()
    
    one_variable_list <- lapply(unique(subset_pheno),
                                data_combiner,
                                variable = variable,
                                granularity = granularity,
                                path_variable = path_variable,
                                year_envs_reps = year_envs_reps,
                                phase_start_id = phase_start_id,
                                phase_end_id = phase_end_id)
    end_time <- Sys.time()
    print(end_time - start_time)
    

    
    variable_restructured <- list()
    for(i in 1:length(one_variable_list)){
      for(name in names(one_variable_list[[i]]) ){
        if(is.null(name)){
          next
        }else{
          variable_restructured[[name]] <- one_variable_list[[i]][[name]]
        }
      }
    }
    
    # data_path_training <- "P:/Evaluation/Projects/KP0030_ftschurr/data/DWD/historic/climate_phenology_new"
    data_file_name <- paste0(paste(variable,granularity,"training_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")
    data_file_name_valid <- paste0(paste(variable,granularity,"validation_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")
    
    pheno_list_training <- readRDS(paste(data_path_training,data_file_name,sep="/"))
    pheno_list_valid <- readRDS(paste(data_path_training,data_file_name_valid,sep="/"))
    
    # browser()
    training <- variable_restructured[which(names(pheno_list_training) %in% names(variable_restructured))]
    validation <- variable_restructured[which(names(pheno_list_valid) %in% names(variable_restructured))]
    
    out_path_train <- paste(path_data,"climate_phenology_training", sep="/")
    out_path_valid <- paste(path_data,"climate_phenology_validation", sep="/")
    
    dir.create(out_path_train,showWarnings = F, recursive = T)
    dir.create(out_path_valid,showWarnings = F, recursive = T)
    
    saveRDS(training,paste(out_path_train,paste0(variable,"_",granularity,"_training_dataset_wheat_phenology_",pheno_phase,".rds"),sep="/"))
    saveRDS(validation,paste(out_path_valid,paste0(variable,"_",granularity,"_validation_dataset_wheat_phenology_",pheno_phase,".rds"),sep="/"))
    
    
  }
  
}
