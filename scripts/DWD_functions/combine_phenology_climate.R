# Author: Flavian Tschurr
# Project: KP030
# Date: 30.05.2022
# Purpose: combine pheno data and DWD envs
################################################################################

library(parallel)
# data path
path_data <- "O:/Projects/KP0030_ftschurr/data/DWD/historic"
source("P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/DyMEnvModel_paper/scripts/functions/FUN_utils.R")
library(stringr)
library(lubridate)

# pheno_data <- read.table(paste(path_data,"phenology/PH_Jahresmelder_Landwirtschaft_Kulturpflanze_Winterweizen_1925_2020_hist.txt",sep="/"), header=T, sep=";")
# pheno_data$Eintrittsdatum <- as.Date(strptime(pheno_data$Eintrittsdatum, format="%Y%m%d"))
# 
# out_list <- list()
# # emergence
# start_id <-12
# # bbch30
# end_id <- 15
# stations <- unique(pheno_data$Stations_id)
# for(station in stations){
#   one_stat <- subset(pheno_data, Stations_id == station)
#   # browser()
#   one_stat <- one_stat[order(one_stat$Eintrittsdatum),]
#   potential_starts <- which(one_stat$Phase_id == start_id)
#   
#   for(pot_start in potential_starts){
#     pot_start_date <- one_stat$Eintrittsdatum[pot_start]
#     one_veg_period <- subset(one_stat, Eintrittsdatum >= pot_start_date & Eintrittsdatum < pot_start_date+300)
#     if(start_id %in% one_veg_period$Phase_id && end_id %in% one_veg_period$Phase_id){
#       yr <- year(pot_start_date)
#       out_list[[paste(station,yr,sep="_")]] <- one_veg_period
#     }
#     
#   }
# 
#   # for(yr in unique(one_stat$Referenzjahr)){
#   #   one_year <- subset(one_stat, Referenzjahr == yr)
#   #   if(start_id %in% one_year$Phase_id && end_id %in% one_year$Phase_id){
#   # 
#   #     out_list[[paste(station,yr,sep="_")]] <- one_year
#   #   }
#   # }
# 
# }

# year_envs_reps <- out_list
# saveRDS(year_envs_reps,paste(path_data,"year_envs_reps_wheat_phenology_DWD.rds",sep="/"))
# correcter version
# pheno_data <- read.table(paste(path_data,"phenology/PH_Jahresmelder_Landwirtschaft_Kulturpflanze_Winterweizen_1925_2020_hist.txt",sep="/"), header=T, sep=";")
# pheno_data$Eintrittsdatum <- as.Date(strptime(pheno_data$Eintrittsdatum, format="%Y%m%d"))
# 
# out_list <- list()
# # emergence
# start_id <-10
# # bbch30
# end_id <- 12
# pheno_phase <- "sowing-emergence"
# 
# stations <- unique(pheno_data$Stations_id)
# for(station in stations){
#   one_stat <- subset(pheno_data, Stations_id == station)
#   # browser()
#   one_stat <- one_stat[order(one_stat$Eintrittsdatum),]
#   potential_starts <- which(one_stat$Phase_id == start_id)
#   
#   for(pot_start in potential_starts){
#     pot_start_date <- one_stat$Eintrittsdatum[pot_start]
#     one_veg_period <- subset(one_stat, Eintrittsdatum >= pot_start_date & Eintrittsdatum < pot_start_date+300)
#     if(start_id %in% one_veg_period$Phase_id && end_id %in% one_veg_period$Phase_id){
#       yr <- year(pot_start_date)
#       out_list[[paste(station,yr,sep="_")]] <- one_veg_period
#     }
#     
#   }
#   
#   # for(yr in unique(one_stat$Referenzjahr)){
#   #   one_year <- subset(one_stat, Referenzjahr == yr)
#   #   if(start_id %in% one_year$Phase_id && end_id %in% one_year$Phase_id){
#   #
#   #     out_list[[paste(station,yr,sep="_")]] <- one_year
#   #   }
#   # }
#   
# }
# 
# 
# year_envs_reps <- out_list
# saveRDS(year_envs_reps,paste(path_data,paste0("year_envs_reps_wheat_phenology_DWD_",pheno_phase,".rds"),sep="/"))
year_envs_reps <- readRDS(paste(path_data,"year_envs_reps_wheat_phenology_DWD.rds",sep="/"))


# sowing-emergence: 10-12, emergence-booting 12-15, booting-heading: 25-18, heading-senescence 18-22

# emergence
phase_start_id <-10
# bbch30
phase_end_id <- 12
pheno_phase <- "sowing-emergence"

# variables <- c("tas","global_radiation","SPI","VPD","RH")
variables <- c("tas","global_radiation","tasmin","tasmax","SPI","VPD","RH")

# hourly variables
variables <- c("tas","global_radiation","SPI","VPD","RH")

granularity <- "hourly"
# granularity <- "daily"


pheno_phases <- c("sowing-emergence","emergence-booting","booting-heading","heading-senescence")
# pheno_phases <- c("booting-heading","heading-senescence")

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
    one_variable_list <- list()
    
    path_variable <- paste(path_data,"climate",granularity,variable,sep="/")
    available_stations <- na.omit(as.numeric(unlist(str_split(pattern="_", list.files(path_variable))),suppressWarnings=T))
    
    pheno_stations <- as.numeric(substr(names(year_envs_reps),start=1, stop=nchar(names(year_envs_reps))-5))
    subset_pheno <- pheno_stations[which(pheno_stations %in% available_stations)]
    data_combiner <- function(station, variable,granularity,path_variable, year_envs_reps, phase_start_id, phase_end_id){
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
           end_date <- as.Date(year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_end_id),"Eintrittsdatum"])
           one_env$timestamp <- as.Date(one_env$timestamp)
           timespan <- which(one_env$timestamp >= start_date & one_env$timestamp<=end_date)
           
         }else if(granularity =="hourly"){
           start_date <- year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_start_id),"Eintrittsdatum"]
           end_date <- year_envs_reps[[yr]][which(year_envs_reps[[yr]]$Phase_id == phase_end_id),"Eintrittsdatum"]
           timespan <- which(one_env$timestamp >= start_date & one_env$timestamp<=end_date)
           
         }
   
         if(length(which(is.na(one_env[[variable]][timespan]))== FALSE)<= length(timespan)/2 & length(timespan)>0){
           if(length(which(is.na(one_env[[variable]][timespan]))) >= length(one_env[[variable]][timespan])-1){browser()}
           
           gapfilled <- gap_filling_linear(one_env[[variable]][timespan], max_gap_length = gapfilling_length)
           one_station_list[[names(year_envs_reps)[yr]]] <-gapfilled
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
        variable_restructured[[name]] <- one_variable_list[[i]][[name]]
      }
      }
      
    out_path <- paste(path_data,"climate_phenology_new", sep="/")
    dir.create(out_path,showWarnings = T, recursive = T)
    
    saveRDS(variable_restructured,paste(out_path,paste0(variable,"_",granularity,"_wheat_phenology_",pheno_phase,".rds"),sep="/"))
    
    
  }
  
  
  
  # create a training and validation set
  
  out_path <- paste(path_data,"climate_phenology_new", sep="/")
  variable <- "global_radiation"
  whole_data <- readRDS(paste(out_path,paste0(variable,"_",granularity,"_wheat_phenology_",pheno_phase,".rds"),sep="/"))
  
  sample_size =0.2
  validation <- sample(names(whole_data), size=length(names(whole_data))*sample_size)
  
  for(variable in variables){
    whole_data <- readRDS(paste(out_path,paste0(variable,"_",granularity,"_wheat_phenology_",pheno_phase,".rds"),sep="/"))
    
     # browser()
     validation_data <- whole_data[validation[which(validation %in% names(whole_data))]]
     # training_data <- whole_data[-validation[which(validation %in% names(whole_data))]]
     training_data <- whole_data[base::setdiff( names(whole_data), validation)]
     
     saveRDS(validation_data,paste(out_path,paste0(variable,"_",granularity,"_validation_dataset_wheat_phenology_",pheno_phase,".rds"),sep="/"))
     saveRDS(training_data,paste(out_path,paste0(variable,"_",granularity,"_training_dataset_wheat_phenology_",pheno_phase,".rds"),sep="/"))
     
}

}


# training_ data 
# 
# quantile(unlist(training_data),probs=c(0.1,0.5,0.9),na.rm=T)
# 
# length(which(is.na(unlist(whole_data))))
# length(((unlist(whole_data))))


# variable = "SPI"
# whole_data <- readRDS(paste(out_path,paste0(variable,"_",granularity,"_wheat_phenology_emergence-booting.rds"),sep="/"))
# hist(unlist(whole_data))

