# calculate SPI --> out of pr
# get RH tasmin tasmax and VP
# VPD --> get tasmax, tasmin, VP



base_path <- "O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_DWD_and_JRC/daily"


stations <- unlist(lapply(list.files(file.path(data_path, "global_radiation")), function(filename) as.character(strsplit(filename, split = "_")[[1]][2])))




variable_creater <- function(station, base_path){
  out_path_SPI <- paste(base_path, "SPI",sep="/")
  dir.create(out_path_SPI, recursive = T, showWarnings = F)
  out_path_VPD <- paste(base_path, "VPD",sep="/")
  dir.create(out_path_VPD, recursive = T, showWarnings = F)
  out_path_RH <- paste(base_path, "RH",sep="/")
  dir.create(out_path_RH, recursive = T, showWarnings = F)
  
  env_data <- list()
  
  ##############################################################################
  # funcitons
  # Function to calculate Vapor Pressure Deficit (VPD) in hPa
  calculateVPD <- function(max_temp, min_temp, vapor_pressure) {
    # saturation_vapor_pressure <- 0.6108 * exp((17.27 * max_temp) / (max_temp + 237.3))
    saturation_vapor_pressure <- 6.108 * exp((17.27 * max_temp) / (max_temp + 237.3))# vapour pressure in hPA
    
    actual_vapor_pressure <- vapor_pressure - (0.611 * exp((17.27 * min_temp) / (min_temp + 237.3)))
    vpd <- saturation_vapor_pressure - actual_vapor_pressure
    
    return(vpd)
  }
  
  # Function to calculate Relative Humidity (RH) in percentage
  calculateRH <- function(max_temp, min_temp, vapor_pressure) {
    # saturation_vapor_pressure <- 0.6108 * exp((17.27 * max_temp) / (max_temp + 237.3))
    saturation_vapor_pressure <- 6.108 * exp((17.27 * max_temp) / (max_temp + 237.3)) # vapour pressure in hPA
    
    actual_vapor_pressure <- vapor_pressure - (0.611 * exp((17.27 * min_temp) / (min_temp + 237.3)))
    rh <- (actual_vapor_pressure / saturation_vapor_pressure) * 100
    
    return(rh)
  }
    #functions
    gap_filling_linear <- function(data_vect,max_gap_length){
      #'@param  data_vect input data vector
      #'@param max_gap_length maximal length of a NA gap in the data until which interpolation via spline will be done
      #'@description gapfilling routine, using base spline function go fill gaps. If a NA gap is too long (over max_gap_length), NA will by introduced again

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
      # browser()
      return(filled$y)
    }

  ##################################################################################
      
  if(file.exists(file.path(base_path,"pr",paste0("station_",station,"_pr_daily.csv")))){
    env_data[["pr"]] <- read.csv(file.path(base_path,"pr",paste0("station_",station,"_pr_daily.csv")))


      require(SPEI)
      
      
      if(anyNA(env_data[["pr"]]$pr)){
        env_data[["pr"]]$pr <- gap_filling_linear(env_data[["pr"]]$pr, max_gap_length = 50)
        
      }
      # 3 month SPI value
   
      
      SPI <- spi(env_data[["pr"]]$pr,90,na.rm=T)
      out_file_SPI <- data.frame("station_id" = env_data[["pr"]]$station_id, "timestamp"=env_data[["pr"]]$timestamp,"SPI"=as.numeric(SPI$fitted))
      out_name_SPI <- paste0(paste("station",env_data[["pr"]]$station_id[1],"SPI_daily",sep="_"),".csv")
      write.csv(out_file_SPI, paste(out_path_SPI,out_name_SPI,sep="/"),row.names = F)
      

    


  }
  if(file.exists(file.path(base_path,"tasmax",paste0("station_",station,"_tasmax_daily.csv")))){
    env_data[["tasmax"]] <- read.csv(file.path(base_path,"tasmax",paste0("station_",station,"_tasmax_daily.csv")))
    if(anyNA(env_data[["tasmax"]]$tasmax)){
      env_data[["tasmax"]]$tasmax <- gap_filling_linear(env_data[["tasmax"]]$tasmax, max_gap_length = 50)
      
    }

  }
  if(file.exists(file.path(base_path,"tasmin",paste0("station_",station,"_tasmin_daily.csv")))){
    env_data[["tasmin"]] <- read.csv(file.path(base_path,"tasmin",paste0("station_",station,"_tasmin_daily.csv")))
    if(anyNA(env_data[["tasmin"]]$tasmin)){
      env_data[["tasmin"]]$tasmin <- gap_filling_linear(env_data[["tasmin"]]$tasmin, max_gap_length = 50)
      
    }
  }
  
  if(file.exists(file.path(base_path,"VP",paste0("station_",station,"_VP_daily.csv")))){
    env_data[["VP"]] <- read.csv(file.path(base_path,"VP",paste0("station_",station,"_VP_daily.csv")))
    if(anyNA(env_data[["VP"]]$VP)){
      env_data[["VP"]]$VP <- gap_filling_linear(env_data[["VP"]]$VP, max_gap_length = 50)
      
    }

  }
    
  if(length(which(names(env_data)%in% c("tasmin","tasmax","VP")))==3){
    VPD <- calculateVPD(max_temp = env_data[["tasmax"]]$tasmax, 
                        min_temp = env_data[["tasmin"]]$tasmin, 
                        vapor_pressure = env_data[["VP"]]$VP )
    
    RH <- calculateRH(max_temp = env_data[["tasmax"]]$tasmax, 
                        min_temp = env_data[["tasmin"]]$tasmin, 
                        vapor_pressure = env_data[["VP"]]$VP )
    
    VPD_df <- data.frame("station_id" = env_data[["tasmax"]]$station_id, "timestamp"=env_data[["tasmax"]]$timestamp,"VPD"=VPD)
    RH_df <- data.frame("station_id" = env_data[["tasmax"]]$station_id, "timestamp"=env_data[["tasmax"]]$timestamp,"RH"=RH)
    
    out_name_VPD <- paste0(paste("station",env_data[["tasmax"]]$station_id[1],"VPD_daily",sep="_"),".csv")
    write.csv(VPD_df, paste(out_path_VPD,out_name_VPD,sep="/"),row.names = F)
    
    out_name_RH <- paste0(paste("station",env_data[["tasmax"]]$station_id[1],"RH_daily",sep="_"),".csv")
    write.csv(RH_df, paste(out_path_RH,out_name_RH,sep="/"),row.names = F)
    
  }
  
  
}


# 
# lapply( stations,
#                     variable_creater,
#                     base_path)


library(parallel)
numCores <- detectCores()
cl <- makePSOCKcluster(numCores)
start_time <- Sys.time()
parallel::parLapply(cl, stations,
                    variable_creater,
                    base_path)
end_time <- Sys.time()
print(end_time - start_time)
stopCluster(cl)
