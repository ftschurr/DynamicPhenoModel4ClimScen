
library(parallel)
# phenology <- read.csv("O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/phenology/PH_Jahresmelder_Landwirtschaft_Kulturpflanze_Winterweizen_1925_2022_hist.csv")

# stations <- read.csv("O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/daily/stations_meta_info.csv",sep=";")

stations <- read.csv("O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/phenology/PH_Beschreibung_Phaenologie_Stationen_Jahresmelder.csv",sep=",")
stations <- stations[which(names(stations) %in% c("geoBreite","geoLaenge","Stations_id"))]
stations$geoBreite <- as.numeric(as.character(stations$geoBreite))
stations$geoLaenge <- as.numeric(as.character(stations$geoLaenge))


station_coords <- list()
for(station in unique(stations$Stations_id)){
  one_station <- subset(stations, Stations_id == station)
  station_coords[[as.character(station)]] <- data.frame(LATITUDE= as.numeric(as.character(one_station$geoBreite[1])), LONGITUDE=as.numeric(as.character(one_station$geoLaenge[1])), station_id= station )
}

stations <- na.omit(stations)

DWD_path <- "O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/daily"

out_path <- "O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_DWD_and_JRC/daily"


variables <- c("VP","tasmin","tasmax","pr")
variables <- c("global_radiation","tas","pr","VP","tasmin","tasmax")

variables <- c("tasmin")
for(variable in variables){
    
  print(variable)
  JRC_data <- read.csv(paste("O:/Projects/KP0030_ftschurr/data/JRC/Germany/",variable,paste0("germany_",variable,"_1979010120221231.csv"),sep="/"))
  print("JRC data read")
  

  # stats <- do.call("rbind",station_coords)
  # unique(JRC_data$GRID_NO)
  unique_JRC_grid_points <- JRC_data[!duplicated(JRC_data$GRID_NO),c(2:4) ]
  
  #############################################
  find_closes_JRC_grid <- function(input_coords, potential_coords_df){
  
    require(geosphere)
    
    # Function to calculate distance between two points
    calculate_distance <- function(lon1, lat1, lon2, lat2) {
      dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
      return(dist)
    }
    
    # Function to find closest point
    find_closest_point <- function(station_lon, station_lat, data_df) {
      distances <- sapply(1:nrow(data_df), function(i) {
        calculate_distance(station_lon, station_lat, data_df[i, "LONGITUDE"], data_df[i, "LATITUDE"])
      })
      distances <- na.omit(distances)
      if(length(distances)==0){
        print("No available grid point")
        closest_point <- data.frame(GRID_NO =NA, LATITUDE=NA, LONGITUDE=NA,dist_to_station_km =NA)
        return(closest_point)
      }else{
        closest_index <- which.min(distances)
        print(paste(min(distances)/1000," km to next weather station", input_coords$station_id,sep=" "))
        closest_point <- data_df[closest_index, ]
        closest_point$dist_to_station_km <-min(distances)/1000
        return(closest_point)
        
      }
    }
  
    
    closest_point <- find_closest_point(input_coords$LONGITUDE, input_coords$LATITUDE, potential_coords_df)
    closest_point$closest_DWD_station_id <- input_coords$station_id
    
    return(closest_point)
  }
  
  
  numCores <- detectCores()
  cl <- makePSOCKcluster(numCores)
  
  
  JRC_to_DWD <- parallel::parLapplyLB(cl,
                                      station_coords,
                                      find_closes_JRC_grid,
                                      unique_JRC_grid_points)
  
  
  
  stopCluster(cl)
  
  print("JRC to DWD merged")
  # JRC_to_DWD <- lapply(station_coords, find_closes_JRC_grid, unique_JRC_grid_points)
  
  
  
  JRC_to_DWD_df <- do.call("rbind",JRC_to_DWD)
  JRC_to_DWD_df <- JRC_to_DWD_df[-which(JRC_to_DWD_df$dist_to_station_km>=20),]
  
  # unique(JRC_to_DWD_df$closest_DWD_station_id)
  
  hist(as.numeric(JRC_to_DWD_df$dist_to_station_km))
  
  
  
  JRC_data_with_DWD <- merge(JRC_data,JRC_to_DWD_df,by="GRID_NO")
  JRC_data_with_DWD$date <- as.Date(as.character(JRC_data_with_DWD$DAY), format="%Y%m%d")
  # names(JRC_data_with_DWD)[which(names(JRC_data_with_DWD)== variable)] <- "RADIATION"
  if(variable == "global_radiation"){
    JRC_data_with_DWD$global_radiation <- JRC_data_with_DWD$RADIATION/10 # adjust unit!
  }
  if(variable == "tasmin"){
    JRC_data_with_DWD$tasmin <- JRC_data_with_DWD$TEMPERATURE_MIN
  }
  if(variable == "tasmax"){
    JRC_data_with_DWD$tasmax <- JRC_data_with_DWD$TEMPERATURE_MAX
  }
  if(variable == "tas"){
    JRC_data_with_DWD$tas <- JRC_data_with_DWD$TEMPERATURE_AVG
  }
  if(variable == "pr"){
    JRC_data_with_DWD$pr <- JRC_data_with_DWD$PRECIPITATION
  }
  
  if(variable == "VP"){
    JRC_data_with_DWD$VP <- JRC_data_with_DWD$VAPOURPRESSURE
  }
  
  

  # variable <- "global_radiation"
  
  read_impute_write_DWD_JRC_data <- function(station,variable ,DWD_path, out_path, JRC_data,granularity="daily"){
    print(station)
    # Function to fill gaps in the time series
    fill_date_gaps <- function(x) {
      require(zoo)
      # Forward fill missing values
      x <- zoo::na.locf(x)
      # Backward fill remaining missing values
      x <- zoo::na.locf(x, fromLast = TRUE)
      return(x)
    }
  
    out_dir <- file.path(out_path,variable)
    file_name <- paste0("station_",station,"_",variable,"_",granularity,".csv")
    if(file.exists(file.path(out_dir,file_name))){
      return("already exists")
    }
    
      file_path <- file.path(DWD_path,variable,paste0("station_",station,"_",variable,"_",granularity,".csv"))
      # try to read in data
      data_file <- tryCatch({suppressWarnings(read.csv(file_path))}, error = function(err) {
        print(paste("Error:", conditionMessage(err)))
        NA
      })
        # subset JRC data
      if(length(which(JRC_data$closest_DWD_station_id == station))==0){
        return(paste("no data available for", station, sep=" : "))
      }
        JRC_one_station <- subset(JRC_data, closest_DWD_station_id == station )
        
      if(is.data.frame(data_file)){
        data_file$date <- as.Date(data_file$timestamp)
        if(length(which(is.na(data_file$date)==T))>0){
          data_file$date <- fill_date_gaps(data_file$date)
        }
        # Determine the earliest start date and the latest end date among all dates in both dataframes
        start_date <- min(min(data_file$date,na.rm=T), min(JRC_one_station$date,na.rm=T),na.rm=T)
        end_date <- max(max(data_file$date,na.rm=T), max(JRC_one_station$date,na.rm=T),na.rm=T)
        
        # Create a new dataframe with the combined date range
        combined_dates <- data.frame(date = seq(start_date, end_date, by = "day"), stringsAsFactors = FALSE)
        
        # Merge the dataframes based on the new date range, giving priority to df1
        combined_df <- merge(combined_dates, data_file, by = "date", all.x = TRUE)
        
        # If there are overlapping dates, fill missing values with data from df2
        combined_df <- merge(combined_df, JRC_one_station, by = "date", all.x = TRUE)
        
        # If there are still missing values, fill them with data from df1
        combined_df[[variable]] <- ifelse(is.na(combined_df[[paste0(variable,".x")]]), combined_df[[paste0(variable,".y")]], combined_df[[paste0(variable,".x")]])
        out_df <- combined_df[, c("station_id","date", variable)]
        out_df$station_id <- unique(data_file$station_id)
        names(out_df) <- names(data_file)[1:3]
      
        # plot(combined_df$timestamp,combined_df$global_radiation, type="l")
        
      }else{
        # browser()
        JRC_one_station <- JRC_one_station[order(JRC_one_station$date),]
        out_df <- data.frame(station_id = station, timestamp= JRC_one_station$date, value = JRC_one_station[[variable]])
        names(out_df) <- c("station_id","timestamp", variable)
      }
        # out_dir <- file.path(out_path,variable)
        # file_name <- paste0("station_",station,"_",variable,"_",granularity,".csv")
        dir.create(out_dir, recursive = T, showWarnings = F)
        
        write.table(out_df, file.path(out_dir,file_name),row.names = F, sep=",")
  }
  
  #################################################################################
  # stations_list <- as.list(unique(stations$Stations_id))
  dir.create(file.path(out_path,variable), recursive = T,showWarnings = F)
  # existing_files <- substr(list.files(file.path(out_path,variable)), start=9, stop=12)
  
  existing_files <- strsplit(list.files(file.path(out_path,variable)),split="_")
  if(length(existing_files)!=0){
    existing_files <- do.call("rbind",existing_files)
    existing_files <- existing_files[,2]
    
    stations_to_do <- stations$Stations_id[-which(stations$Stations_id %in% as.numeric(existing_files))]
    stations_list <- as.list(unique(na.omit(stations_to_do)))
    
  }else{
    stations_list <- as.list(unique(stations$Stations_id))
    
  }
  
  # stations_list <- rev(stations_list)
  # library(parallel)
  print("start writing files")
  # 
  output <- lapply(
              stations_list,
              read_impute_write_DWD_JRC_data,
              variable = variable,
              DWD_path = DWD_path,
              out_path = out_path,
              JRC_data = JRC_data_with_DWD,
              granularity="daily")
  
  # numCores <- detectCores()
  # cl <- makePSOCKcluster(numCores)
  # 
  # start_time <- Sys.time()
  # 
  # output <- parallel::parLapplyLB(cl,
  #                                 stations_list,
  #                                 read_impute_write_DWD_JRC_data,
  #                                 variable = variable,
  #                                 DWD_path = DWD_path,
  #                                 out_path = out_path,
  #                                 JRC_data = JRC_data_with_DWD,
  #                                 granularity="daily")
  # 
  # 
  # 
  # stopCluster(cl)
  # 
  # end_time <- Sys.time()
  # print(end_time - start_time)
  
}


####################################################################################
####################################################################################
####################################################################################
# 
# 
# # take the station available for temperature
# temp_files <- list.files("O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/hourly/tas")
# 
# 
# 
# 
# ## DWD data
# 
# path_weather_data <- "P:/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/daily"
# 
# weather_list <- list()
# variables <- c("global_radiation")
# 
# for(station in JRC_to_DWD_df$closest_DWD_staiton_id){
#   
#   for(variable in variables){
#     path_variable_file <- paste(path_weather_data,variable,paste0("station_",as.numeric(station),"_",variable,"_daily.csv"),sep="/")
#     if(file.exists(path_variable_file)){
#       weather_list[[as.character(station)]] <- read.csv(path_variable_file)
#       weather_list[[as.character(station)]]$timestamp <- as.Date(weather_list[[as.character(station)]]$timestamp)
#       names(weather_list[[as.character(station)]]) <- c("station_id","W_DATE",variable )
#       # weather_list[[as.character(station)]] <- weather_list[[as.character(station)]][, -which(names(weather_list[[as.character(station)]]) == "station_id")]
#     } 
#   }
#   
#   
# }
# 
# DWD_radiation <- do.call("rbind", weather_list)
# JRC_data_with_DWD$date <- as.Date(as.character(JRC_data_with_DWD$DAY), format="%Y%m%d")
# 
# JRC_data_with_DWD$station_id_date <- paste(JRC_data_with_DWD$closest_DWD_staiton_id, JRC_data_with_DWD$date,sep="_")
# JRC_data_with_DWD$RADIATION <- JRC_data_with_DWD$RADIATION /10
# DWD_radiation$station_id_date <- paste(DWD_radiation$station_id,DWD_radiation$W_DATE,sep="_")
# 
# DWD_JRC<- merge(DWD_radiation, JRC_data_with_DWD ,by="station_id_date")
# DWD_JRC$global_radiation <- as.numeric(DWD_JRC$global_radiation)
# test <- na.omit(DWD_JRC)
# cor(test$RADIATION,test$global_radiation)
# hist(test$RADIATION-test$global_radiation)
# mean(test$RADIATION-test$global_radiation)
# plot(test$RADIATION,test$global_radiation)
# 
# library(lme4)
# DWD_JRC$station_id <- as.factor(DWD_JRC$station_id)
# imputing_model_fitting <- lmer(global_radiation ~ RADIATION +(1| station_id), data=DWD_JRC)
# summary(imputing_model_fitting)
# plot(imputing_model_fitting)
# plot(residuals(imputing_model_fitting))
# 
# DWD_JRC$fitted_global_radiation <- predict(imputing_model_fitting,DWD_JRC)
# plot(DWD_JRC$fitted_global_radiation, DWD_JRC$global_radiation)
# test <- na.omit(DWD_JRC)
# cor(test$fitted_global_radiation, test$global_radiation)
# 

# 
# # Load required libraries
# library(geosphere)
# 
# # Function to calculate distance between two points
# calculate_distance <- function(lon1, lat1, lon2, lat2) {
#   dist <- distHaversine(c(lon1, lat1), c(lon2, lat2))
#   return(dist)
# }
# 
# # Function to find closest point
# find_closest_point <- function(station_lon, station_lat, data_df) {
#   distances <- sapply(1:nrow(data_df), function(i) {
#     calculate_distance(station_lon, station_lat, data_df[i, "longitude"], data_df[i, "latitude"])
#   })
#   
#   closest_index <- which.min(distances)
#   print(min(distances))
#   closest_point <- data_df[closest_index, ]
#   
#   return(closest_point)
# }
# 
# # Example usage
# # Assuming you have a dataframe called "station" with longitude and latitude of the given station
# station <- data.frame(longitude = 11.1, latitude = 67.880)
# 
# # Assuming you have another dataframe called "data_df" with longitude and latitude columns
# data_df <- data.frame(
#   longitude = c(11.111, 12.345, 13.579),
#   latitude = c(66.666, 67.890, 68.912)
# )
# 
# closest_point <- find_closest_point(station$longitude, station$latitude, data_df)
# 
# # Print the closest point
# print(closest_point)
# 
# 