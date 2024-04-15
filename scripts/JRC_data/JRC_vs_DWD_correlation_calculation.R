# Author: Flavian Tschurr
# Project: KP030
# Date: 14.09.2023
# Purpose: correlation dwd JRC
################################################################################


# base_path <- "O:\Projects\KP0030_ftschurr\data\DWD_2023\historic"
# variables <- c("tas","tasmin","tasmax","RH","VPD","global_radiation","SPI")
# 
# for(variable in variables){
#   DWD_data_files <- list.files(file.path(base_path,"climate","daily",variable)) 
#   JRC
#   
#   
# }


# library(parallel)
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

out_path <- "P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output/plots/DWD_vs_JRC"
dir.create(out_path, recursive=T, showWarnings = F)

# variables <- c("VP","tasmin","tasmax","pr")
variables <- c("global_radiation","tas","pr","tasmin","tasmax")
variables <- c("tas","pr","tasmin","tasmax")

# variable <- c("tasmin")
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
  JRC_data_with_DWD$UID <- paste(JRC_data_with_DWD$closest_DWD_station_id,JRC_data_with_DWD$date,sep="_")
  print("JRC formated")
  DWD_data_files <- list.files(file.path(DWD_path,variable),full.names = T)
  DWD_data <- list()
  DWD_data <- lapply(DWD_data_files, read.csv)
  DWD_data_df <- do.call("rbind", DWD_data)
  DWD_data_df$UID <- paste(DWD_data_df$station_id,DWD_data_df$timestamp,sep="_")
  
  print("DWD read")
  JRC_DWD <- merge(DWD_data_df, JRC_data_with_DWD, by="UID")
  
  print("JRC and DWD merged")
  ######################################
  # names(JRC_data_with_DWD)[which(names(JRC_data_with_DWD)== variable)] <- "RADIATION"
  if(variable == "global_radiation"){
    # JRC_data_with_DWD$global_radiation <- JRC_data_with_DWD$RADIATION/10 # adjust unit!
    combined_data <- na.omit(data.frame(DWD= JRC_DWD$global_radiation,JRC = JRC_DWD$RADIATION/10))
    
    axis_adder <- "global radiation (J/cm^2)"
    variable <- "global radiation"
    
  }
  if(variable == "tasmin"){
    plot(JRC_DWD$tasmin,JRC_DWD$TEMPERATURE_MIN)
    combined_data <- na.omit(data.frame(DWD= JRC_DWD$tasmin,JRC = JRC_DWD$TEMPERATURE_MIN))
    # cor(combined_data$DWD,combined_data$JRC)
    axis_adder <- "tasmin (°C)"
    
    # JRC_data_with_DWD$tasmin <- JRC_data_with_DWD$TEMPERATURE_MIN
  }
  if(variable == "tasmax"){
    combined_data <- na.omit(data.frame(DWD= JRC_DWD$tasmax,JRC = JRC_DWD$TEMPERATURE_MAX))
    axis_adder <- "tasmax (°C)"
    # JRC_data_with_DWD$tasmax <- JRC_data_with_DWD$TEMPERATURE_MAX
  }
  if(variable == "tas"){
    combined_data <- na.omit(data.frame(DWD= JRC_DWD$tas,JRC = JRC_DWD$TEMPERATURE_AVG))
    axis_adder <- "tas (°C)"
    # JRC_data_with_DWD$tas <- JRC_data_with_DWD$TEMPERATURE_AVG
  }
  if(variable == "pr"){
    combined_data <- na.omit(data.frame(DWD= JRC_DWD$pr,JRC = JRC_DWD$PRECIPITATION))
    axis_adder <- "pr (mm)"
    # JRC_data_with_DWD$pr <- JRC_data_with_DWD$PRECIPITATION
  }
  
  # if(variable == "VP"){
  #   JRC_data_with_DWD$VP <- JRC_data_with_DWD$VAPOURPRESSURE
  # }
  
  # Create the scatterplot with ggplot2
  library(ggplot2)
  library(cowplot)
  correlation_value <-  cor(combined_data$DWD,combined_data$JRC)
  cor_plot <- ggplot(combined_data, aes(x = DWD, y = JRC)) +
    geom_point() +  # Scatterplot
    geom_abline(intercept = 0, slope = 1, color = "red") +  # 1:1 line
    # geom_text(aes(x = max(combined_data$DWD), y = max(combined_data$JRC), label = paste("Correlation:", round(correlation_value, 2))),
    #           hjust = 1.2, vjust = 0.2) +  # Add text label for correlation
    labs(title = paste0(variable," correlation: ",round(correlation_value, 2) ),
         x =paste0("DWD ",axis_adder),
         y= paste0("JRC ",axis_adder))+  # Title
    theme_cowplot()
  # Print the plot
  print(cor_plot)
  if(variable== "global radiation"){
    variable ="global_radiation"
  }
  file_name_plot <- paste0("correlation_DWD_vs_JRC_",variable)
  pdf(file.path(out_path,paste0(file_name_plot,".pdf")), height = 5, width = 5)
  print(cor_plot)
  dev.off()
  
  
  png(file.path(out_path,paste0(file_name_plot,".png")),units="in", height = 5, width = 5, res=800)
  print(cor_plot)
  dev.off()
  print(paste(variable, "done plotting",sep=" "))
  #################################################################################
  # # stations_list <- as.list(unique(stations$Stations_id))
  # dir.create(file.path(out_path,variable), recursive = T,showWarnings = F)
  # # existing_files <- substr(list.files(file.path(out_path,variable)), start=9, stop=12)
  # 
  # existing_files <- strsplit(list.files(file.path(out_path,variable)),split="_")
  # if(length(existing_files)!=0){
  #   existing_files <- do.call("rbind",existing_files)
  #   existing_files <- existing_files[,2]
  #   
  #   stations_to_do <- stations$Stations_id[-which(stations$Stations_id %in% as.numeric(existing_files))]
  #   stations_list <- as.list(unique(na.omit(stations_to_do)))
  #   
  # }else{
  #   stations_list <- as.list(unique(stations$Stations_id))
  #   
  # }
  # 
  # # stations_list <- rev(stations_list)
  # # library(parallel)
  # print("start writing files")


  
}



