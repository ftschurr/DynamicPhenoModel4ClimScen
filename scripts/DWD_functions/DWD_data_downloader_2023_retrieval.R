# Author: Flavian Tschurr
# Project: KP030
# Date: 14.03.2023
# Purpose: download and process data from DWD
################################################################################

data <- read.table("O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/phenology/PH_Jahresmelder_Landwirtschaft_Kulturpflanze_Winterweizen_1925_2022_hist.csv", header=T, sep=",")
# finding the correct stations which are needed
###############################################################################

# length(which(data$Phase_id==12))
# data_start_here <- subset(data, Phase_id == 12)
# length(which(data_start_here$Phase_id== 15))

# out_list <- list()
# start_id <-12
# end_id <- 15
# stations <- unique(data$Stations_id)
# for(station in stations){
#   one_stat <- subset(data, Stations_id == station)
# 
#   for(yr in unique(one_stat$Referenzjahr)){
#     one_year <- subset(one_stat, Referenzjahr == yr)
#     if(start_id %in% one_year$Phase_id && end_id %in% one_year$Phase_id){
# 
#       out_list[[paste(station,yr,sep="_")]] <- one_year
#     }
#   }
# 
# }
# 
# library(stringr)
# all <- str_split(names(out_list), pattern="_")
# stations <- NULL
# for(i in 1:length(all)){
#   stations[i] <- str_split(names(out_list[i]),pattern="_")[[1]][1]
# }
# stations <- unique(stations)
# stations <- as.numeric(stations)
# stations <- stringr::str_pad(stations, 5,pad="0")
# stations_df <- data.frame(station_id=as.character(stations))
# write.csv(stations_df,"O:/Projects/KP0030_ftschurr/data/DWD_2023/stations_pheno.csv" )

###############################################################################
# downlaod data
###############################################################################

library(stringr)

stations <- read.csv("O:/Projects/KP0030_ftschurr/data/DWD_2023/stations_pheno.csv")$station_id
stations <- stringr::str_pad(stations, 5,pad="0")

# https://github.com/earthobservations/dwd-grib-downloader
# https://statisticsglobe.com/download-file-in-r-example
# https://statistics.berkeley.edu/computing/faqs/reading-web-pages-r

# thepage = readLines('https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate/hourly/precipitation/historical/')
# test <- thepage[10]


#####
granularities <- c("daily")
granularities <- c("hourly","10_minutes")

variables <- c("ST","RR","TU")
variable_names <- c("solar","precipitation","air_temperature")
# variables <- c("KL","ST")
# variable_names <- c("kl","solar")
names(variables) <- variable_names
download_path <- "O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/zips"



names(variables) <- variable_names
for(granularity in granularities){
  base_link_DWD <- paste("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate", granularity,sep="/")
  for( i in 1:length(variables)){
    vari_name <- names(variables)[i]
    vari <- variables[i]
    if(vari == "ST"){
      link_DWD_vari <- paste0(paste(base_link_DWD,vari_name,sep="/"),"/")
      
    }else{
      link_DWD_vari <- paste(base_link_DWD,vari_name,"historical/",sep="/")
      
    }
    # search on webpage for the stations
    data_page <- readLines(link_DWD_vari)
    # function
    data_download_unzip <- function(station, vari,data_page,link_DWD_vari,granularity){
      pages <- data_page[grep(pattern=paste0("_",vari,"_", station,"_"),data_page)]
      for(page in pages){
        # browser()
        file_name <- str_split(page,pattern='"')[[1]][2]
        download_link <- paste0(link_DWD_vari,file_name)
        
        zip_path <- paste(download_path,granularity,vari,sep="/")
        dir.create(zip_path,recursive = T,showWarnings = F)
        zip_path_file <- paste(zip_path,file_name,sep="/")
        
        download.file(download_link,zip_path_file)

                unzip_dir <- paste(zip_path,substring(file_name,first=1,last=nchar(file_name)-4),sep="/")
        dir.create(unzip_dir, recursive = T, showWarnings = F)
        setwd(unzip_dir)
        # unzip(zip_path_file, list=T, exdir = unzip_dir, unzip = "unzip")
        unzip(zip_path_file)
        file.remove(zip_path_file)
        
        
      }
      
    }
    
    sapply(stations, data_download_unzip, vari=vari, data_page=data_page,link_DWD_vari = link_DWD_vari,granularity=granularity)
    # zip_downloader(station,vari,data_page,link_DWD_vari,granularity)
    
    # pages <- data_page[grep(pattern=paste0("_",vari,"_", station,"_"),data_page)]
    # str_split(pages,pattern='"')

  }
  
}

################################################################################
# download recent 
vari <- "KL"
vari_name <- "kl"
for(granularity in granularities){
  base_link_DWD <- paste("https://opendata.dwd.de/climate_environment/CDC/observations_germany/climate", granularity,sep="/")
  # for( i in 1:length(variables)){
    # vari_name <- names(variables)[i]
    # vari <- variables[i]

    link_DWD_vari <- paste(base_link_DWD,vari_name,"recent/",sep="/")

    # search on webpage for the stations
    data_page <- readLines(link_DWD_vari)
    # function
    data_download_unzip <- function(station, vari,data_page,link_DWD_vari,granularity){
      pages <- data_page[grep(pattern=paste0("_",vari,"_", station,"_"),data_page)]
      for(page in pages){
        # browser()
        file_name <- str_split(page,pattern='"')[[1]][2]
        download_link <- paste0(link_DWD_vari,file_name)
        
        zip_path <- paste(download_path,granularity,vari,sep="/")
        dir.create(zip_path,recursive = T,showWarnings = F)
        zip_path_file <- paste(zip_path,file_name,sep="/")
        
        download.file(download_link,zip_path_file)
        
        unzip_dir <- paste(zip_path,substring(file_name,first=1,last=nchar(file_name)-4),sep="/")
        dir.create(unzip_dir, recursive = T, showWarnings = F)
        setwd(unzip_dir)
        # unzip(zip_path_file, list=T, exdir = unzip_dir, unzip = "unzip")
        unzip(zip_path_file)
        file.remove(zip_path_file)
        
        
      }
      
    }
    
    sapply(stations, data_download_unzip, vari=vari, data_page=data_page,link_DWD_vari = link_DWD_vari,granularity=granularity)
    # zip_downloader(station,vari,data_page,link_DWD_vari,granularity)
    
    # pages <- data_page[grep(pattern=paste0("_",vari,"_", station,"_"),data_page)]
    # str_split(pages,pattern='"')
    
  # }
  
}
################################################################################
# combine historic and akt data
################################################################################
for(granularity in granularities){
    base_output_path <- paste("O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/zips",granularity,"KL",sep="/")
    all_dirs <- list.files(base_output_path)
  for(station in stations){
    files_station <- all_dirs[grep(station, all_dirs)]
    if(length(files_station)==0){
      next
    } else if(length(files_station)== 1){
      in_folder <- paste(base_output_path,files_station[1],sep="/")
      files <- list.files(in_folder)
      # open is
      correct_file <- files[grep(pattern="produkt_",files)]
      
      one_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=";",header=T)
      
      out_path <- paste(base_output_path,paste0(station,"_combined"),sep="/")
      dir.create(out_path, recursive = T,showWarnings = F)
      write.table(one_file, paste(out_path,correct_file,sep="/"))
      
    }else if(length(files_station)==2){
      in_folder <- paste(base_output_path,files_station[1],sep="/")
      files <- list.files(in_folder)
      # open is
      correct_file <- files[grep(pattern="produkt_",files)]
      
      f_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=";",header=T)
      # second file
      in_folder <- paste(base_output_path,files_station[2],sep="/")
      files <- list.files(in_folder)
      # open is
      correct_file <- files[grep(pattern="produkt_",files)]
      
      s_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=";",header=T)
      
      if(grepl("_akt",in_folder)){
        comb <- rbind(f_file,s_file)
      }else{
        comb <- rbind(s_file,f_file)
      }

      
      out_path <- paste(base_output_path,paste0(station,"_combined"),sep="/")
      dir.create(out_path, recursive = T,showWarnings = F)
      write.table(comb, paste(out_path,correct_file,sep="/"))
    }
                              
                              
  }
}

################################################################################
# clean data, bring into smooth format
################################################################################


# routine

variables <- c("RR","TU","ST")
# variables <- c("KL","ST")
# variables <- c("KL")

base_output_path <- "O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate"
granularities <- c("daily")
granularities <- c("hourly")

download_path <- "O:/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate/zips"

for (variable in variables) {
  for(granularity in granularities){
    print(paste(variable,granularity,sep=" : "))
    # create in and output path
    in_path <- paste(download_path, granularity, variable, sep="/")
    # out_path <- paste(base_output_path, granularity,variable,sep="/")
    out_path_base <- paste(base_output_path, granularity,sep="/")
    dir.create(out_path_base,recursive = T,showWarnings = F)
    in_folders <- list.dirs(in_path,recursive = F)
    if(variable == "KL"){
      in_folders <- in_folders[grep("_combined",in_folders)]
    }
    
    # browser()
    
    RR_cleaner <- function(in_folder, out_path_base,granularity){
      
      out_path <- paste(out_path_base, "pr",sep="/")
      dir.create(out_path,recursive = T,showWarnings = F)
      out_path_SPI <- paste(out_path_base, "SPI",sep="/")
      dir.create(out_path_SPI,recursive = T,showWarnings = F)
      
      files <- list.files(in_folder)
      
      correct_file <- files[grep(pattern="produkt_",files)]
      
      one_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=";",header=T)
      
      # one_file$MESS_DATUM <- as.POSIXlt(one_file$MESS_DATUM,format="%Y%m%d%H", origin="2022-05-25 16 CEST")
      one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d%H")
      one_file$R1 <- ifelse(one_file$R1 == -999, NA, one_file$R1)
      
      out_file <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"pr"=one_file$R1)
      out_name <- paste0(paste("station",one_file$STATIONS_ID[1],"pr",granularity,sep="_"),".csv")
      write.csv(out_file, paste(out_path,out_name,sep="/"),row.names = F)

            # SPI
      require(SPEI)
      
      #functions
      gap_filling_linear <- function(data_vect,max_gap_length){
        #'@param  data_vect input data vector
        #'@param max_gap_length maximal length of a NA gap in the data until which interpolation via spline will be done
        #'@description gapfilling routine, using base spline function go fill gaps. If a NA gap is too long (over max_gap_length), NA will by introduced again
        #'
        # x <- c(1:length(data_vect))
        # filled <- spline(x,data_vect,n=length(data_vect))
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
      out_file$pr <- gap_filling_linear(out_file$pr, max_gap_length = 50)
      # 3 month SPI value
      
      SPI <- spi(out_file$pr,720,na.rm=T)
      out_file_SPI <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"SPI"=as.numeric(SPI$fitted))
      out_name_SPI <- paste0(paste("station",one_file$STATIONS_ID[1],"SPI",granularity,sep="_"),".csv")
      write.csv(out_file_SPI, paste(out_path_SPI,out_name_SPI,sep="/"),row.names = F)
      
      print(one_file$STATIONS_ID[1])
      
    }
    
    TU_cleaner <- function(in_folder, out_path_base,granularity){
      
      out_path_tas <- paste(out_path_base, "tas",sep="/")
      dir.create(out_path_tas,recursive = T,showWarnings = F)
      
      out_path_RH <- paste(out_path_base, "RH",sep="/")
      dir.create(out_path_RH,recursive = T,showWarnings = F)
      
      out_path_VPD <- paste(out_path_base, "VPD",sep="/")
      dir.create(out_path_VPD,recursive = T,showWarnings = F)
      
      files <- list.files(in_folder)
      
      correct_file <- files[grep(pattern="produkt_",files)]
      
      one_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=";",header=T)
      one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d%H")
      # replace -999 with NA
      one_file$RF_TU <- ifelse(one_file$RF_TU == -999, NA, one_file$RF_TU)
      one_file$TT_TU <- ifelse(one_file$TT_TU == -999, NA, one_file$TT_TU)
      
      out_file_RH <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"RH"=one_file$RF_TU)
      out_name_RH <- paste0(paste("station",one_file$STATIONS_ID[1],"RH",granularity,sep="_"),".csv")
      write.csv(out_file_RH, paste(out_path_RH,out_name_RH,sep="/"),row.names = F)
      
      out_file_tas <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"tas"=one_file$TT_TU)
      out_name_tas <- paste0(paste("station",one_file$STATIONS_ID[1],"tas",granularity,sep="_"),".csv")
      write.csv(out_file_tas, paste(out_path_tas,out_name_tas,sep="/"),row.names = F)
      
      VPD <- (0.61*exp((17.27*one_file$TT_TU)/(one_file$TT_TU+237.3)))*(1-(one_file$RF_TU/100))
      out_file_VPD <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"VPD"=VPD)
      out_name_VPD <- paste0(paste("station",one_file$STATIONS_ID[1],"VPD",granularity,sep="_"),".csv")
      write.csv(out_file_VPD, paste(out_path_VPD,out_name_VPD,sep="/"),row.names = F)
      
      # write.csv(out_file, paste(out_path,out_name,sep="/"),row.names = F)
      print(one_file$STATIONS_ID[1])
      
    }
    
    ST_cleaner <- function(in_folder, out_path_base,granularity){
      
      out_path <- paste(out_path_base, "global_radiation",sep="/")
      dir.create(out_path,recursive = T,showWarnings = F)
   
      files <- list.files(in_folder)
      
      correct_file <- files[grep(pattern="produkt_",files)]
      
      one_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=";",header=T)
      
      if(granularity == "daily"){
        variable <- "FG_STRAHL"
      }else if(granularity =="hourly"){
        variable <- "FG_LBERG"
        
      }
      
      # one_file$MESS_DATUM <- as.POSIXlt(one_file$MESS_DATUM,format="%Y%m%d%H", origin="2022-05-25 16 CEST")
      # one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d%H")
      if(granularity == "hourly"){
        one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d%H")
      }else if(granularity =="daily"){
        one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d")
      }
      one_file[[variable]] <- ifelse(one_file[[variable]] == -999, NA, one_file[[variable]])
      
      # # convert J/cm^2 into W/m^2
      # if(granularity == "hourly"){
      #   nr_secs <- 60*60
      # }
      # W_m2 <- one_file$FG_LBERG * nr_secs / 10000
      # browser()
      out_file <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"global_radiation"=one_file[[variable]])
      out_name <- paste0(paste("station",one_file$STATIONS_ID[1],"global_radiation",granularity,sep="_"),".csv")
      write.csv(out_file, paste(out_path,out_name,sep="/"),row.names = F)

      print(one_file$STATIONS_ID[1])
      
    }
    
    
    KL_cleaner <- function(in_folder, out_path_base,granularity){
      # browser()
      # TNK = tasmin, TXK = tasmax, TMK = tas, RSK =pr, FM= scfWind, UPM = RH
      variables <- c("TNK","TXK","TMK","RSK","FM","UPM")
      names(variables) <- c("tasmin","tasmax","tas","pr","scfWind","RH")
      # find correct file
      files <- list.files(in_folder)
      # open is
      correct_file <- files[grep(pattern="produkt_",files)]
      one_file <- read.table(paste(in_folder,correct_file,sep="/"),sep=" ",header=T)
      if(granularity == "hourly"){
        one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d%H")
      }else if(granularity =="daily"){
        one_file$timestamp <- strptime(one_file$MESS_DATUM, format="%Y%m%d")
      }
      
      for( vari in 1:length(variables)){
        variable <- variables[vari]
        variable_name <- names(variables)[vari]
        
        out_path_variable <-   paste(out_path_base, variable_name,sep="/")
        dir.create(out_path_variable,recursive = T,showWarnings = F)
        
        one_file[[variable]] <- ifelse( one_file[[variable]] == -999, NA, one_file[[variable]])
        
        
        out_file_variable <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,variable_name=one_file[[variable]])
        names(out_file_variable) <- c("station_id","timestamp",variable_name)
        out_name_variable <- paste0(paste("station",one_file$STATIONS_ID[1],variable_name,granularity,sep="_"),".csv")
        write.csv(out_file_variable, paste(out_path_variable,out_name_variable,sep="/"),row.names = F)
        
        
      }
      
      
      # calculate VPD --> allready transofrmed into NA etc. from previous steps
      VPD <- (0.61*exp((17.27*one_file$TMK)/(one_file$TMK+237.3)))*(1-(one_file$UPM/100))
      
      out_file_VPD <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"VPD"=VPD)
      out_name_VPD <- paste0(paste("station",one_file$STATIONS_ID[1],"VPD",granularity,sep="_"),".csv")
      out_path_VPD <-   paste(out_path_base, "VPD",sep="/")
      dir.create(out_path_VPD,recursive = T,showWarnings = F)
      write.csv(out_file_VPD, paste(out_path_VPD,out_name_VPD,sep="/"),row.names = F)
      
      # calculate SPI
      require(SPEI)
      
      #functions
      gap_filling_linear <- function(data_vect,max_gap_length){
        #'@param  data_vect input data vector
        #'@param max_gap_length maximal length of a NA gap in the data until which interpolation via spline will be done
        #'@description gapfilling routine, using base spline function go fill gaps. If a NA gap is too long (over max_gap_length), NA will by introduced again
        #'
        # x <- c(1:length(data_vect))
        # filled <- spline(x,data_vect,n=length(data_vect))
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
      out_file <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"pr"=one_file$RSK)
      if(length(out_file$pr)> length(which(is.na(out_file$pr)==TRUE))){
        out_file$pr <- gap_filling_linear(out_file$pr, max_gap_length = 5)
        # 3 month SPI value
        
        SPI <- spi(out_file$pr,30,na.rm=T)
        out_file_SPI <- data.frame("station_id" = one_file$STATIONS_ID, "timestamp"=one_file$timestamp,"SPI"=as.numeric(SPI$fitted))
        out_name_SPI <- paste0(paste("station",one_file$STATIONS_ID[1],"SPI",granularity,sep="_"),".csv")
        out_path_SPI <-   paste(out_path_base, "SPI",sep="/")
        dir.create(out_path_SPI,recursive = T,showWarnings = F)
        
        write.csv(out_file_SPI, paste(out_path_SPI,out_name_SPI,sep="/"),row.names = F)
      }

  
      print(one_file$STATIONS_ID[1])
      
    }
    
    
    .cleaner_fun. <- get(paste0(variable,"_cleaner"))
    library(parallel)
    numCores <- detectCores()
    cl <- makePSOCKcluster(numCores)
    start_time <- Sys.time()
    parallel::parLapply(cl, in_folders,
                        .cleaner_fun.,
                        out_path_base,
                        granularity)
    end_time <- Sys.time()
    print(end_time - start_time)
    stopCluster(cl)
    
    # lapply(in_folders, .cleaner_fun., out_path ,granularity)
    
  }
}


































