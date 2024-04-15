# Author: Flavian Tschurr
# Project: KP030
# Date: 16.03.2023
# Purpose: dymenvmodel: acalculate SPI and VPD for CH2018 data on stations
################################################################################


library(SPEI)


Sys.setenv(LANG = "en")

script_base <- "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/scripts"
data_path <- "O:/Projects/KP0030_ftschurr/data/CH2018_data"
data_path_stations <- paste0(data_path,"/QMstations")
meta_path <-  "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/meta/CH2018"
output_base_path <-"O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output"

#vectors with the used stations
stations <- c("SMA","ABO","BER","COV","ZER","MAG","PAY","KLO","LUG")
stations <- c('ABO' , 'AIG' , 'ALT' , 'ANT' , 'ARO' , 'BAS' , 'BEH' , 'BER' , 
              'BIL' , 'BRL' , 'BUF' , 'BUS' , 'CDF' , 'CGI' , 'CHA' , 'CHD' ,
              'CHM' , 'CHU' , 'CIM' , 'COV' , 'DAV' , 'DEM' , 'DIS' , 'DOL' , 
              'EBK' , 'EIN' , 'ELM' , 'ENG' , 'FAH' , 'FRE' , 'GLA' , 'GRA' , 
              'GRC' , 'GRH' , 'GRO' , 'GSB' , 'GST' , 'GUE' , 'GUT' , 'GVE' , 
              'HAI' , 'HIR' , 'HLL' , 'INT' , 'JUN' , 'KLO' , 'KOP' , 'LAG' , 
              'LUG' , 'LUZ' , 'MAG' , 'MER' , 'MLS' , 'MVE' , 'NAP' , 'NEU' , 
              'OTL' , 'PAY' , 'PIL' , 'PIO' , 'PUY' , 'RAG' , 'REH' , 'ROB' , 
              'RUE' , 'SAE' , 'SAM' , 'SBE' , 'SBO' , 'SCU' , 'SHA' , 'SIA' ,
              'SIO' , 'SMA' , 'SMM' , 'STG' , 'TAE' , 'ULR' , 'VAD' , 'VIS' ,
              'WAE' , 'WFJ' , 'WYN' , 'ZER')
stations <- as.list(stations)
# used scenarios
scenarios <- c("RCP26","RCP45","RCP85")
# scenarios <- c("RCP26")

################################## functions ###################################

source(paste0(script_base,"/CH2018_functions/utility/FUN_gapfilling.R"))
# source(paste0(script_base,"/CH2018_functions/utility/FUN_crop_year_slicer.R"))
# source(paste0(script_base,"/CH2018_functions/utility/FUN_period_value.R"))
# source(paste0(script_base,"/CH2018_functions/utility/FUN_utils.R"))
# source(paste0(script_base,"/CH2018_functions/crop_specific/FUN_sowing_date_determination.R"))

################################## helper tables ##################################
modelchain_table <- read.csv(paste(meta_path,"modelchains/modelchains_CH2018_climate_suitability_rsds.csv",sep="/"),header=TRUE,skip = 1,sep=";")

################################## start routine ################################## 
# for(station in stations){
rewriter_VPD_SPI <- function(station, modelchain_table, scenarios,data_path_stations,script_base){
  
  source(paste0(script_base,"/CH2018_functions/utility/FUN_gapfilling.R"))
  require(SPEI)
  
  for(scenario in scenarios){
    

    modelchains <- subset(modelchain_table,rcp==scenario)
    
    for(modelchain in modelchains$chain_name){
      print(paste(station,scenario,modelchain,"will be calculated",sep=" "))
      

      env_data <- list()
      env_variables<- c("tas","pr","hurs")
      for(variable in env_variables){
        print(variable)
        # folder_name <- paste("CH2018",variable,modelchain, scenario,"QMstations_1981-2099_csv",sep="_")
        folder_name <- paste("CH2018",variable,modelchain,"QMstations_1981-2099_csv",sep="_")
        # file_name <- paste0(paste("CH2018",variable,modelchain,scenario,"QMstations_1981-2099",station,sep="_"),".csv")
        file_name <- paste0(paste("CH2018",variable,modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
        
        if(file.exists(paste(data_path_stations,variable,folder_name,file_name,sep="/"))){
          env_data[[variable]] <- read.csv(paste(data_path_stations,variable,folder_name,file_name,sep="/"),skip=16,header=TRUE,sep=";")
          env_data[[variable]]$VALUE <- gapfilling(as.numeric(env_data[[variable]]$VALUE),as.Date(env_data[[variable]]$DATE))
          
        }else{
          env_data[["gaggi"]] <-data.frame(DATE=NA,VALUE=NA)
        }
        if(variable =="tas"){
          header <- read.csv(paste(data_path_stations,variable,folder_name,file_name,sep="/"),header =F,sep=";")[c(1:16),]
          names(header) <- c("DATE","VALUE")
        }
      } # end env_variables loop)
      if("tas" %in% names(env_data) & "hurs" %in% names(env_data)){
        header_VPD <- header
        header_VPD[2,2] <- "VPD"
        header_VPD[3,2] <- "kPa"
        header_VPD[17,] <- c("DATE","VALUE")
        VPD <- (0.61*exp((17.27* env_data[["tas"]]$VALUE)/( env_data[["tas"]]$VALUE+237.3)))*(1-( env_data[["hurs"]]$VALUE/100)) *100 # UPM = hurs, TMK =tas
        env_data[["VPD"]] <- rbind(header_VPD,data.frame("DATE"=env_data[[1]]$DATE, "VALUE"=VPD))
        folder_name_VPD <- paste("CH2018_VPD",modelchain,"QMstations_1981-2099_csv",sep="_")
        file_name_VPD <- paste0(paste("CH2018_VPD",modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
        dir.create(paste(data_path_stations,"VPD",folder_name_VPD,sep="/"),recursive = T,showWarnings = F)
        write.table(env_data[["VPD"]], paste(data_path_stations,"VPD",folder_name_VPD,file_name_VPD,sep="/"),row.names = F,col.names = F, sep=";")
        
      }

      if("pr" %in% names(env_data)){

        header_SPI <- header
        header_SPI[2,2] <- "SPI"
        header_SPI[3,2] <- "30 day smoothed"
        header_SPI[17,] <- c("DATE","VALUE")

        SPI <- spi(env_data[["pr"]]$VALUE,30,na.rm=T)
        env_data[["SPI"]] <- rbind(header_SPI,data.frame("DATE"=env_data[[1]]$DATE, "VALUE"=as.numeric(SPI$fitted)))
        folder_name_SPI <- paste("CH2018_SPI",modelchain,"QMstations_1981-2099_csv",sep="_")
        file_name_SPI <- paste0(paste("CH2018_SPI",modelchain,"QMstations_1981-2099",station,sep="_"),".csv")
        dir.create(paste(data_path_stations,"SPI",folder_name_SPI,sep="/"),recursive = T,showWarnings = F)
        write.table(env_data[["SPI"]], paste(data_path_stations,"SPI",folder_name_SPI,file_name_SPI,sep="/"),row.names = F,col.names = F,sep=";")


      }
    }
  }
}


library(parallel)
cl <- makePSOCKcluster(30)
parLapplyLB(cl,
            stations,
            rewriter_VPD_SPI,
            modelchain_table,
            scenarios,
            data_path_stations,
            script_base)

stopCluster(cl)

# 
# lapply(
#             stations,
#             rewriter_VPD_SPI,
#             modelchain_table,
#             scenarios,
#             data_path_stations,
#             script_base)
