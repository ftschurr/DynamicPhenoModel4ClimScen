# Author: Flavian Tschurr
# Project: KP030
# Date: 06.04.2023
# Purpose: dymenvmodel: compare CH2018 with obs - stations
################################################################################

rm(list = ls())


Sys.setenv(LANG = "en")

git_base <- "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"
setwd(git_base)
script_base <- file.path(git_base,"scripts")
# data_path <- "O:/Projects/KP0030_ftschurr/data/CH2018_data"
# data_path_stations <- paste0(data_path,"/observations")
meta_path <-  "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/meta/CH2018"
output_base_path <-"O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output"
# 

source("scripts/00_variable_settings.R")
source(paste0(script_base,"/functions/FUN_skillscores.R"))
library(ggplot2)
library(cowplot)



##
################################## helper tables ##################################
modelchain_table <- read.csv(paste(meta_path,"modelchains/modelchains_CH2018_climate_suitability_rsds.csv",sep="/"),header=TRUE,skip = 1,sep=";")
################################### variables #######################################

stations <- c("TAE", "MAG", "PUY", "SIO", "SMA")
stations <- c("TAE", "RAG", "ELM", "OTL", "MAG", "MER", "NEU", "PAY", "PIO", "PUY", "RUE",
              "SHA", "SIO", "STG", "SBO", "VAD", "VIS", "WYN", "WAE", "REH", "SMA")

stations <- c('TAE','AIG','ALT','RAG','BAS','BER','BUS','CHU','ELM','FAH','GVE','GLA','GUT',
  'INT','OTL','LUG','LUZ','MAG','MER','NEU','CGI','PAY','PIO','PUY','RUE','SHA',
  'SIO','STG','SBO','VAD','VIS','WYN','WAE','REH','SMA','KLO')

sowing_date_determination_method = "fixed_sowing_date"

################################### data reading #######################################

# obs <- read.csv(paste0(paste(output_base_path,"CH2018/model_application",model_run,sowing_date_determination_method,sep="/"),"/observations_",model_run ,".csv"))
obs <- read.csv(paste0(paste(output_base_path,"CH2018/model_application",model_run,sowing_date_determination_method,"combined",sep="/"),"/observations_",model_run,"_fixed-phase-start" ,".csv"))
# CH2018 <- read.csv( paste0(paste(output_base_path,"CH2018/model_application",model_run,sowing_date_determination_method,sep="/"),"/all_modelchains_",model_run ,".csv"))
CH2018 <- read.csv(paste0(paste(output_base_path,"CH2018/model_application",model_run,sowing_date_determination_method,"combined",sep="/"),"/all_modelchains_",model_run,"_fixed-phase-start" ,".csv"))

names(obs) <- paste(names(obs), "obs",sep="_")
names(CH2018) <- paste(names(CH2018), "CH2018", sep="_")

obs$UID <- paste(obs$station_obs,obs$pheno_phase_obs,obs$sowing.year_obs,obs$model_complexity_obs,sep="_")
CH2018$UID <- paste(CH2018$station_CH2018,CH2018$pheno_phase_CH2018,CH2018$sowing.year_CH2018,CH2018$model_complexity_CH2018,sep="_")


CH2018_obs_merged <- merge(CH2018, obs, by="UID")

CH2018_obs_merged <- subset(CH2018_obs_merged, station_CH2018 %in% stations)

CH2018_obs_merged$pheno_phase_CH2018 <- as.factor(CH2018_obs_merged$pheno_phase_CH2018)
CH2018_obs_merged$station_CH2018 <- as.factor(CH2018_obs_merged$station_CH2018)
CH2018_obs_merged$model_complexity_CH2018 <- as.factor(CH2018_obs_merged$model_complexity_CH2018)

CH2018_obs_merged$duration_obs <- as.numeric(CH2018_obs_merged$duration_obs)
CH2018_obs_merged$duration_CH2018 <- as.numeric(CH2018_obs_merged$duration_CH2018)




overview <- ggplot(data=CH2018_obs_merged, aes(x=duration_obs, y =duration_CH2018, color=station_CH2018))+
  geom_point()+ 
  theme_bw()+
  geom_abline(slope=1, intercept=0)+
  xlab("observations")+ ylab("CH2018")+
  facet_grid(model_complexity_CH2018 ~pheno_phase_CH2018, scales = "free")

print(overview)




# modelchain_table <- read.csv(paste(meta_path,"modelchains/modelchains_CH2018_climate_suitability_rsds.csv",sep="/"),header=TRUE,skip = 1,sep=";")

skills_df_per_chain <- data.frame(pheno_phase=NA, model_complexity =NA, cor=NA, RMSE=NA,RRMSE=NA, MAE=NA)
counter <- 1
skills_df_per_station <- data.frame(pheno_phase=NA,station=NA, model_complexity =NA, cor=NA, RMSE=NA,RRMSE=NA,MAE=NA)
counter_stat <- 1

for (compl in unique(CH2018_obs_merged$model_complexity_CH2018)) {
  
  for(pheno_phase in unique(CH2018_obs_merged$pheno_phase_CH2018)){
    # browser()
    modchain_df <- data.frame(modelchain=NA,obs.series=NA,qm.corrected.series=NA, statio=NA)
    modchainCounter <- 1
    for(stat in stations){
      for(RCM in unique(modelchain_table$RCM)){
        one_sub <- subset(CH2018_obs_merged, model_complexity_CH2018 == compl & pheno_phase_CH2018 == pheno_phase & station_CH2018 == stat)
        toMean <- one_sub[grep( RCM, one_sub$modelchain_CH2018),]
        for(yr in unique(toMean$sowing.year_CH2018)){
          toMean_yr <- subset(toMean, sowing.year_CH2018 == yr)
          obs <- mean(toMean_yr$duration_obs)
          qm <- mean(toMean_yr$duration_CH2018)
          modchain_df[modchainCounter,] <- c(RCM,obs,qm, stat)
          modchainCounter <- modchainCounter +1
          # browser()
        }
        
      }
      
      ##
      # browser()
      modchain_df$obs.series <- as.numeric(modchain_df$obs.series)
      modchain_df$qm.corrected.series <- as.numeric(modchain_df$qm.corrected.series)
      modchain_df <- na.omit(modchain_df)
      modchain_stat <- subset(modchain_df, statio == stat)
      
      error <- modchain_stat$obs.series-modchain_stat$qm.corrected.series
      
      # toStandardise <- sd(valTable$obs.series)
      
      
      mae_station <- mean(abs(error))#/toStandardise
      rmse_station <- (sqrt(mean(error**2)))#/toStandardise
      rrmse_station <- calc_RRMSE(measured=modchain_stat$obs.series, modelled = modchain_stat$qm.corrected.series)
      
      # one_sub <- subset(CH2018_obs_merged, model_complexity_CH2018 == compl & pheno_phase_CH2018 == pheno_phase)
      skills_df_per_station[counter_stat,] <- c(pheno_phase, stat, compl,
                                         cor(modchain_stat$obs.series,modchain_stat$qm.corrected.series),
                                         rmse_station,
                                         rrmse_station,
                                         mae_station)
      counter_stat <- counter_stat + 1
      
      ##
      
      
    }
    # browser()
    modchain_df$obs.series <- as.numeric(modchain_df$obs.series)
    modchain_df$qm.corrected.series <- as.numeric(modchain_df$qm.corrected.series)
    modchain_df <- na.omit(modchain_df)
    
    error <- modchain_df$obs.series-modchain_df$qm.corrected.series
    
    # toStandardise <- sd(valTable$obs.series)
    
    
    mae <- mean(abs(error))#/toStandardise
    rmse <- (sqrt(mean(error**2)))#/toStandardise
    rrmse <- calc_RRMSE(measured=modchain_df$obs.series, modelled = modchain_df$qm.corrected.series)
    
    
    one_sub <- subset(CH2018_obs_merged, model_complexity_CH2018 == compl & pheno_phase_CH2018 == pheno_phase)
    skills_df_per_chain[counter,] <- c(pheno_phase, compl,
                                       cor(modchain_df$obs.series,modchain_df$qm.corrected.series),
                                       rmse,
                                       rrmse,
                                       mae)
    counter <- counter + 1
    
  }
}



skills_df_per_chain$cor <- as.numeric(skills_df_per_chain$cor)
skills_df_per_chain$RMSE <- as.numeric(skills_df_per_chain$RMSE)
skills_df_per_chain$RRMSE <- as.numeric(skills_df_per_chain$RRMSE)
skills_df_per_chain$MAE <- as.numeric(skills_df_per_chain$MAE)
skills_df_per_chain$model_complexity <- as.numeric(skills_df_per_chain$model_complexity)
skills_df_per_chain$pheno_phase <- as.factor(skills_df_per_chain$pheno_phase)


outpath_table <- file.path(output_base_path,"CH2018","validation")
dir.create(outpath_table, recursive = T, showWarnings = F)
write.csv(skills_df_per_chain, file.path(outpath_table,"skill_scores_CH2018_obs.csv"))

# Define the desired order of facets
desired_order <- c("sowing-emergence", "emergence-booting", "booting-heading", "heading-senescence")  # Replace with your desired order

# Reorder the pheno_phase variable based on the desired order
skills_df_per_chain$pheno_phase <- factor(skills_df_per_chain$pheno_phase, levels = desired_order)



cor_plot <- ggplot(skills_df_per_chain, aes(x= model_complexity, y= cor))+
  geom_point(aes(color=pheno_phase))+ 
  geom_line(aes(color=pheno_phase))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  # geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "fixed")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "Correlation") +  # Change legend title
  theme_cowplot()


rmse_plot <- ggplot(skills_df_per_chain, aes(x= model_complexity, y= RMSE))+
  geom_point(aes(color=pheno_phase))+ 
  geom_line(aes(color=pheno_phase))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  # geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "RMSE") +  # Change legend title
  theme_cowplot()

rrmse_plot <- ggplot(skills_df_per_chain, aes(x= model_complexity, y= RRMSE))+
  geom_point(aes(color=pheno_phase))+ 
  geom_line(aes(color=pheno_phase))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  # geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2)+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "RRMSE") +  # Change legend title
  theme_cowplot()


mae_plot <- ggplot(skills_df_per_chain, aes(x= model_complexity, y= MAE))+
  geom_point(aes(color=pheno_phase))+ 
  geom_line(aes(color=pheno_phase))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  # geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "MAE") +  # Change legend title
  theme_cowplot()


# save the plots

outpath_figures <- file.path(output_base_path,"plots","CH2018","obs_vs_model")
dir.create(outpath_figures, recursive = T, showWarnings = F)

file_name_cor <- "obs_vs_modelchains_correlation_per_phenophase_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_cor,".pdf")), width = 7, height = 5)
print(cor_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_cor,".png")), width = 7, height = 5, res = 600, units = "in")
print(cor_plot)
dev.off()



file_name_rmse <- "obs_vs_modelchains_RMSE_per_phenophase_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_rmse,".pdf")), width = 7, height = 5)
print(rmse_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_rmse,".png")), width = 7, height = 5, res = 600, units = "in")
print(rmse_plot)
dev.off()


file_name_rrmse <- "obs_vs_modelchains_RRMSE_per_phenophase_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_rrmse,".pdf")), width = 7, height = 5)
print(rrmse_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_rrmse,".png")), width = 7, height = 5, res = 600, units = "in")
print(rrmse_plot)
dev.off()

file_name_mae <- "obs_vs_modelchains_MAE_per_phenophase_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_mae,".pdf")), width = 7, height = 5)
print(mae_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_mae,".png")), width = 7, height = 5, res = 600, units = "in")
print(mae_plot)
dev.off()


#################################################################################







skills_df_per_station$cor <- as.numeric(skills_df_per_station$cor)
skills_df_per_station$RMSE <- as.numeric(skills_df_per_station$RMSE)
skills_df_per_station$RRMSE <- as.numeric(skills_df_per_station$RRMSE)
skills_df_per_station$MAE <- as.numeric(skills_df_per_station$MAE)
skills_df_per_station$model_complexity <- as.numeric(skills_df_per_station$model_complexity)
skills_df_per_station$pheno_phase <- as.factor(skills_df_per_station$pheno_phase)

# Define the desired order of facets
desired_order <- c("sowing-emergence", "emergence-booting", "booting-heading", "heading-senescence")  # Replace with your desired order

# Reorder the pheno_phase variable based on the desired order
skills_df_per_station$pheno_phase <- factor(skills_df_per_station$pheno_phase, levels = desired_order)




cor_station_plot <- ggplot(skills_df_per_station, aes(x= model_complexity, y= cor, color=station))+
  geom_point(aes(color=station))+ 
  # geom_line(aes(color=pheno_phase))+ 
  geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "fixed")+
  labs(color = "MCH Station", x= "# of environmental variables", y= "Correlation") +  # Change legend title
  theme_cowplot()
print(cor_station_plot)



rmse_station_plot <- ggplot(skills_df_per_station, aes(x= model_complexity, y= RMSE, color=station))+
  geom_point(aes(color=station))+ 
  # geom_line(aes(color=pheno_phase))+ 
  geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "free_y")+
  labs(color = "MCH Station", x= "# of environmental variables", y= "RMSE") +  # Change legend title
  theme_cowplot()
print(rmse_station_plot)

rrmse_station_plot <- ggplot(skills_df_per_station, aes(x= model_complexity, y= RRMSE, color=station))+
  geom_point(aes(color=station))+ 
  # geom_line(aes(color=pheno_phase))+ 
  geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "free_y")+
  labs(color = "MCH Station", x= "# of environmental variables", y= "RRMSE") +  # Change legend title
  theme_cowplot()
print(rrmse_station_plot)


mae_station_plot <- ggplot(skills_df_per_station, aes(x= model_complexity, y= MAE, color=station))+
  geom_point(aes(color=station))+ 
  # geom_line(aes(color=pheno_phase))+ 
  geom_smooth(se=F, color="grey50")+
  facet_wrap(~pheno_phase, ncol=2,scales = "free_y")+
  labs(color = "MCH Station", x= "# of environmental variables", y= "MAE") +  # Change legend title
  theme_cowplot()
print(mae_station_plot)



file_name_cor <- "obs_vs_modelchains_correlation_per_phenophase_per_station_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_cor,".pdf")), width = 7, height = 5)
print(cor_station_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_cor,".png")), width = 7, height = 5, res = 600, units = "in")
print(cor_station_plot)
dev.off()



file_name_rmse <- "obs_vs_modelchains_RMSE_per_phenophase_per_station_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_rmse,".pdf")), width = 7, height = 5)
print(rmse_station_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_rmse,".png")), width = 7, height = 5, res = 600, units = "in")
print(rmse_station_plot)
dev.off()

file_name_rrmse <- "obs_vs_modelchains_RRMSE_per_phenophase_per_station_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_rrmse,".pdf")), width = 7, height = 5)
print(rrmse_station_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_rrmse,".png")), width = 7, height = 5, res = 600, units = "in")
print(rrmse_station_plot)
dev.off()

file_name_mae <- "obs_vs_modelchains_MAE_per_phenophase_per_station_fixed-phase-start"

pdf(file.path(outpath_figures,paste0(file_name_mae,".pdf")), width = 7, height = 5)
print(mae_station_plot)
dev.off()

png(file.path(outpath_figures,paste0(file_name_mae,".png")), width = 7, height = 5, res = 600, units = "in")
print(mae_station_plot)
dev.off()

