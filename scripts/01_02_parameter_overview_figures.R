# Author: Flavian Tschurr
# Project: KP030
# Date: 20.06.2022
# Purpose: dymenvmodel: visualize DRC parameters
################################################################################
# paths
windows_p <- "P:"
# set paths

setwd(file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))

data_path <- file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/data/DWD_2023/historic/climate_phenology_DWD_and_JRC")


plot_output_path <- "P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output/plots"

################################################################################
# load packages & functions
################################################################################
library(ggplot2)
library(cowplot)
library(parallel)

source("scripts/00_variable_settings.R")

source("scripts/functions/FUN_utils.R")
source("scripts/functions/FUN_dose_response_curves.R")

################################################################################
# set parameter
################################################################################
# 
# granularity <- "daily"
# crop_abbrev <- "WW"
# 
# pheno_phases <- c("sowing-emergence","emergence-booting",  "booting-heading","heading-senescence")
# 
# 
# # environmental covariates
# env_variable_DRCs_inputs <- list("tas" = c("WangEngels"),
#                                  "tasmin" = c("WangEngels"),
#                                  "tasmax" = c("WangEngels"),
#                                  "RH" = c("reg_linear","non_linear", "asymptotic","WangEngels"),
#                                  "global_radiation" =  c("WangEngels"), # too high global radiation may lead to damage
#                                  "SPI"=c("reg_linear","non_linear", "asymptotic","WangEngels"),
#                                  "VPD" =  c("reg_linear","non_linear", "asymptotic","WangEngels") )

################################################################################
################################################################################
################################################################################
plot_list <- list()
for(env_variable in names(env_variable_DRCs_inputs)){
  combined_output <- list()
  for(pheno_phase in pheno_phases){
  
    print(paste(pheno_phase,env_variable,sep=" : "))
    response_curve_types <- env_variable_DRCs_inputs[[env_variable]]
    
    # create file name
    data_file_name <- paste0(paste(env_variable,granularity,"training_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")
    # load file
    pheno_list <- readRDS(file.path(data_path,data_file_name))
    
    data = seq(from=range(unlist(pheno_list),na.rm=T)[1], to= range(unlist(pheno_list),na.rm=T)[2], by=0.01)
    
    if(env_variable == "RH"){
      data = seq(from=0, to= 100, by=0.01)
    }
    
    if(env_variable %in% c("tas","tasmax","tasmin")){
      data = seq(from=-15, to= 40, by=0.01)
    }
    
    
    if(env_variable == "global_radiation"){
      data = seq(from=0, to= 3500, by=0.1)
    }
    
    if(env_variable == "SPI"){
      data = seq(from=-6, to= 6, by=0.001)
    }
  
    
    # out_repsonse <- matrix(data=NA,nrow = length(data),ncol=(length(response_curve_types)+1))
    # out_repsonse[,1] = data
    # colnames(out_repsonse) <- c("covariate", response_curve_types)
    # 
    out_repsonse <- list()
    
    params_out <- list()
    for(response_curve_type in response_curve_types){
      # catch response function
      .response_function. <- get(paste0(response_curve_type,"_prediction"))
      
      # create directory and file names
      output_path_base <- file.path(getwd(),"output","parameter_model",response_curve_type,crop_abbrev)
      out_file_name <- paste0(response_curve_type,"_parameter_",pheno_phase,"_" , env_variable,"_",granularity,".rds")
      # save output as .rds
      parameters_all <- readRDS(file= file.path(output_path_base,out_file_name))
      
      params <- parameters_all$median_output[[1]]
      names(params) <- parameters_all$median_output$meta$parameter_list
      params_out[[response_curve_type]] <- params
      response_value <- unlist(lapply(data, .response_function., params))
      
      # out_repsonse[,response_curve_type] <- response
      out_repsonse[[response_curve_type]] <- data.frame("covariate" = data, "response"=response_value, "DRC"=rep(response_curve_type,length(data)), "phenological_phase"=rep(pheno_phase,length(data)))
      
    }
    out_repsonse_df <- do.call("rbind",out_repsonse)
    
    if(env_variable == "VPD"){
      no_nonlinear <- subset(out_repsonse_df, DRC != "non_linear")
      out_repsonse_df$response <- ifelse(out_repsonse_df$response < (max(no_nonlinear$response)*1.25), out_repsonse_df$response, NA)
      # browser()
    }
    
    
    out_repsonse_df$DRC <- as.factor(out_repsonse_df$DRC)
    
    # DRC_colors <- c("#003f5c", "#7a5195", "#ef5675", "#ffa600", "#58508d", "#ff6361", "#bc5090")
    
    over_view <- ggplot(data=out_repsonse_df, aes(x=covariate, y=response, color = DRC))+
      geom_line(size=1.1)+
      scale_color_manual(values = DRC_colors_vect, labels= DRC_name_vect)+
      ylab("Dose Response")+
      xlab(env_variable)+
      ggtitle(pheno_phase)+
      theme_cowplot()
    
    dir.create(file.path(plot_output_path,"DRC_overview"), showWarnings = F,recursive = F)
    
    # pdf(paste0(plot_output_path,"/DRC_overview/DRC_",env_variable,"_",pheno_phase,".pdf"),width = 9, height=6)
    # print(over_view)
    # dev.off()
    # 
    # png(paste0(plot_output_path,"/DRC_overview/DRC_",env_variable,"_",pheno_phase,".png"),width = 9, height=6, units = "in",res=650)
    # print(over_view)
    # dev.off()
    
    combined_output[[pheno_phase]] <- out_repsonse_df
    
  }
  
  combined_output_df <- do.call("rbind",combined_output)
  combined_output_df$DRC <- as.factor(combined_output_df$DRC)
  combined_output_df$phenological_phase <- as.factor(combined_output_df$phenological_phase)
  
  if(env_variable =="global_radiation"){
    env_variable_xlab = "GR"
  }else{
    env_variable_xlab = env_variable
  }
  # browser()
  combined_output_df$phenological_phase <- gsub("booting","jointing", combined_output_df$phenological_phase )
  pheno_phases_jointing <-  gsub("booting","jointing",pheno_phases )
  
  over_view_all <- ggplot(data=combined_output_df, aes(x=covariate, y=response, color = DRC))+
    geom_line(size=1.1)+
    # scale_color_manual(values = DRC_colors)+
    scale_color_manual(values = DRC_colors_vect, labels= DRC_name_vect)+
    ylab("Dose Response")+
    xlab(env_variable_xlab)+
    # ggtitle(pheno_phase)+
    facet_wrap(. ~ factor(phenological_phase, levels = pheno_phases_jointing) , ncol = 2, scales = "free_y")+
    theme_cowplot()
  
  plot_list[[env_variable]] <- over_view_all
  
  pdf(paste0(plot_output_path,"/DRC_overview/DRC_all_pheno_phases_",env_variable,".pdf"),width = 10, height=7)
  print(over_view_all)
  dev.off()
  
  png(paste0(plot_output_path,"/DRC_overview/DRC_all_pheno_phases_",env_variable,".png"),width = 10, height=7, units = "in",res=850)
  print(over_view_all)
  dev.off()
  
}



library(patchwork)
# p_axis <- rrmse_diff_plot_heading + labs( x = "Environmental Variables used in Model", y = "Absolute Difference in RRMSE")
# x_axis <- cowplot::get_plot_component(p_axis, "xlab-b")
# y_axis <- cowplot::get_plot_component(p_axis, "ylab-l")

design = "
ABC
DEF
#GH
"



c( plot_list)  |>
  wrap_plots() +  guide_area()+
  plot_layout(design=design, ncol = 3, guides = "collect")


pdf(paste0(plot_output_path,"/DRC_overview/complete_overview_DRC_all_pheno_phases.pdf"),width = 14, height=13)
c( plot_list)  |>
  wrap_plots() + 
  guide_area()+
  plot_layout(design=design, ncol = 3, guides = "collect")
dev.off()

png(paste0(plot_output_path,"/DRC_overview/complete_overview_DRC_all_pheno_phases.png"),width = 14, height=13, units = "in",res=1200)
c( plot_list)  |>
  wrap_plots() + 
  guide_area()+
  plot_layout(design=design, ncol = 3, guides = "collect")
dev.off()


