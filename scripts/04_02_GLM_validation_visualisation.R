# Author: Flavian Tschurr
# Project: KP030
# Date: 02.10.2023
# Purpose: phase prediction visulatisation
################################################################################
euler_p <-"/nfs/nas12/fs1202/green_groups_kp_public/"
windows_p <- "P:"
# set paths
euler = F

if(euler == T){
  setwd(file.path(euler_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
}else{
  setwd(file.path(windows_p,"/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"))
  
}

rdsnx = F
################################################################################
# load packages & functions
################################################################################
library(parallel)
library(ggplot2)
library(cowplot)
source("scripts/00_variable_settings.R")
source("scripts/functions/FUN_dose_response_curves.R")
source("scripts/functions/FUN_GLM_prediction.R")

################################################################################


####################################################################################
# read validation scores file
####################################################################################
glm_skillcores_df <- list()
all_best_skillscores <- list()
for(pheno_phase in pheno_phases){
  
  out_path_validation_csv <- file.path(getwd(),"output/validation",model_run)
  file_name_csv <- paste0(paste("dymenv_validation_skillscores",pheno_phase,model_run,sep="_"),".csv")
  glm_skillcores_df[[pheno_phase]] <- read.csv(file.path(out_path_validation_csv,file_name_csv))
  glm_skillcores_df[[pheno_phase]]["pheno_phase"] <- pheno_phase
  
  all_best_skillscores[[pheno_phase]] <- glmpred.skillscore_ranker(df_skillscores = glm_skillcores_df[[pheno_phase]], names_skillscores = c("RMSE"))
  all_best_skillscores[[pheno_phase]]$pheno_phase <- pheno_phase
}
glm_skillcores_df <- do.call("rbind", glm_skillcores_df)

all_best_skillscores <- do.call("rbind", all_best_skillscores)
# save best models per period table
write.csv(all_best_skillscores,file.path(out_path_validation_csv,"best_models_selected_with_RMSE.csv"),row.names = F)
write.csv(glm_skillcores_df,file.path(out_path_validation_csv,"all_models_selected_with_RMSE.csv"),row.names = F)

# create leveled factor
glm_skillcores_df$pheno_phase <- factor(glm_skillcores_df$pheno_phase, levels = pheno_phases)

# cor_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = cor, color=pheno_phase)) +
#   facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
#   geom_point(show.legend = F) + 
#   theme_cowplot() +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   xlab("# of environmental variables") +
#   ylab("Correlation") +  
#   scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
#   scale_color_manual(values=pheno_phase_colors_vect)+
#   geom_hline(data = all_best_skillscores, aes(yintercept = cor)) +
#   geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))
# 
# print(cor_plot)

# with medians instead of smooths 
library(dplyr)

cor_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = cor, color = pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases), scales = "free_y") +
  geom_hline(data = all_best_skillscores, aes(yintercept = cor)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  geom_point(show.legend = FALSE) +
  xlab("# of environmental variables") +
  ylab("Correlation coefficient") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values = pheno_phase_colors_vect) +
  theme_cowplot()

print(cor_plot)



# rmse_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = RMSE, color=pheno_phase)) +
#   facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
#   geom_point(show.legend = F) + 
#   theme_cowplot() +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   xlab("# of environmental variables") +
#   ylab("RMSE (days)") +  
#   scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
#   scale_color_manual(values=pheno_phase_colors_vect)+
#   geom_hline(data = all_best_skillscores, aes(yintercept = RMSE)) +
#   geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))
# 
# print(rmse_plot)

# with median values
rmse_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = RMSE, color=pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
  geom_hline(data = all_best_skillscores, aes(yintercept = RMSE)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  xlab("# of environmental variables") +
  geom_point(show.legend = F) +
  ylab("RMSE (days)") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values=pheno_phase_colors_vect)+
  theme_cowplot()

print(rmse_plot)


# rrmse_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = RRMSE, color=pheno_phase)) +
#   facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
#   geom_point(show.legend = F) + 
#   theme_cowplot() +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   xlab("# of environmental variables") +
#   ylab("RRMSE") +  
#   scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
#   scale_color_manual(values=pheno_phase_colors_vect)+
#   geom_hline(data = all_best_skillscores, aes(yintercept = RRMSE)) +
#   geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))
# 
# print(rrmse_plot)

# with median values
rrmse_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = RRMSE, color=pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  geom_hline(data = all_best_skillscores, aes(yintercept = RRMSE)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  geom_point(show.legend = F) +
  xlab("# of environmental variables") +
  ylab("RRMSE") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values=pheno_phase_colors_vect)+
  theme_cowplot()

print(rrmse_plot)

# 
# mae_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = MAE, color=pheno_phase)) +
#   facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
#   geom_point(show.legend = F) + 
#   theme_cowplot() +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   xlab("# of environmental variables") +
#   ylab("MAE (days)") +  
#   scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
#   scale_color_manual(values=pheno_phase_colors_vect)+
#   geom_hline(data = all_best_skillscores, aes(yintercept = MAE)) +
#   geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))
# 
# print(mae_plot)


# with meidans

mae_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = MAE, color=pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  geom_hline(data = all_best_skillscores, aes(yintercept = MAE)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  geom_point(show.legend = F) +
  xlab("# of environmental variables") +
  ylab("MAE (days)") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values=pheno_phase_colors_vect)+
  theme_cowplot()

print(mae_plot)



# aic_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = AIC, color=pheno_phase)) +
#   facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
#   geom_point(show.legend = F) + 
#   theme_cowplot() +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   xlab("# of environmental variables") +
#   ylab("AIC") +  
#   scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
#   scale_color_manual(values=pheno_phase_colors_vect)+
#   geom_hline(data = all_best_skillscores, aes(yintercept = AIC)) +
#   geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))
# 
# print(aic_plot)


# with median values
aic_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = AIC, color=pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
  geom_hline(data = all_best_skillscores, aes(yintercept = AIC)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  geom_point(show.legend = F) +
  xlab("# of environmental variables") +
  ylab("AIC") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values=pheno_phase_colors_vect)+
  theme_cowplot()

print(aic_plot)



# SLL_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = SumLogLikelihood, color=pheno_phase)) +
#   facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
#   geom_point(show.legend = F) + 
#   theme_cowplot() +
#   geom_smooth(method = "loess", se = FALSE, color = "black") +
#   xlab("# of environmental variables") +
#   ylab("Sum Log Likelihood") +  
#   scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
#   scale_color_manual(values=pheno_phase_colors_vect)+
#   geom_hline(data = all_best_skillscores, aes(yintercept = SumLogLikelihood)) +
#   geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))
# 
# print(SLL_plot)


# with median values

SLL_plot <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = SumLogLikelihood, color=pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y") +
  geom_hline(data = all_best_skillscores, aes(yintercept = SumLogLikelihood)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  geom_point(show.legend = F) + 
  xlab("# of environmental variables") +
  ylab("Sum Log Likelihood") +  
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values=pheno_phase_colors_vect)+
  theme_cowplot()

print(SLL_plot)

################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# combine correlation and rmse in one plot for publication
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
cor_plot_onecol <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = cor, color = pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases), scales = "free_y",ncol = 1) +
  geom_hline(data = all_best_skillscores, aes(yintercept = cor)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  geom_point(show.legend = FALSE) +
  xlab("# of environmental variables") +
  ylab("Correlation coefficient") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values = pheno_phase_colors_vect) +
  theme_cowplot()

print(cor_plot_onecol)


# with median values
rmse_plot_onecol <- ggplot(data = glm_skillcores_df, aes(x = nr_of_variables, y = RMSE, color=pheno_phase)) +
  facet_wrap(~factor(pheno_phase, levels = pheno_phases),scales="free_y",ncol = 1) +
  geom_hline(data = all_best_skillscores, aes(yintercept = RMSE)) +
  geom_vline(data = all_best_skillscores, aes(xintercept = nr_of_variables))+
  stat_summary(fun = "median", geom = "line", aes(group = pheno_phase, color="black"), linewidth = 1, show.legend = F) +  # Draw median line
  xlab("# of environmental variables") +
  geom_point(show.legend = F) +
  ylab("RMSE (days)") +
  scale_x_continuous(breaks = unique(glm_skillcores_df$nr_of_variables)) +
  scale_color_manual(values=pheno_phase_colors_vect)+
  theme_cowplot()

print(rmse_plot_onecol)

library(patchwork)

design = "
AB
"






c(list(cor_plot_onecol,rmse_plot_onecol )) |>
  wrap_plots() + 
  plot_layout(heights = c(20,20),widths = c(25,25),design=design,ncol=2, guides = "collect")


# save plots and selected best models as table
################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################

out_path_plot <- file.path(getwd(),"output/plots/validation_overview",model_run)
dir.create(out_path_plot, recursive = T, showWarnings = F)

################################################################################
# correlation
################################################################################

plot_name_cor <- paste("COR_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_cor,".pdf")),width= 8, height =7)
print(cor_plot)
dev.off()

png(file.path(out_path_plot,paste0(plot_name_cor,".png")),width= 8, height =7, res = 900, units = "in")
print(cor_plot)
dev.off()

################################################################################
# RMSE
################################################################################

plot_name_rmse <- paste("RMSE_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_rmse,".pdf")),width= 8, height =7)
print(rmse_plot)
dev.off()

png(file.path(out_path_plot,paste0(plot_name_rmse,".png")),width= 8, height =7, res = 900, units = "in")
print(rmse_plot)
dev.off()
################################################################################
# COr and RMSE
################################################################################
plot_name_cor_and_rmse <- paste("COR_and_RMSE_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_cor_and_rmse,".pdf")),width= 7, height =8)

c(list(cor_plot_onecol,rmse_plot_onecol )) |>
  wrap_plots() + 
  plot_layout(heights = c(20,20),widths = c(25,25),design=design,ncol=2, guides = "collect")
dev.off()

png(file.path(out_path_plot,paste0(plot_name_cor_and_rmse,".png")),width= 7, height =8, res = 1200, units = "in")

c(list(cor_plot_onecol,rmse_plot_onecol )) |>
  wrap_plots() + 
  plot_layout(heights = c(20,20),widths = c(25,25),design=design,ncol=2, guides = "collect")
dev.off()
################################################################################
# RRMSE
################################################################################

plot_name_rrmse <- paste("RRMSE_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_rrmse,".pdf")),width= 8, height =7)
print(rrmse_plot)
dev.off()

png(file.path(out_path_plot,paste0(plot_name_rrmse,".png")),width= 8, height =7, res = 900, units = "in")
print(rrmse_plot)
dev.off()
################################################################################
# MAE
################################################################################

plot_name_mae <- paste("MAE_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_mae,".pdf")),width= 8, height =7)
print(mae_plot)
dev.off()

png(file.path(out_path_plot,paste0(plot_name_mae,".png")),width= 8, height =7, res = 900, units = "in")
print(mae_plot)
dev.off()

################################################################################
# AIC
################################################################################

plot_name_aic <- paste("AIC_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_aic,".pdf")),width= 8, height =7)
print(aic_plot)
dev.off()

png(file.path(out_path_plot,paste0(plot_name_aic,".png")),width= 8, height =7, res = 900, units = "in")
print(aic_plot)
dev.off()

################################################################################
# SSL
################################################################################

plot_name_sll <- paste("SLL_dymenv_validation_overview",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name_sll,".pdf")),width= 8, height =7)
print(SLL_plot)
dev.off()

png(file.path(out_path_plot,paste0(plot_name_sll,".png")),width= 8, height =7, res = 900, units = "in")
print(SLL_plot)
dev.off()

