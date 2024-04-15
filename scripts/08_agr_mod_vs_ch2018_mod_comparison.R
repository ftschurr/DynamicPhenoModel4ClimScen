# Author: Flavian Tschurr
# Project: KP030
# Date: 06.04.2023
# Purpose: dymenvmodel: compare CH2018  - and agro model
################################################################################
rm(list = ls())

Sys.setenv(LANG = "en")

git_base <- "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"
setwd(git_base)

script_base <- file.path(git_base,"scripts")

source("scripts/00_variable_settings.R")

meta_path <-  "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/meta/CH2018"
output_base_path <-"O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output"
out_path_validation_csv <- file.path(getwd(),"output/validation",model_run)



#

agr_model <- read.csv(file.path(out_path_validation_csv,"all_models_selected_with_RMSE.csv"))
agr_model_best <- list()
for(comp in agr_model$nr_of_variables){
  for(phase in unique(agr_model$pheno_phase) ){
    one_sub <- subset(agr_model, nr_of_variables == comp & pheno_phase ==phase)
    agr_model_best[[paste(phase,comp,sep="-")]] <- one_sub[which.min(one_sub$RMSE),]
    
  }
  
}
agr_model_best <- do.call("rbind",agr_model_best)
agr_model_best$ID <- paste(agr_model_best$pheno_phase,agr_model_best$nr_of_variables,sep="_")
agr_model_best$model <-"agr"

ch2018_model <- read.csv(file.path(output_base_path,"CH2018","validation","skill_scores_CH2018_obs.csv"))
ch2018_model$ID <- paste(ch2018_model$pheno_phase,ch2018_model$model_complexity,sep="_")
ch2018_model$model <-"ch2018"



merged_skills <- merge(agr_model_best,ch2018_model,by="ID", suffixes = c("_agr","_ch2018"))

# Define the desired order of facets
desired_order <- c("sowing-emergence", "emergence-booting", "booting-heading", "heading-senescence")  # Replace with your desired order

# Reorder the pheno_phase variable based on the desired order
merged_skills$pheno_phase_ch2018 <- factor(merged_skills$pheno_phase_ch2018, levels = desired_order)


# calculated differences in skill scores
# merged_skills$RRMSE_diff <-  abs(merged_skills$RRMSE_ch2018 - merged_skills$RRMSE_agr)
# merged_skills$RMSE_diff <-  abs(merged_skills$RMSE_ch2018 - merged_skills$RMSE_agr)
# merged_skills$MAE_diff <-  abs(merged_skills$MAE_ch2018 - merged_skills$MAE_agr)
# merged_skills$cor_diff <-  abs(merged_skills$cor_ch2018 - merged_skills$cor_agr)
merged_skills$RMSE_diff <-  (merged_skills$RMSE_ch2018 + merged_skills$RMSE_agr)/2

ranked_phases <- list()
min_diffs <- list()
for(phase in unique(merged_skills$pheno_phase_ch2018)){
  one_phase <- subset(merged_skills, pheno_phase_ch2018 == phase)
  one_phase$RMSE_rank_agr <- rank(one_phase$RMSE_agr)
  one_phase$RMSE_rank_ch2018 <- rank(one_phase$RMSE_ch2018)
  # get the range to look for the optimum
  potential_complexities <- c(one_phase$model_complexity[which.min(one_phase$RMSE_rank_agr)]:one_phase$model_complexity[which.min(one_phase$RMSE_rank_ch2018)])
  
  #rank according to RRMSE difference  
  one_phase$RRMSE_diff_rank <- NA
  one_phase$RRMSE_diff_rank[potential_complexities] <- rank(one_phase$RMSE_diff[potential_complexities])
  ranked_phases[[phase]] <- one_phase
  # browser()
  min_diffs[[phase]] <- one_phase[which.min(one_phase$RMSE_diff),]
}

min_diffs <- do.call("rbind", min_diffs)
merged_skills <- do.call("rbind", ranked_phases)


## extract vertical lines for plot
library(dplyr)
vline_data_agr <- merged_skills %>%
  group_by(pheno_phase_ch2018) %>%
  filter(RMSE_rank_agr == 1) %>%
  summarise(xintercept = unique(model_complexity), env_variables = unique(env_variables))


vline_data_ch2018 <- merged_skills %>%
  group_by(pheno_phase_ch2018) %>%
  filter(RMSE_rank_ch2018 == 1) %>%
  summarise(xintercept = unique(model_complexity), env_variables = unique(env_variables))

vline_data_diff <- merged_skills %>%
  group_by(pheno_phase_ch2018) %>%
  filter(RRMSE_diff_rank == 1) %>%
  summarise(xintercept = unique(model_complexity), env_variables = unique(env_variables))


vline_data_combined <- bind_rows(
  mutate(vline_data_agr, type = "maxGT"),
  mutate(vline_data_ch2018, type = "maxCS"),
  mutate(vline_data_diff, type = "Opt")
)



#create labels
library(stringr)
merged_skills$xaxis_labels <- str_replace_all(merged_skills$env_variables,"global_radiation","GR")
merged_skills$xaxis_labels <- str_replace_all(merged_skills$xaxis_labels,"_"," +\n")

# 
# # Plot with a single geom_segment statement
# rrmse_diff_plot <- ggplot(merged_skills, aes(x = model_complexity, y = RRMSE_diff)) +
#   geom_line(aes(color = pheno_phase_ch2018)) +
#   geom_segment(data = vline_data_combined, 
#                aes(x = ifelse(type == "diff", xintercept + 0.05, xintercept - 0.05), 
#                    xend = ifelse(type == "diff", xintercept + 0.05, xintercept - 0.05), 
#                    y = -Inf, yend = Inf, color = type, linetype = type), show.legend = F) +
#   scale_color_manual(name = "Best Model", 
#                      values = c(agr = "#3b6353", ch2018 = "#9e2b25", diff = "#7c4660"),
#                      guide = guide_legend(override.aes = list(linetype = c("dashed", "dashed", "solid")))) +
#   scale_linetype_manual(values = c(agr = "dashed", ch2018 = "dashed", diff = "solid")) +
#   facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
#              labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
#   labs(color = "Phenological Phase", x = "# of environmental variables", y = "absolute difference in RRMSE") +
#   scale_x_continuous(breaks = merged_skills$model_complexity, labels = merged_skills$xaxis_labels) +
#   labs(x = "Environmental Variables") +  # Update the x-axis label
#   theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
#   theme_cowplot()
# 
# 
# print(rrmse_diff_plot)

################################################################################
## per season
################################################################################


# sowing to emergence
merged_skills_sowing <- subset(merged_skills, pheno_phase_ch2018 == "sowing-emergence")
vline_data_combined_sowing <- subset(vline_data_combined, pheno_phase_ch2018 == "sowing-emergence")

rrmse_diff_plot_sowing <- ggplot(merged_skills_sowing, aes(x = model_complexity, y = RMSE_diff)) +
  geom_line(aes(color = pheno_phase_ch2018), size=1.2, show.legend = F) +
  geom_segment(data = vline_data_combined_sowing, 
               aes(x = ifelse(type == "Opt", xintercept + 0.05, xintercept - 0.05), 
                   xend = ifelse(type == "Opt", xintercept + 0.05, xintercept - 0.05), 
                   y = -Inf, yend = Inf, color = type, linetype = type), show.legend = F, size=1.2) +
  scale_color_manual(
                      name = "Best Model", 
                     values = c(maxGT = "#3b6353", maxCS = "#9e2b25",Opt = "#7c4660"),
                     labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt"),
                     guide = guide_legend(override.aes = list(linetype = c("dashed", "solid", "dashed")))
  ) +
  scale_linetype_manual(values = c(maxGT = "dashed", maxCS = "dashed",Opt = "solid")) +
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(breaks = merged_skills_sowing$model_complexity, labels = merged_skills_sowing$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()

rrmse_diff_plot_sowing <- rrmse_diff_plot_sowing +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(rrmse_diff_plot_sowing)


# emergence to booting 


merged_skills_emergence <- subset(merged_skills, pheno_phase_ch2018 == "emergence-booting")
vline_data_combined_emergence <- subset(vline_data_combined, pheno_phase_ch2018 == "emergence-booting")

rrmse_diff_plot_emergence <- ggplot(merged_skills_emergence, aes(x = model_complexity, y = RMSE_diff)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1.2,show.legend = F) +
  geom_segment(data = vline_data_combined_emergence, 
               aes(x = ifelse(type == "Opt", xintercept + 0.05, xintercept - 0.05), 
                   xend = ifelse(type == "Opt", xintercept + 0.05, xintercept - 0.05), 
                   y = -Inf, yend = Inf, color = type, linetype = type), show.legend = F,
               size=1.2) +
  scale_color_manual(
    name = "Best Model", 
    values = c(maxGT = "#3b6353",maxCS = "#9e2b25", Opt= "#7c4660"),
    labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt"),
    guide = guide_legend(override.aes = list(linetype = c("dashed", "solid", "dashed")))
  ) +
  scale_linetype_manual(values = c(maxGT = "dashed",maxCS = "dashed", Opt= "solid")) +
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(breaks = merged_skills_emergence$model_complexity, labels = merged_skills_emergence$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()

rrmse_diff_plot_emergence <- rrmse_diff_plot_emergence +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(rrmse_diff_plot_emergence)



# booting-heading 


merged_skills_booting <- subset(merged_skills, pheno_phase_ch2018 == "booting-heading")
vline_data_combined_booting <- subset(vline_data_combined, pheno_phase_ch2018 == "booting-heading")

rrmse_diff_plot_booting <- ggplot(merged_skills_booting, aes(x = model_complexity, y = RMSE_diff)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1.2, show.legend = F) +
  geom_segment(data = vline_data_combined_booting, 
               aes(x = ifelse(type == "Opt", xintercept + 0.05, xintercept - 0.05), 
                   xend = ifelse(type == "Opt", xintercept + 0.05, xintercept - 0.05), 
                   y = -Inf, yend = Inf, color = type, linetype = type), show.legend = F,
               size=1.2) +
  scale_color_manual(
    name = "Best Model", 
    values = c(maxGT = "#3b6353",maxCS = "#9e2b25", Opt= "#7c4660"),
    labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt"),
    guide = guide_legend(override.aes = list(linetype = c("dashed", "solid", "dashed")))
  ) +
  scale_linetype_manual(values = c(maxGT = "dashed",maxCS = "dashed", Opt= "solid")) +
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(breaks = merged_skills_booting$model_complexity, labels = merged_skills_booting$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()
print(rrmse_diff_plot_booting)

rrmse_diff_plot_booting <- rrmse_diff_plot_booting +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label

print(rrmse_diff_plot_booting)


# heading-senescence 


merged_skills_heading <- subset(merged_skills, pheno_phase_ch2018 == "heading-senescence")
vline_data_combined_heading <- subset(vline_data_combined, pheno_phase_ch2018 == "heading-senescence")

rrmse_diff_plot_heading <- ggplot(merged_skills_heading, aes(x = model_complexity, y = RMSE_diff)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1.2, show.legend = F) +
  geom_segment(data = vline_data_combined_heading, 
               aes(x = ifelse(type == "Opt", xintercept - 0.05, xintercept + 0.05), 
                   xend = ifelse(type == "Opt", xintercept - 0.05, xintercept + 0.05), 
                   y = -Inf, yend = Inf, color = type, linetype = type), show.legend = T,
               size=1.2) +
  scale_color_manual(
    name = "Best Model", 
    values = c(maxGT = "#3b6353",maxCS = "#9e2b25", Opt= "#7c4660"),
    labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt"),
    guide = guide_legend(override.aes = list(linetype = c("dashed", "solid", "dashed")))
  ) +
  scale_linetype_manual(values = c(maxGT = "dashed",maxCS = "dashed", Opt= "solid")) +
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_x_continuous(breaks = merged_skills_heading$model_complexity, labels = merged_skills_heading$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()

rrmse_diff_plot_heading <- rrmse_diff_plot_heading +
  labs(x = NULL, y = NULL) +
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label

print(rrmse_diff_plot_heading)


library(patchwork)
p_axis <- rrmse_diff_plot_heading + labs( x = "Environmental Variables used in Model", y = "Absolute Difference in RMSE")
x_axis <- cowplot::get_plot_component(p_axis, "xlab-b")
y_axis <- cowplot::get_plot_component(p_axis, "ylab-l")

design = "
FAB
FCD
#EE
"



################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################

out_path_plot <- file.path(getwd(),"output/plots/agr_vs_ch2018_best_model")
dir.create(out_path_plot, recursive = T, showWarnings = F)
write.csv(vline_data_combined,file.path(out_path_plot,"best_models_agro_ch2018_combination.csv"))

################################################################################
# correlation
################################################################################

plot_name <- paste("agr_vs_ch2018_best_model_with_RMSE_difference",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 10.5, height =8)
c( list(rrmse_diff_plot_sowing,
        rrmse_diff_plot_emergence,
        rrmse_diff_plot_booting,
        rrmse_diff_plot_heading), list( x_axis, y_axis))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")

dev.off()

png(file.path(out_path_plot,paste0(plot_name,".png")),width= 10.5, height =8, res = 1200, units = "in")
c( list(rrmse_diff_plot_sowing,
        rrmse_diff_plot_emergence,
        rrmse_diff_plot_booting,
        rrmse_diff_plot_heading), list( x_axis, y_axis))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")

dev.off()



c( list(rrmse_diff_plot_sowing,
  rrmse_diff_plot_emergence,
  rrmse_diff_plot_booting,
  rrmse_diff_plot_heading), list( x_axis, y_axis))  |>
    wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")


################################################################################
################################################################################
################################################################################
# correlation validaiton



# sowing to emergence

cor_plot_sowing <- ggplot(merged_skills_sowing, aes(x = model_complexity, y = cor_ch2018)) +
  geom_line(aes(color = pheno_phase_ch2018), size=1, show.legend = F) +
  geom_point(aes(color = pheno_phase_ch2018), show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_sowing$model_complexity, labels = merged_skills_sowing$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(cor_plot_sowing)


# emergence to booting 

cor_plot_emergence <- ggplot(merged_skills_emergence, aes(x = model_complexity, y = cor_ch2018,color = pheno_phase_ch2018)) +
  geom_line(size=1,show.legend = F) +
  geom_point( show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_emergence$model_complexity, labels = merged_skills_emergence$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(cor_plot_emergence)



# booting-heading 

cor_plot_booting <- ggplot(merged_skills_booting, aes(x = model_complexity, y = cor_ch2018)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1, show.legend = F) +
  geom_point(aes(color = pheno_phase_ch2018), show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_booting$model_complexity, labels = merged_skills_booting$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(cor_plot_booting)


# heading-senescence 

cor_plot_heading <- ggplot(merged_skills_heading, aes(x = model_complexity, y = cor_ch2018)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1, show.legend = F) +
  geom_point(aes(color = pheno_phase_ch2018), show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_heading$model_complexity, labels = merged_skills_heading$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label

print(cor_plot_heading)


library(patchwork)
p_axis <- cor_plot_heading + labs( x = "Environmental Variables used in Model", y = "Correlation coefficient")
x_axis <- cowplot::get_plot_component(p_axis, "xlab-b")
y_axis_cor <- cowplot::get_plot_component(p_axis, "ylab-l")

design = "
FAB
FCD
#EE
"
c( list(cor_plot_sowing,
        cor_plot_emergence,
        cor_plot_booting,
        cor_plot_heading), list( x_axis, y_axis_cor))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")



################################################################################
# RMSE validation
###############################################################################

# sowing to emergence

RMSE_plot_sowing <- ggplot(merged_skills_sowing, aes(x = model_complexity, y = RMSE_ch2018)) +
  geom_line(aes(color = pheno_phase_ch2018), size=1, show.legend = F) +
  geom_point(aes(color = pheno_phase_ch2018), show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_sowing$model_complexity, labels = merged_skills_sowing$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(RMSE_plot_sowing)


# emergence to booting 

RMSE_plot_emergence <- ggplot(merged_skills_emergence, aes(x = model_complexity, y = RMSE_ch2018,color = pheno_phase_ch2018)) +
  geom_line(size=1,show.legend = F) +
  geom_point( show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_emergence$model_complexity, labels = merged_skills_emergence$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(RMSE_plot_emergence)



# booting-heading 

RMSE_plot_booting <- ggplot(merged_skills_booting, aes(x = model_complexity, y = RMSE_ch2018)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1, show.legend = F) +
  geom_point(aes(color = pheno_phase_ch2018), show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_booting$model_complexity, labels = merged_skills_booting$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label


print(RMSE_plot_booting)


# heading-senescence 

RMSE_plot_heading <- ggplot(merged_skills_heading, aes(x = model_complexity, y = RMSE_ch2018)) +
  geom_line(aes(color = pheno_phase_ch2018),size=1, show.legend = F) +
  geom_point(aes(color = pheno_phase_ch2018), show.legend = F)+
  facet_wrap(~pheno_phase_ch2018, ncol = 2, scales = "free",
             labeller = labeller(pheno_phase_ch2018 = label_parsed)) +
  # labs( x = "Environmental Variables", y = "Absolute Difference in RRMSE") +
  labs(x = NULL, y = NULL)+
  scale_color_manual(values=pheno_phase_colors_vect)+
  scale_x_continuous(breaks = merged_skills_heading$model_complexity, labels = merged_skills_heading$xaxis_labels) +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1))+  # Rotate labels by 90 degrees
  theme_cowplot()+
  theme(axis.text.x = element_text(size = 8))  # Adjust the font size for x-axis label

print(RMSE_plot_heading)


library(patchwork)
p_axis <- RMSE_plot_heading + labs( x = "Environmental Variables used in Model", y = "RMSE (days)")
x_axis <- cowplot::get_plot_component(p_axis, "xlab-b")
y_axis_RMSE <- cowplot::get_plot_component(p_axis, "ylab-l")

design = "
FAB
FCD
#EE
"


c( list(RMSE_plot_sowing,
        RMSE_plot_emergence,
        RMSE_plot_booting,
        RMSE_plot_heading), list( x_axis, y_axis_RMSE))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")



################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################


out_path_plot_validation <- file.path(getwd(),"output/plots/CH2018/obs_vs_model")
dir.create(out_path_plot, recursive = T, showWarnings = F)

################################################################################
# correlation
################################################################################



plot_name <- paste("COR_obs_vs_model_ch2018_with_env_names",model_run,sep="_")

pdf(file.path(out_path_plot_validation,paste0(plot_name,".pdf")),width= 9.5, height =8)
c( list(cor_plot_sowing,
        cor_plot_emergence,
        cor_plot_booting,
        cor_plot_heading), list( x_axis, y_axis_cor))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")


dev.off()

png(file.path(out_path_plot_validation,paste0(plot_name,".png")),width= 9.5, height =8, res = 1200, units = "in")
c( list(cor_plot_sowing,
        cor_plot_emergence,
        cor_plot_booting,
        cor_plot_heading), list( x_axis, y_axis_cor))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")

dev.off()







# ################################################################################
# RMSE
################################################################################
plot_name <- paste("RMSE_obs_vs_model_ch2018_with_env_names",model_run,sep="_")


pdf(file.path(out_path_plot_validation,paste0(plot_name,".pdf")),width= 9.5, height =8)

c( list(RMSE_plot_sowing,
        RMSE_plot_emergence,
        RMSE_plot_booting,
        RMSE_plot_heading), list( x_axis, y_axis_RMSE))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")

dev.off()


png(file.path(out_path_plot_validation,paste0(plot_name,".png")),width= 9.5, height =8, res = 1200, units = "in")
c( list(RMSE_plot_sowing,
        RMSE_plot_emergence,
        RMSE_plot_booting,
        RMSE_plot_heading), list( x_axis, y_axis_RMSE))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20, 1), widths = c(1, 25, 25),design=design, ncol = 2, guides = "collect")

dev.off()



# ################################################################################
# cor and rmse
################################################################################
plot_name <- paste("COR_and_RMSE_obs_vs_model_ch2018_with_env_names",model_run,sep="_")

design_combined = "
JAIE
JBIF
JCIG
JDIH
#KKK
"
pdf(file.path(out_path_plot_validation,paste0(plot_name,".pdf")),width= 8, height =12)

c( list(cor_plot_sowing,
        cor_plot_emergence,
        cor_plot_booting,
        cor_plot_heading,
        RMSE_plot_sowing,
        RMSE_plot_emergence,
        RMSE_plot_booting,
        RMSE_plot_heading), list(y_axis_RMSE,y_axis_cor, x_axis ))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20,20,20, 1), widths = c(1, 25,2,25),design=design_combined, ncol = 2, guides = "collect")
dev.off()

png(file.path(out_path_plot_validation,paste0(plot_name,".png")),width= 8, height =12, res = 1200, units = "in")
c( list(cor_plot_sowing,
        cor_plot_emergence,
        cor_plot_booting,
        cor_plot_heading,
        RMSE_plot_sowing,
        RMSE_plot_emergence,
        RMSE_plot_booting,
        RMSE_plot_heading), list(y_axis_RMSE,y_axis_cor, x_axis ))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20,20,20, 1), widths = c(1, 25,1,25),design=design_combined, ncol = 2, guides = "collect")
dev.off()

################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################



##
agr_ch2018_long <- data.frame(pheno_phase=c(agr_model_best$pheno_phase,ch2018_model$pheno_phase),
                              complexity=c(agr_model_best$nr_of_variables,ch2018_model$model_complexity),
                              model=c(agr_model_best$model,ch2018_model$model),
                              RMSE=c(agr_model_best$RMSE,ch2018_model$RMSE),
                              RRMSE=c(agr_model_best$RRMSE,ch2018_model$RRMSE),
                              MAE=c(agr_model_best$MAE,ch2018_model$MAE),
                              cor=c(agr_model_best$cor,ch2018_model$cor))

desired_order <- c("sowing-emergence", "emergence-booting", "booting-heading", "heading-senescence")  # Replace with your desired order

# Reorder the pheno_phase variable based on the desired order
agr_ch2018_long$pheno_phase <- factor(agr_ch2018_long$pheno_phase, levels = desired_order)

rrmse_diff_plot <- ggplot(agr_ch2018_long, aes(x= complexity, y= MAE,color=model))+
  geom_line()+ 
  # scale_color_manual(values=pheno_phase_colors_vect)+
  facet_wrap(~pheno_phase, ncol=2,scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "RRMSE") +  # Change legend title
  theme_cowplot()
print(rrmse_diff_plot)

###
################################################################################
################################################################################
################################################################################




##

rrmse_diff_plot <- ggplot(merged_skills, aes(x= model_complexity, y= RRMSE_diff))+
  geom_line(aes(color=pheno_phase_ch2018))+ 
  geom_vline(xintercept= merged_skills$RMSE_rank_agr[which(merged_skills$RMSE_rank_agr ==1)] )+
  scale_color_manual(values=pheno_phase_colors_vect)+
  facet_wrap(~pheno_phase_ch2018, ncol=2, scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "absolute difference in RRMSE") +  # Change legend title
  theme_cowplot()
print(rrmse_diff_plot)


ranks_plot <- ggplot(merged_skills)+
  geom_line(aes(x= model_complexity, y= RMSE_rank_agr,color="agr"))+ 
  geom_line(aes(x= model_complexity, y= RMSE_rank_ch2018,color="ch2018"))+ 
  geom_line(aes(x= model_complexity, y= RRMSE_diff_rank,color="difference"))+ 
  # scale_color_manual(values=pheno_phase_colors_vect)+
  facet_wrap(~pheno_phase_ch2018, ncol=2, scales = "free_y")+
  # labs(color = "Phenological Phase", x= "# of environmental variables", y= "absolute difference in RRMSE") +  # Change legend title
  theme_cowplot()
print(ranks_plot)




rmse_diff_plot <- ggplot(merged_skills, aes(x= model_complexity, y= RMSE_diff))+
  geom_line(aes(color=pheno_phase_ch2018))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  facet_wrap(~pheno_phase_ch2018, ncol=2, scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "RMSE") +  # Change legend title
  theme_cowplot()
print(rmse_diff_plot)


mae_diff_plot <- ggplot(merged_skills, aes(x= model_complexity, y= MAE_diff))+
  geom_line(aes(color=pheno_phase_ch2018))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  facet_wrap(~pheno_phase_ch2018, ncol=2, scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "MAE") +  # Change legend title
  theme_cowplot()
print(mae_diff_plot)


cor_diff_plot <- ggplot(merged_skills, aes(x= model_complexity, y= cor_diff))+
  geom_line(aes(color=pheno_phase_ch2018))+ 
  scale_color_manual(values=pheno_phase_colors_vect)+
  facet_wrap(~pheno_phase_ch2018, ncol=2, scales = "free_y")+
  labs(color = "Phenological Phase", x= "# of environmental variables", y= "cor") +  # Change legend title
  theme_cowplot()
print(cor_diff_plot)


names(merged_skills)


plot(merged_skills$model_complexity,merged_skills$RRMSE_agr )
points(merged_skills$model_complexity,merged_skills$RRMSE_ch2018,col="red")
