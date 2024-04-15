# Author: Flavian Tschurr
# Project: KP030
# Date: 12.12.2023
# Purpose: dymenvmodel: compare CH2018  - and agro model in future cliamtes
################################################################################


rm(list = ls())


Sys.setenv(LANG = "en")

git_base <- "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel"
setwd(git_base)
script_base <- file.path(git_base,"scripts")
meta_path <-  "O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/meta/CH2018"
output_base_path <-"O:/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output"
# 

source("scripts/00_variable_settings.R")
source(paste0(script_base,"/functions/FUN_skillscores.R"))
library(ggplot2)
library(cowplot)
library(patchwork)
period_names <- c("reference","2035","2060","2085")
scenarios <- c("RCP26", "RCP45", "RCP85")

period_table <- read.table(file.path(getwd(),"meta","CH2018","periods","CH2018_periods_FT.txt"), header=T,sep="/")


################################## helper tables ##################################
modelchain_table <- read.csv(paste(meta_path,"modelchains/modelchains_CH2018_climate_suitability_rsds.csv",sep="/"),header=TRUE,skip = 1,sep=";")
################################### variables #######################################

stations <- c('TAE','AIG','ALT','RAG','BAS','BER','BUS','CHU','ELM','FAH','GVE','GLA','GUT',
              'INT','OTL','LUG','LUZ','MAG','MER','NEU','CGI','PAY','PIO','PUY','RUE','SHA',
              'SIO','STG','SBO','VAD','VIS','WYN','WAE','REH','SMA','KLO')

sowing_date_determination_method = "fixed_sowing_date"

################################### data reading #######################################

obs <- read.csv(paste0(paste(output_base_path,"CH2018/model_application_best_model_selection",model_run,sowing_date_determination_method,"combined",sep="/"),"/observations_",model_run ,".csv"))
obs <- subset(obs, station %in% stations)

CH2018 <- read.csv(paste0(paste(output_base_path,"CH2018/model_application_best_model_selection",model_run,sowing_date_determination_method,"combined",sep="/"),"/all_modelchains_",model_run,".csv"))
CH2018 <- subset(CH2018, station %in% stations)

CH2018 <- rbind(obs, CH2018)
CH2018$pheno_phase <- factor(CH2018$pheno_phase, levels=c("sowing-emergence", "emergence-booting",  "booting-heading" ,   "heading-senescence"))

model_selection <- read.csv(file.path(getwd(),"output","plots","agr_vs_ch2018_best_model","best_models_agro_ch2018_combination.csv"))


# rename booting to jointing
CH2018$pheno_phase <- gsub("booting","jointing", CH2018$pheno_phase )
model_selection$pheno_phase_ch2018 <- gsub("booting","jointing", model_selection$pheno_phase_ch2018 )
names(pheno_phase_colors_vect) <- gsub("booting","jointing",names(pheno_phase_colors_vect))
pheno_phases <-  gsub("booting","jointing", pheno_phases )



pheno_phase_model_list <- list()
# select different phase
# select the three models
for(pheno_ph in pheno_phases){
  one_phase <- subset(CH2018, pheno_phase == pheno_ph)
  best_models <- subset(model_selection, pheno_phase_ch2018 == pheno_ph)
  for(model in best_models$type){
    # browser()
    
    # complexity_wanted <- best_models$xintercept[which(best_models$type==model)]
    # pheno_phase_model_list[[paste(pheno_ph,model,sep = "_")]] <- subset(one_phase,model_complexity == complexity_wanted)
    pheno_phase_model_list[[paste(pheno_ph,model,sep = "_")]] <- subset(one_phase,model_type == model)
    
    pheno_phase_model_list[[paste(pheno_ph,model,sep = "_")]]$model <- model
  }
}


# cut each model into 30 years period, get mean per period and chain

period_cutter_mean_calculator <- function(pheno_phase_model_df){
library(dplyr)
  out_list <- list()
  for (period in unique(period_table$period_name)) {
    one_period_definer <- period_table[which(period_table$period_name == period),]
    period_years <- c(one_period_definer$start:one_period_definer$stop)
    one_period <- pheno_phase_model_df[which(pheno_phase_model_df$sowing.year %in% period_years),]
    # browser()
    for (scen in unique(one_period$scenario)) {
      one_scenario <- subset(one_period, scenario == scen)
      # browser()
      result <- one_scenario %>%
        group_by(station) %>%
        summarise(mean_value = mean(duration, na.rm = TRUE),
                  median_value= median(duration,na.rm=T),
                  q5 = as.numeric(quantile(duration,probs=c(0.05),na.rm = T)),
                  q95 = as.numeric(quantile(duration,probs=c(0.95),na.rm = T)),
                  end_doy_median = median(as.numeric(format(as.Date(end_date),format="%j"))) )
      result$scenario = scen
      result$period = period
      result$pheno_phase = one_scenario$pheno_phase[1]
      result$model = one_scenario$model[1] 
      out_list[[paste(period,scen,sep="_")]] <- result
      
      
    }
    
  }
  # browser()
  out_df <- do.call("rbind",out_list)
  return(out_df)
}


statistics_boxplot_list <- lapply(pheno_phase_model_list, period_cutter_mean_calculator)
statistics_boxplot_df <- do.call("rbind", statistics_boxplot_list)

statistics_boxplot_df$scenario_header <-statistics_boxplot_df$scenario
statistics_boxplot_df$scenario_header <- ifelse(statistics_boxplot_df$scenario_header=="RCP26","RCP 2.6", statistics_boxplot_df$scenario_header)
statistics_boxplot_df$scenario_header <- ifelse(statistics_boxplot_df$scenario_header=="RCP45","RCP 4.5", statistics_boxplot_df$scenario_header)
statistics_boxplot_df$scenario_header <- ifelse(statistics_boxplot_df$scenario_header=="RCP85","RCP 8.5", statistics_boxplot_df$scenario_header)


# make plot
statistics_boxplot_df_2035_2085 <- subset(statistics_boxplot_df, period %in% c("2035","2085") & scenario !="RCP45")

# model_colors <- c(maxGT = "#3b6353",maxCS = "#9e2b25", Opt= "#7c4660")
model_colors <- c(maxGT = "#1A6FBF",maxCS = "#9e2b25", Opt= "#7c4660") # cbf


scenario_colors <- c("RCP26" = "#abcfdb", "RCP45" = "#fbda87", "RCP85" = "#8B0000" )
scenario_colors <- c("RCP26" = "#abcfdb", "RCP45" = "#fbda87", "RCP85" = "#9F3D3E" )



overview_boxplot <- ggplot(subset(statistics_boxplot_df_2035_2085, scenario !="RCP45"),aes(x=model, y= median_value,fill=model))+
  geom_boxplot()+
  scale_fill_manual(values = model_colors, guide = "none") +
  facet_wrap(pheno_phase~period~scenario, ncol=4,scales = "free_y")+
  theme_cowplot()+
  theme(strip.background =element_rect(fill="red"))
print(overview_boxplot)



overview_plot_list_generator <- function(input_data,model_colors,scenario_colors){
  
  plot_list <- list()
  
  for(peri in unique(input_data$period)){
    for(rcp in unique(input_data$scenario)){
      header_col <- scenario_colors[which(names(scenario_colors)==rcp)]
      
      for(pheno_ph in unique(input_data$pheno_phase)){
        ylims <- range(subset(input_data, pheno_phase == pheno_ph)$median_value)
        
        plot_list[[paste(peri,rcp,pheno_ph,sep="_")]] <- ggplot(subset(input_data, period ==peri & scenario == rcp & pheno_phase == pheno_ph),aes(x=model, y= median_value,fill=model))+
          geom_boxplot()+
          scale_fill_manual(values = model_colors, guide = "none") +
          facet_wrap(pheno_phase~period~scenario_header, ncol=1,scales = "free_y")+
          theme_cowplot()+
          labs(x = NULL, y = NULL) +
          ylim(c(round(ylims[1]-0.1*ylims[1]),round(ylims[2]+0.1*ylims[2])))+
          scale_x_discrete(labels = c("maxGT" = expression("Max"[GT]),
                                      "maxCS" = expression("Max"[CS]),
                                      "Opt" = "Opt"))
        # browser()
        if(pheno_ph !="heading-senescence"){
          
          plot_list[[paste(peri,rcp,pheno_ph,sep="_")]] <-plot_list[[paste(peri,rcp,pheno_ph,sep="_")]] +
            theme(strip.background =element_rect(fill=header_col))+
            theme(axis.text.x = element_blank())
          
        }else{
          plot_list[[paste(peri,rcp,pheno_ph,sep="_")]] <-plot_list[[paste(peri,rcp,pheno_ph,sep="_")]] +
            theme(strip.background =element_rect(fill=header_col),
                  axis.text.x = element_text(size = 10))
        }
        
        print( plot_list[[paste(peri,rcp,pheno_ph,sep="_")]])
        # browser()
      }
      
      
    }
    
  }
  return(plot_list)
}



overview_plots <- overview_plot_list_generator(statistics_boxplot_df_2035_2085,model_colors,scenario_colors)





library(patchwork)
p_axis <- overview_plots[[1]] + labs( x = "2035", y = "Duration (days)")
x_axis_2035 <- cowplot::get_plot_component(p_axis, "xlab-b")
y_axis <- cowplot::get_plot_component(p_axis, "ylab-l")

p_axis <- overview_plots[[1]] + labs( x = "2085", y = "Duration (days)")
x_axis_2085 <- cowplot::get_plot_component(p_axis, "xlab-b")

design = "
QAEIM
QBFJN
QCGKO
QDHLP
#RRSS
"





c(overview_plots , list(  y_axis,x_axis_2035,x_axis_2085))  |>
  wrap_plots() + 
  plot_layout(heights = c(20,20,20,20,1), widths = c(1, 20, 20,20,20),design=design,ncol=5, guides = "collect")

################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################

out_path_plot <- file.path(getwd(),"output/plots/agr_vs_ch2018_best_model")
dir.create(out_path_plot, recursive = T, showWarnings = F)

################################################################################
# correlation
################################################################################

plot_name <- paste("model_comparison_RCP26andRCP85_2035and2085_fixed_sowing_free_start",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 12, height =10)
c(overview_plots , list(  y_axis,x_axis_2035,x_axis_2085))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20,20,20, 1), widths = c(1, 20, 20,20,20),design=design,ncol=5, guides = "collect")

dev.off()

png(file.path(out_path_plot,paste0(plot_name,".png")),width= 12, height =10, res = 1200, units = "in")
c(overview_plots , list(  y_axis,x_axis_2035,x_axis_2085))  |>
  wrap_plots() + 
  plot_layout(heights = c(20, 20,20,20, 1), widths = c(1, 20, 20,20,20),design=design,ncol=5, guides = "collect")

dev.off()






################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot over time
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################
DOY_sowing <- as.numeric(format(as.Date("2022-10-30"),"%j"))
adder <- 365-DOY_sowing

statistics_boxplot_df$end_das_median <- statistics_boxplot_df$end_doy_median + adder
statistics_boxplot_df$end_das_median <- ifelse(statistics_boxplot_df$end_das_median >= 365, statistics_boxplot_df$end_das_median -365, statistics_boxplot_df$end_das_median )

plot_df <- data.frame(scenario=NA, period=NA, model=NA, emergence_start=NA, emergence_end=NA, booting_start=NA,booting_end=NA, heading_start=NA,heading_end=NA, senescence_start=NA,senescence_end=NA,emergence_median=NA,
                      booting_median=NA, heading_median=NA, senescence_median=NA)
counter <- 1
for(scen in unique(statistics_boxplot_df$scenario)){
  for(peri in unique(statistics_boxplot_df$period)){
    for(mod in unique(statistics_boxplot_df$model)){
      if(scen=="obs" & peri != "reference"){
          next
        }
      one_sub <- subset(statistics_boxplot_df, scenario == scen & period == peri & model == mod)
      
      plot_df[counter,] <- c(scen,
                             peri,
                             mod,
                             min(subset(one_sub,pheno_phase == "sowing-emergence")$end_das_median),
                             max(subset(one_sub,pheno_phase == "sowing-emergence")$end_das_median),
                             min(subset(one_sub,pheno_phase == "emergence-booting")$end_das_median),
                             max(subset(one_sub,pheno_phase == "emergence-booting")$end_das_median),
                             min(subset(one_sub,pheno_phase == "booting-heading")$end_das_median),
                             max(subset(one_sub,pheno_phase == "booting-heading")$end_das_median),
                             min(subset(one_sub,pheno_phase == "heading-senescence")$end_das_median),
                             max(subset(one_sub,pheno_phase == "heading-senescence")$end_das_median),
                             median(subset(one_sub,pheno_phase == "sowing-emergence")$end_das_median),
                             median(subset(one_sub,pheno_phase == "emergence-booting")$end_das_median),
                             median(subset(one_sub,pheno_phase == "booting-heading")$end_das_median),
                             median(subset(one_sub,pheno_phase == "heading-senescence")$end_das_median)
                             
                             
      )
      counter <- counter + 1
      
    }
  }
  
  
}

for(i in c(4:15)){
  plot_df[,i] <- as.numeric(plot_df[,i])
}

plot_df_sub <- subset(plot_df, scenario %in% c("obs","RCP26","RCP85") & period %in% c("reference","2035","2085"))
plot_df_sub$factor <- paste(plot_df_sub$scenario,plot_df_sub$period,sep="_")
plot_df_sub <- subset(plot_df_sub, factor != "RCP26_reference" & factor != "RCP85_reference")
plot_df_sub$factor <- as.factor(paste(plot_df_sub$scenario,plot_df_sub$period, plot_df_sub$model,sep="_"))
plot_df_sub$factor_nr <- as.numeric(as.factor(paste(plot_df_sub$scenario,plot_df_sub$period, plot_df_sub$model,sep="_")))
plot_df_sub$facet <-paste(plot_df_sub$scenario,plot_df_sub$period ,sep="\n")
plot_df_sub$facet <-gsub("RCP(\\d)(\\d*)", "RCP \\1.\\2", plot_df_sub$facet )
plot_df_sub$facet <- gsub("obs","Observations",plot_df_sub$facet)


# 
# ggplot(plot_df_sub,aes(color=factor)) +
#   geom_segment(aes(x = emergence_start, xend = emergence_end, y = factor_nr, yend = factor_nr), size = 2) +
#   geom_segment(aes(x = booting_start, xend = booting_end, y = factor_nr+0.1, yend = factor_nr+0.1), size = 2) +
#   geom_segment(aes(x = heading_start, xend = heading_end, y = factor_nr-0.1, yend = factor_nr-0.1), size = 2) +
#   geom_segment(aes(x = senescence_start, xend = senescence_end, y = factor_nr, yend = factor_nr), size = 2) +
#   facet_wrap(~facet, ncol=1)+
#   labs(x = "X-axis", y = "Y-axis", title = "Multiple Line Segments") +
#   theme_minimal()

time_plot_list <- list()
ablines_df <- subset(plot_df_sub, period == "reference" & model=="Opt")
ablines_vect <- c(ablines_df$emergence_median,ablines_df$booting_median, ablines_df$heading_median, ablines_df$senescence_median)-ablines_df$emergence_median

for(peri_scen in unique(plot_df_sub$facet)){
  # print(ablines_vect)
  one_peri_scen <- subset(plot_df_sub, facet == peri_scen)
  one_peri_scen$model <- as.factor(one_peri_scen$model)

  one_peri_scen$model_nr <- as.numeric(one_peri_scen$model)
  
  one_peri_scen[,c(4:15)] <- one_peri_scen[,c(4:15)]-one_peri_scen$emergence_median
  # browser()
  
  time_plot_list[[peri_scen]] <- ggplot(one_peri_scen, aes(color = factor)) +
    geom_vline(xintercept=ablines_vect)+
    geom_point(aes(x=emergence_median, y=model_nr), size=4, color = pheno_phase_colors_vect["sowing-emergence"])+
    geom_point(aes(x=booting_median, y=model_nr+0.1), size=4, color = pheno_phase_colors_vect["emergence-booting"])+
    geom_point(aes(x=heading_median, y=model_nr-0.1), size=4, color = pheno_phase_colors_vect["booting-heading"])+
    geom_point(aes(x=senescence_median, y=model_nr), size=4, color = pheno_phase_colors_vect["heading-senescence"])+
    geom_segment(aes(x = emergence_start, xend = emergence_end, y = model_nr, yend = model_nr), size = 2, color = pheno_phase_colors_vect["sowing-emergence"]) +
    geom_segment(aes(x = booting_start, xend = booting_end, y = model_nr + 0.1, yend = model_nr + 0.1), size = 2, color = pheno_phase_colors_vect["emergence-booting"]) +
    geom_segment(aes(x = heading_start, xend = heading_end, y = model_nr - 0.1, yend = model_nr - 0.1), size = 2, color = pheno_phase_colors_vect["booting-heading"]) +
    geom_segment(aes(x = senescence_start, xend = senescence_end, y = model_nr, yend = model_nr), size = 2, color = pheno_phase_colors_vect["heading-senescence"]) +
    facet_wrap(~facet, ncol = 1) +
    xlim(c(-6,268))+
    labs(x = "Days after emergence", y = unique(one_peri_scen$facet)) +
    scale_y_continuous(breaks = unique(one_peri_scen$model_nr),
                       # labels = unique(one_peri_scen$model)
                       labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt"),
                       
                       )+  # Replace y-axis ticks with factor levels
    xlab(NULL)+
    theme_cowplot()+
    theme(axis.text.x = element_blank())+
    theme(strip.text = element_blank(), strip.background = element_blank())
  

  if(peri_scen ==unique(plot_df_sub$facet)[length(unique(plot_df_sub$facet))] ){
    
    time_plot_list[[peri_scen]] <- ggplot(one_peri_scen, aes(color = factor)) +
      geom_vline(xintercept=ablines_vect)+
      geom_point(aes(x=emergence_median, y=model_nr), size=4, color = pheno_phase_colors_vect["sowing-emergence"])+
      geom_point(aes(x=booting_median, y=model_nr+0.1), size=4, color = pheno_phase_colors_vect["emergence-booting"])+
      geom_point(aes(x=heading_median, y=model_nr-0.1), size=4, color = pheno_phase_colors_vect["booting-heading"])+
      geom_point(aes(x=senescence_median, y=model_nr), size=4, color = pheno_phase_colors_vect["heading-senescence"])+
      geom_segment(aes(x = emergence_start, xend = emergence_end, y = model_nr, yend = model_nr), size = 2, color = pheno_phase_colors_vect["sowing-emergence"]) +
      geom_segment(aes(x = booting_start, xend = booting_end, y = model_nr + 0.1, yend = model_nr + 0.1), size = 2, color = pheno_phase_colors_vect["emergence-booting"]) +
      geom_segment(aes(x = heading_start, xend = heading_end, y = model_nr - 0.1, yend = model_nr - 0.1), size = 2, color = pheno_phase_colors_vect["booting-heading"]) +
      geom_segment(aes(x = senescence_start, xend = senescence_end, y = model_nr, yend = model_nr), size = 2, color = pheno_phase_colors_vect["heading-senescence"]) +
      facet_wrap(~facet, ncol = 1) +
      xlim(c(-6,268))+
      labs(x = "Days after sowing", y = unique(one_peri_scen$facet)) +
      scale_y_continuous(breaks = unique(one_peri_scen$model_nr), 
                         # labels = unique(one_peri_scen$model)
                         labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt"),
                         )+  # Replace y-axis ticks with factor levels
      xlab("Days after emergence")+
    theme_cowplot()+
      # theme(axis.text.x = element_blank())+
      theme(strip.text = element_blank(), strip.background = element_blank())

    
  }
  
}


# Create a blank plot with only the legend
legend_plot <- ggplot(data= statistics_boxplot_df, aes(x=median_value, y= station,color= pheno_phase)) +
  geom_point() +  # Adding a point to generate legend
  scale_color_manual(values=pheno_phase_colors_vect,
                     labels =c("emergence","booting","heading","senescence"))+
  theme_void()+  # Remove axes and background
  labs(color="Phenlogy stage")


library(patchwork)

design = "
#F
A#
B#
C#
D#
E#
"






c(time_plot_list ,list(legend_plot)) |>
  wrap_plots() + 
  plot_layout(heights = c(0.0000000001,20, 20,20,20,20),widths = c(20,0.000000001),design=design,ncol=1, guides = "collect") &
  theme(legend.position = 'bottom',
  legend.direction = 'horizontal')
                  



################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################

out_path_plot <- file.path(getwd(),"output/plots/agr_vs_ch2018_best_model")
dir.create(out_path_plot, recursive = T, showWarnings = F)

################################################################################
# correlation
################################################################################

plot_name <- paste("ablines_model_comparison_over_time_RCP26andRCP85_referenceand2035and2085_fixed_sowing_free_start",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 12, height =10)

c(time_plot_list ,list(legend_plot)) |>
  wrap_plots() + 
  plot_layout(heights = c(0.0000000001,20, 20,20,20,20),widths = c(20,0.000000001),design=design,ncol=1, guides = "collect") &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


dev.off()

png(file.path(out_path_plot,paste0(plot_name,".png")),width= 12, height =10, res = 1200, units = "in")

c(time_plot_list ,list(legend_plot)) |>
  wrap_plots() + 
  plot_layout(heights = c(0.0000000001,20, 20,20,20,20),widths = c(20,0.000000001),design=design,ncol=1, guides = "collect") &
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal')


dev.off()





################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# select one station
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################

# CH2018_REH_RCP85 <- subset(CH2018, station =="REH" & scenario =="RCP85")

# 

# CH2018_REH_RCP85 <- subset(CH2018, scenario %in% c("RCP85","obs"))

# chosen_phase="sowing-emergence"
# chosen_phase="emergence-booting"
# chosen_phase="booting-heading"
# 
# chosen_phase="heading-senescence"

# scenarios

for(scen in scenarios){
  
  
  for(sta in stations){
    
  
  
  time_plot_pheno_stage <- list()
  
  for(pheno_ph in pheno_phases){
    print(pheno_ph)
    CH2018_REH_RCP85 <- subset(CH2018, station ==sta & scenario ==scen)
    # CH2018_REH_obs <- subset(CH2018, station =="REH" & scenario =="obs")
    # CH2018_REH_RCP85 <- rbind(CH2018_REH_RCP85,CH2018_REH_obs)
    
    # browser()
    
    # CH2018_REH_RCP85 <- subset(CH2018_REH_RCP85,model_complexity %in% model_selection$xintercept[which(model_selection$pheno_phase_ch2018 ==pheno_ph)])
    # CH2018_REH_RCP85 <- subset(CH2018_REH_RCP85,model_complexity %in% model_selection$xintercept[which(model_selection$pheno_phase_ch2018 ==pheno_ph)])
    
    CH2018_REH_RCP85 <- subset(CH2018_REH_RCP85, pheno_phase ==pheno_ph)
    
    DOY_sowing <- as.numeric(format(as.Date("2022-10-30"),"%j"))
    
    adder <- 365-DOY_sowing
    CH2018_REH_RCP85$end_das <- as.numeric(format(as.Date(CH2018_REH_RCP85$end_date),format="%j"))+ adder
    CH2018_REH_RCP85$end_das  <- ifelse(CH2018_REH_RCP85$end_das >= 364, CH2018_REH_RCP85$end_das-364,CH2018_REH_RCP85$end_das)
    
    # CH2018_REH_RCP85$model_type <- CH2018_REH_RCP85$model_complexity
    # for(mod in which(model_selection$pheno_phase_ch2018 ==pheno_ph)){
    #   # model_selection[mod,"xintercept"]
    #   CH2018_REH_RCP85$model_type = ifelse(CH2018_REH_RCP85$model_type == model_selection[mod,"xintercept"],model_selection[mod,"type"],CH2018_REH_RCP85$model_type)
    # }
    # CH2018_REH_RCP85$model_type <- ifelse(CH2018_REH_RCP85$model_type =="CH2018", "CH2018 / Combination",CH2018_REH_RCP85$model_type )
    CH2018_REH_RCP85$model_type <- as.factor(CH2018_REH_RCP85$model_type)
    
    library(zoo)
    
    
    rollmean_CH2018_REH_RCP85 <- CH2018_REH_RCP85 %>%
      arrange(sowing.year) %>%
      group_by(scenario,model_type,modelchain) %>%
      summarise(roll_mean_end_das = rollmean(end_das,30,na.pad = TRUE)
      ) %>%
      ungroup()
    
    rollmean_CH2018_REH_RCP85$sowing.year <- CH2018_REH_RCP85$sowing.year
    # rollmean_CH2018_REH_RCP85$UID <- paste(rollmean_CH2018_REH_RCP85$scenario,rollmean_CH2018_REH_RCP85$model_type,rollmean_CH2018_REH_RCP85$modelchain,rollmean_CH2018_REH_RCP85$sowing.year,paste="_")
    
    # if(pheno_ph == "sowing-emergence"){
    #   roll_mean_sowing <- rollmean_CH2018_REH_RCP85
    #   roll_mean_sowing <- roll_mean_sowing[,c(4,6)]
    #   names(roll_mean_sowing) <- c("emergence_date","UID")
    # }
    
    # rollmean_CH2018_REH_RCP85 <- merge(rollmean_CH2018_REH_RCP85,roll_mean_sowing,by="UID")
    # rollmean_CH2018_REH_RCP85$roll_mean_end_das <- rollmean_CH2018_REH_RCP85$roll_mean_end_das-rollmean_CH2018_REH_RCP85$emergence_date
    # 
    # browser()
    
    result_CH2018_REH_RCP85 <- rollmean_CH2018_REH_RCP85 %>%
      group_by(scenario,model_type, sowing.year) %>%
      summarise(
        median_value= median(roll_mean_end_das,na.rm=T),
        q5 = as.numeric(quantile(roll_mean_end_das,probs=c(0.05),na.rm = T)),
        q95 = as.numeric(quantile(roll_mean_end_das,probs=c(0.95),na.rm = T)) )
    
    
    
    result_CH2018_REH_RCP85 <- na.omit(result_CH2018_REH_RCP85)
    result_CH2018_REH_RCP85$UID <- paste(result_CH2018_REH_RCP85$scenario,result_CH2018_REH_RCP85$model_type,result_CH2018_REH_RCP85$sowing.year,sep="_")
    if(pheno_ph == "sowing-emergence"){
      result_mean_sowing <- result_CH2018_REH_RCP85
      result_mean_sowing <- result_mean_sowing[,c(4,7)]
      names(result_mean_sowing) <- c("emergence_date","UID")
    }
    result_CH2018_REH_RCP85 <- merge(result_CH2018_REH_RCP85,result_mean_sowing,by="UID")
    result_CH2018_REH_RCP85$median_value <- result_CH2018_REH_RCP85$median_value-result_CH2018_REH_RCP85$emergence_date
    result_CH2018_REH_RCP85$q5 <- result_CH2018_REH_RCP85$q5-result_CH2018_REH_RCP85$emergence_date
    result_CH2018_REH_RCP85$q95 <- result_CH2018_REH_RCP85$q95-result_CH2018_REH_RCP85$emergence_date
    
    
    # my_colors <- c(maxGT = "#3b6353", maxCS = "#9e2b25", Opt = "#7c4660" )
    my_colors <- c(maxGT = "#1A6FBF",maxCS = "#9e2b25", Opt= "#7c4660") # cbf
    
    # heading_dates <- ggplot(subset(result_CH2018_REH_RCP85, scenario == "RCP85"& sowing.year >2007), aes(x = sowing.year, y = median_value, color = model_type)) +
    time_plot_pheno_stage[[pheno_ph]] <- ggplot(subset(result_CH2018_REH_RCP85, scenario == scen), aes(x = sowing.year, y = median_value, color = model_type)) +
      geom_ribbon(aes(ymin = q5, ymax = q95, fill = model_type), alpha = 0.15, color = NA) +  
      geom_line(aes(group = model_type),linetype="solid") +
      geom_line(data = subset(result_CH2018_REH_RCP85, scenario == "obs"), aes(x = sowing.year, y = median_value, color = model_type, group = model_type)) 
    
    # browser()
    # 
    # ##
    # ggplot(subset(result_CH2018_REH_RCP85, scenario == "RCP85"), aes(x = sowing.year, y = median_value, color = model_type)) +
    #   geom_smooth(method="lm",se=F)
    # ##
    
    if(pheno_ph == "sowing-emergence" ){
      
      # y_lab_label <- "Emergence date (days after sowing)"
      y_lab_label <- "Emergence date (days after emergence)"
      
      time_plot_pheno_stage[[pheno_ph]]  <- time_plot_pheno_stage[[pheno_ph]]  +
        scale_color_manual(values = my_colors,
                           labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        scale_fill_manual(values = my_colors,
                          labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        labs(x=" ", y=y_lab_label, color="Best Model Selection", fill="Best Model Selection")+
        theme_cowplot()
      
    }
    if(pheno_ph ==  "emergence-booting" ){
      
      # y_lab_label <- "Booting date (days after sowing)"
      y_lab_label <- "Booting date (days after emergence)"
      
      time_plot_pheno_stage[[pheno_ph]]  <- time_plot_pheno_stage[[pheno_ph]]  +
        scale_color_manual(values = my_colors,
                           labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        scale_fill_manual(values = my_colors,
                          labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        labs(x=" ", y=y_lab_label, color="Best Model Selection", fill="Best Model Selection")+
        theme_cowplot()
      
      
    }
    if(pheno_ph == "booting-heading" ){
      
      # y_lab_label <- "Heading date (days after sowing)"
      y_lab_label <- "Heading date (days after emergence)"
      
      time_plot_pheno_stage[[pheno_ph]]  <- time_plot_pheno_stage[[pheno_ph]]  +
        scale_color_manual(values = my_colors,
                           labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        scale_fill_manual(values = my_colors,
                          labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        labs(x=" ", y=y_lab_label, color="Best Model Selection", fill="Best Model Selection")+
        theme_cowplot()
      
      
    }
    if(pheno_ph == "heading-senescence" ){
      
      # y_lab_label <- "Senescence date (days after sowing)"
      y_lab_label <- "Senescence date (days after emergence)"
      
      # browser()
      
      time_plot_pheno_stage[[pheno_ph]]  <- time_plot_pheno_stage[[pheno_ph]]  +
        scale_color_manual(values = my_colors,
                           labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        scale_fill_manual(values = my_colors,
                          labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
        labs(x="Sowing year", y=y_lab_label, color="Best Model Selection", fill="Best Model Selection")+
        theme_cowplot()
    }
      # scale_color_manual(values = my_colors,
      #                    labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
      # scale_fill_manual(values = my_colors,
      #                   labels = c(maxGT = expression("Max"[GT]), maxCS = expression("Max"[CS]), Opt = "Opt")) +  # Integrate the defined colors
      # labs(x="Sowing year", y=y_lab_label, color="Best Model Selection", fill="Best Model Selection")+
      # theme_cowplot()
    
    print(time_plot_pheno_stage[[pheno_ph]])
    
    
  }
  
  
  design = "
  A#
  B#
  C#
  "
  
  
  
  
  
  
  plot_over_time <- c(time_plot_pheno_stage[2:4] ) |>
    wrap_plots() + 
    plot_layout(heights = c(20, 20,20,20),widths = c(25,1),design=design,ncol=1, guides = "collect")&
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal')
  
  
  
  
  ################################################################################
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # save plots
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  ################################################################################
  
  out_path_plot <- file.path(getwd(),"output/plots/agr_vs_ch2018_best_model/stations")
  dir.create(out_path_plot, recursive = T, showWarnings = F)
  
  ################################################################################
  # correlation
  ################################################################################
  
  plot_name <- paste(sta,"booting_heading_senescence_date_per_sowing_year",scen,"fixed_sowing_free_start",model_run,sep="_")
  
  pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 12, height =12)
  
  # 
  # c(time_plot_pheno_stage[2:4] ) |>
  #   wrap_plots() + 
  #   plot_layout(heights = c(20, 20,20,20),widths = c(25,1),design=design,ncol=1, guides = "collect")&
  #   theme(legend.position = 'bottom',
  #         legend.direction = 'horizontal')
  print(plot_over_time)
  dev.off()
  
  png(file.path(out_path_plot,paste0(plot_name,".png")),width= 12, height =12, res = 1200, units = "in")
  print(plot_over_time)
  
  # c(time_plot_pheno_stage[2:4] ) |>
  #   wrap_plots() + 
  #   plot_layout(heights = c(20, 20,20,20),widths = c(25,1),design=design,ncol=1, guides = "collect")&
  #   theme(legend.position = 'bottom',
  #         legend.direction = 'horizontal')
  
  dev.off()
  
  
  }
  
}




#################################################################################

library(dplyr)

# # Assuming your dataframe is named CH2018
# # Group by pheno_phase, station, model_type, and scenario
# grouped_data <- CH2018 %>%
#   group_by(pheno_phase, station, model_type, scenario)
# 
# # Define a function to fit a linear model and extract slope and intercept
# fit_lm <- function(data) {
#   lm_model <- lm(as.numeric(format(as.Date(data$end_date), "%j")) ~ sowing.year, data = data)
#   return(data.frame(
#     pheno_phase = unique(data$pheno_phase),
#     station = unique(data$station),
#     model_type = unique(data$model_type),
#     scenario = unique(data$scenario),
#     slope = coef(lm_model)[2],
#     intercept = coef(lm_model)[1]
#   ))
# }
# 
# # Apply the function within each group
# result <- grouped_data %>%
#   do(fit_lm(.))
# 
# # Print the results
# print(result)
# 




# Group by pheno_phase, station, model_type, and scenario
grouped_data <- CH2018 %>%
  group_by(pheno_phase, station, model_type, scenario)

# Define a function to calculate slopes for each combination
calculate_slopes <- function(data) {
  # Fit a linear model to calculate the slope
  lm_model <- lm(as.numeric(format(as.Date(data$end_date), "%j")) ~ sowing.year, data = data)
  # Extract the slope
  slope <- coef(lm_model)[2]
  # Create a data frame with sowing.year values and corresponding slopes
  result_data <- data.frame(
    sowing.year = 1:100,
    pheno_phase = unique(data$pheno_phase),
    station = unique(data$station),
    model_type = unique(data$model_type),
    scenario = unique(data$scenario),
    slope = slope,
    intercept= coef(lm_model)[1]
  )
  result_data$value <- result_data$sowing.year * result_data$slope -  result_data$slope
  return(result_data)
}

# Apply the function within each group
result <- grouped_data %>%
  do(calculate_slopes(.))

# Print the results
print(result)

result$model_type <- factor(result$model_type, levels = c("Opt","maxGT","maxCS"))

result_less_stations <- subset(result, station %in% c("ALT","REH","SHA","TAE","WYN","MAG"))


# 
# 
# 
# ggplot(result_less_stations, aes(x = sowing.year, y = value)) +
#   geom_point() +
#   geom_line(aes(group = interaction(scenario, model_type), color = interaction(scenario, model_type))) +
#   facet_grid(pheno_phase ~ station) +
#   theme_cowplot() 
#   # labs(x = "Sowing Year", y = "Slope", color = "Scenario & Model Type") +
#   # scale_color_manual(values = c("red", "blue", "green"))  # Customize color as needed$
library(stringr)

for(stat in unique(result$station)){
  # model_colors <- c(maxGT = "#3b6353",maxCS = "#9e2b25", Opt= "#7c4660")
  model_colors <- c(maxGT = "#1A6FBF",maxCS = "#9e2b25", Opt= "#7c4660") # cbf

one_station <- subset(result, station== stat &  scenario != "obs")
scenario_colors <- c("obs"="#A96C00","RCP26" = "#abcfdb", "RCP45" = "#fbda87", "RCP85" = "#9F3D3E" )

trend_lines <- ggplot(one_station, aes(x = sowing.year, y = value)) +
  geom_abline(intercept = 0, slope = 0, color = "grey")+
  geom_line(aes( color = scenario), linewidth=2) +
  facet_grid(pheno_phase ~ model_type) +
  scale_color_manual(values=scenario_colors,
                     breaks = c("obs","RCP26","RCP45","RCP85"),
                     labels= c("Observation","RCP 2.6","RCP 4.5","RCP 8.5"))+
  ylab("Phase Duration")+
  xlab("Time")+
  labs(color = "Scenarios") +
  theme_cowplot() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

print(trend_lines)

one_station$scenario_long <- gsub("RCP([0-9])([0-9])", "RCP \\1.\\2",  one_station$scenario)

trend_lines_model <- ggplot(one_station, aes(x = sowing.year, y = value)) +
  geom_abline(intercept = 0, slope = 0, color = "grey")+
  geom_line(aes( color = model_type), linewidth=2) +
  facet_grid(pheno_phase ~ scenario_long) +
  scale_color_manual(values=model_colors,
                     breaks = c("maxGT","maxCS","Opt"),
                     labels= c(expression("Max"[GT]), expression("Max"[CS]),  "Opt"))+
  ylab("Phase Duration")+
  xlab("Time")+
  labs(color = "Model Selection") +
  theme_cowplot() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())

print(trend_lines_model)

################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save plots
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
################################################################################

out_path_plot <- file.path(getwd(),"output/plots/agr_vs_ch2018_best_model/stations_trendline")
dir.create(out_path_plot, recursive = T, showWarnings = F)

################################################################################
# trendline modeltype
################################################################################

plot_name <- paste(stat,"trendlines_fixed_sowing_free_start",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 8, height =8)

print(trend_lines)
dev.off()

png(file.path(out_path_plot,paste0(plot_name,".png")),width= 8, height =8, res = 1200, units = "in")
print(trend_lines)
dev.off()


################################################################################
# trendline modeltype
################################################################################


plot_name <- paste(stat,"per_scenario_trendlines_fixed_sowing_free_start",model_run,sep="_")

pdf(file.path(out_path_plot,paste0(plot_name,".pdf")),width= 8, height =8)

print(trend_lines_model)
dev.off()

png(file.path(out_path_plot,paste0(plot_name,".png")),width= 8, height =8, res = 1200, units = "in")
print(trend_lines_model)
dev.off()
  
}
