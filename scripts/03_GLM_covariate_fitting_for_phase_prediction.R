# Author: Flavian Tschurr
# Project: KP030
# Date: 28.07.2023
# Purpose: phase prediction
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
source("scripts/00_variable_settings.R")
source("scripts/functions/FUN_dose_response_curves.R")
source("scripts/functions/FUN_GLM_prediction.R")

################################################################################
# allocate cores / core number
if(rdsnx == T){
  cores_percent = 0.75
}else{
  cores_percent = 0.7
}
numCores <- round(detectCores()*cores_percent)
if(euler ==T){
  numCores <- 25
}

# pheno_phases <- pheno_phases[3:4] # run on euler
# pheno_phases <- pheno_phases[1] # run on rdsnx
pheno_phases <- pheno_phases[2:3] # run on ppc2

################################################################################
################################################################################
################################################################################
for(pheno_phase in pheno_phases){
  print(pheno_phase)

# initaite empty lists
predictions <- list()
timestamps_list <- list()


# load meta inforamtion for phenological pahses 
pheno_phase_ID <- read.csv(file.path(getwd(),meta_data_path,"phenological_phases_IDs_DWD.csv"))


cl <- makePSOCKcluster(numCores)

for(env_variable in env_variables){
  print(env_variable)

  path_best_curve <- file.path(getwd(),"output","paramter_best_response_curve",model_run)
  file_name_best <- paste0(env_variable,"_",granularity,"_",pheno_phase,"_best_response_curve_type.rds")
  best_curve <- readRDS(file.path(path_best_curve,file_name_best))
  
  # create file name
  data_file_name <- paste0(paste(env_variable,granularity,"training_dataset_wheat_phenology",pheno_phase,sep="_"),".rds")

  # laod file
  pheno_list <- readRDS(file.path(data_path_prediction,data_file_name))
  # clean envs with too many NA's
  pheno_list <- glmpred.data_cleaner(pheno_list = pheno_list, granularity = granularity)
  
  .response_function. <- get(paste0(best_curve$response_curve_type,"_prediction"))
  parameters <- best_curve$parameters$curve
  

  predictions[[env_variable]][["modelled"]]<- parLapplyLB(cl,pheno_list, glmpred.prediction_lapplier, .response_function., parameters)

  predictions[[env_variable]][["measured"]] <- rep(100,length(pheno_list))
  predictions[[env_variable]][["response_curve"]][["parameters"]] <- parameters
  predictions[[env_variable]][["response_curve"]][["type"]] <- best_curve$response_curve_type
  predictions[[env_variable]][["env_names"]] <- names(pheno_list)
  
  # extract timestamps
  timestamps_list[[env_variable]] <- lapply(pheno_list, glmpred.timestamp_extractor)
  
}

print("DRCs calcualted")
# common_predictions_base <- glmpred.prediction_matching_envs_finder(predictions = predictions)
# 


pheno_phase_dates <- list()
pheno_phase_dates <- parLapplyLB(cl,pheno_list, glmpred.pheno_phase_start_stop_extractor,pheno_phase_ID = pheno_phase_ID,pheno_phase = pheno_phase)
print("dates extracted")

# # add start and stop of the phase as dates, combine the single envs to the combined prediction, add timevector
# common_predictions_base <- parLapplyLB(cl,common_predictions_base, 
#                                   glmpred.prediction_cumulative_growth,
#                              timestamps_list = timestamps_list,
#                              pheno_phase_dates = pheno_phase_dates)
# print("common envs selected")
stopCluster(cl)

####################################################################################
####################################################################################
# calculate glm one to every variable us all combinations
####################################################################################
####################################################################################

# create list with all potential models to fit
model_fit_list <- list()
counter <- 1
for(i in 1:length(env_variables)){
  combos <- combn(env_variables,i,unique=T)
  for(cols in 1:dim(combos)[2]){
    model_fit_list[[counter]] <- combos[,cols]
    counter <- counter + 1
  }
}

glm_model_fiter <- function(one_variables_list, predictions, pheno_phase, model_run, timestamps_list, pheno_phase_dates ){
  require(pROC)
  source("scripts/functions/FUN_skillscores.R")
  source("scripts/functions/FUN_GLM_prediction.R")
  
  overwrite=FALSE
  
  start_time <- Sys.time()
  
  # create filename and output path
  file_out_name <- paste0("dymenv_glm_params_",pheno_phase,"_NrEnvVariables_",length(unlist(one_variables_list)),"_",paste(unlist(one_variables_list),collapse="_"),"_",model_run,".rds")
  output_path_dir <- file.path(getwd(),"output","parameter_multi_env_model",model_run)
  output_path_file <- file.path(output_path_dir, file_out_name)
  # create folder
  dir.create(output_path_dir,recursive = T,showWarnings = F)
  if(overwrite==FALSE){
    # check if file exists
    if(file.exists(output_path_file)){
      return(NA)
    }
  }
  ####################################################################################
  # create formula
  ####################################################################################
  
  model_forumla = as.formula(paste0("response ~ ", paste(unlist(one_variables_list),collapse=" + ")))
  
  ####################################################################################
  # subset data
  ####################################################################################
  predictions_sub <- predictions[which(names(predictions) %in% unlist(one_variables_list))]
  common_predictions <- glmpred.prediction_matching_envs_finder(predictions = predictions_sub)
  
  common_predictions <- lapply(common_predictions,
                               glmpred.prediction_cumulative_growth,
                               timestamps_list = timestamps_list,
                               pheno_phase_dates = pheno_phase_dates)
  # common_predictions <- parLapplyLB(cl,common_predictions, 
  #                              glmpred.prediction_cumulative_growth,
  #                              timestamps_list = timestamps_list,
  #                              pheno_phase_dates = pheno_phase_dates)
  
  combined_envs <- glmpred.data_frame_creator(common_predictions = common_predictions,
                                                pheno_phase_dates = pheno_phase_dates)
  
  ####################################################################################
  # fit model
  ####################################################################################
  
  glm_model <- glm(model_forumla,
                   data=combined_envs,
                   family = binomial())

  
  stripGlmLR = function(cm) {
    cm$y = c()
    cm$model = c()
    
    cm$residuals = c()
    cm$fitted.values = c()
    cm$effects = c()
    cm$qr$qr = c()  
    cm$linear.predictors = c()
    cm$weights = c()
    cm$prior.weights = c()
    cm$data = c()
    
    
    cm$family$variance = c()
    cm$family$dev.resids = c()
    # cm$family$aic = c()
    cm$family$validmu = c()
    cm$family$simulate = c()
    attr(cm$terms,".Environment") = c()
    attr(cm$formula,".Environment") = c()
    
    return(cm)
  }
  light_model <- stripGlmLR(glm_model)

  # saveRDS(light_model,output_path_file_model)
  
  # saveRDS(light_model,output_path_file_model, compress = "xz")

  # summary(glm_model)

  
  combined_envs$fitted <- as.numeric(glm_model$fitted.values)
  rm(glm_model)
  ####################################################################################
  # use ROC to determine best threshold
  ####################################################################################
  # find best threshold
  roc_obj <- pROC::roc(combined_envs$response,combined_envs$fitted)
  roc_coords <- pROC::coords(roc_obj, "best", ret="threshold")
  combined_envs$stage_prediction <- ifelse(combined_envs$fitted <= roc_coords$threshold, 0, 1)
  
  # # plot result
  # library(ggplot2)
  # overview_model_values <- ggplot(combined_envs, aes(x = phase_index, y= fitted, color = env))+
  #   geom_line(show.legend = F)+
  #   geom_hline(yintercept = roc_coords$threshold)+
  #   theme_classic()+
  #   xlab("phase time [days]")+ ylab("response")+ ggtitle(as.character(model_forumla))
  # print(overview_model_values)
  
  ####################################################################################
  # calculate correlation between measured and modelled phase length
  #################################################################################### 
  # combined_env_correlation <- data.frame("env"=NA, "measurement" = NA, "prediction" = NA)
  # browser()
  env_combiner <- function(en, combined_envs){
    # print(en)
    one_env <- subset(combined_envs, env == en)
    length_measurement <- sum(one_env$response == 0)
    length_mod <- sum(one_env$stage_prediction == 0)
    # length_measurement <- length(which(one_env$response == 0))
    # length_mod <- length(which(one_env$stage_prediction == 0))
    one_env_correlation <- data.frame("env" = en,"measurement" = length_measurement,"prediction"= length_mod)
    return(one_env_correlation)
  }
  envs_list <- as.list(as.character(unique(combined_envs$env)))
  combined_env_list <- lapply(envs_list,env_combiner,combined_envs)
  # combined_env_list <- parLapplyLB(cl,envs_list,env_combiner,combined_envs)
  
  combined_env_correlation <- do.call("rbind",combined_env_list)
  
  # counter <- 1
  # for(en in as.character(unique(combined_envs$env))){
  #   one_env <- subset(combined_envs, env == en)
  #   length_measurement <- length(which(one_env$response == 0))
  #   length_mod <- length(which(one_env$stage_prediction == 0))
  #   combined_env_correlation[counter,] <- c(en, length_measurement, length_mod)
  #   counter <- counter + 1
  #   
  # }
  
  combined_env_correlation$measurement <- as.numeric(combined_env_correlation$measurement)
  combined_env_correlation$prediction <- as.numeric(combined_env_correlation$prediction)

  model_output_list <- list()
  COR <- calc_cor(combined_env_correlation$measurement, combined_env_correlation$prediction)
  RMSE <- calc_RMSE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
  MAE <- calc_MAE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
  LML <- calc_SumLogLikelihood(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
  skillscores <- c(COR,RMSE,MAE, LML)
  names(skillscores) <- c("cor","RMSE","MAE","SumLogLikelihood")
  
  as.numeric(light_model$coefficients)
  coefficients <- list()
  coefficients[["env_parameters"]] <- light_model$coefficients
  coefficients[["threshold_phase"]] <- roc_coords

  
  model_output_list[["coefficients"]] <- coefficients
  model_output_list[["skillscores"]] <- skillscores
  # model_output_list[["fitted_glm"]] <- glm_model
  model_output_list[["fitted_glm"]] <- light_model
  
  # model_output_list[["data_to_correlate"]] <- combined_env_correlation
  model_output_list[["glm_formula"]] <- as.character(model_forumla)
  ##############################################################################
  # print results
  ##############################################################################
  print(paste("formula:", as.character(model_forumla),collapse=" "))
  print(paste0("coefficients: ", paste(names(light_model$coefficients),collapse=" ")))
  print(paste0("coefficients: ", paste(as.numeric(light_model$coefficients),collapse=" ")))
  print(paste("threshold:", roc_coords,sep=" "))
  print(paste("correlation:",COR,"| RMSE:",RMSE," | MAE:",MAE,sep=" "))
  print( paste0("calculation time: "))
  print(Sys.time() - start_time )
  

  ##############################################################################
  # save output
  ##############################################################################
  # file_out_name <- paste0("dymenv_glm_params_",pheno_phase,"_NrEnvVariables_",length(unlist(one_variables_list)),"_",paste(unlist(one_variables_list),collapse="_"),"_",model_run,".rds")
  # output_path_dir <- file.path(getwd(),"output","parameter_multi_env_model",model_run)
  # output_path_file <- paste(output_path_dir, file_out_name,sep="/" )
  # dir.create(output_path_dir,recursive = T,showWarnings = F)
  # require(pryr)
  # print(object_size(model_output_list))
  # save jsut the meta informations
  saveRDS(model_output_list,output_path_file)
  
  # file_out_name_zip <- paste0("dymenv_glm_params_",pheno_phase,"_NrEnvVariables_",length(unlist(one_variables_list)),"_",paste(unlist(one_variables_list),collapse="_"),"_",model_run,".zip")
  # zip(zipfile = file.path(output_path_dir, file_out_name_zip), files = file.path(output_path_dir, file_out_name))

  return_list <- skillscores
  return_list[["nr_of_variables"]] <- length(unlist(one_variables_list))
  return_list[["variables"]] <- paste(one_variables_list,collapse="_")
  return(return_list)
  
  
}

# glm_skillcores <- lapply( model_fit_list,glm_model_fiter, predictions, pheno_phase, model_run,timestamps_list,
#                           pheno_phase_dates)
##############################################################################
# fit glms in parallell
##############################################################################
print("glm fitting started")
print(Sys.time())
start_time <- Sys.time()
cl <- makePSOCKcluster(numCores)

# glm_skillcores <- lapply(
#                               model_fit_list,
#                               glm_model_fiter,
#                               predictions,
#                               pheno_phase,
#                               model_run,
#                               timestamps_list,
#                               pheno_phase_dates)


glm_skillcores<- list()
glm_skillcores <- parLapplyLB(cl,
                              model_fit_list,
                              glm_model_fiter,
                              predictions,
                              pheno_phase,
                              model_run,
                              timestamps_list,
                              pheno_phase_dates)

stopCluster(cl)

print(Sys.time()-start_time)
##############################################################################
# get overview
##############################################################################
glm_skillcores_df <-as.data.frame(do.call("rbind", glm_skillcores))
glm_skillcores_df$nr_of_variables <- as.numeric(glm_skillcores_df$nr_of_variables)
for(i in c(1:4)){
glm_skillcores_df[,i] <- as.numeric(glm_skillcores_df[,i])
}
library(ggplot2)
cor_plot <- ggplot(data=glm_skillcores_df, aes(x=nr_of_variables,y=cor))+
  geom_point() + theme_classic()+ geom_smooth(method = "loess") + ggtitle(pheno_phase)

RMSE_plot <- ggplot(data=glm_skillcores_df, aes(x=nr_of_variables,y=RMSE))+
  geom_point() + theme_classic()+ geom_smooth(method = "loess")+ ggtitle(pheno_phase)

MAE_plot <- ggplot(data=glm_skillcores_df, aes(x=nr_of_variables,y=MAE))+
  geom_point() + theme_classic() + geom_smooth(method = "loess")+ ggtitle(pheno_phase)

SLL_plot <- ggplot(data=glm_skillcores_df, aes(x=nr_of_variables,y=SumLogLikelihood))+
  geom_point() + theme_classic() + geom_smooth(method = "loess")+ ggtitle(pheno_phase)

print(cor_plot)
print(RMSE_plot)
print(MAE_plot)
print(SLL_plot)
}

####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
# use lasso regression to select and fit optimal parameter
####################################################################################
# library(glmmLasso)
# 
# full_forumlar = as.formula(paste0("response ~ ", paste(env_variables,collapse=" + ")))
# 
# 
# lasso_model <- glmmLasso(full_forumlar,
#                          rnd = list(env= ~1),
#                          family = binomial(),
#                          data = combined_envs,
#                          lambda = 100
# )
# 
# 
# 
# lasso_model <- glmmLasso(response ~ tas + SPI + global_radiation + RH + VPD + tasmin + tasmax ,
#                            rnd = list(env= ~1),
#                            family = binomial(),
#                            data = combined_envs,
#                            lambda = 100
#                          )
# 
# 
# lasso_model <- glmmLasso(response ~ tas  + global_radiation  + VPD + SPI,
#                          # rnd = list(env= ~1),
#                          rnd = NULL,
#                          family = binomial(),
#                          data = combined_envs,
#                          lambda = 100
# )
# 
# summary(lasso_model)
# lasso_model$coefficients
# 
# # add fitted values to the dataframe
# combined_envs$fitted <- as.numeric(lasso_model$fitted.values)
# 
# ####################################################################################
# # use ROC to determine best threshold
# ####################################################################################
# # find best threshold
# library(pROC)
# roc_obj <- roc(combined_envs$response,combined_envs$fitted)
# roc_coords <- pROC::coords(roc_obj, "best", ret="threshold")
# combined_envs$stage_prediction <- ifelse(combined_envs$fitted <= roc_coords$threshold, 0, 1)
# 
# # plot result
# library(ggplot2)
# ggplot(combined_envs, aes(x = phase_index, y= fitted, color = env))+
#   geom_line(show.legend = F)+
#   geom_hline(yintercept = roc_coords$threshold)
# 
# ####################################################################################
# # calculate correlation between measured and modelled phase length
# #################################################################################### 
# combined_env_correlation <- data.frame("env"=NA, "measurement" = NA, "prediction" = NA)
# counter <- 1
# for(en in unique(combined_envs$env)){
#   one_env <- subset(combined_envs, env == en)
#   length_measurement <- length(which(one_env$response == 0))
#   length_mod <- length(which(one_env$stage_prediction == 0))
#   combined_env_correlation[counter,] <- c(en, length_measurement, length_mod)
#   counter <- counter + 1
#   
# }
# combined_env_correlation$measurement <- as.numeric(combined_env_correlation$measurement)
# combined_env_correlation$prediction <- as.numeric(combined_env_correlation$prediction)
# 
# plot(combined_env_correlation$measurement, combined_env_correlation$prediction)
# abline(b=1,a=0)
# cor(combined_env_correlation$measurement, combined_env_correlation$prediction)
# 
# 

####################################################################################
####################################################################################
####################################################################################
# 
# library(lmerTest)
# glmer_all <- glmer(response ~ tas + SPI + global_radiation + RH + VPD + tasmin + tasmax + (1| location.ID)+ (1|harvest.year), data = combined_envs, family = "binomial")
# # glm_all <- glmer(response ~ tas  + (1| location.ID)+ (1|harvest.year), data = combined_envs, family = "binomial")
# summary(glmer_all)
# plot(fitted(glmer_all))
# coef(glmer_all)
# 
# #####################################################################################
# glm_all <- glm(response ~ tas + SPI + global_radiation + RH + VPD + tasmin + tasmax , data = combined_envs, family = "binomial")
# 
# summary(glm_all)
# plot(fitted(glm_all))
# coef(glm_all)
# 
# step.model <- lmerTest::step(glm_all,  scope = response ~ tas , direction = "forward", k=2, steps= 100000)
# step.model <- lmerTest::step(glm_all, reduce.fixed =TRUE , direction = "forward", k=2, steps= 100000)
# 
# summary(step.model)
# x <- step(glm_all)
# summary(x)
# library(glmmTMB)
# # install.packages("glmmLasso")
# library(glmmLasso)
# # Perform Lasso regularization
# 
# 
# 
# lasso_model <- glmmLasso(response ~ tas + SPI + global_radiation + RH + VPD + tasmin + tasmax ,
#                            rnd = list(env= ~1),
#                            family = binomial(),
#                            data = combined_envs,
#                            lambda = 100
#                          )
# 
# lasso_model_2 <- glmmLasso(response ~ tas + SPI + global_radiation + RH + VPD + tasmin + tasmax ,
#                          rnd = list(env= ~1),
#                          family = binomial(),
#                          data = combined_envs,
#                          lambda = 2)
# 
# # lasso_model <- glmmLasso(glm_all, penalty="lasso", lambda=0.1)
# # summary(lasso_model)
# summary(lasso_model_2)
# lasso_model_2
# 
# 
# # test <- combined_envs
# combined_envs$fitted <- as.numeric(lasso_model_2$fitted.values)
# 
# # find best threshold
# library(pROC)
# roc_obj <- roc(combined_envs$response,combined_envs$fitted)
# roc_coords <- pROC::coords(roc_obj, "best", ret="threshold")
# 
# combined_envs$stage_prediction <- ifelse(combined_envs$fitted <= roc_coords$threshold, 0, 1)
# 
# library(ggplot2)
# 
# combined_envs$timestamp <- as.Date(combined_envs$timestamp)
# ggplot(combined_envs, aes(x = phase_index, y= fitted, color = env))+
#   geom_line(show.legend = F)+
#   geom_hline(yintercept = roc_coords$threshold)
# 
# 
# combined_env_correlation <- data.frame("env"=NA, "measurement" = NA, "prediction" = NA)
# counter <- 1
# for(en in unique(combined_envs$env)){
#   one_env <- subset(combined_envs, env == en)
#   length_measurement <- length(which(one_env$response == 0))
#   length_mod <- length(which(one_env$stage_prediction == 0))
#   combined_env_correlation[counter,] <- c(en, length_measurement, length_mod)
#   counter <- counter + 1
#   
# }
# combined_env_correlation$measurement <- as.numeric(combined_env_correlation$measurement)
# combined_env_correlation$prediction <- as.numeric(combined_env_correlation$prediction)
# 
# plot(combined_env_correlation$measurement, combined_env_correlation$prediction)
# abline(b=1,a=0)
# cor(combined_env_correlation$measurement, combined_env_correlation$prediction)
# 
# 
# 
# # Print the results
# cat("Threshold:", threshold, "\n")
# cat("Sensitivity:", sensitivity, "\n")
# cat("Specificity:", specificity, "\n")
# 
# 
# lasso_glm <- glmmLasso(response ~ tas + SPI ,
#                            # rnd = list(env= ~1),
#                            family = binomial(),
#                            data = combined_envs,
#                            lambda = 2)
# 
# # data("soccer")
# # install.packages("glmnet")
# library(glmnet)
# 
# x = as.matrix(combined_envs[,2:8])
# y = combined_envs$response
# lambda <- 0.01
# 
# la.eq <- glmnet(x, y, 
#                 family="binomial", 
#                 intercept = F, alpha=1) 
# 
# summary(la.eq)
# 
# 
# test <- glmnet::bigGlm()
# # # install.packages("party")
# # library(party)
# # test.ct <- ctree(response ~ tas + SPI + global_radiation + RH + VPD + tasmin + tasmax ,data= combined_envs)
# # plot(test.ct)
# # ###
# # env_variables <- c("tas","global_radiation")
# # multi_env_fit_pheno_looper_correlation(predictions_fun = common_predictions,
# #                                        pheno_phase =  pheno_phase, 
# #                                        model_run =   model_run,
# #                                        env_variables =  env_variables, 
# #                                        optimize_all_weights=T, 
# #                                        predictions_raw = predictions)
# # 
# # # out
# # select_best_model_complexity_phenostage_cor(pheno_phase = pheno_phase,
# #                                         env_variables = env_variables,
# #                                         model_run = model_run)
# # visualize_multi_env_fit_cor(pheno_phase = pheno_phase,model_run = model_run, env_variables = env_variables)
# # 










































# ####################################################################################
# # write environments into a data frame
# ####################################################################################
# 
# 
# combined_envs <- LaHelpFun_data_frame_creator(common_predictions = common_predictions_base,
#                                    pheno_phase_dates = pheno_phase_dates)
# ####################################################################################
# # use lasso regression to select and fit optimal parameter -_> iterate over an irradicate
# # one variable after another
# ####################################################################################
# make_lasso = F
# if(make_lasso == T){
# library(glmmLasso)
# library(pROC)
# full_forumla = as.formula(paste0("response ~ ", paste(env_variables,collapse=" + ")))
# start_time <- Sys.time()
#   
#   for(i in 1:length(env_variables)){
#   # browser()
#   # redo the common predictions part --> get more envs if some variables are out.
#   if(i == 1){
#     current_formula = full_forumla
#   }else{
#     # kick out zeros or the worst one
#     used_variables <- lasso_model$coefficients[-1]
#     new_env_variables <- LaHelpFun_variable_selecter(used_variables)
#     current_formula = as.formula(paste0("response ~ ", paste(new_env_variables,collapse=" + ")))
#     
#     predictions_sub <- predictions[which(names(predictions) %in% new_env_variables)]
#     common_predictions <- prediction_matching_envs_finder(predictions = predictions_sub)
#     
#     common_predictions <- lapply(common_predictions, 
#                                  prediction_cumulative_growth,
#                                  timestamps_list = timestamps_list,
#                                  pheno_phase_dates = pheno_phase_dates)
#     
#     combined_envs <- LaHelpFun_data_frame_creator(common_predictions = common_predictions,
#                                                   pheno_phase_dates = pheno_phase_dates)
#     
#     try(rm(lasso_model))
#   }
#     # fit lasso model
#   #   lasso_model <- glmmLasso(current_formula,
#   #                            rnd = list(env= ~1),
#   #                            family = binomial(),
#   #                            data = combined_envs,
#   #                            lambda = 100
#   # )
# 
#   lasso_model <- glmmLasso(current_formula,
#                            rnd = NULL,
#                            family = binomial(),
#                            data = combined_envs,
#                            lambda = 10
#   )
#   
# 
#   
#   
#   # add fitted values to the dataframe
#   combined_envs$fitted <- as.numeric(lasso_model$fitted.values)
#   
#   ####################################################################################
#   # use ROC to determine best threshold
#   ####################################################################################
#   # find best threshold
#   roc_obj <- pROC::roc(combined_envs$response,combined_envs$fitted)
#   roc_coords <- pROC::coords(roc_obj, "best", ret="threshold")
#   combined_envs$stage_prediction <- ifelse(combined_envs$fitted <= roc_coords$threshold, 0, 1)
#   
#   # plot result
#   library(ggplot2)
#   overview_model_values <- ggplot(combined_envs, aes(x = phase_index, y= fitted, color = env))+
#     geom_line(show.legend = F)+
#     geom_hline(yintercept = roc_coords$threshold)+
#     theme_classic()+
#     xlab("phase time [days]")+ ylab("response")+ ggtitle(as.character(current_formula))
#   print(overview_model_values)
#   
#   ####################################################################################
#   # calculate correlation between measured and modelled phase length
#   #################################################################################### 
#   combined_env_correlation <- data.frame("env"=NA, "measurement" = NA, "prediction" = NA)
#   counter <- 1
#   for(en in unique(combined_envs$env)){
#     one_env <- subset(combined_envs, env == en)
#     length_measurement <- length(which(one_env$response == 0))
#     length_mod <- length(which(one_env$stage_prediction == 0))
#     combined_env_correlation[counter,] <- c(en, length_measurement, length_mod)
#     counter <- counter + 1
#     
#   }
#   combined_env_correlation$measurement <- as.numeric(combined_env_correlation$measurement)
#   combined_env_correlation$prediction <- as.numeric(combined_env_correlation$prediction)
#   
#   # plot(combined_env_correlation$measurement, combined_env_correlation$prediction)
#   # abline(b=1,a=0)
#   
#   
#   ##############################################################################
#   # create output list
#   ##############################################################################
#   
#   lasso_output_list <- list()
#   COR <- calc_cor(combined_env_correlation$measurement, combined_env_correlation$prediction)
#   RMSE <- calc_RMSE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
#   MAE <- calc_MAE(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
#   LML <- calc_SumLogLikelihood(measured = combined_env_correlation$measurement, modelled = combined_env_correlation$prediction)
#   skillscores <- c(COR,RMSE,MAE, LML)
#   names(skillscores) <- c("cor","RMSE","MAE","SumLogLikelihood")
#   
#   as.numeric(lasso_model$coefficients)
#   coefficients <- list()
#   coefficients[["env_parameters"]] <- lasso_model$coefficients
#   coefficients[["threshold_phase"]] <- roc_coords
#   
#   # list_name = paste0("nr_of_covariates_",(length(lasso_model$coefficients)-1))
#   lasso_output_list[["coefficients"]] <- coefficients
#   lasso_output_list[["skillscores"]] <- skillscores
#   lasso_output_list[["fitted_lasso"]] <- lasso_model
#   lasso_output_list[["data_to_correlate"]] <- combined_env_correlation
#   lasso_output_list[["lasso_formula"]] <- as.character(current_formula)
#   ##############################################################################
#   # print results
#   ##############################################################################
#   print(paste("iteration:",i,sep=" "))
#   print(paste("formula:", as.character(current_formula),collapse=" "))
#   print(paste0("coefficients: ", paste(names(lasso_model$coefficients),collapse=" ")))
#   print(paste0("coefficients: ", paste(as.numeric(lasso_model$coefficients),collapse=" ")))
#   print(paste("threshold:", roc_coords,sep=" "))
#   print(paste("correlation:",COR,"| RMSE:",RMSE," | MAE:",MAE,sep=" "))
#   print( paste0("calculation time: "))
#   print(Sys.time() - start_time )
#   start_time <- Sys.time()
#   
#   ##############################################################################
#   # save output
#   ##############################################################################
#   
#   file_out_name <- paste0("dymenv_glmmLasso_params_",pheno_phase,"_nr_of_env_variables_",(length(lasso_model$coefficients)-1),"_",model_run,".rds")
#   output_path_dir <- paste(getwd(),"output/parameter_multi_env_model",model_run,sep="/" )
#   dir.create(output_path_dir,recursive = T,showWarnings = F)
#   
#   output_path_file <- paste(output_path_dir, file_out_name,sep="/" )
#   saveRDS(lasso_output_list,output_path_file)
#   
#   
# }
# }
