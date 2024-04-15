# Author: Flavian Tschurr
# Project: KP030
# Date: 13.10.2022
# Purpose: dymenvmodel: extract best DRC skillscores and names for covarie X phase
################################################################################


path_to_files <- "P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/output/paramter_best_response_curve/run_WW1"


all_rds_files <- list.files(path_to_files)

best_drcs <- data.frame("pheno_phase"=NA,"environmental_covariate"=NA, "DRC"=NA,"cor"=NA,"RMSE"=NA, "MAE"=NA)
counter <-1
for(f in all_rds_files){
  one_file <- readRDS(file.path(path_to_files,f))
  env_covariate <- unlist(strsplit(f,"_"))[1]
  if(env_covariate =="global"){
    env_covariate ="GR"
  }
  pheno_pha <- unlist(strsplit(f,"_"))[length(unlist(strsplit(f,"_")))-4]
  best_drcs[counter,] <- c(pheno_pha,env_covariate,  one_file$response_curve_type,  round(one_file$skill_scores$cor,3),  round(one_file$skill_scores$RMSE,3), round(one_file$skill_scores$MAE,3)) 
  counter <- counter + 1  
}
best_drcs$pheno_phase <- factor(best_drcs$pheno_phase,ordered=TRUE,levels =c(    "sowing-emergence","emergence-booting","booting-heading","heading-senescence" ))

best_drcs <- best_drcs[order(best_drcs$pheno_phase),]

write.csv(best_drcs,"P:/Evaluation/Projects/KP0030_ftschurr/GIT_repos/dymenvmodel/manuscript/response_curves/best_DRCs_per_pheno_phase_and_envcovariate_run_WW1.csv",row.names = F)

