# Author: Flavian Tschurr
# Project: KP030
# Date: 03.07.2023
# Purpose: DyMEnv start parameter opimisation
################################################################################

################################################################################
## linear
################################################################################
estimate_starting_params_reg_linear_model <- function(env_vect){
  #'@param env_vect vector with environmental covariate in it
  
  env_vect_sub <- na.omit(unlist(env_vect))
  # base value 
  
  intercept_value <- as.numeric(quantile(env_vect_sub, probs=c(0.01,0.05,0.95)))

  intercept_value_list <- list("lower_bound"=intercept_value[1],"start_value"= intercept_value[2],"upper_bound"=intercept_value[3])
  
  slope_value_list <- list("lower_bound"=0,"start_value"=0.05,"upper_bound"=0.5)

  return(list("intercept_value"=intercept_value_list,"slope_value"=slope_value_list))
}



################################################################################
## non linear
################################################################################
estimate_starting_params_non_linear_model <- function(env_vect){
  #'@param env_vect vector with environmental covariate in it

  env_vect_sub <- na.omit(unlist(env_vect))
  # base value 
  
  base_value <- as.numeric(quantile(env_vect_sub, probs=c(0.05,0.1,0.6)))
  if(base_value[1] == base_value[2]){
    base_value[2] = base_value[2]+0.01
  }
  if(base_value[2] == base_value[3]){
    base_value[3] = base_value[3]+0.01
  }
  
  base_value_list <- list("lower_bound"=base_value[1],"start_value"=base_value[2],"upper_bound"=base_value[3])
  
  slope_value_list <- list("lower_bound"=0,"start_value"=0.05,"upper_bound"=0.5)

  return(list("base_value"=base_value_list,"slope_value"=slope_value_list))
}


################################################################################
## asymptotic
################################################################################

estimate_starting_params_asymptotic_model <- function(env_vect){
  #'@param env_vect vector with environmental covariate in it
  env_vect_sub <- na.omit(unlist(env_vect))
  # base value 
  
  # c0_value <- as.numeric(quantile(env_vect_sub, probs=c(0.01,0.1,0.5)))
  c0_value <- as.numeric(quantile(env_vect_sub, probs=c(0.01,0.05,0.4)))
  
  if(c0_value[1] == c0_value[2]){
    c0_value[2] = c0_value[2]+0.01
  }
  if(c0_value[2] == c0_value[3]){
    c0_value[3] = c0_value[3]+0.01
  }
  
  c0_value_list <- list("lower_bound"=c0_value[1],"start_value"=c0_value[2],"upper_bound"=c0_value[3])
  
  # lrc_list <- list("lower_bound"=0,"start_value"=4,"upper_bound"=12)
  lrc_list <- list("lower_bound"=-15,"start_value"=-1,"upper_bound"=1.5)
  

  # Asym_value <- as.numeric(quantile(env_vect_sub, probs=c(0.6,0.9,0.98)))
  # 
  # if(Asym_value[1] == Asym_value[2]){
  #   Asym_value[2] = Asym_value[2]+0.01
  # }
  # if(Asym_value[2] == Asym_value[3]){
  #   Asym_value[3] = Asym_value[3]+0.01
  # }
  
  
  # Asym_list <- list("lower_bound"=Asym_value[1],"start_value"=Asym_value[2],"upper_bound"=Asym_value[3])
  # asymptote can maximally be 100 --> as the whole model predictes values between 0 and 100. so if this will be used for measurements, it must be adapted!!
  Asym_list <- list("lower_bound"=0,"start_value"=5,"upper_bound"=100)
  
  
  return(list("c0_value"=c0_value_list,"lrc_value"=lrc_list, "Asym_value" = Asym_list))

}

################################################################################
## logistic
################################################################################

estimate_starting_params_logistic_model <- function(env_vect){
  #'@param env_vect vector with environmental covariate in it

    env_vect_sub <- na.omit(unlist(env_vect))
  # base value 
  
  x0_value <- as.numeric(quantile(env_vect_sub, probs=c(0.2,0.5,0.8)))
  if(x0_value[1] == x0_value[2]){
    x0_value[2] = x0_value[2]+0.01
  }
  if(x0_value[2] == x0_value[3]){
    x0_value[3] = x0_value[3]+0.01
  }
  
  x0_value_list <- list("lower_bound"=x0_value[1],"start_value"=x0_value[2],"upper_bound"=x0_value[3])
  
  k_list <- list("lower_bound"=0.01,"start_value"=0.5,"upper_bound"=4)
  # L as fraction of length of the in put (-100 --> convetion: i added 100 days mor of env variables) divided thorugh 100 --> value to reach within this model framework
  L_list <- list("lower_bound"=0,"start_value"=(median(sapply(env_vect, length),na.rm=T)-100)/100,"upper_bound"= (median(sapply(env_vect, length),na.rm=T)-100)/10)

  
  return(list("x0_value"=x0_value_list,"k_value"=k_list, "L_value" = L_list))
  
}

################################################################################
## WangEngels
################################################################################

estimate_starting_params_WangEngels_model <- function(env_vect){
  #'@param env_vect vector with environmental covariate in it
  
  env_vect_sub <- na.omit(unlist(env_vect))
  # base value 
  
  # xmin_value <- as.numeric(quantile(env_vect_sub, probs=c(0.01,0.1,0.5)))
  xmin_value <- as.numeric(quantile(env_vect_sub, probs=c(0.1,0.25,0.4)))
  
  if(xmin_value[1] == xmin_value[2]){
    xmin_value[2] = xmin_value[2]+0.01
  }
  if(xmin_value[2] == xmin_value[3]){
    xmin_value[3] = xmin_value[3]+0.01
  }
  
  xmin_value_list <- list("lower_bound"=xmin_value[1],"start_value"=xmin_value[2],"upper_bound"=xmin_value[3])
  
  
  # xopt_value <- as.numeric(quantile(env_vect_sub, probs=c(0.3,0.5,0.9)))
  xopt_value <- as.numeric(quantile(env_vect_sub, probs=c(0.6,0.85,0.98)))
  
  if(xopt_value[1] == xopt_value[2]){
    xopt_value[2] = xopt_value[2]+0.01
  }
  if(xopt_value[2] == xopt_value[3]){
    xopt_value[3] = xopt_value[3]+0.01
  }
  
  xopt_value_list <- list("lower_bound"=xopt_value[1],"start_value"=xopt_value[2],"upper_bound"=xopt_value[3])
  
  # xmax_value <- as.numeric(quantile(env_vect_sub, probs=c(0.4,0.7,0.99)))
  xmax_value <- as.numeric(quantile(env_vect_sub, probs=c(0.7,0.95,0.99)))
  
  if(xmax_value[1] == xmax_value[2]){
    xmax_value[2] = xmax_value[2]+0.01
  }
  if(xmax_value[2] == xmax_value[3]){
    xmax_value[3] = xmax_value[3]+0.01
  }
  
  
  xmax_value_list <- list("lower_bound"=xmax_value[1],"start_value"=xmax_value[2],"upper_bound"=xmax_value[3])
  
  r_value_list <- list("lower_bound"=0.0000001,"start_value"=1,"upper_bound"=100)
  
  
  return(list("xmin_value"=xmin_value_list,"xopt_value"=xopt_value_list, "xmax_value" = xmax_value_list, "r_value" = r_value_list))
  
}
