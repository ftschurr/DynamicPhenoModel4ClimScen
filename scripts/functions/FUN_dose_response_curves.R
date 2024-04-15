# Author: Flavian Tschurr
# Project: KP030
# Date: 22.03.2022
# Purpose: multi env model: response functions and fitting
################################################################################

################################################################################
# response function linear
################################################################################

reg_linear_response <- function(env_variate,intercept_value,slope){
  #'@param env_variate value of a environmental covariate
  #'@param intercept_value estimated intercept value
  #'@param slope estimated value, slope of the linear phase
  #'@description broken stick model according to an env variable
  y = intercept_value + env_variate*slope
  return(y)
}



reg_linear_response_loop <- function(env_data_measure, params){
  #'@param env_data_measure vector with environmental measurements (i.e. temp)
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description loops over the non linear (broken stick) modell for all given temperature and sums value up)
  #non linear resopnse funtion
  intercept_value = params[which(names(params)=="intercept_value")]
  slope = params[which(names(params)=="slope_value")]
  
  growth_modelled_all <-  sum(unlist(lapply(na.omit(env_data_measure), reg_linear_response, intercept_value=intercept_value,slope=slope)))
  return(growth_modelled_all)
}

reg_linear_prediction <- function(env_variate,params){
  #'@param env_variate value of a environmental covariate
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description broken stick modell according to an env variable
  #'
  intercept_value = as.numeric(params[which(names(params)=="intercept_value")])
  slope = as.numeric(params[which(names(params)=="slope_value")])
  
  y = intercept_value + env_variate*slope
  return(y)
}




################################################################################
# response function non_linear
################################################################################

non_linear_response <- function(env_variate,base_value,slope){
  #'@param env_variate value of a environmental covariate
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description broken stick modell according to an env variable
  y = (env_variate-base_value)*slope
  y = ifelse(env_variate>base_value,y,0)
  return(y)
}



non_linear_response_loop <- function(env_data_measure, params){
  #'@param env_data_measure vector with environmental measurements (i.e. temp)
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description loops over the non linear (broken stick) modell for all given temperature and sums value up)
  #non linear resopnse funtion
  base_value = params[which(names(params)=="base_value")]
  slope = params[which(names(params)=="slope_value")]
  
  growth_modelled_all <-  sum(unlist(lapply(na.omit(env_data_measure), non_linear_response, base_value=base_value,slope=slope)))
  return(growth_modelled_all)
}

non_linear_prediction <- function(env_variate,params){
  #'@param env_variate value of a environmental covariate
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description broken stick modell according to an env variable
  #'
  base_value = as.numeric(params[which(names(params)=="base_value")])
  slope = as.numeric(params[which(names(params)=="slope_value")])

    y = (env_variate-base_value)*slope
  y = ifelse(env_variate>base_value,y,0)
  return(y)
}







################################################################################
# logistic function
################################################################################


# the simple one is not flexible enough 
logistic_response <- function(x, L, k, x0){
  #'@param x input variable
  #'@param L  the curve's maximum value;
  #'@param k 	he logistic growth rate or steepness of the curve.
  #'@param x0  the x value of the sigmoid's midpoin
  #'@description function is: f(x) =  L/(1+ exp(-k*(x-x0)))
  
  y = L/(1+ exp(-k*(x-x0))) 
  y <- ifelse(y > 0, y,0) # no negative growth --> should anyway not be possible
  
  return(y)
}

logistic_response_loop <- function(env_data_measure, params){
  #'@param env_data_measure vector with environmental measurements (i.e. temp)
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description loops over the non linear (broken stick) modell for all given temperature and sums value up)
  #non linear resopnse funtion
  L = params[which(names(params)=="L_value")]
  k = params[which(names(params)=="k_value")]
  x0 = params[which(names(params)=="x0_value")]
  
  
  growth_modelled_all <-  sum(unlist(lapply(na.omit(env_data_measure), logistic_response, L=L,k=k, x0=x0)))
  return(growth_modelled_all)
}

logistic_prediction <- function(x, params){
  #'@param x input variable
  #'@param L  the curve's maximum value;
  #'@param k 	he logistic growth rate or steepness of the curve.
  #'@param x0  the x value of the sigmoid's midpoin
  #'@description function is: f(x) =  L/(1+ exp(-k*(x-x0)))
  
  L = as.numeric(params[which(names(params)=="L_value")])
  k = as.numeric(params[which(names(params)=="k_value")])
  x0 = as.numeric(params[which(names(params)=="x0_value")])
  
  y = L/(1+ exp(-k*(x-x0))) 
  y <- ifelse(y > 0, y,0) # no negative growth --> should anyway not be possible
  
  return(y)
}



logistic_constraint <- function(starting_params){
  if(length(which(is.na(as.numeric(starting_params))==T))!=0){
    return(FALSE)
  }
  # curve must be from small value to bigger
  if(as.numeric(starting_params[which(names(starting_params) == "k_value")]) <= 0){
    return(TRUE)  # Constraint satisfied
  } else {
    return(FALSE) # Constraint violated
  }
  
}

################################################################################
# asymptotic function
################################################################################


# the simple one is not flexible enough 
asymptotic_response <- function(x, Asym, lrc, c0){
  #'@param x input variable
  #'@param Asym  a numeric parameter representing the horizontal asymptote on the right side (very large values of input).
  #'@param lrc 	a numeric parameter representing the natural logarithm of the rate constant.
  #'@param c0  	a numeric parameter representing the x for which the response is zero.
  #'

  y <- SSasympOff(x , Asym, lrc, c0)
  y <- ifelse(y > 0, y,0) # no negative growth
  return(y)
}

asymptotic_response_loop <- function(env_data_measure, params){
  #'@param env_data_measure vector with environmental measurements (i.e. temp)
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description loops over the non linear (broken stick) modell for all given temperature and sums value up)
  #non linear resopnse funtion
  Asym = params[which(names(params)=="Asym_value")]
  lrc = params[which(names(params)=="lrc_value")]
  c0 = params[which(names(params)=="c0_value")]
  
  
  growth_modelled_all <-  sum(unlist(lapply(na.omit(env_data_measure), asymptotic_response, Asym = Asym, lrc = lrc, c0 = c0)))
  return(growth_modelled_all)
}


# the simple one is not flexible enough 
asymptotic_prediction <- function(x, params){
  #'@param x input variable
  #'@param Asym  a numeric parameter representing the horizontal asymptote on the right side (very large values of input).
  #'@param lrc 	a numeric parameter representing the natural logarithm of the rate constant.
  #'@param c0  	a numeric parameter representing the x for which the response is zero.
  #'
  Asym = as.numeric(params[which(names(params)=="Asym_value")])
  lrc = as.numeric(params[which(names(params)=="lrc_value")])
  c0 = as.numeric(params[which(names(params)=="c0_value")])

  y <- SSasympOff(x , Asym, lrc, c0)
  y <- ifelse(y > 0, y,0) # no negative growth
  return(y)
}


# asymptotic_constraint <- function(starting_params){
#   if(length(which(is.na(as.numeric(starting_params))==T))!=0){
#     return(FALSE)
#   }
#   
#   if(as.numeric(starting_params[which(names(starting_params) == "c0_value")]) < as.numeric(starting_params[which(names(starting_params) == "Asym_value")])){
#     return(TRUE)  # Constraint satisfied
#   } else {
#     return(FALSE) # Constraint violated
#   }
#   
# }
# 

################################################################################
# Wang Engels function
################################################################################



WangEngels_response <- function(x, xmin, xopt,xmax,r){
  #'@param x effective env_variable value
  #'@param xmin minimal env_variable value of the wang engels model
  #'@param xopt optimal env_variable value according to wang engel model
  #'@param xmax maximal env_variable value according to the wang engel model
  #'@param r scaling rate
  alpha <- log(2)/(log((xmax-xmin)/(xopt-xmin)))

  if(xmin <= x & x <= xmax){
    
    y <- r*(2*(x-xmin)^alpha*(xopt-xmin)^alpha-(x-xmin)^(2*alpha))/((xopt-xmin)^(2*alpha))
    
  }else{
   y <- 0
  }
  
  return(y)
  
}


WangEngels_response_loop <- function(env_data_measure, params){
  #'@param env_data_measure vector with environmental measurements (i.e. temp)
  #'@param base_value estimated value, start of the linear growing phase
  #'@param slope estimated value, slope of the linear phase
  #'@description loops over the non linear (broken stick) modell for all given temperature and sums value up)
  #non linear resopnse funtion
  xmin = params[which(names(params)=="xmin_value")]
  xopt = params[which(names(params)=="xopt_value")]
  xmax = params[which(names(params)=="xmax_value")]
  r = params[which(names(params)=="r_value")]
  
  growth_modelled_all <-  sum(unlist(lapply(na.omit(env_data_measure), WangEngels_response, xmin = xmin, xopt = xopt, xmax = xmax, r = r)))
  return(growth_modelled_all)
}


WangEngels_prediction <- function(x, params){
  #'@param x effective env_variable value
  #'@param xmin minimal env_variable value of the wang engels model
  #'@param xopt optimal env_variable value according to wang engel model
  #'@param xmax maximal env_variable value according to the wang engel model
  #'
  xmin = as.numeric(params[which(names(params)=="xmin_value")])
  xopt = as.numeric(params[which(names(params)=="xopt_value")])
  xmax = as.numeric(params[which(names(params)=="xmax_value")])
  r = as.numeric(params[which(names(params)=="r_value")])
  
  
  alpha <- log(2)/(log((xmax-xmin)/(xopt-xmin)))
  
  if(xmin < x & x < xmax){
    
    y <- r*(2*(x-xmin)^alpha*(xopt-xmin)^alpha-(x-xmin)^(2*alpha))/((xopt-xmin)^(2*alpha))
    
  }else{
    y <- 0
  }
  
  return(y)
  
}

WangEngels_constraint <- function(starting_params){
  # if (starting_params$xmin_value < starting_params$xopt_value && starting_params$xopt_value < starting_params$xmax_value) {
  if(length(which(is.na(as.numeric(starting_params))==T))!=0){
    return(FALSE)
  }
  if(as.numeric(starting_params[which(names(starting_params) == "xmin_value")]) < as.numeric(starting_params[which(names(starting_params) == "xopt_value")]) &&
     as.numeric(starting_params[which(names(starting_params) == "xopt_value")]) < as.numeric(starting_params[which(names(starting_params) == "xmax_value")])){
    return(TRUE)  # Constraint satisfied
  } else {
    return(FALSE) # Constraint violated
  }
  
}

################################################################################
# fitting functions
################################################################################

fit_dose_response_model_course <- function(params,env_data,control_data,names_params, .response_function., .constraint_function.){
  # fit_dose_response_model_course <- function(params,argument_list){
  
  #'@param params list with paramters (per paramter 3 values, min, start and max)
  #'@param env_data list with envrionmental data (per measurement a vector in the list)
  #'@param names_params names of the paramters
  #'@param .response_function. response function 
  
  # get parameters
  source("scripts/functions/FUN_skillscores.R")
  # name the parameteres in the vector accordingly
  names(params) <- names_params
  print(params)
  if(!is.null(.constraint_function.)){
    if(.constraint_function.(params)==FALSE){
      skill_score <- 10000
      print("constraint")
      return(skill_score)
    }
  }
  
  # get values
  growth_modelled <- unlist(lapply(env_data, .response_function., params))
  # calculate skill score
  
  
  skill_score <- calc_RMSE(measured = control_data, modelled = growth_modelled)

  if(length(which(round(growth_modelled,10)==0)) >= length(growth_modelled)*0.75){
    # skill_score <- 10000
    print("low values")
    skill_score <- skill_score * 100
  }
  if(is.na(skill_score)){
    skill_score <- 10000
  }
  
  return(skill_score)
  
}

