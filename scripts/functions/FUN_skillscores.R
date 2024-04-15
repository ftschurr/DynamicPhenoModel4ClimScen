# Author: Flavian Tschurr
# Project: KP030
# Date: 03.07.2023
# Purpose: DymEnvMod: skill scores
################################################################################

calc_RMSE <- function(measured, modelled){
  return(sqrt(mean((measured - modelled)^2)))
}



calc_MAE <- function(measured, modelled){
  return(abs(mean(measured - modelled)))
}


calc_SumLogLikelihood <- function(measured, modelled, sigma_error=10) {

  if (sigma_error < 0) {
    deviance <- 10000000
  } else {
  
    likelihoods <- dnorm(measured, mean = modelled, sd = sigma_error)
    
    log.likelihoods <- log(likelihoods)
    
    # deviance <- -1 * sum(log.likelihoods)
    deviance <- -2 * sum(log.likelihoods)
    
  }
  
  if (is.infinite(deviance)) {
    deviance <- 10000000
  }
  
  return(deviance)
}


calc_cor <- function(measured, modelled){
  return(cor(measured,modelled,method=c("pearson")))
}



calc_RRMSE <- function (data = NULL, measured, modelled, tidy = FALSE, na.rm = TRUE) 
{
  RRMSE <- rlang::eval_tidy(data = data, rlang::quo(sqrt(sum(({
    {
      measured
    }
  } - {
    {
      modelled
    }
  })^2)/length({
    {
      measured
    }
  }))/(mean({
    {
      measured
    }
  }))))
  if (tidy == TRUE) {
    return(as.data.frame(RRMSE))
  }
  if (tidy == FALSE) {
    return(RRMSE)
  }
}
