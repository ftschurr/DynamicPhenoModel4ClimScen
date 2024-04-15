# Author: Flavian Tschurr
# Project: KP030
# Date: 16.03.2023
# Purpose: dymenvmodel: apply to CH2018 data helper functions
################################################################################

CH2018Utils_W_per_sqm_to_J_per_sqcm <- function(radiation_per_sqm){
  return(radiation_per_sqm /10000 * (24*60*60))
}
