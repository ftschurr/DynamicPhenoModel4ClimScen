predhelputils.get_all_permutations <- function(your_environmental_covariates){
  # Initialize an empty vector to store all permutations
  all_permutations <- NULL
  
  # Generate permutations for subsets of different lengths
  
  for (len in 1:length(your_environmental_covariates)) {
    perms <- permutations(
      n = length(your_environmental_covariates),
      r = len,
      v = your_environmental_covariates
    )
    perms <- apply(perms, 1, paste, collapse = "_")
    
    all_permutations <- c(all_permutations, perms)
    # Convert permutations to a character vector
    
    # Add the permutations to the result vector
    # all_permutations <- c(all_permutations, perms)
  }
  return(all_permutations)
  
}
