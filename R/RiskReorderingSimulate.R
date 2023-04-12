# Simulate the reordering
#' @export
simulate.Reordering <- function(object, nsim, seed, ...){
  # The seed depends on the type of objects that get reordered
  setSeed(seed, class(object)[1])
  
  correlation <- getValue(object, "correlation")
  labelIndependentComponents <- getValue(object, "labelIndependentComponents")
  
  reordering <- data.table(MASS::mvrnorm(n = nsim, mu = rep(0, ncol(correlation)), Sigma = correlation))
  
  # Add additional columns for the independently simulated risks
  for(j in labelIndependentComponents){
    set(reordering, j = j, value = stats::runif(nsim))
  }
  
  # Compute the ranks for each risk component
  for (j in names(reordering)){
    set(reordering, j = j, value = frank(reordering[[j]], ties.method = "first"))
  }
  
  return(reordering)
}