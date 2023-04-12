# Simulate life risks
#' @export
simulate.lifeModule <- function(object, nsim, seed, ...){
  setSeed(seed, "Life")
  
  correlation <- getValue(object, "correlation")
  volatility <- getValue(object, "volatility")
  
  result <- simulationMultivariate(correlation = correlation, volatility = volatility, nsim = nsim)
  return(result)
}