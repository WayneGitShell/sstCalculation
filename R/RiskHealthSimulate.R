# Simulate health risks
#' @export
simulate.healthModule <- function(object, nsim, seed, ...){
  setSeed(seed, "Health")
  
  correlation <- getValue(object, "correlation")
  volatility <- getValue(object, "volatility")
  scenario <- getValue(object, "scenario")
  
  result <- simulationMultivariate(correlation = correlation, volatility = volatility, nsim = nsim)
  
  # The scenarios need to be simulated after the normal distribution to ensure that the multivariate normal distribution
  # has the same random number sequence.
  scenariosSimulations <- sample(x = scenario$effect, prob = scenario$probability, replace = TRUE, size = nsim)
  
  result$sim[, all_beforeScenario := all]
  result$sim[, all := all_beforeScenario + scenariosSimulations]
  result$sim[]
  return(result)
}