# Create a table with constants
#' @export
simulate.constantModule <- function(object, nsim, seed, ...){
  # Additional effects are on purpose with a negative sign as they increase the target capital (left tail)
  additionalEffects <- -getValue(object, "additionalEffects")
  # Expected results (no sign change needed)
  expectedFinancialResult <- getValue(object, "expectedFinancialResult")
  expectedInsuranceResult <- getValue(object, "expectedInsuranceResult")
  
  all <- expectedFinancialResult + expectedInsuranceResult + additionalEffects
  
  sim <- data.table(all = rep(all, nsim), expectedFinancialResult = expectedFinancialResult, expectedInsuranceResult = expectedInsuranceResult, additionalEffects = additionalEffects)
  
  result <- list(sim = sim, RF = NULL)
  return(result)
}
