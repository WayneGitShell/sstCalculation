#' @export
simulate.scenarioModule <- function(object, nsim, seed, ...){
  setSeed(seed, "Scenario")
  
  scenario <- object$scenario
  # RF reference in a unique way to a scenario. The RF is only used for internal purposed but is also exported for more transparency.
  RF <- sample.int(n = nrow(scenario), size = nsim, replace = TRUE, prob = scenario$probability)
  sim <- scenario$effect[RF]

  RF <- data.table(scenarioId = RF)
  sim <- data.table(all = sim)
  result <- list(sim = sim[], RF = RF)
  return(result)
}
