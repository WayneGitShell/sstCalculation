# Simulate instruments
#' @export
simulate.marketInstrumentsModule <- function(object, nsim, seed, ...){
  setSeed(seed, "marketInstruments")
  mu <- getValue(object, "mu")
  Sigma <- getValue(object, "Sigma")
  standalonesList <- getValue(object, "standalonesList")
  instruments <- getValue(object, "instruments")
  progress <- getValue(object, "progress")
  
  RF <- data.table(MASS::mvrnorm(n = nsim, mu = mu, Sigma = Sigma))
  
  sim <- as.data.table(lapply(standalonesList, calculateStandalone, RF = RF, instruments = instruments, centered = TRUE, participationValue = 0, progress = progress))
  
  result <- list(sim = sim[], RF = RF[])
  return(result)
}


# Simulate the participation
#' @export
simulate.marketParticipationModule <- function(object, nsim, seed, ...){
  setSeed(seed, "marketParticipation")
  
  exposure <- getValue(object, "exposure")
  sd <- getValue(object, "sd")
  
  sim <- data.table(all = exposure * expm1(stats::rnorm(n = nsim, sd = sd) - 0.5 * (sd^2)))
  
  result <- list(sim = sim[], RF = NULL)
  return(result)}


# Simulate from a multivariate normal distribution
simulationMultivariate <- function(correlation, volatility, nsim){
  simMatrix <- MASS::mvrnorm(n = nsim, mu = rep(0, ncol(correlation)), Sigma = correlation)
  
  sim <- as.data.table(t(volatility * t(simMatrix)))
  sim[, all := rowSums(.SD)]
  
  # Use the right column order.
  setcolorder(sim, c("all", names(volatility)))
  
  result <- list(sim = sim[], RF = as.data.table(simMatrix)[])
  return(result)
}

