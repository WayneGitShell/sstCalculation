#' @export
simulate.nonlifeModule <- function(object, nsim, seed, ...){
  param <-  getValue(object, "param")
  # The seed add-on is different for each mother/daughter company. Otherwise the simulation would not be independent.
  nonLifeSeedAddon <- getValue(object, "nonLifeSeedAddon")
  scenario <- getValue(object, "scenario")
  
  # Fix the seed now, as a basis for the simulations which will be performed just afterwards.
  setSeed(seed, "NonLife", seedAddon = nonLifeSeedAddon)
  sim <- simulate(param, nsim = nsim, seed = NULL)
  
  # Now use a separate seed for the scenarios to have the same scenarios even when the NL risks change
  setSeed(seed, "NonLifeScenario", seedAddon = nonLifeSeedAddon)
  scenariosSimulations <- sample(x = scenario$effect, prob = scenario$probability, replace = TRUE, size = nsim)
  
  sim[, all_beforeScenario := all]
  sim[, all := all_beforeScenario + scenariosSimulations]
  
  result <- list(sim = sim, RF = NULL)
  return(result)
}

#' @export
simulate.NL_none <- function(object, nsim, seed, ...){
  CYrisk <- getValue(object, "CYrisk")
  PYrisk <- getValue(object, "PYrisk")
  
  simTable <- data.table(all = rep(0, nsim), CY = rep(-CYrisk, nsim), PY = rep(-PYrisk, nsim))
  
  return(simTable)
}

#' @export
simulate.NL_sim <- function(object, nsim, seed, ...){
  simulations <- getValue(object, "simulation")
  CYrisk <- getValue(object, "CYrisk")
  PYrisk <- getValue(object, "PYrisk")
  
  sim <- sample(x = simulations, replace = nsim > length(simulations), size = nsim)
  
  simTable <- data.table(all = sim, CY = rep(-CYrisk, nsim), PY = rep(-PYrisk, nsim))
  return(simTable)
}

#' @export
simulate.NL_log <- function(object, nsim, seed, ...){
  mu <- getValue(object, "mu")
  sigma <- getValue(object, "sigma")
  CYrisk <- getValue(object, "CYrisk")
  PYrisk <- getValue(object, "PYrisk")
  
  sim <- stats::rnorm(n = nsim, mean = mu, sd = sigma)
  sim <- -(exp(sim) - exp(mu + (sigma^2)/2))

  simTable <- data.table(all = sim, CY = rep(-CYrisk, nsim), PY = rep(-PYrisk, nsim))
  return(simTable)
}

#' @export
simulate.NL_cdf <- function(object, nsim, seed, ...){
  cdf <- getValue(object, "cdf")
  CYrisk <- getValue(object, "CYrisk")
  PYrisk <- getValue(object, "PYrisk")
  
  sim <- sample(x = cdf$x, size = nsim, prob = diff(c(0, cdf$cdf)), replace = T)
  
  simTable <- data.table(all = sim, CY = rep(-CYrisk, nsim), PY = rep(-PYrisk, nsim))
  return(simTable)
}

#' @export
simulate.NL_capt <- function(object, nsim, seed, ...){
  CY_param <- getValue(object, "CY")
  PY_param <- getValue(object, "PY")
  
  #PY risks are already sorted 
  CY_sim <- sort(simulate(CY_param, nsim = nsim))
  PY_sim <- simulate(PY_param, nsim = nsim)
  
  simTable <- data.table(all = CY_sim + PY_sim, CY = CY_sim, PY = PY_sim)
  
  # The simulations should not be sorted in the same order for mother and daughter companies
  # Therefore we sample the rows in a random order.
  simTable <- simTable[sample.int(nsim)]
  return(simTable)
}

#' @export
simulate.NL_captCY <- function(object, nsim, seed = NULL, ...){
  stochastic <- getValue(object, "stochastic")
  deterministic <- getValue(object, "deterministic")
  
  sim <- rep(-deterministic, nsim)
  
  for(i in seq_len(nrow(stochastic))){
    sim <- sim  + simulateLineOfBusinessCY(nsim, stochastic[i])
  }
  
  return(sim)
}

#' @export
simulate.NL_captPY <- function(object, nsim, seed = NULL, ...){
  stochastic <- getValue(object, "stochastic")
  
  sim <- rep(0, nsim)
  
  for(i in seq_len(nrow(stochastic))){
    sim <- sim + simulateLineOfBusinessPY(nsim, stochastic[i])
  }
  
  return(sim)
}


# PY Lob simulation
simulateLineOfBusinessPY <- function(nsim, u){
  loss <- stats::rlnorm(nsim, meanlog = u$mu, sdlog = u$sd) - exp(u$mu + u$sd^2/2)
  sim <- -loss
  
  # PY comonotonicity between PY Lobs
  sim <- sort(sim)
  return(sim)
}


# CY Lob simulation
simulateLineOfBusinessCY <- function(nsim, u){
  lossNormal <- simulateCompoundPoisson(nsim = nsim, frequency = u$normalLossNumber, scale = u$normalLossScale, shape = u$normalLossShape, EED = u$EED, EEL = u$EEL, indicatorLarge = FALSE)
  lossLarge <-  simulateCompoundPoisson(nsim = nsim, frequency = u$largeLossNumber, scale = u$largeLossThreshold, shape = u$largeLossShape, EED = u$EED, EEL = u$EEL, indicatorLarge = TRUE)
  
  loss <- lossNormal + lossLarge
  loss <- pmax(loss - u$AAD, 0)
  loss <- pmin(loss, u$AAL) * u$QS
  
  sim <- -(loss - mean(loss))
  return(sim)
}


# Pareto distribution
rpareto <- function(n, shape, scale) {
  return(scale*stats::runif(n = n)^(-1/shape))
}


# Compound poisson
simulateCompoundPoisson <- function(nsim, frequency, scale, shape, EED, EEL, indicatorLarge){
  if(is.na(frequency)){
    sim <- rep(0, nsim)
    return(sim)
  }
  
  numberLoss <- stats::rpois(n = nsim, lambda = frequency)
  
  if(indicatorLarge == FALSE){
    severity <- stats::rgamma(n = sum(numberLoss), shape = shape, scale = scale)
  }else{
    severity <- rpareto(n = sum(numberLoss), shape = shape, scale = scale)
  }
  
  severity <- pmax(severity - EED, 0)
  severity <- pmin(severity, EEL)
  
  simulations <- data.table(simulationId = rep.int(seq_len(nsim), numberLoss), severity = severity)
  
  sim <- rep(0, nsim)
  sim[which(numberLoss > 0)] <- simulations[, .(loss = sum(severity)), by = "simulationId"]$loss
  return(sim)
}

