# Merton credit risk
#' @export
simulate.creditRiskMerton <- function(object, nsim, seed, ...){
  setSeed(seed, "creditMerton")
  
  phi <- stats::rnorm(nsim)
  
  threshold <- getValue(object, "threshold")
  losses <- split(getValue(object, "losses"), by = "counterparty", keep.by = FALSE)
  counterparties <- getValue(object, "counterparties")
  rho <- getValue(object, "rho")
  progress <- getValue(object, "progress")
  
  n <- nrow(counterparties)
  
  sim <- data.table(default = rep(0, nsim), migration = rep(0, nsim))
  
  if(n == 0){
    addProgress(step = progress)
  }
  
  for(i in seq_len(n)){
    addProgress(step = progress/n, message = "Simulating")
    
    x <- counterparties[i]
    epsilon <- stats::rnorm(nsim)
    lossVector <- losses[[x$counterparty]]
    thresholdVector <- threshold[x$rating, ]
    if(!is.null(lossVector)){
      sim <- sim + simulateLossIssuer(lossVector, thresholdVector, phi, rho, epsilon = epsilon)
    }
  }
  sim[, migration := migration - mean(migration)]
  sim[, default := default - mean(default)]
  sim[, all := default + migration]
  setcolorder(sim, c("all", "migration", "default"))
  
  result <- list(sim = sim[], RF = NULL)
  return(result)
}


# Basel credit risk
#' @export
simulate.creditRiskBasel <- function(object, nsim, seed, ...){
  setSeed(seed, "creditBasel")
  
  standardDeviationBasel <- getValue(object, "standardDeviationBasel")
  hypoTerms <- getValue(object, "hypoTerms")
  
  simRest <- stats::rnorm(nsim, sd = standardDeviationBasel)
  
  # Negative value to represent this term as a loss
  simHypo <- rep(-hypoTerms, nsim)
  sim <- data.table(all = simRest + simHypo, Rest = simRest, Hypothek = simHypo)
  
  result <- list(sim = sim[], RF = NULL)
  return(result)
}


# Loss from an issuer
simulateLossIssuer <- function(lossVector, thresholdVector, phi, rho, epsilon){
  percentile <- getPercentile(phi, epsilon, rho)
  lossVector <- unname(unlist(lossVector))
  
  newRating <- 1 + rowSums(vapply(thresholdVector, function(x){percentile < x}, logical(length(percentile))))
  loss <- lossVector[newRating]
  
  sim <- data.table(default = loss, migration = loss)
  sim[newRating == 9, migration := 0]
  sim[newRating != 9, default := 0]
  return(sim[])
}


# Get the percentile of the credit quality
getPercentile <- function(phi, epsilon, rho){
  stats::pnorm(rho*phi + sqrt(1-rho^2)*epsilon)
}