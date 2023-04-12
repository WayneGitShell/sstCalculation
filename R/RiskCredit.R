# Compute the Basel 3 credit risk
getCreditBaselRisks <- function(creditRiskExposure, creditRiskExposureReinsurance, creditRiskExposureHypo, param_creditRiskFactor){
  
  alpha <- getParam("alpha")
  creditRiskRest <- param_creditRiskFactor*(sum(creditRiskExposure$weightedExposure)-sum(creditRiskExposureReinsurance$weightedExposure)-sum(creditRiskExposureHypo$weightedExposure))
  creditRiskHypo <- param_creditRiskFactor*(sum(creditRiskExposureHypo$weightedExposure))
  
  restTerms <- creditRiskRest
  hypoTerms <- creditRiskHypo
  
  standardDeviationBasel <- restTerms/getScalingFactorNormal(alpha)
  x <- list(standardDeviationBasel = standardDeviationBasel, hypoTerms = hypoTerms)
  
  return(retClass(x, "creditRiskBasel"))
}


# Credit risk class
getCreditMertonRisks <- function(spreadCurve, LGDMap, migrationMatrix, initialInterestRate, portfolio, rho, initialFX, refCurrency, progress){
  
  portfolioFX <- convertFX(portfolio, initialFX, refCurrency)
  creditShock <- getCreditShock(portfolioFX, spreadCurve, initialInterestRate, LGDMap)
  threshold <- getThreshold(migrationMatrix)
  counterparties <- getCounterparties(portfolioFX)
  
  x <- list(threshold = threshold, counterparties = counterparties, losses = creditShock, rho = rho, progress = progress)
  return(retClass(x, "creditRiskMerton"))
}


# Get the list of counterparties
getCounterparties <- function(portfolio){
  counterparties <- unique(portfolio[, .(counterparty = counterpartyId, rating)])
  return(counterparties)
}


# Convert the portfolio to the reference currency
convertFX <- function(portfolio, initialFX, refCurrency){
  portfolio <- copy(portfolio)
  
  # Set missing scaling factors to 1
  portfolio[is.na(scalingCF), scalingCF := 1]
  portfolio[is.na(scalingLGD), scalingLGD := 1]
  
  portfolio[initialFX[to == refCurrency], fxRate := fx, on = c("currency" = "from")]
  portfolio[is.na(fxRate), fxRate := 1]
  portfolio[, fxRate := fxRate * scalingCF]
  
  # Convert the market value and cashflows
  for(j in c("marketValue", paste0("Y", 1:50))){
    set(portfolio, j = j, value = portfolio[[j]] * portfolio$fxRate)
  }
  portfolio[, fxRate := NULL]
  
  return(portfolio[])
}


# Get the shock from default and migration
getCreditShock <- function(portfolioFX, spreadCurve, initialInterestRate, LGDMap){
  curve <- spreadCurve$curve
  rating <- portfolioFX$rating
  migration <- portfolioFX$migration
  rowNumber <- portfolioFX$rowNumber
  
  yield <- as.matrix(initialInterestRate[, paste0("Y", 1:50)])
  rownames(yield) <- initialInterestRate$currency
  
  coupons <- getCoupons(portfolioFX)
  spread <- getSpread(coupons, value = portfolioFX$marketValue, yield, migration)

  # Check that the resulting spread is valid
  addErrorCell("portfolioCreditRisk", "Please provide at least one cash-flow for this counterparty", rows = rowNumber[rowSums(coupons, na.rm = TRUE) == 0 & migration == TRUE], columns = "marketValue")
  addErrorCell("portfolioCreditRisk", "Could not compute the implied spread to match the market value with the cash-flows", rows = rowNumber[is.na(spread)], columns = "marketValue")
  addErrorCell("portfolioCreditRisk", "The spread calculated for this counterparty is larger than 30%", type = "Warning", rows = rowNumber[spread > 0.3], columns = "marketValue")
  addErrorCell("portfolioCreditRisk", "The spread calculated for this counterparty is lower than -10%", type = "Warning", rows =  rowNumber[spread < -0.1], columns = "marketValue")
  
  migrationShock <- getMigrationShock(coupons, yield, spread, curve, rating, migration)
  defaultShock <- getDefaultShock(portfolioFX, LGDMap)
  creditShock <- cbind(portfolioFX[, .(counterparty = counterpartyId)], migrationShock, defaultShock)
  
  creditShock[, lapply(.SD, sum), by = c("counterparty")]
}


# Default shock
getDefaultShock <- function(portfolioFX, LGDMap){
  default <- LGDMap[portfolioFX, -marketValue * LGD * i.scalingLGD, on=c("class"="basel")]
  data.table(lossRating9 = default)
}


# Migration shock for a specific rating change
getMigrationShockFromSpread <- function(coupons, yield, spread, curve, ratingOld, ratingNew, migration){
  newSpread <- spread + curve[ratingNew] - curve[ratingOld]
  migrationShock <- getPrice(coupons, spread = newSpread, yield) - getPrice(coupons, spread, yield)
  index <- migration == FALSE
  migrationShock[index] <- rep(0, sum(index))
  return(migrationShock)
}


# Migration shock
getMigrationShock <- function(coupons, yield, spread, curve, rating, migration){
  # There are always 8 rating classes, this is on purpose hardcoded
  ratingNew <- stats::setNames(1:8, nm = paste0("lossRating", 1:8))
  migrationShock <- lapply(ratingNew, getMigrationShockFromSpread, coupons = coupons, yield = yield, spread = spread, curve = curve, ratingOld = rating, migration = migration)
  
  return(as.data.table(migrationShock))
}


# Valuation function for bonds
getPrice <- function(coupons, spread, yield){
  vapply(seq_len(nrow(coupons)), function(i){
    getBondPrice(spread = spread[i], coupons = coupons[i, ], yields = yield[rownames(coupons)[i], ], years = 1:50)
  }, double(1))
}


# Compute the spread for each instrument with migration
getSpread <- function(coupons, value, yield, migration){
  vapply(seq_len(nrow(coupons)), function(i){
    if(migration[i] == TRUE){
      calculateInitialSpread(marketValue = value[i], times = 1:50, coupons = coupons[i, ], riskFree = yield[rownames(coupons)[i], ])  
    }else{
      return(0)
    }
  }, double(1))
}


# Get coupons from the portfolio
getCoupons <- function(portfolioFX){
  coupons <- as.matrix(portfolioFX[, paste0("Y", 1:50)])
  rownames(coupons) <- portfolioFX$currency
  return(coupons)
}


# Compute the bond price
getBondPrice <- function(spread, coupons, yields, years){
  sum(coupons * exp(-(yields + spread) * years), na.rm = TRUE)
}


# Scaling factor to get the expected shortfall from the standard deviation of a normal distribution
getScalingFactorNormal <- function(alpha){
  scalingFactorNormal <- stats::dnorm(stats::qnorm(alpha))/alpha
  return(scalingFactorNormal)
}


# Get the threshold for the transition between rating classes
getThreshold <- function(migrationMatrix){
  nbRating <- nrow(migrationMatrix)
  tolerance <- getParam("correlationTolerance")
  
  # Check that the structure is correct
  indicatorCol <- !identical(c("category", seq_len(nbRating+1), "rowNumber"), names(migrationMatrix))
  indicatorRow <- !identical(migrationMatrix$category, seq_len(nbRating))
  addError("migrationMatrix", "Incorrect matrix definition", condition = indicatorCol | indicatorRow)
  
  # Check that it is a transition matrix
  migration <- as.matrix(migrationMatrix[, as.character(seq_len(nbRating+1)), with = FALSE])
  addError("migrationMatrix", "The row sum should be always 100%", condition = any(abs(rowSums(migration) - 1) > tolerance))
  
  threshold <- t(1-apply(migration, 1, cumsum))
  threshold <- threshold[, -(nbRating+1)]
  threshold <- as.data.table(threshold)
  
  return(threshold)
}


# List of all counterparties
getAllCounterparties <- function(Model){
  
  counterparties <- unique(rbindlist(lapply(Model, function(x){getValue(x$Modules$creditMerton, "counterparties")})))
  
  if (counterparties[,.N]>0){
    errorTable <- counterparties[, nb:=.N, by = "counterparty"][nb > 1, .(errorMessage = paste0("Only one rating is allowed per counterparty. Counterparty '", counterparty, "' has rating ", rating))]
    addError("portfolioCreditRisk", errorTable$errorMessage)
    setorderv(counterparties, c("counterparty", "rating"))
    counterparties[, nb := NULL]
  }
  
  return(counterparties)
}
