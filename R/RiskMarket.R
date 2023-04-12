# Get the risk paramters from market instruments
getMarketInstrumentsModule <- function(instrumentsStandalones, marketCovariance, progress = 0){
  Sigma <- marketCovariance
  mu <- rep(0, ncol(Sigma))
  standalonesList <- stats::setNames(nm = c("all", getParam("standaloneTypes")))
  
  x <- list(instruments = instrumentsStandalones, standalonesList = standalonesList, Sigma = Sigma, mu = mu, progress = progress/length(standalonesList))
  retClass(x, "marketInstrumentsModule")
}


# Get the risk paramters from the immaterial participation
getMarketParticipationModule <- function(participationValue, participationVolatility){
  x <- list(exposure = participationValue, sd = participationVolatility)
  retClass(x, "marketParticipationModule")
}


# Get target currency
getTargetCurrency <- function(marketRiskFactorsCurrency){
  targetCurrency <- setdiff(marketRiskFactorsCurrency$targetCurrency, "")
  addErrorCell("marketRiskFactorsCurrency", "There should be exactly one target currency", columns = rep("targetCurrency", length(targetCurrency) != 1), rows = 0)
  addErrorCell("marketRiskFactorsCurrency", "The currency should not be identical to the target currency", columns = "currency", rows = marketRiskFactorsCurrency[targetCurrency == currency, rowNumber])
  return(targetCurrency)
}


# Currency change mapping
getCurrencyChangeMapping <- function(Currency, Delta, refCurrency, riskFactors, targetCurrency){
  # Check that the currency has the right risk factors names
  U <- Currency[name != paste0(currency, targetCurrency), ]
  addErrorCell("marketRiskFactorsCurrency", paste0("The risk factor name should be '", U$currency, U$targetCurrency, "'"), rows = U$rowNumber, columns = "name")
  
  # Check that the mappping table for the original risk factors has the right names
  currencies <- unique(c(Currency$currency, targetCurrency))
  B <- data.table(from = currencies, target = targetCurrency, reference = refCurrency)
  oldCurrencyName <- B[from != reference, paste0(from, reference)]
  newCurrencyName <- B[from != target, paste0(from, target)]
  adjustedNames <- c(setdiff(Delta$name, oldCurrencyName), newCurrencyName)
  error <- setdiff(adjustedNames, riskFactors)
  addError("marketRiskFactorsDelta", paste0("The element '", error, "' is not defined as a risk factor"), columns = "name", condition = length(error)>0)
  error <- setdiff(riskFactors, adjustedNames)
  addError("marketRiskFactorsDelta", paste0("The risk factor '", error, "' is missing"), columns = "name", condition = length(error)>0)
  
  # Mapping for currency change of the delta terms
  mappingDeltaRF <- rbind(B[!(target == from | reference == from), .(name = paste0(from, reference), newName = paste0(from, target), coef = 1, type = "currency")],
                          B[!(from == reference | target == reference), .(name = paste0(from, reference), newName = paste0(reference, target), coef = -1, type = "currencyBackward")],
                          Delta[!name %in% Currency$name, .(name = name, newName = name, coef = 1, type = "nonCurrency")])
}


# Generate the mapping table between projection term and time
getRateWithTime <- function(marketRiskFactorsRate, maturitiesInterestRate){
  RateWithTime <- maturitiesInterestRate[marketRiskFactorsRate, on = c("projection" = "horizon"), allow.cartesian=TRUE, nomatch = 0]
  return(RateWithTime)
}


# Get the market covariance market
getMarketCovariance <- function(correlation, volatility){
  covariance <- calculateCovariance(keyword = "correlationMarket", correlation = correlation, volatility = volatility)$covariance
  return(covariance)
}


# Raw instruments are the instruments whose exposure is undiscounted and given in the original currency
getRawInstruments <- function(instrumentAsset, instrumentAssetForward, instrumentFX_Forward, instrumentFixedIncome, instrumentLiability, instrumentDelta, referenceCurrency, mappingDeltaRF){
  
  
  # Each iLnstrument is a list
  instrumentList <- list(
    instrumentAsset = process_instrumentAsset(instrumentAsset),
    instrumentAssetForward = process_instrumentAssetForward(instrumentAssetForward),
    instrumentFX_Forward = process_instrumentFX_Forward(instrumentFX_Forward, referenceCurrency),
    instrumentFixedIncome = process_instrumentFixedIncome(instrumentFixedIncome),
    instrumentLiability = process_instrumentLiability(instrumentLiability),
    instrumentDelta = process_instrumentDelta(instrumentDelta, referenceCurrency, mappingDeltaRF)
  )
  rawInstruments <- rbindlist(instrumentList, idcol = "keyword", use.names = TRUE)
  
  return(rawInstruments)
}


# Check that the instruments are associated to a risk factor
checkInstruments <- function(marketRiskFactorsAsset, marketRiskFactorsSpread, marketRiskFactorsCurrency, rawInstruments, targetCurrency, RateWithTime){
  checkReference(market = marketRiskFactorsAsset, targetCurrency, rawInstruments, colNames = c("label", "currency"))
  checkReference(market = RateWithTime, targetCurrency, rawInstruments, colNames = c("time", "currency"))
  checkReference(market = marketRiskFactorsSpread, targetCurrency, rawInstruments, colNames = c("rating", "currency"))
  checkReference(market = marketRiskFactorsCurrency, targetCurrency, rawInstruments, colNames = c("currency"), removeTargetCurrency = TRUE)
}


# Adjustment for currency change
getCurrencyAdjustmentTable <- function(instruments, marketRiskFactorsCurrency, referenceCurrency, targetCurrency){
  if(referenceCurrency != targetCurrency){
    u <- marketRiskFactorsCurrency[currency == referenceCurrency]
    currencyAdjustment <- instruments[!is.na(currency) & currency != referenceCurrency, .(instrumentId, factorId = u$factorId, name =  u$name, scale = - u$scale)]
  }else{
    currencyAdjustment <- data.table(instrumentId = integer(0), factorId = integer(0), name = character(0), scale = numeric(0))
  }
  return(currencyAdjustment)
}


# Get the covariance matrix based on the correlation and volatility
calculateCovariance <- function(keyword, correlation, volatility){
  standalones <- volatility != 0
  lenVola <- length(volatility)
  lenCorr <- ncol(correlation)
  
  # Checks
  message <- paste0("The sensitivity vector has ", lenVola, " elements whereas the correlation matrix has size ", lenCorr, "x", lenCorr, ". Please ensure that both have the same size")
  addError(keyword, message = message, condition = length(volatility) != ncol(correlation))
  
  message <- "The sensitivity vector and the correlation matrix need to have the same names"
  addError(keyword, message, condition = !all(sort(names(volatility)) == sort(colnames(correlation))))
  
  message <- "The names of the sensitivity vector and of the correlation matrix need to be provided in the same order"
  addError(keyword, message, condition = !identical(names(volatility), colnames(correlation)))
  
  # Remove risk factors with no exposure (volatility 0)
  keep <- names(which(volatility != 0))
  correlation <- correlation[rownames(correlation) %in% keep, colnames(correlation) %in% keep, drop = FALSE]
  volatilityMatrix <- diag(volatility[keep], nrow = length(keep))
  
  covariance <- volatilityMatrix %*% correlation %*% volatilityMatrix
  
  # Degenerated case
  if(nrow(covariance) == 0){
    x <- list(covariance = covariance, standalones = standalones)
    return(x)
  }
  
  message <-  "The covariance matrix is not symmetric, the difference is higher than the specified tolerance"
  correlationTolerance <- getParam("correlationTolerance")
  addError(keyword, message, condition = max(abs(covariance - t(covariance))) > correlationTolerance)
  
  covariance <- (covariance + t(covariance))/2
  message <- "The correlation matrix is not positive definite"
  addError(keyword, message, condition = !all(eigen(covariance, symmetric = T, only.values = T)$values >= 0))
  
  # Add row and column names
  rownames(covariance) <- colnames(covariance) <- colnames(correlation)
  
  x <- list(covariance = covariance, standalones = standalones)
  return(x)
}


# Compute the initial spead
calculateInitialSpread <- function(marketValue, times, coupons, riskFree = NULL){
  if(is.null(riskFree)){
    riskFree <- rep(0, length(times))
  }
  # Valuation function
  f <- function(x) {sum(coupons * exp(-(riskFree + x) * times), na.rm = TRUE) - marketValue}
  df <- function(x) {sum(-coupons * times * exp(-(riskFree + x) * times), na.rm = TRUE)}
  
  # Tolerance
  atol <- getParam("relativeMarketValueTolerance") * marketValue[1]
  rtol <- getParam("relativeSpreadTolerance")
  y <- 0
  
  for(i in 1:1e4){
    x <- y
    y <- x - f(x)/df(x)
    # Return NAN if it diverges
    if(!is.finite(y)){
      return(NaN)
    }
    
    if(abs(x - y) <= rtol * abs(y)){
      # Return NA if it does not match the criteria for the absolute error
      if(abs(f(x)) > atol){
        return(NA_real_)
      }else{
        return(x)
      }
    }
  }
  return(NA_real_)
}


# Compute market risk for a given standalone
calculateStandalone <- function(standaloneType, RF, instruments, centered = TRUE, participationValue = 0, progress = 0){
  # Keep only the instruments for the selected standalones
  instruments <- instruments[standaloneTypes == standaloneType]
  
  # Sanity check: ensure that all risk factors from the instruments table have been simulated
  missingRF <- setdiff(instruments$name, names(RF))
  addError(NA, paste0("Internal error: The risk factor ", missingRF, " is missing"), condition = length(missingRF)>0)
  
  x <- rep(0, nrow(RF))
  
  # Special case: for scenario valuation, the centering is not relevant.
  if(!centered){
    # No centering. This is done by setting the normalization constant to 0
    instruments[, constant := 0]
    
    # Add the participation impact
    x <- participationValue * expm1(RF$participation)
  }
  
  instrumentsSplit <- split(instruments, by = "instrumentId")
  
  if(length(instrumentsSplit) == 0){
    if(progress > 0){
      addProgress(step = progress,  message = "Simulating")
    }
  }
  
  # Apply the valuation function for each instrument
  for(u in instrumentsSplit){
    if(progress > 0){
      # Show noting when the macroeconomic scenario valuation is done (when progress == 0)
      addProgress(step = progress / length(instrumentsSplit), message = "Simulating")
    }
    
    if(all(u$exponentialValuation == TRUE)){
      if(nrow(u) == 1){
        x  <- x + u$exposure * expm1(u$scale * RF[[u$name]] + u$constant)
      }else if(nrow(u) == 2){
        x  <- x + u$exposure[1] * expm1(u$scale[1] * RF[[u$name[1]]] + u$scale[2] * RF[[u$name[2]]] + u$constant[1])
      }else if(nrow(u) == 3){
        x  <- x + u$exposure[1] * expm1(u$scale[1] * RF[[u$name[1]]] + u$scale[2] * RF[[u$name[2]]] + u$scale[3] * RF[[u$name[3]]] + u$constant[1])
      }else if(nrow(u) == 4){
        x  <- x + u$exposure[1] * expm1(u$scale[1] * RF[[u$name[1]]] + u$scale[2] * RF[[u$name[2]]] + u$scale[3] * RF[[u$name[3]]] + u$scale[4] * RF[[u$name[4]]] + u$constant[1])
      }else{
        stop("Incorrect number of dimensions")
      }
    }else{
      x <- x + u$exposure[1] * u$scale[1] * RF[[u$name[1]]]
    }
  }
  
  # Named risk factors are used for scenario valuation
  if("name" %in% names(RF)){
    x <- stats::setNames(x, RF$name)
  }
  return(x)
}


# Compute the normalization constant for each standalone type
calculateStandalones <- function(standaloneType, instruments, instrumentsMapping, marketRiskFactorsStandalones, marketCovariance){
  # Select the risk factors for the selected standalone type
  selectedFactorId <- marketRiskFactorsStandalones[standalones == standaloneType | (standaloneType == "all"), factorId]
  mapping <- instrumentsMapping[factorId %in% selectedFactorId]
  
  instruments <- instruments[mapping, on = "instrumentId", nomatch = 0]
  instruments[exponentialValuation == TRUE, constant := -0.5*as.numeric(.SD$scale %*% marketCovariance[.SD$name, .SD$name] %*% .SD$scale), by = "instrumentId"]
  return(instruments)
}