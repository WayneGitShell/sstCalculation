# Get the list of standalone instruments
getInstrumentsStandalones <- function(marketRiskFactorsStandalones, instruments, instrumentsMapping, marketCovariance, numberLoad){
  instruments <- instruments[, .(instrumentId, instrument, exposure, exponentialValuation)]
  
  # Get the instruments for each standalone type
  standaloneTypes <- stats::setNames(nm = c("all", getParam("standaloneTypes")))
  instrumentsStandalonesList <- lapply(standaloneTypes, calculateStandalones, instruments = instruments, instrumentsMapping = instrumentsMapping, marketRiskFactorsStandalones = marketRiskFactorsStandalones, marketCovariance = marketCovariance)
  instrumentsStandalones <- rbindlist(instrumentsStandalonesList, idcol = "standaloneTypes")
  
  return(instrumentsStandalones)
}

# Associate each instrument to a risk factor and a scaling factor
getInstrumentsMapping <- function(marketRiskFactorsAsset, marketRiskFactorsSpread, marketRiskFactorsCurrency, marketRiskFactorsDelta, referenceCurrency, instruments, targetCurrency, RateWithTime){
  instrumentsMappingList = list(rate = getRateTable(instruments, RateWithTime),
                                spread = getSpreadTable(instruments, marketRiskFactorsSpread),
                                asset = getAssetTable(instruments, marketRiskFactorsAsset),
                                currency = getCurrencyTable(instruments, marketRiskFactorsCurrency, referenceCurrency),
                                delta = getDeltaTable(instruments, marketRiskFactorsDelta),
                                currencyAdjustment = getCurrencyAdjustmentTable(instruments, marketRiskFactorsCurrency, referenceCurrency, targetCurrency)
  )
  
  
  instrumentsMapping <- rbindlist(instrumentsMappingList, use.names = TRUE, idcol = "informationJoinedRF")
  setorder(instrumentsMapping, "instrumentId")
  return(instrumentsMapping)
}


# Complete the missing instrument columns and append the instruments
getCompleteColumns <- function(instruments){
  instruments <- rbindlist(lapply(instruments, getCompleteInstrumentsColumns), use.names = TRUE, idcol = "instrument")
  return(instruments)
}


# Complete the missing instrument columns since not all instruments have the same output format.
getCompleteInstrumentsColumns <- function(instrumentOriginal){
  instrumentOriginal <- copy(instrumentOriginal)
  nm = list("currency" = NA_character_,"time" = NA_integer_, "exposure" = NA_real_, "rowNumber" = NA_integer_, "label" = NA_character_, "rating" = NA_character_, "name" = NA_character_, "spread" = NA_real_, "marketValue" = NA_real_)
  
  # Fill missing columns with the correct type
  for(colName in setdiff(names(nm), names(instrumentOriginal))){
    set(instrumentOriginal, j = colName, value = nm[[colName]])
  }
  setcolorder(instrumentOriginal, names(nm))
  
  return(instrumentOriginal)
}


# Appy the discount factors and foreign exchange conversion to the nominal exposures
getInstruments <- function(refCurrency, initialRate, initialFX, rawInstruments){
  
  # Remove instruments with no exposure
  instruments <- rawInstruments[exposure != 0]
  addError(NA, "No market risk exposure is provided", condition = nrow(instruments) == 0, type = "Warning")
  
  instruments <- getDiscountedInstruments(instruments, initialRate)
  instruments <- getCurrencyChangeInstruments(instruments, initialFX, refCurrency)
  instruments <- getSpreadDiscountedInstruments(instruments, refCurrency)
  
  # Exponential valuation for all terms except delta term
  instruments[, ':=' (rowNumber = NULL, marketValue = NULL, spread = NULL, instrumentId = seq_len(.N), exponentialValuation = is.na(name))]
  
  return(instruments)
}


# Apply spread
getSpreadDiscountedInstruments <- function(instruments, refCurrency){
  spreadDiscountedInstruments <- copy(instruments)
  
  checkProvidedSpreads(instrumentFixedIncome = instruments[keyword == "instrumentFixedIncome" & !is.na(spread)],
                       tolerance = getParam("relativeMarketValueTolerance"),
                       refCurrency = refCurrency
  )
  
  spreadDiscountedInstruments[keyword == "instrumentFixedIncome" & is.na(spread), spread := calculateInitialSpread(marketValue = marketValue[1], times = time, coupons = exposure), by = "rowNumber"]
  
  checkFinalSpread(instrumentFixedIncome = spreadDiscountedInstruments[keyword == "instrumentFixedIncome"])
  
  spreadDiscountedInstruments[!is.na(spread), exposure := exposure * exp(- time * spread)]
  return(spreadDiscountedInstruments)
}


# Discounting
getDiscountedInstruments <- function(instruments, initialRate){
  # Prevent modification by reference
  discountedInstruments <- copy(instruments)
  
  discountedInstruments[initialRate, exposure := exposure * exp(- time*i.value), on = c("currency", "time")]
  return(discountedInstruments)
}


# Currency change
getCurrencyChangeInstruments <- function(instruments, initialFX, refCurrency){
  # Prevent modification by reference
  instrumentsCurrencyChange <- copy(instruments)
  
  instrumentsCurrencyChange[initialFX[to == refCurrency], ':=' (exposure = exposure * i.fx, marketValue = marketValue * i.fx), on = c("currency" = "from")]
  return(instrumentsCurrencyChange) 
}


# Check that the spread is valid
checkFinalSpread <- function(instrumentFixedIncome){
  errors <- unique(instrumentFixedIncome[, c("spread", "rowNumber")], by = "rowNumber")
  addErrorCell("instrumentFixedIncome", "Unable to compute initial spread. Please ensure that the cash-flows provided are correct and/or enter the spread manually", rows = errors[is.nan(spread), rowNumber], columns = "spread")
  addErrorCell("instrumentFixedIncome", "Unable to compute initial spread. Please enter the spread manually", rows = errors[is.na(spread) & !is.nan(spread), rowNumber], columns = "spread")
  addErrorCell("instrumentFixedIncome", "The spread calculated for this instrument is larger than 30%", type = "Warning", rows = errors[spread > 0.3, rowNumber], columns = "spread")
  addErrorCell("instrumentFixedIncome", "The spread calculated for this instrument is lower than -10%", type = "Warning", rows =  errors[spread < -0.1, rowNumber], columns = "spread")
}


# Check the spreads provided
checkProvidedSpreads <- function(instrumentFixedIncome, tolerance, refCurrency){
  instrumentsError <- instrumentFixedIncome[, .(error = sum(exposure * exp(-spread*time)) - marketValue[1], threshold = abs(marketValue[1]) * tolerance), by = "rowNumber"][abs(error) > threshold]
  message <-  paste0("Invalid spread, the difference between the present value and the market value is ", round(instrumentsError$error, 2), " m", refCurrency)
  addErrorCell("instrumentFixedIncome", message = message, rows = instrumentsError$rowNumber, columns = "spread", type = "Warning")
}


# Get table with interest risk factors
getRateTable <- function(instruments, RateWithTime){
  instruments[RateWithTime, .(instrumentId, factorId = i.factorId, name = i.name, scale = -i.scale*time), on = c("time", "currency"), nomatch = 0]
}


# Table with spread risk factors
getSpreadTable <- function(instruments, marketRiskFactorsSpread){
  instruments[marketRiskFactorsSpread, .(instrumentId, factorId = i.factorId, name = i.name, scale = -i.scale*time), on = c("rating", "currency"), nomatch = 0]
}


# Table with asset price risk factors
getAssetTable <- function(instruments, marketRiskFactorsAsset){
  instruments[marketRiskFactorsAsset, .(instrumentId, factorId = i.factorId, name = i.name, scale = i.scale), on = c("label", "currency"), nomatch = 0]
}


# Table with currency risk factors
getCurrencyTable <- function(instruments, marketRiskFactorsCurrency, referenceCurrency){
  instruments[marketRiskFactorsCurrency[currency != referenceCurrency], .(instrumentId, factorId = i.factorId, name = i.name, scale = i.scale), on = "currency", nomatch = 0]
}


# Table with delta terms
getDeltaTable <- function(instruments, marketRiskFactorsDelta){
  instruments[marketRiskFactorsDelta, .(instrumentId, factorId = i.factorId, name = i.name, scale = i.scale), on = "name", nomatch = 0]
}


# Process equity instruments
process_instrumentAsset <- function(instrumentAsset){
  instruments <- list("Equity" = instrumentAsset[, .(label, currency, exposure = value, rowNumber)])
  instruments <- getCompleteColumns(instruments)
  return(instruments)
}


# Process asset forwards instruments
process_instrumentAssetForward <- function(instrumentAssetForward){
  instruments <- list("Asset forward underlying" = instrumentAssetForward[, .(label = type , currency, exposure = exposure * toNum(tolower(position) == "long"), rowNumber)],
                      "Asset forward delivery" = instrumentAssetForward[, .(currency, time, exposure = price  * toNum(tolower(position) == "short"), rowNumber)])
  
  instruments <- getCompleteColumns(instruments)
  return(instruments)
}


# Process FX forwards instruments
process_instrumentFX_Forward <- function(instrumentFX_Forward, referenceCurrency){
  addErrorCell("instrumentFX_Forward", "The foreign currency should not be equal to the SST-currency", rows = instrumentFX_Forward[foreign == referenceCurrency, rowNumber], columns = "foreign")
  
  instruments <- list("FX forward underlying" = instrumentFX_Forward[, .(currency = foreign, time, exposure = nominal * toNum(tolower(position) == "long"), rowNumber)],
                      "FX forward delivery" = instrumentFX_Forward[, .(currency = rep(referenceCurrency, .N), time, exposure = -nominal * toNum(tolower(position) == "long") * rate, rowNumber)])
  instruments <- getCompleteColumns(instruments)
  return(instruments)
}


# Process fixed income instruments
process_instrumentFixedIncome <- function(instrumentFixedIncome){
  tolerance <- getParam("relativeMarketValueTolerance")
  
  consistencyCheck <- instrumentFixedIncome[, .(anyMarketValue = any(marketValue != 0), anyExposure = any(value != 0), anyMissingSpread = any(value < 0 & is.na(spread))),  by = "rowNumber"]
  
  addErrorCell("instrumentFixedIncome", "The total market value should not be zero when cash-flows are provided", columns = "marketValue", rows = consistencyCheck[anyMarketValue == FALSE & anyExposure == TRUE, rowNumber])
  addErrorCell("instrumentFixedIncome", "The total market value should be zero when no cash-flows are provided", columns = "marketValue", rows = consistencyCheck[anyMarketValue == TRUE & anyExposure == FALSE, rowNumber])
  addErrorCell("instrumentFixedIncome", "The spread is required since some cash-flows are negative", columns = "spread", rows = consistencyCheck[anyMissingSpread == TRUE, rowNumber])
  
  instruments <- list("Fixed income" = instrumentFixedIncome[, .(currency, rating, time, exposure = value, spread, marketValue, rowNumber)])
  instruments <- getCompleteColumns(instruments)
  return(instruments)
}


# Process liabilitiy cash-flows
process_instrumentLiability <- function(instrumentLiability){
  instruments <- list("Insurance liabilities" = instrumentLiability[, .(exposure = -sum(value), rowNumber = first(rowNumber)), by = c("currency", "time")])
  instruments <- getCompleteColumns(instruments)
  return(instruments)
}


# Process delta sensitivities
process_instrumentDelta <- function(instrumentDelta, referenceCurrency, mappingDeltaRF){
  instrumentDelta <- copy(instrumentDelta)
  error <- setdiff(mappingDeltaRF[type == "currency", name], instrumentDelta$name)
  addError("instrumentDelta", paste0("The currency risk factor '", error, "' is missing" ), columns = "name", condition = length(error)>0)
  error <- setdiff(mappingDeltaRF[type == "nonCurrency", name], instrumentDelta$name)
  addError("instrumentDelta", paste0("The risk factor '", error, "' is missing" ), columns = "name", condition = length(error)>0)
  
  error <- instrumentDelta[! name %in% mappingDeltaRF$name, .(name, rowNumber)]
  message <- paste0("The element ", error$name, " is not a valid delta risk factor. It needs to be either a (non-currency) risk factor included in the correlation matrix or a currency risk factor of the type 'XXX", referenceCurrency, "', where 'XXX' stands for a valid currency")
  addErrorCell("instrumentDelta", message = message, rows = error$rowNumber, columns = "name")
  
  instrumentDelta <- instrumentDelta[mappingDeltaRF, on = "name", .(name = i.newName, exposure = exposure * i.coef, rowNumber)]
  instruments <- list("Delta terms" = instrumentDelta[exposure != 0])
  instruments <- getCompleteColumns(instruments)
  return(instruments)
}