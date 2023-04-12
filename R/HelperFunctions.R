# Transform boolean to a value from {-1, 1}
toNum <- function(x){
  return(x*2-1)
}


# Check if the values are either all na or all not na
allConsistent <- function(x, y, z){
  (is.na(x) & is.na(y) & is.na(z)) | (!is.na(x) & !is.na(y) & !is.na(z))
}


# Increase status of progress bar
addProgress <- function(step, message = NULL){
  # Shiny progress bar (if shiny dashboard is used)
  if(!is.null(shiny::getDefaultReactiveDomain())){
    shiny::incProgress(amount = step, message = message)
  }
  
  # Progress bar in the console (always used)
  pb <- getConfig("pb")
  if(!is.null(pb)){
    newValue <- utils::getTxtProgressBar(pb) + step
    
    # The value is almost always smaller than 1. A larger value may be produced due to very small numerical errors
    if(newValue > 1){
      newValue <- 1
    }
    
    # Update the progress bar
    utils::setTxtProgressBar(packageEnv$pb, newValue)
  }
}


# Create the progress bar
createProgressBar <- function(message, size){
  cat(message, fill = TRUE)
  setConfig("pb", utils::txtProgressBar(style = 3, max = size))
}


# Expected shortfall
expectedShortfall <- function(x, alpha){
  shortfallValue <- - mean(sort(x)[seq_len(length(x) * alpha)])
  return(shortfallValue)
}


# Add additional attributes to the object
namedObject <- function(object = list(), label, type){
  attr(object, "SST_Type") <- type
  attr(object, "SST_Label") <- label
  retClass(object, "namedObject")
}


# Return the same object with an additional class
retClass <- function(x, keyword){
  class(x) <- c(keyword, class(x))
  return(x)
}


#' @export
print.sstModel <- function(x, ...){
  formatHeader(x$Objects$companyName, n = 50)
  cat("Properties:", fill = TRUE)
  formatYesNo("Life risk", any(getValue(x$Modules$life, "volatility") != 0))
  formatYesNo("Non-life risk", length(getValue(x$Modules$nonlife, "param")) != 0)
  formatYesNo("Health risk", any(getValue(x$Modules$health, "volatility") != 0))
  formatYesNo("Market risk", nrow(getValue(x$Modules$marketInstruments, "instruments")) > 0)
  formatYesNo("Scenario risk", x$Modules$scenario$scenario[, any(probability*effect > 0)]) 
  formatYesNo("Is daughter company", !is.na(getValue(x$Objects, "daughterValue"))) 
}


#' @export
print.namedObject <- function(x, ...){
  u <- attributes(x)
  formatHeader(u$SST_Label, n = 50)
  formatList(x, u$SST_Type)
}


# Header formatting for printing SST-Results
formatHeader <- function(x, n){
  headerLine <- paste(rep("-", n), collapse = "")
  cat(headerLine, fill = TRUE)
  cat(paste0(paste(rep(" ", pmax((n - nchar(x))/2, 0)), collapse = ""), x), fill = TRUE)
  cat(headerLine, fill = TRUE)
}


#' @export
print.sstCalculation <- function(x, ...){
  for(i in seq_along(x)){
    print(x[[i]])
  }
}


#' @export
print.sstCalculationVariant <- function(x, ...){
  for(i in seq_along(x)){
    formatHeader(names(x)[i], n = 60)
    print(x[[i]])
    cat("\n", fill = TRUE)
  }
}


# List formatting for printing lists
formatList <- function(x, type){
  n <- length(names(x))
  N <- 6
  for(i in seq_len(pmin(N, n))){
    cat(paste0("- ", type, " '", names(x)[i], "'"), fill = TRUE)
  }
  if(n > N){
    cat(paste0("- (and ", n-N, " more ", tolower(type), ifelse(n-N > 1, "s", ""), ")"))
  }  
}


# Print a bullet item with yes/no indication
formatYesNo <- function(text, condition){
  n <- max(20 - nchar(text), 0)
  cat(paste0("- ", text, ":", paste0(rep(" ", n), collapse = ""), ifelse(condition, "Yes", "No")), fill = TRUE)
}


# Generic message (should not be in ...)
messageNegation <- function(type, message){
  paste0("The value ", ifelse(type == "Error", "should not be ", "is usually not "), message)
}


# Generic message (should be in ...)
messageRequirement <- function(type, message){
  paste0("The value ", ifelse(type == "Error", "should be ", "is usually "), message)
}


# Elementwise sum of a list of data.tables (with identical structure)
sumListDataTable <- function(A){
  B <- A[[1]]
  for(i in seq_along(A)[-1]){
    B <- B + A[[i]]
  }
  return(B)
}


# Specify a value to a global variable
setConfig <- function(object, value){
  packageEnv[[object]] <- value
}


# Check that all elements from a vector are identical
isIdentical <- function(x){
  all(sapply(x, identical, x[[1]]))
}


# Perform a cleanup by resetting the content of the global variable
cleanup <- function(){
  packageEnv$errorLog <- data.table(keyword = character(0), type = character(0), row = integer(0), col = integer(0), sheet = character(0), description = character(0), message = character(0), company = character(0))
  
  packageEnv$param <- list(
    riskTypes = stats::setNames(nm = c("life", "nonlife", "health", "market", "credit", "scenario", "participation")),
    standaloneTypes =                c("equity", "hedge fund", "private equity", "real estate", "currency", "interest rate", "CHF rate", "EUR rate", "GBP rate", "JPY rate", "USD rate", "spread", "other", "additional1", "additional2"),
    standaloneTypesDiversification = c("equity", "hedge fund", "private equity", "real estate", "currency", "interest rate", "spread", "other", "additional1", "additional2", "participation"),
    matchBetweenTemplates = c("configurationVersionId", "correlationMarket", "correlationHealth", "correlationLife", "correlationRiskAggregation", "marketVolatility", "initialFX", "initialInterestRate", "scenarioMacroEconomic", "marketRiskFactorsAsset", "marketRiskFactorsCurrency", "marketRiskFactorsRate", "marketRiskFactorsSpread", "marketRiskFactorsStandalones"),
    notLongShort = c("short", "long"),
    notRating = 1:8,
    nonLifeType = c("no nonlife risk", "simulations", "lognormal parameters", "cumulative distribution function", "captive"),
    notCaptiveBranchType = c("Ground-up loss", "Maximum possible loss"),
    limitNumberError = c(5),
    correlationTolerance = c(0.000001),
    relativeMarketValueTolerance = c(0.0001),
    relativeSpreadTolerance = c(0.000001),
    NL_types = c(NL_none = "no nonlife risk", NL_sim = "simulations", NL_log = "lognormal parameters", NL_cdf = "cumulative distribution function", NL_capt = "captive"),
    lifeQuantile = c(0.995),
    alpha = c(0.01),
    outputValue = c("insurance.all", "market.all", "credit.all", "diversificationMarketInsuranceCredit", "insurance_market_credit.all", "scenarioImpact", "insurance_market_credit_scenario.all", "expectedInsuranceResult", "expectedFinancialResult", "additionalEffectOnTC", "LLPOImpact", "SCR", "MVM", "targetCapital"),
    OutputName = c("Insurance risk", "Market risk", "Credit risk", "Diversification effect", "Insurance, Market & Credit risk", "Scenario", "Insurance, Market, Credit with Scenarios", "Expected insurance result", "Expected financial result", "Additional effect on TC", "Impact of LLPO (only participation model)", "SCR", "MVM", "Target capital"),
    OutputStart = c("0", "1", "1", "0", "1", "0", "1", "0", "1", "1", "0", "1", "1", "0")
  )
  packageEnv$excelStructure <- data.table(name = character(0), col = integer(0), keyword = character(0), description = character(0), sheet = character(0), row = integer(0), isValue = logical(0), company = character(0))
  
  closePb()
}


# Close the progress bar
closePb <- function(){
  pb <- packageEnv$pb
  if(!is.null(pb)){
    close(pb)
  }
  packageEnv$pb <- NULL
}


# Internal function used by FINMA to update the vignette and news file (executed manually)
internalUpdateNews <- function(){
  devtools::build_vignettes()
  roxygen2::roxygenise()
}


# Key results showing the TC decomposition
getKeyResults <- function(values){
  keyResults <- data.table(getParam("OutputName"), getValue(values, getParam("outputValue")))
  referenceCurrency <- getValue(values, "referenceCurrency")
  names(keyResults) <- c("Key Figures", paste0("Value in Mio. ", referenceCurrency))
  return(keyResults)
}


# Compute the expected shortfall for all columns with numeric values
getShortfallValues <- function(simulations, alpha = getParam("alpha")){
  # Ensure that the number of simulations is a multiple of 100. This is necessary because the ES function (for alpha = 1percent) only works with multiple of 100. 
  # Currently, the value of alpha is not intended to be changed in the R code.
  stopifnot(nrow(simulations) %% 100 == 0)
  
  ES <- lapply(simulations[, which(sapply(simulations, is.numeric)), with = FALSE], expectedShortfall, alpha) 
  return(ES)
}


# Get a value from the package environment
getConfig <- function(object){
  return(packageEnv[[object]])
}


# Get an element from a list, and throw an error message when the element does not exist
getValue <- function(Objects, keyword, subLevel = FALSE, objectCategory = "object"){
  force(Objects)
  force(keyword)
  
  stopifnot(is.list(Objects))
  # addError(NA, paste0("Internal error: 'Objects' is not a list"), condition = !is.list(Objects))
  
  if(subLevel){
    valueList <- lapply(Objects, getValue, keyword = keyword)
    return(valueList)
  }
  
  if(length(keyword) > 1){
    valueVector <- sapply(keyword, getValue, Objects = Objects)
    addError(NA, paste0("Internal error: there should be no NA value for the ", objectCategory, " '", keyword, "'"), condition = anyNA(valueVector))
    return(valueVector)
  }
  
  addError(NA, paste0("The ", objectCategory, " '", keyword, "' is missing"), condition = ! keyword %in% names(Objects))
  return(Objects[[keyword]])
}


# Get a value from all sub-elements in a list of list
getSubValue <- function(Objects, keyword){
  
  valueList <- lapply(Objects, getValue, keyword = keyword[1])
  
  if(length(keyword) == 1){
    return(valueList)
  }else{
    return(getSubValue(valueList, keyword[-1]))
  }
}


# In a list of list having always the same objects, we permutate the first list level and the second list level
getPermutate <- function(Objects){
  x <- names(Objects[[1]])
  lapply(stats::setNames(nm = x), getSubValue, Objects = Objects)
}


# Get a value from a list of list
getMultipleValues <- function(ObjectsList, keyword){
  sapply(ObjectsList, getValue, keyword = keyword)
}


# Get parameters associated to a keyword
getParam <- function(keyword){
  Param <- getValue(getConfig("param"), keyword)
  return(Param)
}


# Get the seed for each risk type
getSeed <- function(seed, type){
  set.seed(seed)
  seeds_names <- c("marketInstruments", "marketParticipation", "Life", "NonLife", "Health", "Participation", "Scenario", "creditBasel", "creditMerton", "dependencyRisks", "dependencyCredit", "dependencyMarket", "NonLifeScenario")
  
  # Based on the seed, a pseudo-random (reproducible) seed is generated for each risk type
  seeds_values <- c(seed, sample.int(1e5, length(seeds_names) - 1))
  seeds <- as.list(stats::setNames(seeds_values, seeds_names))
  addError(NA, paste0("Incorrect simulation type: '", type, "'"), condition = !type %in% names(seeds))
  
  return(seeds[[type]])
}


# Fix the seed
setSeed <- function(seed, type, seedAddon = 0){
  if(!is.null(seed)){
    set.seed(getSeed(seed, type) + seedAddon)
  }
}