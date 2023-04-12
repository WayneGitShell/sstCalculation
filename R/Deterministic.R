# FX from the reference currency to CHF, used for the FDS results
getFX_to_CHF <- function(initialFX, referenceCurrency){
  if(referenceCurrency == "CHF"){
    FX_to_CHF <- 1
  }else{
    FX_to_CHF <- initialFX$fx[initialFX$from == referenceCurrency & initialFX$to == "CHF"]
  }
  return(FX_to_CHF)
}


# Get total MVM over al branches (before nhmr factor)
getMVM <- function(MVM_CashFlowsLife, MVM_Health, MVM_NonLife, referenceCurrency, initialInterestRate, volatility, correlationLife, param_costOfCapital){
  # Life MVM needs to be computed
  MVM_Life <- calculateMVMLife(MVM_CashFlowsLife, referenceCurrency, initialInterestRate, volatility, correlationLife, param_costOfCapital)
  
  # Take the sum of the MVM
  MVM <- MVM_Life + MVM_Health + MVM_NonLife
  return(MVM)
}


# Get life MVM (before nhmr factor)
calculateMVMLife <- function(MVM_CashFlowsLife, referenceCurrency, initialInterestRate, volatility, correlationLife, param_costOfCapital, alpha = getParam("alpha")){
  coc <- param_costOfCapital
  scalingFactorNormal <- getScalingFactorNormal(alpha)
  
  # Ensure risk factors consistency
  namesMVM <- unique(MVM_CashFlowsLife$name)
  namesVola <- names(volatility)
  checkNames(namesMVM, namesVola, keyword = "exposureLife", tableName = "market value margin")
  checkNames(namesVola, namesMVM, keyword = "MVM_CashFlowsLife", tableName = "insurance risk sensitivities")
  
  # Ensure data consistency
  MVM_LifeIndicator <- MVM_CashFlowsLife[, .(anyValue = any(value != 0)), by = c("name", "rowNumber")]
  
  if(all(!MVM_LifeIndicator$anyValue) & any(volatility != 0)){
    addError("MVM_CashFlowsLife", "Life risk is modelled but no cash-flows for the MVM were provided. In order to perform the computation, we set the MVM to 0.", type = "Warning")
    return(0)
  }
  
  checkConsistencyCashFlowsMVM(MVM_LifeIndicator, volatility = volatility)
  
  # Remove all empty risk factors
  MVM_CF <- MVM_CashFlowsLife[name %in% names(volatility[volatility != 0])]
  
  # Degenerate case
  if(nrow(MVM_CF) == 0){
    return(0)
  }
  
  initialInterestRate <- initialInterestRate[currency == referenceCurrency]
  initialInterestRate[, discount := exp(-time * value)]
  initialInterestRate <- initialInterestRate[, .(time = c(0, time), discount = c(1, discount))]
  maxTime <- max(initialInterestRate$time)
  lastDiscount <- initialInterestRate[time == maxTime, discount]
  
  # Add discount factors
  MVM_CF[initialInterestRate, discount := i.discount, on = "time"]
  
  # Important to reverse the order (for reverse summations)
  setorder(MVM_CF, -"time")
  
  MVM_CF[, alpha_value := cumsum(value * discount) / (sum(value * discount) * discount), by = "name"]
  MVM_sigma <- MVM_CF[, .(sigma = sqrt(as.numeric(t(alpha_value * volatility[name]) %*% correlationLife[name, name] %*% (alpha_value * volatility[name]) )), discount = first(discount)), by = "time"]
  setorder(MVM_sigma, "time")
  MVM_EK <- MVM_sigma[, .(time = time + 1, EK = sigma * scalingFactorNormal, discount = c(discount[-1], lastDiscount))]
  MVM_Life <- coc * sum(MVM_EK$discount * MVM_EK$EK)
  return(MVM_Life)
}


# All values required for the FDS are stored in a list
calculateValues <- function(ES, Objects, scenarios){
  
  values <- list()
  
  # Constants
  values[["RTK"]] <- getValue(Objects, "RTK_goingConcern")
  
  # Diversification
  values[["diversificationMarketInsuranceCredit"]] <- getValue(ES, "insurance_market_credit.all") - getValue(ES, "market.all") - getValue(ES, "insurance.all") - getValue(ES, "credit.all")
  values[["market.diversification"]] <- getValue(ES, "market.all") - sum(getValue(ES, paste0("market.", getParam("standaloneTypesDiversification"))))
  
  # For the MVM, we take the true market risk value (this is relevant when the risk from the daughter is displayed as a market risk participation)
  values[["MVM"]] <- getValue(Objects, "MVMbeforeNhmr") + getValue(Objects, "param_nhmrFactor") * getValue(ES, "marketunchanged.all")
  
  # Target capital
  values[["SCR"]] <- getValue(ES, "deltaRBC")
  values[["scenarioImpact"]] <- getValue(ES, "insurance_market_credit_scenario.all") - getValue(ES, "insurance_market_credit.all")
  values[["LLPOImpact"]] <- getValue(ES, "insurance_market_credit_scenario_LLPO.all") - getValue(ES, "insurance_market_credit_scenario.all")
  values[["targetCapital"]] <- getValue(values, "SCR") + getValue(values, "MVM")
  
  # Here we extract constants and adjust the sign according the FDS convention
  # Note that the expected shortfall of a constant is equal to the constant
  values[["additionalEffectOnTC"]] <- getValue(ES, "constant.additionalEffects")
  values[["expectedInsuranceResult"]] <- -getValue(ES, "constant.expectedInsuranceResult")
  values[["expectedFinancialResult"]] <- -getValue(ES, "constant.expectedFinancialResult")
  
  # SST Ratio
  values[["SSTRatio"]] <- (getValue(values, "RTK") - getValue(values, "MVM"))/(getValue(values, "targetCapital") - getValue(values, "MVM"))
  
  # Additional outputs for the FDS
  values[["FX_to_CHF"]] <- getValue(Objects, "FX_to_CHF")
  values[["referenceCurrency"]] <- getValue(Objects, "referenceCurrency")
  
  # Remove unnecessary fields
  ES$simulation_id <- NULL
  ES$marketunchanged.all <- NULL
  
  values <- c(values, ES, scenarios)
  return(values)
}