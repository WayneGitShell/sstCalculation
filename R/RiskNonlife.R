checkNonLifeConsistency <- function(nonlifeSimulation, nonlifeMu, nonlifeSigma, nonlifeCDF, captiveCY_deterministic, captiveCY_stochastic, captivePY, CYrisk, PYrisk, keywordNL, nonlifeType){
  indic <- list(nonlifesimulation = nrow(nonlifeSimulation) > 0,
                nonlifemu = !is.na(nonlifeMu),
                nonlifesigma = !is.na(nonlifeSigma),
                nonlifecdf = nrow(nonlifeCDF) > 0,
                nonlifecaptiveCY = nrow(captiveCY_deterministic) + nrow(captiveCY_stochastic) > 0,
                nonlifecaptivePY = nrow(captivePY) > 0,
                nonliferiskCY = (CYrisk != 0),
                nonliferiskPY = (PYrisk != 0)
  )
  
  message <- paste0("The input will be ignored as a different non-life type is selected (type selected: '", nonlifeType, "')")
  addError("nonlifeSimulation", message, condition = (keywordNL != "NL_sim") && indic$nonlifesimulation, type = "Warning")
  addError("nonlifeMu", message, condition = (keywordNL != "NL_log") && indic$nonlifemu, type = "Warning")
  addError("nonlifeSigma", message, condition = (keywordNL != "NL_log") && indic$nonlifesigma, type = "Warning")
  addError("nonlifeCDF", message, condition = (keywordNL != "NL_cdf") && indic$nonlifecdf, type = "Warning")
  addError("captiveCY", message, condition = (keywordNL != "NL_capt") && indic$nonlifecaptiveCY, type = "Warning")
  addError("captivePY", message, condition = (keywordNL != "NL_capt") && indic$nonlifecaptivePY, type = "Warning")
  
  # The value "no nonlife risk" is hard coded but it is very unlikely to change in the future
  message <- paste0("The input to simulate the non-life risk is missing. Please ensure that the input for the risk type '", nonlifeType, "' is provided or set the risk type to 'no nonlife risk' if no non-life risk should be simulated")
  addError("nonlifeSimulation", message, condition = (keywordNL == "NL_sim") && !indic$nonlifesimulation)
  addError("nonlifeMu", message, condition = (keywordNL == "NL_log") && !indic$nonlifemu)
  addError("nonlifeSigma", message, condition = (keywordNL == "NL_log") && !indic$nonlifesigma)
  addError("nonlifeCDF", message, condition = (keywordNL == "NL_cdf") && !indic$nonlifecdf)
  addError("captiveCY", message, condition = (keywordNL == "NL_capt") && !indic$nonlifecaptiveCY && !indic$nonlifecaptivePY)
  
  message <- "The input will be ignored since the non-life risk is computed by the captive model"
  addError("CYrisk", message, condition = (keywordNL == "NL_capt") && indic$nonliferiskCY, type = "Warning")
  addError("PYrisk", message, condition = (keywordNL == "NL_capt") && indic$nonliferiskPY, type = "Warning")
}

createNL_sim <- function(nonlifeSimulation, CYrisk, PYrisk){
  param <- list(simulation = nonlifeSimulation$simulation, CYrisk = CYrisk, PYrisk = PYrisk)
  param <- retClass(param, "NL_sim")
}

createNL_log <- function(nonlifeMu, nonlifeSigma, CYrisk, PYrisk){
  param <- list(mu = nonlifeMu, sigma = nonlifeSigma, CYrisk = CYrisk, PYrisk = PYrisk)
  param <- retClass(param, "NL_log")
}

createNL_cdf <- function(nonlifeCDF, CYrisk, PYrisk){
  param <- list(cdf = nonlifeCDF, CYrisk = CYrisk, PYrisk = PYrisk)
  param <- retClass(param, "NL_cdf")
}

createNL_none <- function(CYrisk, PYrisk){
  param <- list(CYrisk = CYrisk, PYrisk = PYrisk)
  param <- retClass(param, "NL_none")
}

createNL_cap <- function(captiveCY_stochastic, captiveCY_deterministic, captivePY){
  captiveCY_stochastic <- copy(captiveCY_stochastic)
  captivePY <- copy(captivePY)
  
  # Normal losses
  row <- captiveCY_stochastic[allConsistent(normalLossNumber, normalLossSize, normalLossSigma) == FALSE, rowNumber]
  addErrorCell("captiveCY_stochastic", "Normal loss parameters should be either all provided or all left empty.", columns = "normalLossNumber", rows = row)
  
  # Large losses
  row <- captiveCY_stochastic[allConsistent(largeLossNumber, largeLossThreshold, largeLossShape) == FALSE, rowNumber]
  addErrorCell("captiveCY_stochastic", "Large loss parameters should be either all provided or all left empty.", columns = "normalLossNumber", rows = row)
  
  # No losses
  row <- captiveCY_stochastic[is.na(normalLossNumber) & is.na(largeLossNumber), rowNumber]
  addErrorCell("captiveCY_stochastic", "Please provide at least the normal loss parameters or the large loss parameters.", columns = "normalLossNumber", rows = row)
  
  # Maximum possible loss
  row <- captiveCY_deterministic[maximumPossibleLoss < expectedLoss, rowNumber]
  addErrorCell("captiveCY_deterministic", "The maximum possible loss should be larger than the expected loss", columns = "maximumPossibleLoss", rows = row)
  
  captiveCY_stochastic[, ':=' (normalLossScale = normalLossSigma^2/normalLossSize, normalLossShape = normalLossSize^2/normalLossSigma^2)]
  captiveCY_stochastic[is.na(AAL), AAL := Inf]
  captiveCY_stochastic[is.na(EEL), EEL := Inf]
  captiveCY_stochastic[is.na(QS), QS := 1]
  
  captivePY[, sd := sqrt(log(1+cov^2))]
  captivePY[, mu := log(reserve)-0.5*sd^2]
  captivePY[, ':=' (reserve = NULL, cov = NULL)]
  
  CY <- list(stochastic = captiveCY_stochastic, deterministic = captiveCY_deterministic[, sum(maximumPossibleLoss - expectedLoss)])
  CY <- retClass(CY, "NL_captCY")
  
  PY <- list(stochastic = captivePY)
  PY <- retClass(PY, "NL_captPY")
  
  param <- list(CY = CY, PY = PY)
  param <- retClass(param, "NL_capt")
}

getKeywordNL <- function(nonlifeType, NL_types){
  keywordNL <- names(NL_types)[nonlifeType == NL_types]
  stopifnot(length(keywordNL) == 1)
  return(keywordNL)
}

# Non-life risks class
getNonLifeRisks <- function(nonlifeType, nonlifeSimulation, nonlifeMu, nonlifeSigma, nonlifeCDF, nonLifeSeedAddon, captiveCY_deterministic, captiveCY_stochastic, captivePY, CYrisk, PYrisk, scenarioNL){
  NL_types <- getParam("NL_types")
  keywordNL <- getKeywordNL(nonlifeType, NL_types)
  scenario <- getCompleteScenariosSet(keyword = "scenarioNL", scenarioNL)
  checkNonLifeConsistency(nonlifeSimulation, nonlifeMu, nonlifeSigma, nonlifeCDF, captiveCY_deterministic, captiveCY_stochastic, captivePY, CYrisk, PYrisk, keywordNL, nonlifeType)
  
  if(keywordNL == "NL_sim"){
    # Simulation vector
    param <- createNL_sim(nonlifeSimulation, CYrisk, PYrisk)
    
  }else if(keywordNL == "NL_log"){
    # Lognormal parameters
    param <- createNL_log(nonlifeMu, nonlifeSigma, CYrisk, PYrisk)
    
  }else if(keywordNL == "NL_cdf"){
    # Distribution function
    param <- createNL_cdf(nonlifeCDF, CYrisk, PYrisk)
    
  }else if(keywordNL == "NL_none"){
    # No risk
    param <- createNL_none(CYrisk, PYrisk)
    
  }else if(keywordNL == "NL_capt"){
    # Make a copy since they will be edited by reference
    param <- createNL_cap(captiveCY_stochastic, captiveCY_deterministic, captivePY)
  }
  
  x <- list(param = param, nonLifeSeedAddon = nonLifeSeedAddon, scenario = scenario)
  retClass(x, "nonlifeModule")
}
