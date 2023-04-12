
# Check that the mother and daughter template inputs are consistent
checkStandaloneObjectsList <- function(ObjectsList){
  companyName <- getMultipleValues(ObjectsList, "companyName")
  daughterValue <- getMultipleValues(ObjectsList, "daughterValue")
  daughterScaling <- getMultipleValues(ObjectsList, "daughterScaling")
  daughterApplyLLPO <- getMultipleValues(ObjectsList, "daughterApplyLLPO")
  riskAsParticipation <- getMultipleValues(ObjectsList, "riskAsParticipation")
  
  if(length(ObjectsList) == 1){
    # The last two fields are either true or false. An empty cell is interpreted as false.
    message <- "This cell should be left empty unless you are using the standard model for participations."
    addError("daughterValue", message, condition = !is.na(daughterValue))
    addError("daughterScaling", message, condition = !is.na(daughterScaling))
    addError("daughterApplyLLPO", message, condition = daughterApplyLLPO)
    addError("riskAsParticipation", message, condition = riskAsParticipation)
  }else{
    # Consitency template names
    addError("companyName", paste0("Please use distinct company names. The company name '", companyName[duplicated(companyName)], "' was provided twice"), condition = any(duplicated(companyName)))
    
    # Identify the name of mother/daughter templates
    motherNames <- companyName[is.na(daughterValue)]
    daughterNames <- companyName[!is.na(daughterValue)]
    
    # Information about the mother/daughter identified
    addError(NA, paste0("Mother company identified: '", motherNames, "'"), type = "Remark", condition = (length(motherNames) > 0) & (length(daughterNames) > 0))
    addError(NA, paste0("Daughter company identified: '", daughterNames, "'"), type = "Remark", condition = length(daughterNames) > 0)
    
    # There should be exactly one mother template
    addError("daughterValue", "No mother company was found", condition = length(motherNames) == 0)
    addError("daughterValue", "Multiple mother companies were found", condition = length(motherNames) > 1)
    
    # Check consistency for daughter scaling
    errorCompanyConsistency <- companyName[is.na(daughterValue) != is.na(daughterScaling)]
    addError("daughterScaling", paste0("The scaling factor needs to be provided if and only if a daughter value has been provided (Template '", errorCompanyConsistency, "')"), condition = length(errorCompanyConsistency)>0)
    
    # LLPO is not relevant for the mother company
    errorCompanyConsistency <- companyName[is.na(daughterValue) & daughterApplyLLPO]
    addError("daughterScaling", paste0("This cell should be left empty for the mother company (Template '", errorCompanyConsistency, "')"), condition = length(errorCompanyConsistency)>0)
    
    # riskAsParticipation is not relevant for the mother company
    errorCompanyConsistency <- companyName[is.na(daughterValue) & riskAsParticipation]
    addError("riskAsParticipation", paste0("This cell should be left empty for the mother company (Template '", errorCompanyConsistency, "')"), condition = length(errorCompanyConsistency)>0)
  }
}


getProcessedObjectsAllTemplates <- function(ObjectsList){
  
  companyName <- getMultipleValues(ObjectsList, "companyName")
  daughterValue <- getMultipleValues(ObjectsList, "daughterValue")
  
  # Up to now, the order of the companies is determined by the order in which the template have been loaded
  # We need to avoid this 'randomness' to make it possible to reproduce the simulation results, even if the templates were loaded in a different order
  # To achieve this, we enforce a specific order (mother first, daughter sorted by participation value)
  names(ObjectsList) <- companyName
  ObjectsList <- ObjectsList[order(daughterValue, na.last = FALSE)]
  
  # Check the consistency of the participation inputs
  checkStandaloneObjectsList(ObjectsList)
  
  # Apply scaling
  ObjectsListScaling <- lapply(ObjectsList, calculateScaledObjects)
  
  # Independence assumption for NL-simulations
  for(i in seq_along(ObjectsListScaling)){
    ObjectsListScaling[[i]]$nonLifeSeedAddon <- (i-1)*1000
  }
  
  # Ensure that the parameters from mother and daughter companies match
  for(keyword in getParam("matchBetweenTemplates")){
    checkConcistencyDaughters(objectList = getValue(ObjectsList, keyword, subLevel = TRUE), keyword)
  }
  
  # Ensure that the scenarios have the same label and probabilities
  for(keyword in c("scenarioSST", "scenarioHealth", "scenarioNL")){
    objectList <- getValue(ObjectsList, keyword, subLevel = TRUE)
    for(column in c("label", "probability")){
      columnList <- getValue(objectList, column, subLevel = TRUE)
      checkConcistencyDaughters(objectList = columnList, keyword = keyword, content = column)
    }
  }
  
  return(ObjectsListScaling)
}


# Ensure that daughter companies have the same parameters as the mother companies
checkConcistencyDaughters <- function(objectList, keyword, content = "table", type = "Error"){
  match <- sapply(objectList[-1], identical, y = objectList[[1]])
  errorTemplate <- names(match[!match])
  
  addError(keyword, paste0("The ", content, " from the daughter '", errorTemplate, "' does not coincide with the ", content, " from the mother template"), condition = length(errorTemplate) > 0, type = type)
}


# Applyt the scaling to the daughter company
calculateScaledObjects <- function(Objects){
  scaling <- getValue(Objects, "daughterScaling")
  CYrisk <- getValue(Objects, "CYrisk")
  PYrisk <- getValue(Objects, "PYrisk")
  captiveCY <- getValue(Objects, "captiveCY")
  captivePY <- getValue(Objects, "captivePY")
  additionalEffectsOnTC <- getValue(Objects, "additionalEffectsOnTC")
  RTK_goingConcern <- getValue(Objects, "RTK_goingConcern")
  
  
  if(!is.na(scaling)){
    Objects$instrumentAsset[, value := value * scaling]
    Objects$instrumentFixedIncome[, value := value * scaling]
    Objects$instrumentFixedIncome[, marketValue := marketValue * scaling]
    Objects$instrumentLiability[, value := value * scaling]
    Objects$instrumentAssetForward[, exposure := exposure * scaling]
    Objects$instrumentFX_Forward[, nominal := nominal * scaling]
    Objects$instrumentDelta[, exposure := exposure * scaling]
    Objects$participationValue <- getValue(Objects, "participationValue") * scaling
    
    # Scale expected result
    Objects$expectedFinancialResultTable[, exposure := exposure * scaling]
    Objects$expectedInsuranceResult <- getValue(Objects, "expectedInsuranceResult") * scaling
    
    # Credit risk
    for(j in c("marketValue", paste0("Y", 1:50))){
      set(Objects$portfolioCreditRisk, j = j, value = Objects$portfolioCreditRisk[[j]] * scaling)
    }
    Objects$creditRiskExposure[, weightedExposure := weightedExposure * scaling]
    Objects$creditRiskExposureReinsurance[, weightedExposure := weightedExposure * scaling]
    Objects$creditRiskExposureHypo[, weightedExposure := weightedExposure * scaling]
    
    # Scale scenarios
    Objects$scenarioSST[, effect := effect * scaling]
    
    # Scale non-life
    Objects$nonlifeMu <- Objects$nonlifeMu + log(scaling)
    Objects$nonlifeSimulation[, simulation := simulation * scaling]
    Objects$nonlifeCDF[, x := x * scaling]
    Objects$scenarioNL[, effect := effect * scaling]
    
    # The captive model is not compatbile with the participation model
    addError("captiveCY", "The captive model cannot be used for daughter companies", condition = nrow(captiveCY)+nrow(captivePY) > 0)
    
    # The additional effects on TC, CYRisk and PYRisk should only be provided by the mother company
    message <- "This value should should be empty for daughter companies and will be ignored"
    addError("CYrisk", message, condition = CYrisk != 0, type = "Warning")
    addError("PYrisk", message, condition = PYrisk != 0, type = "Warning")
    addError("additionalEffectsOnTC", message, condition = additionalEffectsOnTC != 0, type = "Warning")
    Objects$CYrisk <- 0
    Objects$PYrisk <- 0
    Objects$additionalEffectsOnTC <- 0
    
    # No effects from Delta RTK
    Objects$RTK_runOff <- RTK_goingConcern
    
    # Scale life
    Objects$exposureLife <- getValue(Objects, "exposureLife") * scaling
    
    # Scale health
    Objects$sensitivityHealth <- getValue(Objects, "sensitivityHealth") * scaling
    Objects$scenarioHealth[, effect := effect * scaling]
  }
  return(Objects)
}
