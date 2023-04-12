# Complete the set of scenarios with a scenario with impact 0
getCompleteScenariosSet <- function(keyword, scenario){
  addError(keyword, "The total probability of the scenarios should be strictly smaller than 100%", condition = sum(scenario$probability) >= 1)
  additionalScenario <- data.table(label = "none", effect = 0, probability = 1 - sum(scenario$probability))
  scenario <- scenario[, .(label, effect, probability)]
  scenario <- rbind(scenario, additionalScenario)
  return(scenario)
}


# Get and process macroeconomic scenarios
getMacroScenarios <- function(scenarioMacroEconomic, participationValue, instrumentsStandalones){
  RF <- scenarioMacroEconomic$name
  unit <- scenarioMacroEconomic$unit
  
  macroRF <- as.matrix(scenarioMacroEconomic[, -c("name", "unit", "rowNumber")])
  SZ <- colnames(macroRF)
  
  macroRF[unit == "bp",] <- macroRF[unit == "bp",] / 1e4
  macroRF[unit == "%",] <- log(1 + macroRF[unit == "%",])
  macroRF <- data.table(t(macroRF))
  names(macroRF) <- RF
  macroRF[, name := SZ]
  
  scenarioMacro <- calculateStandalone(standaloneType = "all", RF = macroRF, instruments = instrumentsStandalones, centered = FALSE, participationValue = participationValue)
  scenarioMacro <- data.table(label = names(scenarioMacro), effect = scenarioMacro, probability = rep(0, length(scenarioMacro)))
  return(scenarioMacro) 
}


# Get scenario risks
getScenarioRisks <- function(participationValue, scenarioInput, scenarioMacroEconomic, instrumentsStandalones){
  
  scenarioSST <- getCompleteScenariosSet(keyword = "scenarioSST", scenarioInput)
  scenarioMacro <- getMacroScenarios(scenarioMacroEconomic, participationValue, instrumentsStandalones)
  
  # Add the effect of the macroeconomic scenarios to the scenarios if they have the same label
  addError("scenarioSST", "Macro economic scenarios do not share any common label with the scenarios", condition = length(intersect(scenarioSST$label, scenarioMacro$label)) == 0, type = "Warning")
  scenario <- rbind(scenarioSST, scenarioMacro)
  scenario <- scenario[, .(effect = sum(effect), probability = sum(probability)), by = "label"]
  
  x <- list(scenario = scenario)
  retClass(x, "scenarioModule")
}