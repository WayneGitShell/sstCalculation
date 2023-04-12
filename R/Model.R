# Get the model
getModel <- function(modelInput){
  
  # Generate the submodel for mother (and daughters if any)
  model <- lapply(modelInput, getSubModel, numberLoad = 1)
  
  # Adjustment for the credit risk
  allCounterparties <- getAllCounterparties(model)
  for(i in seq_along(model)){
    model[[i]]$Modules$creditMerton$counterparties <- allCounterparties
  }
  
  retClass(model, "sstCalculation")
}

# Computes the FX rate table with respect to the new currency
getNewTargetFXRate <- function(targetCurrency, FXrate){
  backwardRate <- FXrate[from == targetCurrency, .(from = to, to = from, fx = 1/fx)]
  foreignRate <- FXrate[from %in% setdiff(from, targetCurrency), .(from = from, to = targetCurrency, fx = fx * backwardRate$fx)]
  
  newFXRate <- rbind(backwardRate, foreignRate)
  return(newFXRate)
}

# Get the exchange rate risk
getFullFXrate <- function(FXrate){
  
  referenceCurrency <- unique(FXrate$to)
  
  FXrate[, rowNumber := NULL]
  crossFXrate <- rbindlist(lapply(FXrate$from, getNewTargetFXRate, FXrate = FXrate))
  fullFXrate <- rbind(FXrate, crossFXrate)
  
  # Redefine the row number to have unique values
  fullFXrate[, rowNumber := seq_len(.N)]
  
  return(fullFXrate[])
}

# Get the submodel
getSubModel <- function(Objects, numberLoad){
  
  Objects$initialFX <- getFullFXrate(Objects$initialFX)
  
  preProcessedInputs <- preprocessingSubModel(Objects, numberLoad)
  marketCovariance <- getValue(preProcessedInputs, "marketCovariance")
  instrumentsStandalones <- getValue(preProcessedInputs, "instrumentsStandalones")
  progressMarket <- getValue(preProcessedInputs, "progressMarket")
  progressCredit <- getValue(preProcessedInputs, "progressCredit")
  
  
  lifeModule <- getLifeRisks(correlation = getValue(Objects, "correlationLife"),
                             exposureLife = getValue(Objects, "exposureLife")
  )
  
  
  nonlifeModule <- getNonLifeRisks(nonlifeType = getValue(Objects, "nonlifeType"),
                                   nonlifeSimulation = getValue(Objects, "nonlifeSimulation"),
                                   nonlifeMu = getValue(Objects, "nonlifeMu"),
                                   nonlifeSigma = getValue(Objects, "nonlifeSigma"),
                                   nonlifeCDF = getValue(Objects, "nonlifeCDF"),
                                   nonLifeSeedAddon = getValue(Objects, "nonLifeSeedAddon"),
                                   captiveCY_deterministic = getValue(Objects, "captiveCY_deterministic"),
                                   captiveCY_stochastic = getValue(Objects, "captiveCY_stochastic"),
                                   captivePY = getValue(Objects, "captivePY"),
                                   CYrisk = getValue(Objects, "CYrisk"),
                                   PYrisk = getValue(Objects, "PYrisk"),
                                   scenarioNL = getValue(Objects, "scenarioNL")
  )
  
  
  healthModule <- getHealthRisks(correlation = getValue(Objects, "correlationHealth"),
                                 sensitivityHealth = getValue(Objects, "sensitivityHealth"),
                                 scenarioHealth = getValue(Objects, "scenarioHealth")
  )
  
  
  marketInstrumentsModule <- getMarketInstrumentsModule(instrumentsStandalones, marketCovariance, progressMarket)
  
  marketParticipationModule <- getMarketParticipationModule(participationValue = getValue(Objects, "participationValue"),
                                                            participationVolatility = getValue(Objects, "param_participationVolatility")
  )
  
  creditBaselModule <- getCreditBaselRisks(creditRiskExposure = getValue(Objects, "creditRiskExposure"),
                                           creditRiskExposureReinsurance = getValue(Objects, "creditRiskExposureReinsurance"),
                                           creditRiskExposureHypo = getValue(Objects, "creditRiskExposureHypo"),
                                           param_creditRiskFactor = getValue(Objects, "param_creditRiskFactor")
  )
  
  
  creditMertonModule <- getCreditMertonRisks(spreadCurve = getValue(Objects, "spreadCurve"),
                                             LGDMap = getValue(Objects, "mappingLGD"),
                                             migrationMatrix = getValue(Objects, "migrationMatrix"),
                                             initialInterestRate = getValue(Objects, "initialInterestRate1"),
                                             portfolio = getValue(Objects, "portfolioCreditRisk"),
                                             rho = getValue(Objects, "rho"),
                                             initialFX = getValue(Objects, "initialFX"),
                                             refCurrency = getValue(Objects, "referenceCurrency"),
                                             progress = progressCredit
  )
  
  scenarioModule <- getScenarioRisks(participationValue = getValue(Objects, "participationValue"),
                                     scenarioInput = getValue(Objects, "scenarioSST"),
                                     scenarioMacroEconomic = getValue(Objects, "scenarioMacroEconomic"),
                                     instrumentsStandalones = instrumentsStandalones)
  
  
  
  # All constants
  constantModule <- getConstantModule(expectedFinancialResultTable = getValue(Objects, "expectedFinancialResultTable"),
                                      expectedFinancialResultFactor = getValue(Objects, "expectedFinancialResultFactor"),
                                      expectedInsuranceResult = getValue(Objects, "expectedInsuranceResult"),
                                      RTK_goingConcern = getValue(Objects, "RTK_goingConcern"),
                                      RTK_runOff = getValue(Objects, "RTK_runOff"),
                                      additionalEffectsOnTC = getValue(Objects, "additionalEffectsOnTC")
  )
  
  # Modules to be simulated
  modules <- list(life = lifeModule,
                  nonlife = nonlifeModule,
                  health = healthModule,
                  marketInstruments = marketInstrumentsModule,
                  marketParticipation = marketParticipationModule,
                  creditBasel = creditBaselModule,
                  creditMerton = creditMertonModule,
                  scenario = scenarioModule,
                  constant = constantModule
  )
  
  
  # Dependency structure
  dependencyRisks <- getDependencyRisks(correlation = getValue(Objects, "correlationRiskAggregation"))
  dependencyCredit <- getDependencyCredit(corr = getValue(Objects, "corr"))
  dependencyMarket <- getDependencyMarket()
  
  # Dependency structure to be simulated 
  dependency <- list(risks = dependencyRisks,
                     credit = dependencyCredit,
                     market = dependencyMarket)
  
  modules <- namedObject(modules, "Module from the SST-Model", "Module")
  dependency <- namedObject(dependency, "Dependency structure from the SST-Model", "Dependency")
  
  # Get additional figures
  additionalObjects <- getAdditionalObjects(Objects, lifeModule = lifeModule)
  Objects$MVMbeforeNhmr <- getValue(additionalObjects, "MVMbeforeNhmr")
  Objects$FX_to_CHF <- getValue(additionalObjects, "FX_to_CHF")
  Objects$flooringDeltaRBC <- getValue(additionalObjects, "flooringDeltaRBC")
  Objects$riskAsParticipation <- getValue(additionalObjects, "riskAsParticipation")
  
  Model <- list(Modules = modules,
                Dependency = dependency,
                Objects = Objects
  )
  
  retClass(Model, "sstModel")
}
