getAdditionalObjects <- function(Objects, lifeModule){
  # MVM before nhmr effect
  MVMbeforeNhmr <- getMVM(MVM_CashFlowsLife = getValue(Objects, "MVM_CashFlowsLife"),
                          MVM_Health = getValue(Objects, "MVM_Health"),
                          MVM_NonLife = getValue(Objects, "MVM_NonLife"),
                          referenceCurrency = getValue(Objects, "referenceCurrency"),
                          initialInterestRate = getValue(Objects, "initialInterestRate"),
                          volatility = getValue(lifeModule, "volatility"),
                          correlationLife = getValue(Objects, "correlationLife"),
                          param_costOfCapital = getValue(Objects, "param_costOfCapital")
  )
  
  # Exchange rate to CHF
  FX_to_CHF <- getFX_to_CHF(initialFX = getValue(Objects, "initialFX"),
                            referenceCurrency = getValue(Objects, "referenceCurrency")
  )
  
  # Threshold used to floor the deltaRBC from the daughter company
  flooringDeltaRBC <- ifelse(getValue(Objects, "daughterApplyLLPO"), -getValue(Objects, "daughterValue"), -Inf)
  
  # Risk as participation indicator
  riskAsParticipation <- getValue(Objects, "riskAsParticipation")
  
  x <- list(MVMbeforeNhmr = MVMbeforeNhmr, FX_to_CHF = FX_to_CHF, flooringDeltaRBC = flooringDeltaRBC, riskAsParticipation = riskAsParticipation)  
  return(x)
}


# Pre processing before model creation
preprocessingSubModel <- function(Objects, numberLoad){
  # Check that the risk factors are defined in a consistent way
  checkConsistencyRF(correlationMarket = getValue(Objects, "correlationMarket"),
                     marketVolatility = getValue(Objects, "marketVolatility"),
                     scenarioMacroEconomic = getValue(Objects, "scenarioMacroEconomic"))
  
  # Get target currency
  targetCurrency <- getTargetCurrency(marketRiskFactorsCurrency = getValue(Objects, "marketRiskFactorsCurrency"))
  
  marketCovariance <- getMarketCovariance(correlation = getValue(Objects, "correlationMarket"),
                                          volatility = getValue(Objects, "marketVolatility")
  )
  
  # Maturities are associated with the interest instruments
  RateWithTime <- getRateWithTime(marketRiskFactorsRate = getValue(Objects, "marketRiskFactorsRate"),
                                  maturitiesInterestRate = getValue(Objects, "maturitiesInterestRate"))
  
  # Mapping used for the currency change when the target currency is not CHF 
  mappingDeltaRF <- getCurrencyChangeMapping(Currency = getValue(Objects, "marketRiskFactorsCurrency"),
                                             Delta = getValue(Objects, "marketRiskFactorsDelta"),
                                             refCurrency = getValue(Objects, "referenceCurrency"),
                                             riskFactors = rownames(getValue(Objects, "correlationMarket")),
                                             targetCurrency = targetCurrency
  )
  
  # All instruments, before any adjustment
  rawInstruments <- getRawInstruments(instrumentAsset = getValue(Objects, "instrumentAsset"),
                                      instrumentAssetForward = getValue(Objects, "instrumentAssetForward"),
                                      instrumentFX_Forward = getValue(Objects, "instrumentFX_Forward"),
                                      instrumentFixedIncome = getValue(Objects, "instrumentFixedIncome"),
                                      instrumentLiability = getValue(Objects, "instrumentLiability"),
                                      instrumentDelta = getValue(Objects, "instrumentDelta"),
                                      referenceCurrency = getValue(Objects, "referenceCurrency"),
                                      mappingDeltaRF = mappingDeltaRF
  )
  
  # Check instruments
  checkInstruments(marketRiskFactorsAsset = getValue(Objects, "marketRiskFactorsAsset"),
                   marketRiskFactorsSpread = getValue(Objects, "marketRiskFactorsSpread"),
                   marketRiskFactorsCurrency = getValue(Objects, "marketRiskFactorsCurrency"),
                   rawInstruments = rawInstruments,
                   targetCurrency = targetCurrency,
                   RateWithTime = RateWithTime
  )
  
  # Apply fx change and discounting
  instruments <- getInstruments(refCurrency = getValue(Objects, "referenceCurrency"),
                                initialRate = getValue(Objects, "initialInterestRate"),
                                initialFX = getValue(Objects, "initialFX"),
                                rawInstruments = rawInstruments
  )
  
  # Associate each instrument to a risk factor
  instrumentsMapping <- getInstrumentsMapping(marketRiskFactorsAsset = getValue(Objects, "marketRiskFactorsAsset"),
                                              marketRiskFactorsSpread = getValue(Objects, "marketRiskFactorsSpread"),
                                              marketRiskFactorsCurrency = getValue(Objects, "marketRiskFactorsCurrency"),
                                              marketRiskFactorsDelta = getValue(Objects, "marketRiskFactorsDelta"),
                                              referenceCurrency = getValue(Objects, "referenceCurrency"),
                                              instruments = instruments,
                                              targetCurrency = targetCurrency,
                                              RateWithTime = RateWithTime
  )
  
  # A list of instruments is associated to each standalone category. This is used for the computation of standalone risk.
  instrumentsStandalones <- getInstrumentsStandalones(marketRiskFactorsStandalones = getValue(Objects, "marketRiskFactorsStandalones"),
                                                      instruments = instruments,
                                                      instrumentsMapping = instrumentsMapping,
                                                      marketCovariance = marketCovariance
  )
  
  # Total progress allocated to the SST-Template (there may be multiple templates for the participation model) and the variant (there may be multiple variants).
  totalProgressStep <- 1 / numberLoad
  progressMarket <- totalProgressStep / 2
  progressCredit <- totalProgressStep / 2
  
  x <- list(marketCovariance = marketCovariance, instrumentsStandalones = instrumentsStandalones, progressMarket = progressMarket, progressCredit = progressCredit)
  return(x)
}
