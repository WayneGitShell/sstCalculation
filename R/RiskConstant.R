# Get all constants used for the target capital
getConstantModule <- function(expectedFinancialResultTable, expectedFinancialResultFactor, expectedInsuranceResult, RTK_goingConcern, RTK_runOff, additionalEffectsOnTC){
  
  # Expected financial results
  expectedFinancialResult <- expectedFinancialResultTable[, sum(return*exposure)] * expectedFinancialResultFactor
  
  # Additional effects on the target capital including delta RBC
  additionalEffects <-  RTK_goingConcern - RTK_runOff + additionalEffectsOnTC
  
  x <- list(expectedFinancialResult = expectedFinancialResult,
            expectedInsuranceResult = expectedInsuranceResult,
            additionalEffects = additionalEffects)
  retClass(x, "constantModule")
}