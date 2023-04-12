context("Deterministic")

initialIinterest <- data.table(currency = c("CHF"),
                               rowNumber = 6:8,
                               time = 1:3,
                               value = 0
)


test_that("simulate.constantModule", {
  # Classical case
  constantModule <- getConstantModule(expectedFinancialResultTable, expectedFinancialResultFactor = 0.8, expectedInsuranceResult = 5.5, RTK_goingConcern = 55, RTK_runOff = 50, additionalEffectsOnTC = 3)
  x <- simulate(constantModule, 4, 1)
  y <- list(sim = data.table(all = rep(2.3, 4), expectedFinancialResult = 4.8, expectedInsuranceResult = 5.5, additionalEffects = -8), RF = NULL)
  # Not identical due to numerical error
  expect_equal(x, y)

})

test_that("getConstantModule", {
  # Classical case
  x <- getConstantModule(expectedFinancialResultTable, expectedFinancialResultFactor = 0.8, expectedInsuranceResult = 5.5, RTK_goingConcern = 55, RTK_runOff = 50, additionalEffectsOnTC = 3)
  y <- retClass(list(expectedFinancialResult = 4.8, expectedInsuranceResult = 5.5, additionalEffects = 8), "constantModule")
  # Not identical, just equal due to numerical error
  expect_equal(x, y)

  # Empty financial result table
  x <- getConstantModule(expectedFinancialResultTable[integer(0)], expectedFinancialResultFactor = 0.8, expectedInsuranceResult = 5.5, RTK_goingConcern = 55, RTK_runOff = 50, additionalEffectsOnTC = 3)$expectedFinancialResult
  expect_identical(x, 0)

})

test_that("getFX_to_CHF", {
  initialFX <- data.table(from = c("EUR", "USD"), to = c("CHF", "JPY"), fx = c(1.2, 2.2))
  # Currency change
  expect_identical(getFX_to_CHF(initialFX, "EUR"), 1.2)

  # Same currency
  expect_identical(getFX_to_CHF(initialFX, "CHF"), 1)
})


test_that("calculateMVMLife", {
  scalingFactorNormal <- 0.5
  mvm_cf1 <- data.table(name = "Mortality",
                        rowNumber = 7,
                        time = c(0:2),
                        value = 1
  )
  mvm_cf2 <- data.table(name = "Longevity",
                        rowNumber = 8,
                        time = c(0:2),
                        value = 2
  )
  MVM_CFLife <- MVM_CashFlowsLife <- rbindlist(list(mvm_cf1, mvm_cf2))
  refCurrency <- "CHF"
  volatility <- c(Mortality = 1, Longevity = 1)
  corrLife <- matrix(c(1,0,0,1), ncol=2)
  rownames(corrLife) <- c("Mortality", "Longevity")
  colnames(corrLife) <- c("Mortality", "Longevity")
  coc <- 0.5 # cost of capital

  initialIR <- initialIinterest[currency == refCurrency]
  initialIR[, discount := exp(-time * value)]
  initialIR <- initialIR[, .(time = c(0, time), discount = c(1, discount))]
  lastDiscount <- initialIR[time == 3, discount]

  MVM_CFLife[initialIR, discount := i.discount, on = "time"]
  setorder(MVM_CFLife, -"time")
  MVM_CFLife[, alpha_value := cumsum(value * discount) / (sum(value * discount) * discount), by = "name"]
  MVM_sigma <- MVM_CFLife[, .(sigma = sqrt(as.numeric(t(alpha_value * volatility[name]) %*% corrLife[name, name] %*% (alpha_value * volatility[name]) )), discount = first(discount)), by = "time"]
  setorder(MVM_sigma, "time")
  MVM_EK <- MVM_sigma[, .(time = time + 1, EK = sigma * scalingFactorNormal, discount = c(discount[-1], lastDiscount))]
  MVM_Life <- coc * sum(MVM_EK$discount * MVM_EK$EK)

  y <- calculateMVMLife(MVM_CashFlowsLife, refCurrency, initialIinterest, volatility, corrLife, coc, alpha = 0.6977404152)
  expect_equal(MVM_Life, y, tolerance=1e-10)
})


test_that("calculateValues", {
  ES <- list(simulation_id = 100,
    insurance_market_credit.all = 10,
             insurance_market_credit_scenario.all = 11,
             insurance_market_credit_scenario_LLPO.all = 10.5,
             market.all = 3,
             insurance.all = 3,
             credit.all = 5,
             marketunchanged.all = 4,
             deltaRBC = 2,
             constant.additionalEffects = 100,
             constant.expectedInsuranceResult = 101,
             constant.expectedFinancialResult = 102
  )

  # Add standalones
  standaloneTypesDiversification <- getParam("standaloneTypesDiversification")
  ES <- c(ES, as.list(setNames(rep(1, length(standaloneTypesDiversification)), nm = paste0("market.", standaloneTypesDiversification))))

  Objects <- list(RTK_goingConcern = 200,
                  MVMbeforeNhmr = 10,
                  param_nhmrFactor = 0.5,
                  FX_to_CHF = 1.2,
                  referenceCurrency = "USD"
  )

  scenarios <- list("S1.1" = 3, "S3.3")

  # Calculated
  x <- calculateValues(ES, Objects, scenarios)

  # Expected
  values <- list(RTK = 200, diversificationMarketInsuranceCredit = -1, market.diversification = -8, MVM = 12, SCR = 2, scenarioImpact = 1,
       LLPOImpact = -0.5, targetCapital = 14, additionalEffectOnTC = 100, expectedInsuranceResult = -101, expectedFinancialResult = -102,
       SSTRatio = 94, FX_to_CHF = 1.2, referenceCurrency = "USD")

  # Remove the two unnecessary fields
  ES_reduced <- ES[!names(ES) %in% c("simulation_id", "marketunchanged.all")]
  y <- c(values, ES_reduced, scenarios)

  expect_identical(x, y)
})