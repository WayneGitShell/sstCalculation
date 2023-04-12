context("Get")

### write for ?
# getMarketParticipationModule
# getMarketInstrumentsModule

test_that("getTargetCurrency", {
  marketRiskFactorsCurrency <- data.table(currency = c("EUR", "USD", "GBP", "JPY", "CHF"), targetCurrency = c("CHF", "CHF", "CHF", "USD", "CHF"), rowNumber = 1:5)
  # Classical case
  expect_identical(getTargetCurrency(marketRiskFactorsCurrency[1:3]), "CHF")
  expect_identical(getTargetCurrency(marketRiskFactorsCurrency[4]), "USD")
  
  # Incorrect case
  expect_error(getTargetCurrency(marketRiskFactorsCurrency[2:4]), "There should be exactly one target currency")
  expect_error(getTargetCurrency(marketRiskFactorsCurrency[c(1,2,5)]), "The currency should not be identical to the target currency")
})

# For 'getTable' checks
Template <- list(A = matrix(c(NA, NA, NA, NA, "A", "B", NA, "C", "D", NA, NA, "E", "F", "G", NA), nrow = 3))

test_that("getTable (table)",{

  # Classical case
  configTable <- list(value = list(sheet = "A", row = 2, col = 2:3), columns = list(Name = c("x", "y")), indicator = list(isValue = FALSE, isCorrelation = FALSE), keyword = "test")
  result <- data.table(x = c("A", "B"), y = c("C", "D"), rowNumber = 2:3)
  expect_identical(getTable(configTable, Template), result)

  # One column only
  configTable <- list(value = list(sheet = "A", row = 1, col = 5), columns = list(Name = c("x")), indicator = list(isValue = FALSE, isCorrelation = FALSE), keyword = "test")
  result <- data.table(x = c("F", "G", NA), rowNumber = 1:3)
  expect_identical(getTable(configTable, Template), result)
  
  # Correlation matrix classical case (ensure NA value in first column is ignored)
  configTable <- list(value = list(sheet = "A", row = 3, col = 1:10), columns = list(Name = LETTERS[1:10]), indicator = list(isValue = FALSE, isCorrelation = FALSE), keyword = "correlationMarket")
  result <- data.table(A = NA_character_, B = "B", C = "D", D = "E", E = NA_character_, F = NA_character_, G = NA_character_, H = NA_character_, I = NA_character_, J = NA_character_, rowNumber = 3L)
  expect_identical(getTable(configTable, Template), result)

  # Correlation matrix
  configTable <- list(value = list(sheet = "A", row = 3, col = 2:10), columns = list(Name = LETTERS[2:10]), indicator = list(isValue = FALSE, isCorrelation = FALSE), keyword = "correlationMarket")
  result <- data.table(B = "B", C = "D", D = "E", E = NA_character_, F = NA_character_, G = NA_character_, H = NA_character_, I = NA_character_, J = NA_character_, rowNumber = 3L)
  expect_identical(getTable(configTable, Template), result)

  # Correlation matrix (degenerate case)
  configTable <- list(value = list(sheet = "A", row = 3, col = 3:10), columns = list(Name = LETTERS[3:10]), indicator = list(isValue = FALSE, isCorrelation = FALSE), keyword = "correlationMarket")
  result <- data.table(C = "D", D = "E", E = NA_character_, F = NA_character_, G = NA_character_, H = NA_character_, I = NA_character_, J = NA_character_  ,rowNumber = 3L)
  expect_identical(getTable(configTable, Template), result)
})


test_that("getTable (value)", {
  # One value
  configTable <- list(value = list(sheet = "A", row = 3, col = 4), columns = list(Name = c("x")), indicator = list(isValue = TRUE, isCorrelation = FALSE), keyword = "test")
  result <- data.table(x = "E", rowNumber = 1)
  expect_identical(getTable(configTable, Template), result)
  
  # One value outside of range
  configTable <- list(value = list(sheet = "A", row = 30, col = 4), columns = list(Name = c("x")), indicator = list(isValue = TRUE, isCorrelation = FALSE), keyword = "test")
  result <- data.table(x = NA_character_, rowNumber = 1)
  expect_identical(getTable(configTable, Template), result)
  
})

test_that("getParam",{
  cleanup()
  expect_identical(getParam("riskTypes"), packageEnv$param$riskTypes)
  expect_identical(getParam("alpha"), 0.01)
  expect_error(getParam("xxxx"), "The object 'xxxx' is missing")
})

test_that("getDependency",{
  correlation <- matrix(c(1, 0.1, 0.1, 1), ncol = 2)
  
  x <- list(correlation = correlation, labelIndependentComponents = character(0))
  attr(x, "class") <- c("Reordering", "list")
  expect_identical(getDependency(correlation), x)
  
  x$labelIndependentComponents <- TRUE
  expect_identical(getDependency(correlation, TRUE), x)
})

test_that("getMarketCovariance", {
  correlation <- matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  volatility <- c(a=2, b= 5)
  expect_identical(unname(getMarketCovariance(correlation, volatility)), diag(volatility) %*% correlation %*% diag(volatility))
})

test_that("sumListDataTable", {
  A <- list(X = data.table(a = 1:10, b = 1:5),
            Y = data.table(a = 2:11, b = 1:5),
            Z = data.table(a = 3:12, b = 1:5))
  B <- data.table(a = as.integer((1:10)*3 + 3), b = as.integer((1:5)*3))
  expect_identical(sumListDataTable(A), B)
})

test_that("getLifeRisks", {
  correlation <- matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  exposureLife <- c(a=2, b= 5)
  result <- getLifeRisks(correlation, exposureLife)
  expect_identical(result$correlation, correlation)
  expect_identical(result$volatility, exposureLife/stats::qnorm(0.995))
})

test_that("getHealthRisks", {
  correlation <- matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames = list(c("a", "b"), c("a", "b")))
  sensitivityHealth <- c(a=2, b=5)
  
  scenarioHealth <- data.table(label = c("A", "B", "none"), effect = c(1,3, 0), probability = c(0.1, 0.2, 0.7))
  expect_identical(getCompleteScenariosSet(keyword = NA, scenarioHealth[-3]), scenarioHealth)
  
  result <- getHealthRisks(correlation, sensitivityHealth, scenarioHealth[-3])
  expect_identical(result, retClass(list(correlation = correlation, volatility = sensitivityHealth, scenario = scenarioHealth), "healthModule"))
})

test_that("getRateWithTime",{
  
  marketRiskFactorsRate <- data.table(name = c("CHF 2Y", "CHF 10Y", "CHF 30Y"), horizon = c("k", "m", "l"))
  maturitiesInterestRate <- data.table(time = 1:50, projection = c(rep("k", 5), rep("m", 14), rep("l", 31)))
  
  RateWithTime_result <- data.table(time = 1:50, projection = c(rep("k", 5), rep("m", 14), rep("l", 31)), name = rep(c("CHF 2Y", "CHF 10Y", "CHF 30Y"), times = c(5, 14, 31)))
  expect_identical(getRateWithTime(marketRiskFactorsRate, maturitiesInterestRate), RateWithTime_result)
})

test_that("process", {

  # Data remains unchanged
  expect_identical(process(1L, main = TRUE, resultsForShinyDashboard = FALSE, debugMode = TRUE), 1L)
  expect_identical(process(1L, main = TRUE, resultsForShinyDashboard = FALSE, debugMode = FALSE), 1L)
  expect_identical(process(list(LETTERS), main = TRUE, resultsForShinyDashboard = FALSE, debugMode = FALSE), list(LETTERS))

  # Check option 'resultsForShinyDashboard'
  output <- finalizeProzess(1L, packageEnv$errorLog, resultsForShinyDashboard = TRUE)
  expect_identical(process(1L, main = TRUE, resultsForShinyDashboard = TRUE, debugMode = FALSE), output)

  # Environment remains unchanged
  packageEnv$config <- 1:10
  process("x", main = FALSE, resultsForShinyDashboard = FALSE, debugMode = FALSE)
  expect_identical(packageEnv$config, 1:10)

  packageEnv$config <- 1:10
  process("x", main = TRUE, resultsForShinyDashboard = FALSE, debugMode = TRUE)
  expect_identical(packageEnv$config, 1:10)

  # Environment ist correctly reset
  packageEnv$param <- 1:10
  process("x", main = TRUE, resultsForShinyDashboard = FALSE, debugMode = FALSE)
  param <- list(
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
  expect_identical(packageEnv$param, param)

  # Bug check: Pb should be closed at end of process
  cleanup()
  expect_output(process(createProgressBar("test", 1), main = FALSE), "test")
  expect_null(packageEnv$pb)


  expect_identical(process(NA), NA)

  expect_error(process(stop("Error message")), "Error message")
  expect_warning(process(warning("Warning message")), "Warning message")
  expect_warning(process(addError(NA, "Warning message", type = "Warning")), "Warning message")
  expect_error(process(addError(NA, "Error message", type = "Error")), "Error message")
})
