context("Health")

sensitivityHealth_zeroExposure <- sensitivityHealth[1:2]
sensitivityHealth_Exposure <- sensitivityHealth[3:4]
scenarioHealth_zeroExposure <- scenarioHealth[1:2]
scenarioHealth_Exposure <- scenarioHealth[3:4]

test_that("getHealthRisks", {

  # Total proba = 100%
  expect_error(getHealthRisks(correlationHealth, sensitivityHealth_zeroExposure, scenarioHealth[4:5]), "The total probability of the scenarios should be strictly smaller than 100")
  # Total proba > 100%
  expect_error(getHealthRisks(correlationHealth, sensitivityHealth_zeroExposure, scenarioHealth[5:6]), "The total probability of the scenarios should be strictly smaller than 100")
  # Permuted order risk factors
  expect_error(getHealthRisks(correlationHealth, sensitivityHealth_zeroExposure[2:1], scenarioHealth), "Please ensure that the risk factors are in the same order as the correlation matrix names")

  # Object can be created correctly with 0 exposure
  x <- getHealthRisks(correlationHealth, sensitivityHealth_zeroExposure, scenarioHealth_zeroExposure)
  y <- list(correlation = correlationHealth, volatility = sensitivityHealth_zeroExposure, scenario = scenarioHealth[c(1:2,7), .(label, effect, probability)])
  y <- retClass(y, "healthModule")
  expect_identical(x, y)
  
  # Object can be created correctly
  x <- getHealthRisks(correlationHealth, sensitivityHealth_Exposure, scenarioHealth_Exposure)
  y <- list(correlation = correlationHealth, volatility = sensitivityHealth_Exposure, scenario = scenarioHealth[c(3:4,7), .(label, effect, probability)])
  y <- retClass(y, "healthModule")
  expect_identical(x, y)
})


test_that("simulate.healthModule", {
  
  
  # Simulation with no exposure
  object <- getHealthRisks(correlationHealth, sensitivityHealth_zeroExposure, scenarioHealth_zeroExposure)
 
  x <- simulate(object, nsim = 3, seed = 1)
  setSeed(1, "Health")
 
   y <- simulationMultivariate(correlation = object$correlation, volatility = object$volatility, nsim = 3)
  y$sim[, all_beforeScenario := 0]
  expect_identical(x, y)
  
  
  # Simulation with exposure
  object <- getHealthRisks(correlationHealth, sensitivityHealth_Exposure, scenarioHealth[6])
  
  x <- simulate(object, nsim = 3, seed = 1)
  setSeed(1, "Health")
  y <- simulationMultivariate(correlation = object$correlation, volatility = object$volatility, nsim = 3)
  scenariosSimulations <- sample(x = c(5.5, 0), prob = c(0.5, 0.5), replace = TRUE, size = 3)
  
  y$sim[, all_beforeScenario := all]
  y$sim[, all := all + scenariosSimulations]
  expect_identical(x, y)
  
})
