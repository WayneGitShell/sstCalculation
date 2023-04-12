context("Captives")

captiveCY_stochastic_Empty <- captiveCY_stochastic[integer(0)]
captiveCY_deterministic_Empty <- captiveCY_deterministic[integer(0)]
captivePY_Empty <- captivePY[integer(0)]
nonlifeSimulation_Empty <- nonlifeSimulation[integer(0)]
nonlifeCDF_Empty <- nonlifeCDF[integer(0)]
scenarioNL_Empty <- scenarioNL[integer(0)]

# General cases

objects_Empty <- getNonLifeRisks(nonlifeType = "no nonlife risk",
                                 nonlifeSimulation = nonlifeSimulation_Empty,
                                 nonlifeMu = NA, 
                                 nonlifeSigma = NA,
                                 nonlifeCDF = nonlifeCDF_Empty, 
                                 nonLifeSeedAddon = 1, 
                                 captiveCY_deterministic = captiveCY_deterministic_Empty, 
                                 captiveCY_stochastic = captiveCY_stochastic_Empty,
                                 captivePY = captivePY_Empty,
                                 CYrisk = 5, PYrisk = 10,
                                 scenarioNL = scenarioNL_Empty)

objects_Sim <- getNonLifeRisks(nonlifeType = "simulations",
                               nonlifeSimulation = nonlifeSimulation,
                               nonlifeMu = NA,
                               nonlifeSigma = NA,
                               nonlifeCDF = nonlifeCDF_Empty,
                               nonLifeSeedAddon = -1000,
                               captiveCY_deterministic = captiveCY_deterministic_Empty,
                               captiveCY_stochastic = captiveCY_stochastic_Empty,
                               captivePY = captivePY_Empty,
                               CYrisk = 5, PYrisk = 10,
                               scenarioNL = scenarioNL_Empty)

objects_LN <- getNonLifeRisks(nonlifeType = "lognormal parameters",
                              nonlifeSimulation = nonlifeSimulation_Empty,
                              nonlifeMu = 1, 
                              nonlifeSigma = 2, 
                              nonlifeCDF = nonlifeCDF_Empty,
                              nonLifeSeedAddon = 1,
                              captiveCY_deterministic = captiveCY_deterministic_Empty,
                              captiveCY_stochastic = captiveCY_stochastic_Empty,
                              captivePY = captivePY_Empty,
                              CYrisk = 5, PYrisk = 10,
                              scenarioNL = scenarioNL_Empty)

objects_cdf <- getNonLifeRisks(nonlifeType = "cumulative distribution function",
                               nonlifeSimulation = nonlifeSimulation_Empty, 
                               nonlifeMu = NA,
                               nonlifeSigma = NA, 
                               nonlifeCDF = nonlifeCDF,
                               nonLifeSeedAddon = 0, 
                               captiveCY_deterministic = captiveCY_deterministic_Empty,
                               captiveCY_stochastic = captiveCY_stochastic_Empty,
                               captivePY = captivePY_Empty,
                               CYrisk = 5, PYrisk = 10,
                               scenarioNL = scenarioNL_Empty)

objects_captive <- getNonLifeRisks(nonlifeType = "captive",
                                   nonlifeSimulation = nonlifeSimulation_Empty,
                                   nonlifeMu = NA,
                                   nonlifeSigma = NA,
                                   nonlifeCDF = nonlifeCDF_Empty,
                                   nonLifeSeedAddon = 0,
                                   captiveCY_deterministic = captiveCY_deterministic[1:3],
                                   captiveCY_stochastic = captiveCY_stochastic[1:4],
                                   captivePY = captivePY,
                                   CYrisk = 0, PYrisk = 0,
                                   scenarioNL = scenarioNL)

# Special cases for captives
objects_captiveCYdet <- getNonLifeRisks(nonlifeType = "captive",
                                        nonlifeSimulation = nonlifeSimulation_Empty,
                                        nonlifeMu = NA,
                                        nonlifeSigma = NA,
                                        nonlifeCDF = nonlifeCDF_Empty,
                                        nonLifeSeedAddon = 0,
                                        captiveCY_deterministic = captiveCY_deterministic[1:3], 
                                        captiveCY_stochastic = captiveCY_stochastic_Empty, 
                                        captivePY = captivePY_Empty,
                                        CYrisk = 0, PYrisk = 0,
                                        scenarioNL = scenarioNL)

objects_captiveCYstoch <- getNonLifeRisks(nonlifeType = "captive",
                                          nonlifeSimulation = nonlifeSimulation_Empty,
                                          nonlifeMu = NA,
                                          nonlifeSigma = NA,
                                          nonlifeCDF = nonlifeCDF_Empty,
                                          nonLifeSeedAddon = 0,
                                          captiveCY_deterministic = captiveCY_deterministic_Empty, 
                                          captiveCY_stochastic = captiveCY_stochastic[1:2],
                                          captivePY = captivePY_Empty,
                                          CYrisk = 0, PYrisk = 0,
                                          scenarioNL = scenarioNL)

objects_captiveCYboth <- getNonLifeRisks(nonlifeType = "captive",
                                         nonlifeSimulation = nonlifeSimulation_Empty,
                                         nonlifeMu = NA,
                                         nonlifeSigma = NA,
                                         nonlifeCDF = nonlifeCDF_Empty,
                                         nonLifeSeedAddon = 0,
                                         captiveCY_deterministic = captiveCY_deterministic[1:3],
                                         captiveCY_stochastic = captiveCY_stochastic[1:2], 
                                         captivePY = captivePY_Empty,
                                         CYrisk = 0, PYrisk = 0,
                                         scenarioNL = scenarioNL)

objects_captivePY <- getNonLifeRisks(nonlifeType = "captive",
                                     nonlifeSimulation = nonlifeSimulation_Empty,
                                     nonlifeMu = NA,
                                     nonlifeSigma = NA,
                                     nonlifeCDF = nonlifeCDF_Empty,
                                     nonLifeSeedAddon = 0,
                                     captiveCY_deterministic = captiveCY_deterministic_Empty, 
                                     captiveCY_stochastic = captiveCY_stochastic_Empty, 
                                     captivePY = captivePY,
                                     CYrisk = 0, PYrisk = 0,
                                     scenarioNL = scenarioNL)

objects_captiveCYdetPY <- getNonLifeRisks(nonlifeType = "captive",
                                          nonlifeSimulation = nonlifeSimulation_Empty,
                                          nonlifeMu = NA,
                                          nonlifeSigma = NA,
                                          nonlifeCDF = nonlifeCDF_Empty,
                                          nonLifeSeedAddon = 0,
                                          captiveCY_deterministic = captiveCY_deterministic[1:3], 
                                          captiveCY_stochastic = captiveCY_stochastic_Empty, 
                                          captivePY = captivePY,
                                          CYrisk = 0, PYrisk = 0,
                                          scenarioNL = scenarioNL)

objects_captiveCYbothPY <- getNonLifeRisks(nonlifeType = "captive",
                                           nonlifeSimulation = nonlifeSimulation_Empty,
                                           nonlifeMu = NA,
                                           nonlifeSigma = NA,
                                           nonlifeCDF = nonlifeCDF_Empty,
                                           nonLifeSeedAddon = 0,
                                           captiveCY_deterministic = captiveCY_deterministic[1:3], 
                                           captiveCY_stochastic = captiveCY_stochastic[1:2], 
                                           captivePY = captivePY,
                                           CYrisk = 0, PYrisk = 0,
                                           scenarioNL = scenarioNL)

# The "none" scenario is added automatically
scenarioNL_Expected <- data.table(label = c("A", "B", "none"), effect = c(1, 3, 0), probability = c(0.1, 0.2, 0.7))
scenarioNL_None <- data.table(label = c("none"), effect = c(0), probability = c(1))

test_that("getNonLifeRisks (constructor)", {
  # Constructor
  x <- objects_Empty
  y <- retClass(list(param=retClass(list(CYrisk = 5, PYrisk = 10), "NL_none"), 
                   nonLifeSeedAddon=1, scenario = data.table(label = "none", effect = 0, probability = 1)), "nonlifeModule")
  expect_identical(x, y)
  
  x <- objects_Sim
  y <- retClass(list(param=retClass(list(simulation = c(4, 2, 3, 1), CYrisk = 5, PYrisk = 10), "NL_sim"), nonLifeSeedAddon = -1000, scenario = scenarioNL_None), "nonlifeModule")
  expect_identical(x, y)

  x <- objects_LN
  y <- retClass(list(param=retClass(list(mu = 1, sigma = 2, CYrisk = 5, PYrisk = 10), "NL_log"), nonLifeSeedAddon = 1, scenario = scenarioNL_None), "nonlifeModule")
  expect_identical(x, y)

  x <- objects_cdf
  y <- retClass(list(param=retClass(list(cdf = data.table(x = -2:2, cdf = (1:5)/5), CYrisk = 5, PYrisk = 10),"NL_cdf"), nonLifeSeedAddon = 0, scenario = scenarioNL_None), "nonlifeModule")
  expect_identical(x, y)

  x <- objects_captive
  y <- retClass(list(param=retClass(list(CY=retClass(list(stochastic=captiveCY_stochastic_Result, deterministic=95), "NL_captCY"), PY=retClass(list(stochastic=captivePY_Result), "NL_captPY")), "NL_capt"), nonLifeSeedAddon=0, scenario = scenarioNL_Expected), "nonlifeModule")
  expect_equal(x, y)
})

test_that("getNonLifeRisks (warnings)", {
  # None
  expect_warning(getNonLifeRisks("no nonlife risk", nonlifeSimulation_Empty, NA, NA, nonlifeCDF, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'no nonlife risk')", fixed=TRUE)

  expect_silent(getNonLifeRisks("no nonlife risk", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  
  # CDF
  expect_warning(getNonLifeRisks("cumulative distribution function", nonlifeSimulation, NA, NA, nonlifeCDF, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'cumulative distribution function')", fixed=TRUE)
  
  expect_silent(getNonLifeRisks("cumulative distribution function", nonlifeSimulation_Empty, NA, NA, nonlifeCDF, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL))
  
  # Simulations
  expect_warning(getNonLifeRisks("simulations", nonlifeSimulation, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'simulations')", fixed=TRUE)
  expect_warning(getNonLifeRisks("simulations", nonlifeSimulation, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'simulations')", fixed=TRUE)
  expect_warning(getNonLifeRisks("simulations", nonlifeSimulation, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'simulations')", fixed=TRUE)
  
  expect_silent(getNonLifeRisks("simulations", nonlifeSimulation, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL))
  
  # Captive
  expect_warning(getNonLifeRisks("captive", nonlifeSimulation_Empty, 1, 3.2, nonlifeCDF_Empty, 0, captiveCY_deterministic[1:3], captiveCY_stochastic[1:4], captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'captive')", fixed=TRUE)
  
  expect_silent(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic[1:3], captiveCY_stochastic[1:4], captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  expect_silent(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic_Empty, captiveCY_stochastic[1:4], captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  expect_silent(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic[1:3], captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  expect_silent(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  
})

test_that("getNonLifeRisks (errors)", {
  # Errors
  expect_error(getNonLifeRisks("simulations", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
               "Please ensure that the input for the risk type 'simulations' is provided or set the risk type to 'no nonlife risk' if no non-life risk should be simulated", fixed=TRUE)
  
  expect_error(getNonLifeRisks("cumulative distribution function", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
               "Please ensure that the input for the risk type 'cumulative distribution function' is provided or set the risk type to 'no nonlife risk' if no non-life risk should be simulated", fixed=TRUE)
  
  expect_error(getNonLifeRisks("lognormal parameters", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
               "Please ensure that the input for the risk type 'lognormal parameters' is provided or set the risk type to 'no nonlife risk' if no non-life risk should be simulated", fixed=TRUE)
  
  expect_error(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
               "Please ensure that the input for the risk type 'captive' is provided or set the risk type to 'no nonlife risk' if no non-life risk should be simulated", fixed=TRUE)
  
})

test_that("getNonLifeRisks (warnings for captives)", {
  # No warning for NL_None, as it can be a mother company whose daughter has CY and PY risk
  expect_silent(getNonLifeRisks("no nonlife risk", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 0, scenarioNL = scenarioNL))
  expect_silent(getNonLifeRisks("no nonlife risk", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 0, PYrisk = 5, scenarioNL = scenarioNL))
  expect_silent(getNonLifeRisks("no nonlife risk", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 5, scenarioNL = scenarioNL))
  
  # No warning when no CY/PY split
  expect_silent(getNonLifeRisks("cumulative distribution function", nonlifeSimulation_Empty, NA, NA, nonlifeCDF, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  expect_silent(getNonLifeRisks("simulations", nonlifeSimulation, NA, NA, nonlifeCDF_Empty, 1, captiveCY_deterministic_Empty, captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 0, PYrisk = 0, scenarioNL = scenarioNL))
  
  # Captive
  expect_warning(getNonLifeRisks("captive", nonlifeSimulation_Empty, 1, 3.2, nonlifeCDF_Empty, 0, captiveCY_deterministic[1:3], captiveCY_stochastic[1:4], captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored as a different non-life type is selected (type selected: 'captive')", fixed=TRUE)
  
  expect_warning(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic[1:3], captiveCY_stochastic[1:4], captivePY_Empty, CYrisk = 5, PYrisk = 0, scenarioNL = scenarioNL),
                 "The input will be ignored since the non-life risk is computed by the captive model", fixed = TRUE)
  expect_warning(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic_Empty, captiveCY_stochastic[1:4], captivePY_Empty, CYrisk = 0, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored since the non-life risk is computed by the captive model", fixed = TRUE)
  expect_warning(getNonLifeRisks("captive", nonlifeSimulation_Empty, NA, NA, nonlifeCDF_Empty, 0, captiveCY_deterministic[1:3], captiveCY_stochastic_Empty, captivePY_Empty, CYrisk = 5, PYrisk = 10, scenarioNL = scenarioNL),
                 "The input will be ignored since the non-life risk is computed by the captive model", fixed = TRUE)
  
})

test_that("simulate.nonlifeModule (no captive)",{
  # No risk
  x <- simulate.nonlifeModule(objects_Empty, nsim = 10, seed = 1)
  y <- list(sim = data.table(all = rep(0, 10), CY = -5, PY = -10, all_beforeScenario = rep(0, 10)), RF = NULL)
  expect_identical(x, y)
  
  # Based on simulations
  x <- simulate.nonlifeModule(object = objects_Sim, nsim = 3, seed = 1)
  setSeed(1, "NonLife", -1000)
  all_beforeScenario <- sample(x = c(4, 2, 3, 1), replace = FALSE, size = 3)
  y <- list(sim = data.table(all = all_beforeScenario, CY = -5, PY= -10, all_beforeScenario = all_beforeScenario), RF = NULL)
  expect_identical(x, y)
  
  # Limit case (draw 4 out of 4 simulations given)
  x <- simulate.nonlifeModule(object = objects_Sim, nsim = 4, seed = 1)
  setSeed(1, "NonLife", -1000)
  all_beforeScenario <- sample(x = c(4, 2, 3, 1), replace = FALSE, size = 4)
  y <- list(sim = data.table(all = all_beforeScenario, CY = -5, PY= -10, all_beforeScenario = all_beforeScenario), RF = NULL)
  expect_identical(x, y)
  
  # Limit case (draw 5 out of 4 samples so it is with replacement)
  x <- simulate.nonlifeModule(object = objects_Sim, nsim = 5, seed = 1)
  setSeed(1, "NonLife", -1000)
  all_beforeScenario <- sample(x = c(4, 2, 3, 1), replace = TRUE, size = 5)
  y <- list(sim = data.table(all = all_beforeScenario, CY = -5, PY= -10, all_beforeScenario = all_beforeScenario), RF = NULL)
  expect_identical(x, y)
  
  # Lognormal
  x <- simulate.nonlifeModule(object = objects_LN, nsim = 3, seed = 1)
  setSeed(1, "NonLife", 1)
  sim <- (stats::rnorm(n = 3) * 2 + 1)
  all_beforeScenario <-  -(exp(sim) - exp(1 + (2^2)/2))
  y <- list(sim = data.table(all = all_beforeScenario, CY = -5, PY= -10, all_beforeScenario = all_beforeScenario), RF = NULL)
  expect_identical(x, y)
  
  # CDF
  # Here the "+ 0.0" is needed because else typeof(y$sim$all) = integer, while typeof(x$sim$all) = double
  # and the objects would be equal but not identical
  x <- simulate.nonlifeModule(object = objects_cdf, nsim = 4, seed = 1)
  setSeed(1, "NonLife")
  all_beforeScenario <- sample(x = -2:2, size = 4, prob = diff(c(0, 0.2, 0.4, 0.6, 0.8, 1)), replace = TRUE)
  y <- list(sim = data.table(all = all_beforeScenario + 0.0, CY = -5, PY= -10, all_beforeScenario = all_beforeScenario), RF = NULL)
  expect_identical(x, y)
})

test_that("simulateCompoundPoisson", {
  
  set.seed(1)
  frequency <- 10
  nsim <- 1
  shape <- 5
  scale <- 1
  x <- simulateCompoundPoisson(nsim = nsim, frequency = frequency, shape = shape, scale = scale, 
                               EED = 0, EEL = 100, indicatorLarge = FALSE)
  set.seed(1)
  numberLoss <- stats::rpois(n = nsim, lambda = frequency)
  severity <- stats::rgamma(n = sum(numberLoss), shape = shape, scale = scale)
  y <- sum(severity)
  
  expect_identical(x, y)
  
  
  set.seed(1)
  frequency <- 10
  nsim <- 1
  shape <- 5
  scale <- 1
  x <- simulateCompoundPoisson(nsim = nsim, frequency = frequency, shape = shape, scale = scale, 
                               EED = 0, EEL = 100, indicatorLarge = TRUE)
  set.seed(1)
  numberLoss <- stats::rpois(n = nsim, lambda = frequency)
  severity <- rpareto(n = sum(numberLoss), shape = shape, scale = scale)
  y <- sum(severity)
  
  expect_identical(x, y)
})

test_that("simulate.nonlifeModule (captive)",{
  
  # All test are with scenarios
  # Deterministic CY, no PY
  x <- simulate.nonlifeModule(object = objects_captiveCYdet, nsim = 3, seed = 1)
  setSeed(1, "NonLife")
  loss <- sum(captiveCY_deterministic[1:3]$maximumPossibleLoss - captiveCY_deterministic[1:3]$expectedLoss)
  CY <- rep(-loss, 3)
  PY <- 0
  setSeed(1, "NonLifeScenario")
  scenariosSimulations <- sample(x = scenarioNL_Expected$effect, prob = scenarioNL_Expected$probability, replace = TRUE, size = 3)
  y <- list(sim = data.table(all = CY + PY + scenariosSimulations, CY = CY, PY = PY, all_beforeScenario = CY + PY), RF = NULL)
  expect_identical(x, y)
  
  # Stochastic CY, no PY
  # Here expect_identical does not work
  x <- simulate.nonlifeModule(object = objects_captiveCYstoch, nsim = 3, seed = 1)
  setSeed(1, "NonLife")
  CY <- sort(simulateLineOfBusinessCY(3, captiveCY_stochastic_Result[1]) + simulateLineOfBusinessCY(3, captiveCY_stochastic_Result[2]))
  PY <- 0
  loss <- data.table(all = CY + PY, CY = CY, PY = PY)
  loss <- loss[sample.int(3)]
  setSeed(1, "NonLifeScenario")
  scenariosSimulations <- sample(x = scenarioNL_Expected$effect, prob = scenarioNL_Expected$probability, replace = TRUE, size = 3)
  y <- list(sim = data.table(all = loss$all + scenariosSimulations, CY = loss$CY, PY = loss$PY, all_beforeScenario = loss$all), RF = NULL)
  expect_equal(x, y)
  
  # Stochastic and deterministic CY, no PY
  x <- simulate.nonlifeModule(object = objects_captiveCYboth, nsim = 3, seed = 1)
  setSeed(1, "NonLife")
  loss_det <- sum(captiveCY_deterministic[1:3]$maximumPossibleLoss - captiveCY_deterministic[1:3]$expectedLoss)
  CY_det <- rep(-loss_det, 3)
  CY <- sort(simulateLineOfBusinessCY(3, captiveCY_stochastic_Result[1]) + simulateLineOfBusinessCY(3, captiveCY_stochastic_Result[2]))
  PY <- 0
  loss <- data.table(all = CY + CY_det + PY, CY = CY + CY_det, PY = PY)
  loss <- loss[sample.int(3)]
  setSeed(1, "NonLifeScenario")
  scenariosSimulations <- sample(x = scenarioNL_Expected$effect, prob = scenarioNL_Expected$probability, replace = TRUE, size = 3)
  y <- list(sim = data.table(all = loss$all + scenariosSimulations, CY = loss$CY, PY = loss$PY, all_beforeScenario = loss$all), RF = NULL)
  expect_identical(x, y)
  
  # Only PY risk
  x <- simulate.nonlifeModule(object = objects_captivePY, nsim = 3, seed = 1)
  setSeed(1, "NonLife")
  CY <- 0
  PY <- simulateLineOfBusinessPY(3, captivePY_Result[1]) + simulateLineOfBusinessPY(3, captivePY_Result[2])
  loss <- data.table(all = CY + PY, CY = CY, PY = PY)
  loss <- loss[sample.int(3)]
  setSeed(1, "NonLifeScenario")
  scenariosSimulations <- sample(x = scenarioNL_Expected$effect, prob = scenarioNL_Expected$probability, replace = TRUE, size = 3)
  y <- list(sim = data.table(all = loss$all + scenariosSimulations, CY = loss$CY, PY = loss$PY, all_beforeScenario = loss$all), RF = NULL)
  expect_identical(x, y)
  
  # PY and deterministic CY
  x <- simulate.nonlifeModule(object = objects_captiveCYdetPY, nsim = 3, seed = 1)
  setSeed(1, "NonLife")
  loss_det <- sum(captiveCY_deterministic[1:3]$maximumPossibleLoss - captiveCY_deterministic[1:3]$expectedLoss)
  CY_det <- rep(-loss_det, 3)
  CY <- 0
  PY <- simulateLineOfBusinessPY(3, captivePY_Result[1]) + simulateLineOfBusinessPY(3, captivePY_Result[2])
  loss <- data.table(all = CY_det + CY + PY, CY = CY_det + CY, PY = PY)
  loss <- loss[sample.int(3)]
  setSeed(1, "NonLifeScenario")
  scenariosSimulations <- sample(x = scenarioNL_Expected$effect, prob = scenarioNL_Expected$probability, replace = TRUE, size = 3)
  y <- list(sim = data.table(all = loss$all + scenariosSimulations, CY = loss$CY, PY = loss$PY, all_beforeScenario = loss$all), RF = NULL)
  expect_identical(x, y)
  
  # PY and Stochastic and deterministic CY
  x <- simulate.nonlifeModule(object = objects_captiveCYbothPY, nsim = 3, seed = 1)
  setSeed(1, "NonLife")
  loss_det <- sum(captiveCY_deterministic[1:3]$maximumPossibleLoss - captiveCY_deterministic[1:3]$expectedLoss)
  CY_det <- rep(-loss_det, 3)
  CY <- sort(simulateLineOfBusinessCY(3, captiveCY_stochastic_Result[1]) + simulateLineOfBusinessCY(3, captiveCY_stochastic_Result[2]))
  PY <- simulateLineOfBusinessPY(3, captivePY_Result[1]) + simulateLineOfBusinessPY(3, captivePY_Result[2])
  loss <- data.table(all = CY_det + CY + PY, CY = CY_det + CY, PY = PY)
  loss <- loss[sample.int(3)]
  setSeed(1, "NonLifeScenario")
  scenariosSimulations <- sample(x = scenarioNL_Expected$effect, prob = scenarioNL_Expected$probability, replace = TRUE, size = 3)
  y <- list(sim = data.table(all = loss$all + scenariosSimulations, CY = loss$CY, PY = loss$PY, all_beforeScenario = loss$all), RF = NULL)
  expect_identical(x, y)
})

test_that("simulate.NL_captCY",{
  # Deterministic
  x <- simulate.NL_captCY(objects_captiveCYdet$param$CY, nsim=3)
  y <- rep(-95, 3)
  expect_identical(x, y)
  
  # Stochastic (small numerical error therefore check for equality instead of identical)
  set.seed(1)
  x <- simulate.NL_captCY(objects_captiveCYstoch$param$CY, nsim=3)
  set.seed(1)
  y <- simulateLineOfBusinessCY(nsim=3, captiveCY_stochastic_Result[1]) + simulateLineOfBusinessCY(nsim=3, captiveCY_stochastic_Result[2]) 
  expect_equal(x, y)
  
  # Both
  set.seed(1)
  x <- simulate.NL_captCY(objects_captiveCYboth$param$CY, nsim=3)
  set.seed(1)
  y <- rep(-95, 3) + simulateLineOfBusinessCY(nsim=3, captiveCY_stochastic_Result[1]) + simulateLineOfBusinessCY(nsim=3, captiveCY_stochastic_Result[2]) 
  expect_identical(x, y)
  
  # None
  x <- simulate.NL_captCY(objects_captivePY$param$CY, nsim=3)
  y <- rep(0, 3)
  expect_identical(x, y)
})


test_that("simulate.NL_captPY", {
  # PY Risk
  set.seed(1)
  x <- simulate.NL_captPY(objects_captivePY$param$PY, nsim=3)
  set.seed(1)
  y <- (simulateLineOfBusinessPY(nsim=3, captivePY_Result[1]) + simulateLineOfBusinessPY(nsim=3, captivePY_Result[2]))
  expect_identical(x, y)
  
  # Make sure that x is sorted
  expect_identical(x, sort(x))
  
  # No PY risk
  x <- simulate.NL_captPY(objects_captiveCYboth$param$PY, nsim=3)
  y <- rep(0, 3)
  expect_identical(x, y)
})


test_that("simulate.NL_capt", {
  # PY only
  set.seed(1)
  x <- simulate.NL_capt(objects_captivePY$param, nsim = 3)
  set.seed(1)
  u <- simulate.NL_captPY(objects_captivePY$param$PY, nsim = 3)[sample.int(3)]
  y <- data.table(all = u, CY = 0, PY = u)
  expect_identical(x,y)
  
  # CYdet only
  x <- simulate.NL_capt(objects_captiveCYdet$param, nsim = 3)
  u <- rep(-95, 3)
  y <- data.table(all = u, CY = u, PY = 0)
  expect_identical(x,y)
  
  # CYstoch only
  set.seed(1)
  x <- simulate.NL_capt(objects_captiveCYstoch$param, nsim = 3)
  set.seed(1)
  u <- sort(simulate.NL_captCY(objects_captiveCYstoch$param$CY, nsim = 3))[sample.int(3)]
  y <- data.table(all = u, CY = u, PY = 0)
  expect_identical(x,y)
  
  # CY and PY 
  set.seed(1)
  x <- simulate.NL_capt(objects_captiveCYbothPY$param, nsim = 3)
  set.seed(1)
  u <- sort(simulate.NL_captCY(objects_captiveCYbothPY$param$CY, nsim = 3))
  v <- sort(simulate.NL_captPY(objects_captiveCYbothPY$param$PY, nsim = 3))
  y <- data.table(all = u + v, CY = u, PY = v)[sample.int(3)]
  expect_identical(x,y)
})
