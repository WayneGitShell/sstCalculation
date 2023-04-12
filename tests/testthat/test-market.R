context("market")

test_that("calculateStandalone", {
  
  # Zero exposure
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 3,
                           instrument = "Delta",
                           exposure = 0,
                           exponentialValuation = FALSE,
                           informationJoinedRF = "asset",
                           factorId = "RF4",
                           name = "MSCI JP",
                           scale = 2,
                           constant = -5,
                           progress = 1
  )
  sim <- calculateStandalone("all", data.table(`MSCI JP`=2), instrument)
  expect_identical(sim, 0)
  
  
  ## Delta case
  # positive RF
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 3,
                           instrument = "Delta",
                           exposure = 5,
                           exponentialValuation = FALSE,
                           informationJoinedRF = "asset",
                           factorId = "RF4",
                           name = "MSCI JP",
                           scale = 2,
                           constant = -5, 
                           progress = 1
  )
  sim <- calculateStandalone("all", data.table(`MSCI JP`=2), instrument)
  expect_identical(sim, 20)
  # negative RF
  sim <- calculateStandalone("all", data.table(`MSCI JP`=-1), instrument)
  expect_identical(sim, -10)
  
  ## 1 risk factor
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 1,
                           instrument = "Equity",
                           exposure = 1,
                           exponentialValuation = TRUE,
                           informationJoinedRF = "asset",
                           factorId = "RF4",
                           name = "MSCI JP",
                           scale = 2,
                           constant = 2,
                           progress = 1
  )
  sim <- calculateStandalone("all", data.table(`MSCI JP`=-1), instrument)
  expect_identical(sim, 0)
  
  
  ## 2 risk factors
  # null scale
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 1,
                           instrument = "Equity",
                           exposure = 1,
                           exponentialValuation = TRUE,
                           informationJoinedRF = c("asset", "currency"),
                           factorId = c("RF4", "RF21"),
                           name = c("MSCI JP", "JPYCHF"),
                           scale = 0,
                           constant = 0,
                           progress = 0.5
  )
  sim <- calculateStandalone("all", data.table(`MSCI JP`=-1, `JPYCHF`=1), instrument)
  expect_identical(sim, 0)
  # other case
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 1,
                           instrument = rep("Equity", 2),
                           exposure = 1,
                           exponentialValuation = TRUE,
                           informationJoinedRF = c("asset", "currency"),
                           factorId = c("RF4", "RF21"),
                           name = c("MSCI JP", "JPYCHF"),
                           scale = 1,
                           constant = 1,
                           progress = 0.5
  )
  sim <- calculateStandalone("all", data.table(`MSCI JP`=-1, `JPYCHF`=1), instrument)
  expect_identical(sim, exp(1)-1)
  
  
  ## 3 risk factors - different scale
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 1,
                           instrument = "Equity",
                           exposure = 1,
                           exponentialValuation = TRUE,
                           informationJoinedRF = c("asset", "currency", "currency"),
                           factorId = c("RF4", "RF21", "RF18"),
                           name = c("MSCI JP", "JPYCHF", "EURCHF"),
                           scale = 1:3,
                           constant = 1,
                           progress = 1/3
  )
  sim <- calculateStandalone("all", data.table(`MSCI JP`=-1, `JPYCHF`=1/2, `EURCHF`=-1/3), instrument)
  expect_identical(sim, 0)
  
  ## 4 risk factors
  # 1 simulation per risk factor
  instrument <- data.table(standaloneTypes = "all",
                           instrumentId = 1,
                           instrument = "Fixed Income",
                           exposure = 1,
                           exponentialValuation = TRUE,
                           informationJoinedRF = c( "spread", "currency", "rate", "currency"),
                           factorId = c("RF48", "RF18", "RF25", "RF21"),
                           name = c("EUR Spread AA", "EURCHF", "EUR 2Y", "JPYCHF"),
                           scale = 1,
                           constant = 0,
                           progress = 0.25
  )
  sim <- calculateStandalone("all", data.table(`EUR Spread AA`=1, `JPYCHF`=1, `EURCHF`=1, `EUR 2Y`=1), instrument)
  expect_identical(sim, exp(4)-1)
  
  
  # case with 2 simulations per risk factor
  RF <- data.table(`EUR 2Y` = c(0,1),
                   `EUR Spread AA` = c(0,-1),
                   `EURCHF` = c(0,1),
                   `JPYCHF` = c(0, -2),
                   `MSCI JP` = c(0,100)
  )
  sim <- calculateStandalone("all", RF, instrument)
  expect_identical(sim, c(0, exp(-1)-1))
})

test_that("calculateInitialSpread", {
  # Exact match
  expect_identical(calculateInitialSpread(marketValue = 10, times = 1:10, coupons = rep(1, 10), riskFree = rep(0, 10)), 0)
  
  # NaN produced by out of range value
  expect_true(is.nan(calculateInitialSpread(marketValue = 10, times = 1:10, coupons = rep(1/1000, 10), riskFree = rep(0, 10))))
  
  # Correct spread
  expect_equal(calculateInitialSpread(marketValue = sum(exp(-0.01 * (1:10))), times = 1:10, coupons = rep(1, 10), riskFree = rep(0, 10)), 0.01)
  
  # Correct extreme spread (positive)
  expect_equal(calculateInitialSpread(marketValue = sum(exp(-10 * (1:10))), times = 1:10, coupons = rep(1, 10), riskFree = rep(0, 10)), 10)
  # Correct extreme spread (negative)
  expect_equal(calculateInitialSpread(marketValue = sum(exp(0.30 * (1:10))), times = 1:10, coupons = rep(1, 10), riskFree = rep(0, 10)), -0.3)
  
  # non-null riskFree
  expect_equal(calculateInitialSpread(marketValue = 13, times = 1:4, coupons = c(rep(1, 3),10), riskFree = rep(0.1, 4)), -0.1, tolerance=1e-10)
})

test_that("calculateStandalones", {
  standaloneType <- "all"
  instruments <- data.table(instrumentId = 1,
                            instrument = "Equity",
                            exposure = 1,
                            exponentialValuation = TRUE
  )
  instrumentsMapping <- data.table(informationJoinedRF = c("asset","currency"),
                                   instrumentId = 1,
                                   factorId = c("RF4", "RF21"),
                                   name = c("MSCI JP", "JPYCHF"),
                                   scale = 1
  )
  MR_FactorsStandalones <- data.table(factorId = c("RF4", "RF21"),
                                      rowNumber = c(7,8),
                                      standalones = c("equity", "currency")
  )
  marketCovariance <- matrix(c(1,0,0,1), ncol=2, dimnames = list(c("MSCI JP", "JPYCHF"),c("MSCI JP", "JPYCHF")))
  x <- copy(instruments[instrumentsMapping, on = "instrumentId", nomatch = 0])
  x[, constant:= c(-1,-1)]
  setindex(x, "exponentialValuation")
  
  y <- calculateStandalones(standaloneType, instruments, instrumentsMapping, MR_FactorsStandalones, marketCovariance)
  expect_identical(x,y)
})

test_that("simulate.MarketInstrumentsModule", {
  
  ### Test for one asset with delta valuation
  Sigma <- matrix(1, dimnames = list("Test"))
  
  standalonesList <- c("all", "asset")
  names(standalonesList) <- c("all", "asset")
  
  instrument <- data.table(standaloneTypes = c("all", "asset"),
                           instrumentId = c(1, 1),
                           instrument = c("Delta terms", "Delta terms"),
                           exposure = c(1, 1),
                           exponentialValuation = c(FALSE, FALSE),
                           informationJoinedRF = c("delta", "delta"),
                           factorId = c("RF1", "RF1"),
                           name = c("Test", "Test"),
                           scale = c(2, 2),
                           constant = c(-5, -5)
  )
  
  object <- list(instruments = instrument, mu = rep(0, nrow(Sigma)), Sigma = Sigma, standalonesList = standalonesList, progress = 0)
  attr(object, "class") <- c("marketInstrumentsModule", "list")
  
  nsim <- 10
  seed <- 1
  B <- simulate.marketInstrumentsModule(object, nsim, seed)
  
  # For marketrisk the seed is the same
  set.seed(seed)
  RF <- data.table(MASS::mvrnorm(n = nsim, mu = rep(0, nrow(Sigma)), Sigma = Sigma))
  
  # One time for delta valuation
  value <- instrument$exposure[1] * instrument$scale[1] * RF
  
  sim <- cbind(value, value)
  names(sim) <- c("all", "asset")
  
  A <- list(sim = sim, RF = RF)
  
  expect_identical(A, B)
  
  ### Test for two assets with exponential-delta valuation
  Sigma <- matrix(c(1, 0.1, 0.1, 1), ncol = 2, dimnames = list(c("Test1", "Test2")))
  
  standalonesList <- c("all", "asset", "spread")
  names(standalonesList) <- c("all", "asset", "spread")
  
  instrument <- data.table(standaloneTypes = c("all", "all", "asset", "spread"),
                           instrumentId = c(1, 2, 1, 2),
                           instrument = c("Instrument1", "Instrument2", "Instrument1", "Instrument2"),
                           exposure = c(1, 5, 1, 5),
                           exponentialValuation = c(TRUE, FALSE, TRUE, FALSE),
                           informationJoinedRF = c("normal", "delta", "normal", "delta"),
                           factorId = c("RF1", "RF2", "RF1", "RF2"),
                           name = c("Test1", "Test2", "Test1", "Test2"),
                           scale = c(1, 2, 1, 2),
                           constant = c(-1, 0, -1, 0)
  )
  
  object <- list(instruments = instrument, mu = rep(0, nrow(Sigma)), Sigma = Sigma, standalonesList = standalonesList, progress = 0)
  
  attr(object, "class") <- c("marketInstrumentsModule", "list")
  
  nsim <- 10
  seed <- 1
  
  B <- simulate.marketInstrumentsModule(object, nsim, seed)
  
  # For marketrisk the seed is the original one
  set.seed(seed)
  RF <- data.table(MASS::mvrnorm(n = nsim, mu = rep(0, nrow(Sigma)), Sigma = Sigma))
  
  # One time for delta valuation
  value1 <- instrument$exposure[1] * expm1(instrument$scale[1] * RF[,1] + instrument$constant[1])
  value2 <- instrument$exposure[2] * instrument$scale[2] * RF[,2]
  value <- value1 + value2
  
  sim <- cbind(value, value1, value2)
  names(sim) <- c("all", "asset", "spread")
  
  A <- list(sim = sim, RF = RF)
  
  expect_identical(A, B)
})

test_that("simulate.MarketInstrumentsModule", {
  object <- list(exposure = 10, sd = 0.1)
  attr(object, "class") <- c("marketParticipationModule", "list" )
  
  nsim <- 10
  seed <- 1
  B <- simulate.marketParticipationModule(object, nsim, seed)
  
  set.seed(getSeed(seed, "marketParticipation"))
  sim <- data.table(all = object$exposure * expm1(stats::rnorm(n = nsim, sd = object$sd) - 0.5 * (object$sd^2)))
  A <- list(sim = sim, RF = NULL)
  
  expect_identical(A, B)
})
