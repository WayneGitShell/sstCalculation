context("Credit")

portfolio_Copy <- copy(portfolio)
scaling <- getScalingFactorNormal(0.01)

curve <- spreadCurve$curve
rating <- portfolioFX$rating
migration <- portfolioFX$migration
rowNumber <- portfolioFX$rowNumber
spread <- rep(0.01, nrow(portfolioFX))
coupons <- getCoupons(portfolioFX)
counterparties <- data.table(counterparty = c("A", "B", "C", "D", "A"), rating = c(1, 2, 3, 4, 5))

marketValue <- portfolioFX$marketValue

yield <- as.matrix(initialInterestRate[, paste0("Y", 1:50)])
rownames(yield) <- initialInterestRate$currency

migrationMatrix_Error <- copy(migrationMatrix)[c(1:8, 8, 8)]
migrationMatrix_Error[9, `2` := `2` - 0.001]
migrationMatrix_Error[10, `3` := `3` + 0.001]

rho <- 0.1234

zz <- integer(0)


test_that("getCreditBaselRisks", {
  x <- getCreditBaselRisks(creditRiskExposure, creditRiskExposureReinsurance, creditRiskExposureHypo, 0.1)
  y <- retClass(list(standardDeviationBasel = 0.1 * 200/scaling, hypoTerms = 15), "creditRiskBasel")
  expect_identical(x, y)
  
  x <- getCreditBaselRisks(creditRiskExposure, creditRiskExposureReinsurance[integer(0)], creditRiskExposureHypo, 0.1)
  y <- retClass(list(standardDeviationBasel = 0.1 * 201/scaling, hypoTerms = 15), "creditRiskBasel")
  expect_identical(x, y)
  
  x <- getCreditBaselRisks(creditRiskExposure, creditRiskExposureReinsurance[integer(0)], creditRiskExposureHypo[integer(0)], 0.1)
  y <- retClass(list(standardDeviationBasel = 0.1 * 351/scaling, hypoTerms = 0), "creditRiskBasel")
  expect_identical(x, y)
})

test_that("getCounterparties",{
  # Multiple
  x <- getCounterparties(portfolio)
  y <- data.table(counterparty = c("ABC", "ABCD"), rating = c(5L, 4L))
  expect_identical(x, y)
  
  # One
  x <- getCounterparties(portfolio[2:12])
  y <- data.table(counterparty = "ABCD", rating = 4L)
  expect_identical(x, y)
  
  # Zero
  x <- getCounterparties(portfolio[integer(0)])
  y <- data.table(counterparty = character(0), rating = integer(0))
  expect_identical(x, y)
})

test_that("convertFX",{
  # Classical case
  x <- convertFX(portfolio, initialFX, "CHF")
  y <- portfolioFX
  expect_identical(x, y)
  
  # Foreign currency
  x <- convertFX(portfolio[8], initialFX, "CHF")
  y <- portfolioFX[8]
  expect_identical(x, y)
  
  # Same currency entry
  x <- convertFX(portfolio[1], initialFX, "CHF")
  y <- portfolioFX[1]
  expect_identical(x, y)
  
  # No scaling factor
  x <- convertFX(portfolio[4], initialFX, "CHF")
  y <- portfolioFX[4]
  expect_identical(x, y)
  
  # Empty portfolio
  x <- convertFX(portfolio[integer(0)], initialFX, "CHF")
  y <- portfolioFX[integer(0)]
  expect_identical(x, y)
  
  # Ensure that the data.table is not modified by reference
  expect_identical(portfolio, portfolio_Copy)
})


test_that("getCreditShock (error handling)",{
  expect_silent(getCreditShock(portfolioFX[1:6], spreadCurve, initialInterestRate, LGDMap))
  
  # Cashflows are not required when no migration risk is modelled
  expect_error(getCreditShock(portfolioFX[9], spreadCurve, initialInterestRate, LGDMap), "Please provide at least one cash-flow for this counterparty", fixed = TRUE)
  expect_error(getCreditShock(portfolioFX[8:9], spreadCurve, initialInterestRate, LGDMap), "Please provide at least one cash-flow for this counterparty", fixed = TRUE)
  expect_error(getCreditShock(portfolioFX[c(9, 16)], spreadCurve, initialInterestRate, LGDMap), "Please provide at least one cash-flow for this counterparty", fixed = TRUE)
  expect_silent(getCreditShock(portfolioFX[16], spreadCurve, initialInterestRate, LGDMap))
  
  expect_silent(getCreditShock(portfolioFX[integer(0)], spreadCurve, initialInterestRate, LGDMap))
  
  expect_error(getCreditShock(portfolioFX[7], spreadCurve, initialInterestRate, LGDMap), "Could not compute the implied spread", fixed = TRUE)
  expect_warning(getCreditShock(portfolioFX[13], spreadCurve, initialInterestRate, LGDMap), "spread calculated for this counterparty is larger than 30", fixed = TRUE)
  expect_warning(getCreditShock(portfolioFX[14], spreadCurve, initialInterestRate, LGDMap), "spread calculated for this counterparty is lower than -10", fixed = TRUE)

    
})


test_that("getDefaultShock", {
  x <- getDefaultShock(portfolioFX, LGDMap)
  u <- - portfolioFX$marketValue * portfolioFX$scalingLGD * c(0.5, 2, rep(0.5, 14))
  y <- data.table(lossRating9 = u)
  expect_identical(x, y)
  
  # One entry with lgd scaling
  x <- getDefaultShock(portfolioFX[1], LGDMap)
  y <- data.table(lossRating9 =- portfolioFX$marketValue[1] * 0.5 * 0.6)
  expect_identical(x, y)
  
  x <- getDefaultShock(portfolioFX[integer(0)], LGDMap)
  y <- data.table(lossRating9 = numeric(0))
  expect_identical(x, y)
})

test_that("getMigrationShockFromSpread", {
  # Empty
  x <- getMigrationShockFromSpread(coupons[zz,], yield, spread[zz], curve, rating[zz], 1, migration[zz])
  y <- numeric(0)
  expect_identical(x, y)
  
  # Same rating
  x <- getMigrationShockFromSpread(coupons[1:6,], yield, spread[1:6], curve, ratingOld = rep(3, 6), ratingNew = 3, migration[1:6])
  y <- rep(0, 6)
  expect_identical(x, y)
  
  # No migration
  x <- getMigrationShockFromSpread(coupons[1:6,], yield, spread[1:6], curve, ratingOld = 1:6, ratingNew = 3, migration = rep(FALSE, 6))
  y <- rep(0, 6)
  expect_identical(x, y)
  
  # Migration
  x <- getMigrationShockFromSpread(coupons[1:6,], yield, spread[1:6], curve, ratingOld = 1:6, ratingNew = 2, migration = rep(TRUE, 6))
  newSpread <- spread[1:6] + curve[2] - curve[1:6]
  y <- getPrice(coupons[1:6,], spread = newSpread, yield) - getPrice(coupons[1:6,], spread[1:6], yield)
  expect_identical(x, y)
  
  
  # Migration
  x <- getMigrationShockFromSpread(coupons[1:6,], yield, spread[1:6], curve, ratingOld = 3:8, ratingNew = 7, migration = c(FALSE, rep(TRUE, 5)))
  newSpread <- spread[1:6] + curve[7] - curve[3:8]
  y <- getPrice(coupons[1:6,], spread = newSpread, yield) - getPrice(coupons[1:6,], spread[1:6], yield)
  y[1] <- 0
  expect_identical(x, y)
  
  
  # Migration
  x <- getMigrationShockFromSpread(coupons[8,, drop = F], yield, spread[8], curve, ratingOld = 2, ratingNew = 7, migration = TRUE)
  newSpread <- spread[8] + curve[7] - curve[2]
  y <- getPrice(coupons[8,, drop = F], spread = newSpread, yield) - getPrice(coupons[8,,drop = F], spread[2], yield)
  expect_identical(x, y)
})

test_that("getMigrationShock", {
  # Classical case
  x <- getMigrationShock(coupons, yield, spread, curve, rating, migration)
  y <- as.data.table(sapply(1:8, getMigrationShockFromSpread, coupons = coupons, yield = yield, spread = spread, curve = curve, ratingOld = rating, migration = migration))
  names(y) <- paste0("lossRating", 1:8)
  expect_identical(x, y)
  
  # One row
  x <- getMigrationShock(coupons[1,, drop = FALSE], yield, spread[1], curve, rating[1], migration[1])
  y <- as.data.table(lapply(1:8, getMigrationShockFromSpread, coupons = coupons[1,, drop = FALSE], yield = yield, spread = spread[1], curve = curve, ratingOld = rating[1], migration = migration[1]))
  names(y) <- paste0("lossRating", 1:8)
  expect_identical(x, y)
  
  # No row
  x <- getMigrationShock(coupons[integer(0),, drop = FALSE], yield, spread[integer(0)], curve, rating[integer(0)], migration[integer(0)])
  y <- as.data.table(lapply(1:8, getMigrationShockFromSpread, coupons = coupons[integer(0),, drop = FALSE], yield = yield, spread = spread[integer(0)], curve = curve, ratingOld = rating[integer(0)], migration = migration[integer(0)]))
  names(y) <- paste0("lossRating", 1:8)
  expect_identical(x, y)
  
  # Correct type
  x <- getMigrationShock(coupons[integer(0),, drop = FALSE], yield, spread[integer(0)], curve, rating[integer(0)], migration[integer(0)])
  a <- unname(sapply(x, class))
  b <- rep("numeric", 8)
  expect_identical(a, b)
  
  # No migration risk
  x <- getMigrationShock(coupons[16,, drop = FALSE], yield, spread[16], curve, rating[16], migration[16])
  y <- as.data.table(as.list(rep(0, 8)))
  names(y) <- paste0("lossRating", 1:8)
  expect_identical(x, y)
})

test_that("getPrice", {
  # General case
  x <- getPrice(coupons, 1:nrow(coupons), yield)
  y <- sapply(1:nrow(coupons), function(i){
    getBondPrice(spread = i, coupons = coupons[i, ], yields = yield[rownames(coupons)[i], ], years = 1:50)
  })
  expect_identical(x, y)
  
  # Specific case
  x <- getPrice(coupons[7:8,], spread[7:8], yield)
  u <- getBondPrice(spread = spread[7], coupons = coupons[7, ], yields = yield[1, ], years = 1:50)
  v <- getBondPrice(spread = spread[8], coupons = coupons[8, ], yields = yield[2, ], years = 1:50)
  y <- c(u, v)
  expect_identical(x, y)
  
  # One instrument
  x <- getPrice(coupons[8,, drop=FALSE], spread[8], yield)
  y <- getBondPrice(spread = spread[8], coupons = coupons[8, ], yields = yield[2, ], years = 1:50)
  expect_identical(x, y)
  
  # One instrument
  x <- getPrice(coupons[15,, drop=FALSE], spread[15], yield)
  y <- getBondPrice(spread = spread[15], coupons = coupons[15, ], yields = yield[5, ], years = 1:50)
  expect_identical(x, y)
  
  # No instrument
  x <- getPrice(coupons[integer(0),, drop=FALSE], spread[integer(0)], yield)
  y <- numeric(0)
  expect_identical(x, y)
  
  # One instrument with only NAs
  x <- getPrice(coupons[16,, drop=FALSE], spread[16], yield)
  y <- 0
  expect_identical(x, y)
  
  # One instrument with NAs
  x <- getPrice(coupons[9:11,, drop=FALSE], spread[9:11], yield)
  y <- rep(0, 3)
  expect_identical(x, y)
  
})

test_that("getSpread", {
  
  # Empty case
  x <- getSpread(coupons[integer(0),], marketValue[integer(0)], yield, migration[integer(0)])
  y <- numeric(0)
  expect_identical(x, y)
  
  # No migration
  x <- getSpread(coupons[1:3, ], marketValue[1:3], yield, rep(FALSE, 3))
  y <- rep(0, 3)
  expect_identical(x, y)
  
  # No migration and incorrect spread
  x <- getSpread(coupons[15,,drop  = FALSE], 1e6, yield, FALSE)
  y <- 0
  expect_identical(x, y)
  
  # Correct result
  x <- getSpread(coupons[1:2, ], c(9, 1e6), yield, c(TRUE, TRUE))
  y <- c(calculateInitialSpread(marketValue = 9, times = 1:50, coupons = coupons[1, ], riskFree = yield["CHF", ]), NaN)
  expect_identical(x, y)
})

test_that("getCoupons", {
  cols <- paste0("Y", 1:50)
  
  # # Empty case
  # x <- getCoupons(portfolioFX[integer(0)])
  # y <- matrix(ncol = 50, nrow = 0)
  # colnames(y) <- cols
  # expect_equal(x, y)
  
  # Classical case
  x <- getCoupons(portfolioFX)
  y <- as.matrix(portfolioFX[, cols, with = FALSE])
  rownames(y) <- portfolioFX$currency
  expect_identical(x, y)
})

test_that("getBondPrice", {
  # Degenerate case
  x <- getBondPrice(0, rep(1, 50), rep(0, 50), 1:50)
  y  <- 50
  expect_identical(x, y)
  
  # Spread mean offset of yield curve
  x <- getBondPrice(0.1, rep(1, 50), rep(0, 50), 1:50)
  y <- getBondPrice(0, rep(1, 50), rep(0.1, 50), 1:50)
  expect_identical(x, y)
  
  # Case with flat yield curve
  x <- getBondPrice(0.1, rep(1, 50), rep(0, 50), 1:50)
  y  <- sum(exp(-0.1 * (1:50)))
  expect_identical(x, y)
})


test_that("getScalingFactorNormal", {
  # Symmetry property
  x <-  getScalingFactorNormal(0.02)*0.02
  y <- getScalingFactorNormal(0.98)*0.98
  expect_equal(x, y)
  
  # Hard coded implementation
  x <-  getScalingFactorNormal(0.01)
  y <- stats::dnorm(stats::qnorm(0.01))/0.01
  expect_identical(x, y)
})

test_that("getThreshold", {
  # Incorrect migration matrix specification
  expect_error(getThreshold(migrationMatrix[-1]), "Incorrect matrix definition")
  expect_error(getThreshold(migrationMatrix[c(2,1,3:8)]), "Incorrect matrix definition")
  expect_error(getThreshold(migrationMatrix[, c("category", 2:9, "rowNumber"), with = FALSE]), "Incorrect matrix definition")
  expect_error(getThreshold(migrationMatrix[, c("category", c(2,1,3:8), "rowNumber"), with = FALSE]), "Incorrect matrix definition")
  
  # Incorrect row sum
  expect_error(getThreshold(migrationMatrix_Error[c(1:7, 9)]), "The row sum should be always 100%", fixed = TRUE)
  expect_error(getThreshold(migrationMatrix_Error[c(1:7, 10)]), "The row sum should be always 100%", fixed = TRUE)
  
  # Calculation of the thresholds
  x <- getThreshold(migrationMatrix)
  u <- as.matrix(migrationMatrix[, 3:10])[, 8:1]
  y <- as.data.table(t(apply(u, 1, cumsum))[, 8:1])
  names(y) <- as.character(1:8)
  expect_equal(x, y)
})

test_that("getAllCounterparties", {
  
  Model <- list(A = list(Modules= list(creditMerton= list(counterparties = counterparties[integer(0)]))),
                B = list(Modules= list(creditMerton= list(counterparties = counterparties[1:2]))),
                C = list(Modules= list(creditMerton= list(counterparties = counterparties[2:4]))),
                D = list(Modules= list(creditMerton= list(counterparties = counterparties[4:5]))))
  
  # Empty case
  x <- getAllCounterparties(Model[c(1, 1)])
  y <- counterparties[integer(0)]
  expect_identical(x, y)
  
  # Simple case
  x <- getAllCounterparties(Model[c(1, 2)])
  y <- counterparties[1:2]
  expect_identical(x, y)
  
  
  # Classical case
  x <- getAllCounterparties(Model[c(2, 3)])
  y <- counterparties[1:4]
  expect_identical(x, y)
  
  # Check that the order is always the same
  x <- getAllCounterparties(Model[c(3, 2)])
  y <- counterparties[1:4]
  expect_identical(x, y)
  
  # Check multiple counterparties
  expect_error(getAllCounterparties(Model[c(2, 4)]), "Only one rating is allowed per counterparty")
})


test_that("simulate.creditRiskBasel", {
  object <- getCreditBaselRisks(creditRiskExposure, creditRiskExposureReinsurance, creditRiskExposureHypo, 0.1)
  
  # Classical case
  x <- simulate.creditRiskBasel(object, nsim = 3, seed = 1)
  setSeed(1, "creditBasel")
  baselRest <- rnorm(3, mean = 0, sd = object$standardDeviationBasel)
  baselHypo <- rep(-15, 3)
  sim <- data.table(all = baselRest+baselHypo, Rest = baselRest, Hypothek = baselHypo)
  y <- list(sim = sim, RF = NULL)
  expect_identical(x, y)
  
  # No basel risk
  object <- getCreditBaselRisks(creditRiskExposure[integer(0)], creditRiskExposureReinsurance[integer(0)], creditRiskExposureHypo[integer(0)], 0.1)
  x <- simulate.creditRiskBasel(object, nsim = 3, seed = 1)
  sim <- data.table(all = rep(0, 3), Rest = 0, Hypothek = 0)
  y <- list(sim = sim, RF = NULL)
  expect_identical(x, y)
  
  # No hypo risk
  object <- getCreditBaselRisks(creditRiskExposure, creditRiskExposureReinsurance, creditRiskExposureHypo[integer(0)], 0.1)
  x <- simulate.creditRiskBasel(object, nsim = 3, seed = 1)
  setSeed(1, "creditBasel")
  baselRest <- rnorm(3, mean = 0, sd = object$standardDeviationBasel)
  sim <- data.table(all = baselRest, Rest = baselRest, Hypothek = 0)
  y <- list(sim = sim, RF = NULL)
  expect_identical(x, y)
  
  # Only hypo risk
  object <- getCreditBaselRisks(creditRiskExposure[2:3], creditRiskExposureReinsurance[integer(0)], creditRiskExposureHypo, 0.1)
  x <- simulate.creditRiskBasel(object, nsim = 3, seed = 1)
  sim <- data.table(all = rep(-15, 3), Rest = 0, Hypothek = rep(-15, 3))
  y <- list(sim = sim, RF = NULL)
  expect_identical(x, y)
})


test_that("getPercentile", {
  phi <- c(-0.1, 0, 0.1, 0.3, 0.1)
  epsilon <- c(0, 0, 0, 0.1, 0.2)
  
  # Classical case
  x <- getPercentile(phi = phi, epsilon = epsilon, rho)
  y <- stats::pnorm(phi*rho + sqrt(1-rho^2)*epsilon)
  expect_identical(x, y)
  
  # Special case
  x <- getPercentile(phi = phi, epsilon = epsilon, 0)
  y <- stats::pnorm(epsilon)
  expect_identical(x, y)
  
  # Special case
  x <- getPercentile(phi = phi, epsilon = epsilon, 1)
  y <- stats::pnorm(phi)
  expect_identical(x, y)
})


test_that("getCreditMertonRisks", {
  # Classical case
  x <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix, initialInterestRate, portfolio[c(1:6)], rho, initialFX, "CHF", progress = 0)
  portfolioFX <- convertFX(portfolio[c(1:6)], initialFX, "CHF")
  creditShock <- getCreditShock(portfolioFX, spreadCurve, initialInterestRate, LGDMap)
  threshold <- getThreshold(migrationMatrix)
  counterparties <- getCounterparties(portfolioFX)
  y <- retClass(list(threshold = threshold, counterparties = counterparties, losses = creditShock, rho = rho, progress = 0), "creditRiskMerton")
  expect_identical(x, y)
})

test_that("simulate.creditRiskMerton", {
  # No instrument
  object <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix_adjusted, initialInterestRate, portfolio[integer(0)], rho, initialFX, "CHF", progress = 0)
  x <- simulate.creditRiskMerton(object, 3, 1)
  y <- list(sim = data.table(all = rep(0, 3), migration = 0, default = 0), RF = NULL)
  expect_identical(x, y)
  
  
  # One instrument
  object <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix_adjusted, initialInterestRate, portfolio[c(2:6)], rho, initialFX, "CHF", progress = 0)
  
  x <- simulate.creditRiskMerton(object, 10, 1)
  setSeed(1, "creditMerton")
  phi <- stats::rnorm(10)
  epsilon1 <- stats::rnorm(10)
  sim <- simulateLossIssuer(object$losses[1, -1], object$threshold[4], phi, rho, epsilon = epsilon1)
  sim <- sim[, .(all = default + migration - mean(default+migration), migration = migration - mean(migration), default = default - mean(default))]
  y <- list(sim = sim, RF = NULL)
  expect_equal(x, y)
  
  
  # Two instruments
  object <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix_adjusted, initialInterestRate, portfolio[c(1:6)], rho, initialFX, "CHF", progress = 0)
  
  x <- simulate.creditRiskMerton(object, 10, 1)
  setSeed(1, "creditMerton")
  phi <- stats::rnorm(10)
  epsilon1 <- stats::rnorm(10)
  epsilon2 <- stats::rnorm(10)
  sim <- simulateLossIssuer(object$losses[1, -1], object$threshold[5], phi, rho, epsilon = epsilon1)
  sim <- sim + simulateLossIssuer(object$losses[2, -1], object$threshold[4], phi, rho, epsilon = epsilon2)
  sim <- sim[, .(all = default + migration - mean(default+migration), migration = migration - mean(migration), default = default - mean(default))]
  y <- list(sim = sim, RF = NULL)
  expect_equal(x, y)
  
  
  # The sum  of a daughter run and mother run (except scaling effects) should be identical to one run
  object <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix_adjusted, initialInterestRate, portfolio[c(1:6)], rho, initialFX, "CHF", progress = 0)
  object1 <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix_adjusted, initialInterestRate, portfolio[c(1)], rho, initialFX, "CHF", progress = 0)
  object2 <- getCreditMertonRisks(spreadCurve, LGDMap, migrationMatrix_adjusted, initialInterestRate, portfolio[c(2:6)], rho, initialFX, "CHF", progress = 0)

  counterparties <- rbind(object1$counterparties, object2$counterparties)
  object1$counterparties <- counterparties
  object2$counterparties <- counterparties
  x <- simulate.creditRiskMerton(object, 10, 1)
  y1 <- simulate.creditRiskMerton(object1, 10, 1)
  y2 <- simulate.creditRiskMerton(object2, 10, 1)
  y <- list(sim = y1$sim+y2$sim, RF = NULL)
  expect_equal(x, y)
})