context("life")

correlation <- matrix(c(1, 0.2, 0.2, 1), 2, dimnames = list(c("RF1", "RF2"), c("RF1", "RF2")))
exposureLife <- c(RF1 = 0, RF2 = 1.5, RF3 = 2)

test_that("getLifeRisks", {
  # Errors caught correctly
  expect_error(getLifeRisks(correlation, exposureLife[2:3]), 
               "The risk factor 'RF1' is defined in the life risk correlation matrix but could not be found in this table")
  expect_silent(getLifeRisks(correlation, exposureLife[1:2]))
  
  # Object can be created correctly
  x <- getLifeRisks(correlation, exposureLife[1:2])
  # y
  volatility <- exposureLife[1:2]/stats::qnorm(0.995)
  y <- list(correlation = correlation, volatility = volatility)
  y <- retClass(y, "lifeModule")
  expect_identical(x, y)
})


test_that("simulate.lifeModule", {
  
  # No scenario
  object <- getLifeRisks(correlation, exposureLife[1:2])
  x <- simulate.lifeModule(object = object, nsim = 10, seed = 1)
  # y
  setSeed(1, "Life")
  y <- simulationMultivariate(object$correlation, object$volatility, 10)
  expect_identical(x, y)
})
