context("helper functions")


test_that("getSeed",{

  seeds_names <- c("marketInstruments", "marketParticipation", "Life", "NonLife", "Health", "Participation", "Scenario", "creditBasel", "creditMerton", "dependencyRisks", "dependencyCredit", "dependencyMarket")
  set.seed(5)
  seeds_values <- c(5, sample.int(1e5, 11))

  # Check seeds
  expect_identical(getSeed(5, "marketInstruments"), 5)
  expect_identical(getSeed(5, "Life"), seeds_values[3])
  expect_identical(getSeed(5, "dependencyMarket"), seeds_values[12])

  # Incorrect type
  expect_error(getSeed(1, "xxxx"), "Incorrect simulation type: 'xxxx'")
})



test_that("getShortfallValues", {

  simulations <- data.table(x = 1:1000, y = as.numeric(1:1000), z = as.character(1:1000))
  simulations_result <- list(x = -5.5, y = -5.5)
  
  # Check shortfall
  expect_identical(getShortfallValues(simulations), simulations_result)
  expect_identical(getShortfallValues(simulations[, .(x)]), simulations_result[1])
  expect_identical(getShortfallValues(simulations[, .(z)]), simulations_result[integer(0)])
})


