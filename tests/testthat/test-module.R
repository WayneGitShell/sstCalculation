context("Module")


test_that("getNewTargetFXRate", {
  # Initial FX table
  FXrate <- data.table(from = c("USD", "EUR", "GBP", "JPY"), to = c("CHF", "CHF", "CHF", "CHF"), fx = c(0.1, 1:3) ,rowNumber = 1:4)
  # Converted table
  FXrateUSD <- data.table(from = c("CHF", "EUR", "GBP", "JPY"), to = c("USD", "USD", "USD", "USD"), fx = c(10, 10, 20, 30))
  FXrateGBP <- data.table(from = c("CHF", "USD", "EUR", "JPY"), to = c("GBP", "GBP", "GBP", "GBP"), fx = c(0.5, 0.05, 0.5, 1.5))
  
  # Classical case
  expect_identical(getNewTargetFXRate("USD", FXrate), FXrateUSD)
  expect_identical(getNewTargetFXRate("GBP", FXrate), FXrateGBP)
})

test_that("getFullFXrate", {
  # Initial FX table
  FXrate <- data.table(from = c("USD", "EUR", "GBP", "CHF", "EUR"), to = c("CHF", "CHF", "CHF", "USD", "USD"), fx = c(0.1, 1:4) ,rowNumber = 1:5)

  result <- rbind(FXrate[1, !c("rowNumber")], getNewTargetFXRate("USD", FXrate[1]))  
  expect_identical(getFullFXrate(FXrate[1])[, !c("rowNumber")], result)
})
