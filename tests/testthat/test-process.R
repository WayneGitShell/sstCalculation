context("Process")


instrumentAsset <- data.table(label = c("equity", "hedge fund"), currency = c("CHF", "USD"), value = c(1.1, 2), rowNumber = 1:2)
instrumentAssetForward <- data.table(contact = c("Contact 1", "Contact 2"), type = "equity", currency = c("EUR", "CHF"), time = 1:2, exposure = 20, price = 22, position = c("short", "long"), rowNumber = 1:2)
instrumentFX_Forward <- data.table(contract = c("Contact 1", "Contact 2"), time = 1:2, nominal = c(0.5, 0.6), rate = c(1.1, 1.2), foreign = c("EUR", "USD"), position = c("short", "long"), rowNumber = 1:2)
instrumentFixedIncome <- data.table(currency = "CHF", rating = "AAA", marketValue = c(1, 0, 1, 1), spread = NA_real_, rowNumber = 1:4, time = 1, value = c(0, 1, -1, 1))
instrumentLiability <- data.table(type = c("Leben", "Kranken", "Kranken"), currency = c("CHF", "CHF", "EUR"), rowNumber = 1:3, time = 1L, value = c(10, 20, 40))

mappingDeltaRF <- data.table(name = c("EURCHF", "RF1", "RF2"), newName = c("EURCHF", "RF1", "RF2"), coef = 1, type = c("currency", "nonCurrency", "nonCurrency"))



# Asset
test_that("process_instrumentAsset",{
  instrumentAsset_result <- instrumentAsset[, .(instrument = "Equity", currency, time = NA_integer_, exposure = value, rowNumber, label, rating = NA_character_, name = NA_character_, spread = NA_real_, marketValue = NA_real_)]
  
  expect_identical(process_instrumentAsset(instrumentAsset), instrumentAsset_result)
  expect_identical(process_instrumentAsset(instrumentAsset[rep(1, 10)]), instrumentAsset_result[rep(1, 10)])
  expect_identical(process_instrumentAsset(instrumentAsset[integer(0)]), instrumentAsset_result[integer(0)])
})


# Asset forward
test_that("instrumentAssetForward",{
  instrumentAssetForward_result <- data.table(instrument = c(rep("Asset forward underlying", 2), rep("Asset forward delivery", 2)), currency = c("EUR", "CHF", "EUR", "CHF"), time = c(NA, NA, 1L, 2L), exposure = c(-20, 20, 22, -22), rowNumber = c(1:2, 1:2), label = c("equity", "equity", NA, NA), rating = NA_character_, name = NA_character_, spread = NA_real_, marketValue = NA_real_)

  expect_identical(process_instrumentAssetForward(instrumentAssetForward), instrumentAssetForward_result)
  expect_identical(process_instrumentAssetForward(instrumentAssetForward[rep(1, 10)]), instrumentAssetForward_result[rep(c(1,3), each = 10)])
  expect_identical(process_instrumentAssetForward(instrumentAssetForward[integer(0)]), instrumentAssetForward_result[integer(0)])
})


# FX forward
test_that("process_instrumentFX_Forward",{
  instrumentFX_Forward_result <- data.table(instrument = c("FX forward underlying", "FX forward underlying", "FX forward delivery", "FX forward delivery"), currency = c("EUR", "USD", "CHF", "CHF"), time = c(1:2,1:2), exposure = c(-0.5, 0.6, 0.55, -0.72), rowNumber = c(1:2, 1:2), label = NA_character_, rating = NA_character_, name = NA_character_, spread = NA_real_, marketValue = NA_real_)

  expect_identical(process_instrumentFX_Forward(instrumentFX_Forward, referenceCurrency = "CHF"), instrumentFX_Forward_result)
  expect_identical(process_instrumentFX_Forward(instrumentFX_Forward[rep(1, 10)], referenceCurrency = "CHF"), instrumentFX_Forward_result[rep(c(1,3), each = 10)])
  expect_identical(process_instrumentFX_Forward(instrumentFX_Forward[integer(0)], referenceCurrency = "CHF"), instrumentFX_Forward_result[integer(0)])

  # Incorrect foreign currency
  expect_error(process_instrumentFX_Forward(instrumentFX_Forward, referenceCurrency = "EUR"), "The foreign currency should not be equal to the SST-currency")
})


# Fixed income
test_that("process_instrumentFixedIncome (consistency)",{
  instrumentFixedIncome_result <- data.table(instrument = "Fixed income", currency = "CHF", time = 1, exposure = 1, rowNumber = 4L, label = NA_character_, rating = "AAA", name = NA_character_, spread = NA_real_, marketValue = 1)
  
  expect_identical(process_instrumentFixedIncome(instrumentFixedIncome[4]), instrumentFixedIncome_result)
  
  # Consistency checks
  expect_error(process_instrumentFixedIncome(instrumentFixedIncome[1]), "The total market value should be zero when no cash-flows are provided")
  expect_error(process_instrumentFixedIncome(instrumentFixedIncome[2]), "The total market value should not be zero when cash-flows are provided")
  expect_error(process_instrumentFixedIncome(instrumentFixedIncome[3]), "The spread is required since some cash-flows are negative")
})


# Insurance liability
test_that("process_instrumentLiability",{
  instrumentLiability_result <- data.table(instrument = "Insurance liabilities", currency = c("CHF", "EUR"), time = 1L, exposure = c(-30, -40), rowNumber = c(1L, 3L), label = NA_character_, rating = NA_character_, name = NA_character_, spread = NA_real_, marketValue = NA_real_)

  expect_identical(process_instrumentLiability(instrumentLiability), instrumentLiability_result)
})


# Delta
test_that("process_instrumentDelta",{

  instrumentDelta <- data.table(name = c("USDCHF", "EURCHF", "RF1", "RF2"), unit = c("%", "%", "bp", "bp"), exposure = c(0, 90, 2800, 3800), rowNumber = 1:4)
  instrumentDelta_result <- data.table(instrument = "Delta terms", currency = NA_character_, time = NA_integer_, exposure = c(90, 2800, 3800), rowNumber = 2:4, label = NA_character_, rating = NA_character_, name = c("EURCHF", "RF1", "RF2"), spread = NA_real_, marketValue = NA_real_)
  # instrument_copy <- copy(instrument) # added
  
  expect_identical(process_instrumentDelta(instrumentDelta[-1], "CHF", mappingDeltaRF), instrumentDelta_result)
  # expect_identical(instrument, instrument_copy) I can't figure out the purpose!
  
  expect_error(process_instrumentDelta(instrumentDelta[integer(0)], "CHF", mappingDeltaRF), "risk factor 'EURCHF' is missing")
  expect_error(process_instrumentDelta(instrumentDelta, "CHF", mappingDeltaRF), "element USDCHF is not a valid delta risk factor")
  expect_error(process_instrumentDelta(instrumentDelta[3:4], "CHF", mappingDeltaRF), "currency risk factor 'EURCHF' is missing")
  expect_error(process_instrumentDelta(instrumentDelta[2:3], "CHF", mappingDeltaRF), "risk factor 'RF2' is missing")

})


test_that("process_instrumentFixedIncome (spread)",{
  instrument <- data.table(currency = "USD", rating = "AAA", marketValue = 1e6, spread = NA_real_, 
                           rowNumber = 1, time = 1, value = 1e6)
  expect_silent(process_instrumentFixedIncome(instrument))
  
  instrument <- data.table(currency = "CHF", rating = "AAA", marketValue = 0, spread = 0, rowNumber = 1, 
                           time = 1, value = 1e6)
  expect_error(process_instrumentFixedIncome(instrument),
                        "The total market value should not be zero when cash-flows are provided")
  
  instrument <- data.table(currency = "CHF", rating = "AAA", marketValue = 1e6, spread = 0, rowNumber = 1, 
                           time = 1, value = 0)
  expect_error(process_instrumentFixedIncome(instrument),
               "The total market value should be zero when no cash-flows are provided")
  
  instrument <- data.table(currency = "CHF", rating = "AAA", marketValue = 1e6, spread = NA_real_, 
                           rowNumber = 1, time = 1, value = -10)
  expect_error(process_instrumentFixedIncome(instrument),
               "The spread is required since some cash-flows are negative")

})
