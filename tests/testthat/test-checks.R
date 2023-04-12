context("Checks")

test_that("expectedShortfall", {
  # Constant case
  x <- rep(-10, 1000)
  expect_identical(expectedShortfall(x, alpha = 0.01), 10)
  # Local constant case
  x <- c(rep(-500, 200), rep(0, 800))
  expect_identical(expectedShortfall(x, 0.1), 500)
  # Tricky case
  x <- c(rep(-500, 50), rep(-300, 100), rep(0, 850))
  expect_identical(expectedShortfall(x, 0.1), 400)
  # Random
  set.seed(1)
  x <- sample.int(1000)
  expect_identical(expectedShortfall(x, 0.01), -5.5)
})



test_that("checkRF", {
  expect_error(checkRF(keyword = NA, vector = c("A", "B", "C"), vectorRF = c("A", "B")), "risk factor 'C' could not be found in the market risk correlation matrix")
  expect_error(checkRF(keyword = NA, vector = c("A", "B"), vectorRF = c("A", "B", "C")), "risk factor 'C' is defined in the market risk correlation matrix but could not be found in this table")
  expect_silent(checkRF(keyword = NA, vector = c("A", "B"), vectorRF = c("A", "B")))
})


test_that("checkMissingValues", {
  A <- data.table(someColumn = c(1:5, NA), someColumn1 = c(NA, 1:5), rowNumber = 1:6)

  expect_error(checkMissingValues(A, c("someColumn", "someColumn1"), keyword = NA), "cell should be non-empty")
  expect_error(checkMissingValues(A, c("someColumn"), keyword = NA), "cell should be non-empty")
  expect_silent(checkMissingValues(A[-6], c("someColumn"), keyword = NA))
})


test_that("checkEmptyTable", {
  expect_error(checkEmptyTable(A = data.table(A = integer(0)), keyword = NA, emptyAllowed = FALSE), "table should not be empty")
  expect_silent(checkEmptyTable(A = data.table(A = integer(0)), keyword = NA, emptyAllowed = TRUE))
})


test_that("checkPrimaryKey", {
  A <- data.table(A = c(1, 1, 3), B = c(1, 1, 2), rowNumber = 1:3)
  expect_error(checkPrimaryKey(A = A, columns = c("A", "B"), keyword = NA), "Duplicate values are not allowed")

  A <- data.table(A = 1:10, B = 1:5, rowNumber = 1:10)
  expect_silent(checkPrimaryKey(A = A, columns = c("A", "B"), keyword = NA))
})


test_that("checkRange", {

  Table <- data.table(maturity = c(49, 50, 51, 1e6), unit = c("%", "bp", "%", "p"), shortlong = c("short", "sHoRt", "LonG", "shong"), increase = c(2, 3, 4, 1),
                      curr = c("EUR", "CHF", "chf", "DDD"), rf = c("CHF1", "CHF2", "chf3", "DDD"), empty = c("", "", "a", ""),
                      horizon = c("k", "m", "l", "L"), standalone = c("equity", "hedge fund", "Equity", "DDD"),
                      nltype = c("simulations", "lognormal parameters", "no nonlife risk", "aviation"),
                      negative = (-1):2, rowNumber = 1:4)
  currencies = c("EUR", "CHF")
  riskFactors = c("CHF1", "CHF2")

  # Negative
  expect_error(checkRange(ranges = list(name = "negative", range = "Negative", type ="Error"), Table, NA, currencies, riskFactors), "value should not be negative")
  expect_error(checkRange(ranges = list(name = "negative", range = "Negative", type ="Error"), Table, "instrumentLiability", currencies, riskFactors), "negative liability cash-flows are considered as gains")
  expect_silent(checkRange(ranges = list(name = "negative", range = "Negative", type ="Error"), Table[-1], NA, currencies, riskFactors))


  # Zero
  expect_error(checkRange(ranges = list(name = "negative", range = "Zero", type ="Error"), Table, NA, currencies, riskFactors), "value should not be zero")
  expect_warning(checkRange(ranges = list(name = "negative", range = "Zero", type ="Warning"), Table, NA, currencies, riskFactors), "value is usually not zero")
  expect_silent(checkRange(ranges = list(name = "negative", range = "Zero", type ="Error"), Table[-2], NA, currencies, riskFactors))


  # Positive
  expect_error(checkRange(ranges = list(name = "negative", range = "Positive", type ="Error"), Table, NA, currencies, riskFactors), "value should not be positive")
  expect_silent(checkRange(ranges = list(name = "negative", range = "Positive", type ="Error"), Table[1:2], NA, currencies, riskFactors))

  # LargerEqualOne
  expect_error(checkRange(ranges = list(name = "negative", range = "LargerOne", type ="Error"), Table[3], NA, currencies, riskFactors), "value should not be larger than 100% or equal to 100%")
  expect_warning(checkRange(ranges = list(name = "negative", range = "LargerOne", type ="Warning"), Table[4], NA, currencies, riskFactors), "value is usually not larger than 100% or equal to 100%")
  expect_silent(checkRange(ranges = list(name = "negative", range = "LargerOne", type ="Error"), Table[1:2], NA, currencies, riskFactors))

  # Larger50
  expect_error(checkRange(ranges = list(name = "maturity", range = "Larger50", type ="Error"), Table, NA, currencies, riskFactors), "value should not be larger than 50")
  expect_error(checkRange(ranges = list(name = "maturity", range = "Larger50", type ="Error"), Table[4], NA, currencies, riskFactors), "value should not be larger than 50")
  expect_silent(checkRange(ranges = list(name = "maturity", range = "Larger50", type ="Warning"), Table[1], NA, currencies, riskFactors))

  # notUnit
  expect_error(checkRange(ranges = list(name = "unit", range = "notUnit", type ="Error"), Table[4], NA, currencies, riskFactors), "value should be either 'bp' or '%'")
  expect_silent(checkRange(ranges = list(name = "unit", range = "notUnit", type ="Error"), Table[-4], NA, currencies, riskFactors))

  # notLongShort
  expect_error(checkRange(ranges = list(name = "shortlong", range = "notLongShort", type ="Error"), Table, NA, currencies, riskFactors), "value should be either 'short' or 'long'")
  expect_silent(checkRange(ranges = list(name = "shortlong", range = "notLongShort", type ="Error"), Table[1:3], NA, currencies, riskFactors))

  # notIncreasing
  expect_error(checkRange(ranges = list(name = "increase", range = "notIncreasing", type ="Error"), Table, NA, currencies, riskFactors), "values in the column 'F(x)' need to be increasing", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "increase", range = "notIncreasing", type ="Error"), Table[1:3], NA, currencies, riskFactors))
  expect_silent(checkRange(ranges = list(name = "increase", range = "notIncreasing", type ="Error"), Table[integer(0)], NA, currencies, riskFactors))

  # notStrictlyIncreasing
  expect_error(checkRange(ranges = list(name = "increase", range = "notStrictlyIncreasing", type ="Error"), Table[c(1, 2, 2, 3)], NA, currencies, riskFactors), "values in the column 'x' need to be strictly increasing", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "increase", range = "notStrictlyIncreasing", type ="Error"), Table[1:3], NA, currencies, riskFactors))
  expect_silent(checkRange(ranges = list(name = "increase", range = "notStrictlyIncreasing", type ="Error"), Table[integer(0)], NA, currencies, riskFactors))

  # notContainOne
  expect_error(checkRange(ranges = list(name = "increase", range = "notContainOne", type ="Error"), Table[1:3], NA, currencies, riskFactors), "last value in the column 'F(x)' should be equal to 1", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "increase", range = "notContainOne", type ="Error"), Table, NA, currencies, riskFactors))
  expect_silent(checkRange(ranges = list(name = "increase", range = "notContainOne", type ="Error"), Table[integer(0)], NA, currencies, riskFactors))

  # notCurrency
  expect_error(checkRange(ranges = list(name = "curr", range = "notCurrency", type ="Error"), Table, NA, currencies, riskFactors), "No initial exchange rate is defined for the currency 'chf'", fixed = TRUE)
  expect_error(checkRange(ranges = list(name = "curr", range = "notCurrency", type ="Error"), data.table(curr = paste0(letters, collapse = ""), rowNumber = 1), NA, currencies, riskFactors), "No initial exchange rate is defined for the currency 'abcdefghijklmno'", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "curr", range = "notCurrency", type ="Error"), Table[1:2], NA, currencies, riskFactors))

  # notRiskFactor
  expect_error(checkRange(ranges = list(name = "rf", range = "notRiskFactor", type ="Error"), Table, NA, currencies, riskFactors), "'chf3' is not defined as a risk factor", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "rf", range = "notRiskFactor", type ="Error"), Table[1:2], NA, currencies, riskFactors))

  # notEmpty
  expect_error(checkRange(ranges = list(name = "empty", range = "notEmpty", type ="Error"), Table, NA, currencies, riskFactors), "value should be empty", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "empty", range = "notEmpty", type ="Error"), Table[-3], NA, currencies, riskFactors))

  # notHorizon
  expect_error(checkRange(ranges = list(name = "horizon", range = "notHorizon", type ="Error"), Table, NA, currencies, riskFactors), "value should be either 'k', 'm' or 'l'", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "horizon", range = "notHorizon", type ="Error"), Table[-4], NA, currencies, riskFactors))

  # notStandalone
  expect_error(checkRange(ranges = list(name = "standalone", range = "notStandalone", type ="Error"), Table, NA, currencies, riskFactors), "'Equity' is not a valid standalone", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "standalone", range = "notStandalone", type ="Error"), Table[1:2], NA, currencies, riskFactors))

  # notNonLifeType
  expect_error(checkRange(ranges = list(name = "nltype", range = "notNonLifeType", type ="Error"), Table, NA, currencies, riskFactors), "'aviation' is not a valid selection for the non-life simulation type", fixed = TRUE)
  expect_silent(checkRange(ranges = list(name = "nltype", range = "notNonLifeType", type ="Error"), Table[1:3], NA, currencies, riskFactors))

  # Incorrect range
  expect_error(checkRange(ranges = list(name = "nltype", range = "someRange", type ="Error"), Table[1:3], NA, currencies, riskFactors), "Internal error: unknown range 'someRange'")
})


test_that("checkSeed", {
  expect_error(checkSeed("abc"), "Invalid seed")
  expect_error(checkSeed(NA), "Invalid seed")
  expect_silent(checkSeed(0))
})


test_that("checkSim", {
  expect_error(checkSim(NULL), "provide the number of simulation")
  expect_error(checkSim(1:5), "Invalid number of simulations")
  expect_error(checkSim(NA), "Invalid number of simulations")
  expect_error(checkSim("NA"), "Invalid number of simulations")
  expect_error(checkSim(1000.1), "should be an integer")
  expect_error(checkSim(0), "number of simulations should be at least 1000")
  expect_error(checkSim(-10000), "number of simulations should be at least 1000")
  expect_error(checkSim(1e8), "number of simulations for one run should be at most 10 mio")
  expect_error(checkSim(1e88), "number of simulations for one run should be at most 10 mio")
  expect_silent(checkSim(1e6))
  expect_silent(checkSim(1e3))
})

test_that("checkPath", {
  expect_error(checkPath(NULL), "provide a path")
  expect_error(checkPath(list("a", "b")), "Invalid path")
  expect_error(checkPath(tempfile()), "does not exist")

  tmp <- tempfile(fileext = ".xls")
  on.exit(unlink(tmp), add = TRUE)
  fwrite(list(a=1), file = tmp)
  expect_error(checkPath(tmp), "Incorrect file extension")

  tmp <- tempfile(fileext = ".xlsx")
  on.exit(unlink(tmp), add = TRUE)
  fwrite(list(a=1), file = tmp)
  expect_silent(checkPath(tmp))
})

