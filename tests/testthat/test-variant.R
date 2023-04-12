context("Variant")

variantNames <- c("Base case" = "", "Variant V1" = "V1", "Variant V2" =  "V2", "Variant V3" = "V3")
test_that("getVariant", {
  path <- system.file("extdata", c("Testfile.xlsx", "TestfileVariant.xlsx"), package="sstCalculation")
  
  # No variant
  x <- getVariant(path[1])
  y <- variantNames[1]
  expect_identical(x, y)
  
  # Variant
  x <- getVariant(path[2])
  y <- variantNames
  expect_identical(x, y)
  
  # Variant
  x <- getVariant(path)
  y <- variantNames
  expect_identical(x, y)
})


test_that("getVariantFromSheets", {
  sheetsList <- list(a = c("A", "B", "Test", "ABC__V3"), b = list("A__V1", "X__V2"), c = list("Z"))
  
  # Classical case
  x <- getVariantFromSheets(sheetsList)
  y <- variantNames
  expect_identical(x, y)

  # No variant
  x <- getVariantFromSheets(sheetsList[3])
  y <- variantNames[1]
  expect_identical(x, y)
  
  # One variant
  x <- getVariantFromSheets(sheetsList[1])
  y <- variantNames[c(1,4)]
  expect_identical(x, y)
})
