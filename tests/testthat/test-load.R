context("Loading data")

test_that("Correct loading Excel file",{
  path <- system.file("extdata", "Testfile.xlsx", package="sstCalculation")
  
  data <- matrix(as.character(c(1, 2, 3, NA, NA, NA, 5, 6)), ncol = 2)
  data_result <- importTemplate(path = path, nbTemplates = 1, pathCache = NULL)
  expect_identical(data, data_result$`Sheet A`)
  
  data <- matrix(nrow = 0, ncol = 0)
  expect_identical(data, data_result$`Sheet B`)

  data <- matrix(as.character(c("Merged1", NA, NA, NA, "Merged", "A")), ncol = 2)
  expect_identical(data, data_result$`Sheet C`)
})
