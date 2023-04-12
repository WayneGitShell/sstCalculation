context("Error handling")


test_that("cleanup",{
  # Ensure the content is correctly reset
  packageEnv$pb <- utils::txtProgressBar(0, 1)
  packageEnv$errorLog <- 1:2
  packageEnv$param <- 1:2

  expect_silent(cleanup())
  testCorrectEnvironment()
  
  # Ensure two cleanup work
  cleanup()
  cleanup()
  testCorrectEnvironment()
})

test_that("Progress bar",{
  
  expect_output(createProgressBar("Progress bar name", 3), "Progress bar name")
  expect_equal(class(packageEnv$pb), "txtProgressBar")
  expect_output(addProgress(1), "33%", fixed = TRUE)
  expect_identical(utils::getTxtProgressBar(packageEnv$pb), 1)
  expect_silent(cleanup())
  
  # Flooring fixed the rounding issue
  expect_output({
    createProgressBar("Progress bar", 1)
    for(i in 1:8){expect_output(addProgress(1/9))}
  })
  
  expect_output(addProgress(1/9), "100%", fixed = TRUE)
  expect_silent(cleanup())
})


test_that("safeEvaluation (correct output)",{
  cleanup()
  
  # Correct evaluation
  expect_output(safeEvaluation(cat("abc")), "abc")
  expect_identical(safeEvaluation("abcd"),"abcd")
  expect_identical(safeEvaluation(1+100),101)
})

test_that("safeEvaluation (debug mode)",{
  cleanup()

  # Correct error handling
  expect_silent(safeEvaluation(stop("error")))
  expect_silent(safeEvaluation(warning("warning")))
  expect_silent(safeEvaluation(addError("test", "error message", debugMode = FALSE)))
  expect_silent(safeEvaluation(addError("test", "warning message", type = "Warning", debugMode = FALSE)))

  errorLog <- data.table(keyword = c("", "", "test", "test"),
                         type = c("Error", "Error", "Error", "Warning"),
                         row = NA_integer_, col = NA_integer_, sheet = NA_character_, description = NA_character_,
                         message = c("An unexpected error was thrown: error", "An unexpected warning was thrown: warning", "error message", "warning message"),
                         company = "")
  expect_identical(packageEnv$errorLog, errorLog)

  # Stop after first unexpected warning or error
  cleanup()
    expect_silent(safeEvaluation({
    warning("warning1")
    warning("warning2")
  }))
  expect_identical(packageEnv$errorLog$message, "An unexpected warning was thrown: warning1")

  # Stop after first error
  cleanup()
  expect_silent(safeEvaluation({
    addError(NA, "warning message 1", type = "Warning", debugMode = FALSE)
    addError(NA, "warning message 2", type = "Warning", debugMode = FALSE)
    addError(NA, "error message 1", type = "Error", debugMode = FALSE)
    addError(NA, "error message 2", type = "Error", debugMode = FALSE)
  }))

  errorLog <- data.table(keyword = "", type = c("Warning", "Warning", "Error"), 
                         row = NA_integer_, col = NA_integer_, sheet = NA_character_, description = NA_character_, 
                         message = c("warning message 1", "warning message 2", "error message 1"), company = "")
  expect_identical(packageEnv$errorLog, errorLog)
})


test_that("messageTableToMessage", {
  cleanup()
  addError(NA, "warning message 1", type = "Warning", debugMode = FALSE)
  addError(NA, "warning message 2", type = "Warning", debugMode = FALSE)
  
  expect_identical(messageTableToMessage(packageEnv$errorLog), "Warning\n - warning message 1\n - warning message 2")
  
})


test_that("toNum", {
  x <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
  expect_identical(toNum(x), c(1, -1, 1, 1, -1))
  expect_identical(toNum(x[integer(0)]), numeric(0))
})

test_that("formatList", {
  x <- as.list(stats::setNames(1:8, LETTERS[1:8]))
  expect_output(formatList(x, "Element"), "- Element 'A'\n- Element 'B'\n- Element 'C'\n- Element 'D'\n- Element 'E'\n- Element 'F'\n- (and 2 more elements)", fixed = TRUE)
  expect_output(formatList(x[1:7], "Element"), "- Element 'A'\n- Element 'B'\n- Element 'C'\n- Element 'D'\n- Element 'E'\n- Element 'F'\n- (and 1 more element)", fixed = TRUE)
  expect_output(formatList(x[1:2], "Element"), "- Element 'A'\n- Element 'B'", fixed = TRUE)
  expect_silent(formatList(x[integer(0)], "Element"))
  })
