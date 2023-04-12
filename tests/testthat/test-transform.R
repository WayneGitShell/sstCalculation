context("Transform")


A <- data.table(A = c(1, NA, 2, 3, NA, NA), B = c(1, NA, NA, NA, NA, 1), C = c("X", NA, "Y", "Y", NA, NA), rowNumber = 1)
Azeros <- data.table(A = c(1, NA, 2, 3, NA, NA), B = c(1, NA, NA, NA, NA, 1), C = c("X", "0", "Y", "Y", "0", "0"), rowNumber = 1)

B <- copy(A)

test_that("transformAutoComplete",{
  values <- data.table(x = c(1, NA, NA, 2, NA, 3), y = c("a", NA, "b", NA, NA, NA))
  valuesFilled <- data.table(x = c(1, 1, 1, 2, 2, 3), y = c("a", "a", "b", "b", "b", "b"))
  
  # No impact
  expect_identical(values, transformAutoComplete(values, character(0)))
  expect_identical(values, transformAutoComplete(values, c("x", "y")))
  
  expect_identical(transformAutoComplete(values, "x"), data.table(x = c(1, 1, 1, 2, 2, 3), y = values$y))
  expect_identical(transformAutoComplete(values, "y"), data.table(x = values$x, y = c("a", "a", "b", "b", "b", "b")))
  expect_identical(transformAutoComplete(values[-1], "x"), data.table(x = c(NA, NA, 2, 2, 3), y = values$y[-1]))
})
 


test_that("transformTranspose", {
  tableType <- data.table(A = c(as.character(2:3), NA), B = c(as.character(1:2 + 0.5), NA), C = c(LETTERS[1:2], NA), D = rep(NA_character_, 3), E = c("Yes", "No", ""), EE = c(0, 1, NA), rowNumber = 1:3)
  tableType_copy <- copy(tableType)
  # Text
  expect_identical(transformType(Table = tableType, column = "A", type = "Text", keyword = NA), c("2", "3", ""))
  expect_identical(transformType(Table = tableType, column = "B", type = "Text", keyword = NA), c("1.5", "2.5", ""))
  expect_identical(transformType(Table = tableType, column = "C", type = "Text", keyword = NA), c("A", "B", ""))
  expect_identical(transformType(Table = tableType, column = "D", type = "Text", keyword = NA), rep("", 3))

  # Numeric
  expect_identical(transformType(Table = tableType, column = "A", type = "Numeric", keyword = NA), c(2, 3, NA))
  expect_identical(transformType(Table = tableType, column = "B", type = "Numeric", keyword = NA), c(1.5, 2.5, NA))
  expect_error(transformType(Table = tableType, column = "C", type = "Numeric", keyword = NA), "Can't convert 'A' into a real number", fixed = TRUE)
  expect_identical(transformType(Table = tableType, column = "D", type = "Numeric", keyword = NA), rep(NA_real_, 3))

  # Integer
  expect_identical(transformType(Table = tableType, column = "A", type = "Integer", keyword = NA), c(2L, 3L, NA_integer_))
  expect_error(transformType(Table = tableType, column = "B", type = "Integer", keyword = NA), "Can't convert '1.5' into an integer", fixed = TRUE)
  expect_error(transformType(Table = tableType, column = "C", type = "Integer", keyword = NA), "Can't convert 'A' into an integer", fixed = TRUE)
  expect_identical(transformType(Table = tableType, column = "D", type = "Integer", keyword = NA), rep(NA_integer_, 3))

  # Logical (classical case)
  x <- transformType(Table = tableType, column = "E", type = "Logical", keyword = NA)
  y <- c(TRUE, FALSE, FALSE)
  expect_identical(x, y)
  
  # Logical (binary input)
  x <- transformType(Table = tableType, column = "EE", type = "Logical", keyword = NA)
  y <- c(FALSE, TRUE, FALSE)
  expect_identical(x, y)
  
  # Logical (NA are false)
  x <- transformType(Table = tableType, column = "D", type = "Logical", keyword = NA)
  y <- c(FALSE, FALSE, FALSE)
  expect_identical(x, y)
  
  # Incorrect column type
  expect_error(transformType(Table = tableType[1], column = "A", type = "Logical", keyword = NA), "The value should be either 'Yes' or 'No'", fixed = TRUE)
  expect_error(transformType(Table = tableType[2], column = "C", type = "Logical", keyword = NA), "The value should be either 'Yes' or 'No'", fixed = TRUE)
  
  # Object not altered
  expect_identical(tableType, tableType_copy)
})


test_that("transformNamedVector", {
  # Data set only relevant for this test
  tableName <- data.table(A = LETTERS[1:10], B = letters[1:10], C = 1:10)
  tableName_copy <- copy(tableName)

  expect_equal(transformNamedVector(tableName, columnValue = character(0), columnLabel = character(0)), tableName)
  expect_equal(transformNamedVector(tableName, columnValue = "A", columnLabel = "B"), unlist(setNames(tableName$A, tableName$B)))
  expect_equal(transformNamedVector(tableName, columnValue = "B", columnLabel = "A"), unlist(setNames(tableName$B, tableName$A)))
  expect_equal(transformNamedVector(tableName, columnValue = "A", columnLabel = "A"), unlist(setNames(tableName$A, tableName$A)))
  expect_equal(transformNamedVector(tableName, columnValue = "C", columnLabel = "A"), unlist(setNames(tableName$C, tableName$A)))

  # Object not altered
  expect_identical(tableName, tableName_copy)
})

test_that("transformCorrelation", {
  # Data set only relevant for this test
  tableCorrelation <- data.table(name = LETTERS[1:2], A = c(1, 0), B = c(0, 1), rowNumber = 1:2)
  tableCorrelation_copy <- copy(tableCorrelation)
  tableCorrelation_result <- matrix(c(1, 0, 0, 1), 2, dimnames = list(LETTERS[1:2], LETTERS[1:2]))
  tableCorrelation_resultPos <- matrix(c(1, 0.5*1e-9, 0.5*1e-9, 1), 2, dimnames = list(LETTERS[1:2], LETTERS[1:2]))

  # Additional column
  tableCorrelation_extended <- data.table(name = LETTERS[1:2], additionalName = letters[1:2], A = c(1, 0), B = c(0, 1), rowNumber = 1:2)
  
  # 2x2 correlation
  expect_identical(transformCorrelation(tableCorrelation), tableCorrelation_result)
  expect_identical(transformCorrelation(tableCorrelation_extended, columnsToExclude = "additionalName"), tableCorrelation_result)
  expect_error(transformCorrelation(tableCorrelation_extended), "The correlation matrix needs to be a square matrix. The input provided has 2 rows and 3 columns")
  # 1x1 correlation
  expect_identical(transformCorrelation(tableCorrelation[1, c("name", "A", "rowNumber")]), tableCorrelation_result[1, 1, drop = F])
  
  # Diagonal terms
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A + 1e-4, B, rowNumber)]), "Diagonal terms must be equal to '1'")
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A - 1e-4, B, rowNumber)]), "Diagonal terms must be equal to '1'")

  # Symmetry check
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A + c(0, 1e-4), B, rowNumber)]), "Non-symmetric matrix")
  expect_identical(transformCorrelation(tableCorrelation[, .(name, A = A + c(0, 1e-9), B, rowNumber)]), tableCorrelation_resultPos)

  # Tolerance parameter
  expect_silent(transformCorrelation(tableCorrelation[, .(name, A = A + c(0, 1e-4), B, rowNumber)], correlationTolerance = 1))
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A + c(0, 1e-4), B, rowNumber)], correlationTolerance = 1e-5), "Non-symmetric matrix")
  
  # Diagonal check
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A + 1e-4, B, rowNumber)]), "Diagonal terms must be equal to '1'")
  expect_identical(transformCorrelation(tableCorrelation[, .(name, A = A + 1e-9, B, rowNumber)]), tableCorrelation_resultPos)

  # Large value check
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A + c(0, 1 + 1e-4), B + c(1 + 1e-4, 0), rowNumber)]), "Correlation larger than 1")

  # Positive definite check
  expect_error(transformCorrelation(tableCorrelation[, .(name, A = A + c(0, 1 + 1e-9), B + c(1 + 1e-9, 0), rowNumber)]), "The correlation matrix has negative eigenvalues")

  # Object not altered
  expect_identical(tableCorrelation, tableCorrelation_copy)
})


test_that("transformTranspose", {
  # Data set only relevant for this test
  tableTranspose <- data.table(A = LETTERS[1:2], LOB = letters[1:2], "1" = c(0.1, 0.8), "2" = c(-1, -2))
  tableTranspose_copy <- copy(tableTranspose)
  tableTranspose_result <- data.table(A = c("A", "B", "A", "B"), LOB = c("a", "b", "a", "b"), time = rep(1:2, each = 2), value = c(0.1, 0.8, -1, -2))

  expect_equal(transformTranspose(tableTranspose, columns = c("1", "2")), tableTranspose_result)
  expect_equal(transformTranspose(tableTranspose[integer(0)], columns = c("1", "2")), tableTranspose_result[integer(0)])
  expect_equal(transformTranspose(tableTranspose[, -"LOB"], columns = c("1", "2")), tableTranspose_result[, -"LOB"])
  expect_equal(transformTranspose(tableTranspose[, c("1", "2")], columns = c("1", "2")), tableTranspose_result[, c("time", "value")])

  # Object not altered
  expect_identical(tableTranspose, tableTranspose_copy)
})


test_that("transformSplitRows", {
  # Data set only relevant for this test
  split <- data.table(A = c("a", "a,b", "a   , b", "a ,,b", ",", ",a", ","), rowNumber = 1:7)
  split_copy <- copy(split)
  split_result <- data.table(rowNumber = c(1, 2, 2, 3, 3), A = c("a", "a", "b", "a", "b"))


  expect_error(transformSplitRows(split[4], columns = "A"), "the cell should not contain the string ',,'")
  expect_error(transformSplitRows(split[5], columns = "A"), "Invalid input, the cell should not begin or end with ','")
  expect_error(transformSplitRows(split[6], columns = "A"), "Invalid input, the cell should not begin or end with ','")
  expect_equal(transformSplitRows(split[1:3], columns = "A"), split_result)
  expect_equal(transformSplitRows(split[c(2, 2)], columns = "A"), split_result[c(2, 2, 3, 3)])
  expect_equal(transformSplitRows(split[integer(0)], columns = "A"), split_result[integer(0)])

  # No split
  expect_equal(transformSplitRows(split, columns = character(0)), split)

  # Object not altered
  expect_identical(split, split_copy)
})


test_that("transformReplaceNA", {

  expect_equal(transformReplaceNA(copy(A), "C"), Azeros)
  expect_equal(transformReplaceNA(copy(A)[integer(0)], "C"), Azeros[integer(0)])
  expect_equal(transformReplaceNA(copy(A)[c(2, 5, 6)], "C"), Azeros[c(2, 5, 6)])

  # Sanity check
  expect_error(transformReplaceNA(copy(A), "A"), "Internal error: Replacement of NA over non-character columns")

  # The object is modified by reference for performance reasons.
  # # Object not altered
  # expect_identical(A, B)
})


test_that("transformFilter", {
  expect_equal(transformFilter(A, columns = "A", filters = "1"), A[1])
  expect_equal(transformFilter(A, columns = "A", filters = 5), A[integer(0)])
  expect_equal(transformFilter(A, columns = character(0), filters = character(0)), A)
  
  # Exclusion
  expect_equal(transformFilter(A, columns = "A", filters = "[[EXCLUDE]]1"), A[-1])
  expect_equal(transformFilter(A, columns = "A", filters = "[[EXCLUDE]]5"), A)
  
  # Mutiple columns
  expect_equal(transformFilter(A, columns = c("A", "C"), filters = c("[[EXCLUDE]]2", "Y")), A[4])
  
  # Regular expression
  expect_equal(transformFilter(A, columns = "C", filters = "^(X)|(Y)$"), A[c(1,3, 4)])
  
  # Sanity check
  expect_error(transformFilter(A, columns = "A", filters = character(0)), "Internal error: incorrect filtering")
  expect_error(transformFilter(A, columns = character(0), filters = "1"), "Internal error: incorrect filtering")
  
  # Object not altered
  expect_identical(A, B)
})

test_that("transformRemoveEmptyRows", {
  expect_equal(transformRemoveEmptyRows(A, indicator = TRUE), A[-c(2, 5)])
  expect_equal(transformRemoveEmptyRows(A[c(2, 5)], indicator = TRUE), A[integer(0)])
  expect_equal(transformRemoveEmptyRows(A[integer(0)], indicator = TRUE), A[integer(0)])
  expect_equal(transformRemoveEmptyRows(A[-c(2, 5)], indicator = TRUE), A[-c(2, 5)])

  expect_equal(transformRemoveEmptyRows(A, indicator = FALSE), A)
  expect_equal(transformRemoveEmptyRows(A[2], indicator = FALSE), A[2])

  # Object not altered
  expect_identical(A, B)
})

test_that("transformRemoveTrailingRows", {

  expect_equal(transformRemoveTrailingRows(A, columns = character()), A)
  expect_equal(transformRemoveTrailingRows(A[1:5], columns = character()), A[1:4])
  expect_equal(transformRemoveTrailingRows(A[integer(0)], columns = character()), A[integer(0)])
  expect_equal(transformRemoveTrailingRows(A[1], columns = character()), A[1])

  expect_equal(transformRemoveTrailingRows(A, columns = c("A")), A)
  expect_equal(transformRemoveTrailingRows(A, columns = c("B")), A[1:4])
  expect_equal(transformRemoveTrailingRows(A[1:5], columns = c("A", "C")), A[1])
  expect_equal(transformRemoveTrailingRows(A[integer(0)], columns = c("A")), A[integer(0)])

  # Object not altered
  expect_identical(A, B)
}
)
