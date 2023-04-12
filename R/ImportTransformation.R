# Filter a table to remove all trailing rows.
transformRemoveTrailingRows <- function(A, columns){
  colName <- setdiff(names(A), c(columns, "rowNumber"))
  index <- rowSums(!is.na(A[, colName, with = FALSE])) != 0
  A <- A[seq_len(max(c(0, which(index)))), ]
  return(A)
}

# Filter a table to remove all empty rows.
transformRemoveEmptyRows <- function(A, indicator){
  if(indicator){
    index <- rowSums(!is.na(A[, -"rowNumber"])) != 0
    A <- A[index, ]
  }
  return(A)
}

# Fill down with the same value while the value is not NA
transformAutoComplete <- function(A, columns){
  if(length(columns) == 1){
    v <- A[[columns]]
    for(i in seq_len(length(v)-1)){
      if(is.na(v[i+1])){
        v[i+1] <- v[i]
      }
    }
    A[[columns]] <- v
  }
  return(A)
}

# Filter a table to keep only the rows matching a filter condition, which is specified through a column name and the value it should contain.
transformFilter <- function(A, columns, filters){
  addError(NA, "Internal error: incorrect filtering", condition = !(length(columns) == length(filters)))
  for(i in seq_along(columns)){
    
    pattern <- filters[i]
    isExclude <- substr(pattern, 1, 11) == "[[EXCLUDE]]"
    pattern <- gsub("[[EXCLUDE]]", "", pattern, fixed = TRUE)
    
    index <- grepl(pattern = pattern, x = A[[columns[i]]])
    if(isExclude){
      index <- !index
    }
    A <- A[index, ]
  }
  return(A)
}


# Replace NA by 0
transformReplaceNA <- function(A, columns){
  # Warning: it will also modify the object by reference
  # A <- copy(A)
  for(colName in columns){
    addError(NA, "Internal error: Replacement of NA over non-character columns", condition = class(A[[colName]]) != "character")
    index <- is.na(A[[colName]])
    A[index, (colName) := "0"]
  }
  return(A[])
}


# Split the rows of a data.table whenever a ',' is found in the specified column
transformSplitRows <- function(A, columns, keyword = NA){
  if(length(columns) == 1){
    addError(NA, "Internal error: The selected column should be a character column", condition = !is.character(A[[columns]]))
    addErrorCell(keyword, "Invalid input, the cell should not contain the string ',,'", columns = columns, rows = A$rowNumber[grepl(",,",A[[columns]], fixed = TRUE)])
    error <- substr(A[[columns]], 1, 1) == "," | substr(A[[columns]], sapply(A[[columns]], nchar), sapply(A[[columns]], nchar)) == ","
    addErrorCell(keyword, "Invalid input, the cell should not begin or end with ','", columns = columns, rows = A$rowNumber[error])
    
    A <- A[, lapply(.SD, function(x) trimws(unlist(tstrsplit(x, ",", fixed=TRUE)))), by = setdiff(names(A), columns), .SDcols = columns]
  }
  return(A)
}


# Transpose from narrow to wide
transformTranspose <- function(A, columns){
  if(length(columns) == 0){
    return(A)
  }
  
  A <- melt(data = A, measure.vars = columns, value.name = "value", variable.name = "time", variable.factor = FALSE)
  A[, time := as.integer(time)]
  return(A[])
}


# Transform into a correlation matrix
transformCorrelation <- function(A, keyword = NA, columnsToExclude = character(0), correlationTolerance = getParam("correlationTolerance")){
  
  rowNumber <- getValue(A, "rowNumber")
  rowNames <- getValue(A, "name")
  
  matData <- as.matrix(A[, -c("name", "rowNumber", columnsToExclude), with=FALSE])
  rownames(matData) <- rowNames
  
  addError(keyword, paste0("The correlation matrix needs to be a square matrix. The input provided has ", nrow(matData), " rows and ", ncol(matData), " columns"), columns = "name", condition = nrow(matData) != ncol(matData))
  
  addMatrixError <- function(U, message){
    u <- which(U > correlationTolerance, arr.ind = TRUE)
    error <- list(row = u[, "row"], col = colnames(matData)[u[, "col"]])
    addErrorCell(keyword, message, rows = rowNumber[error$row], columns = error$col)
  }
  
  # Ensure that the correlation matrix is well defined
  addMatrixError(diag(abs(diag(matData) - 1)), "Diagonal terms must be equal to '1'")
  addMatrixError(abs(matData - t(matData)), "Non-symmetric matrix")
  addMatrixError(abs(matData) -1, "Correlation larger than 1")
  
  matData <- (matData + t(matData))/2
  diag(matData) <- 1
  
  indicator <- !all(eigen(matData, symmetric = T, only.values = T)$values >= 0)
  addError(keyword, "The correlation matrix has negative eigenvalues", condition = indicator)
  colnames(matData) <- rownames(matData)
  return(matData)
}


# Transform into a named vector
transformNamedVector <- function(A, columnValue, columnLabel){
  if(length(columnValue) == 0){
    return(A)
  }
  A <- stats::setNames(object = A[[columnValue]], nm = A[[columnLabel]])
  return(A)
}


# Cast the column into the correct type
transformColumnTypes <- function(Table, colTypes, keyword){
  Table <- copy(Table)
  
  lapplyCatch(colTypes, function(colType){
    set(Table, j = colType$name, value = transformType(Table, column = colType$name, type = colType$type, keyword = keyword))
  })
  return(Table)
}


# Transform the value into a specified type
transformType <- function(Table, column, type, keyword){
  values <- getValue(Table, column)
  oldValues <- values
  rows <- Table[["rowNumber"]]
  
  # Convert to text
  if(type == "Text"){
    # Empty cells are considered as an empty string.
    values[is.na(values)] <- ""
    return(values)
  }
  
  # Convert to logical
  if(type == "Logical"){
    # Empty or NA are interepreted as FALSE
    values[is.na(values)] <- "No"
    values[values == ""] <- "No"
    # Binary values
    values[values == "0"] <- "No"
    values[values == "1"] <- "Yes"
    index <- !values %in% c("Yes", "No")
    
    addErrorCell(keyword, "The value should be either 'Yes' or 'No'", rows = rows[index], columns = column)
    
    values <- values == "Yes"
    return(values)
  }
  
  # Convert to numeric
  if(type == "Numeric"){
    values <- suppressWarnings(as.numeric(values))
  }else if(type == "Integer"){
    numericValue <- suppressWarnings(as.numeric(values))
    
    # Identify entries which were not integers
    index <-  is.na(numericValue) | (numericValue != as.integer(numericValue))
    values[index] <- NA
    
    values <- as.integer(values)
  }else{
    stop("Incorrect type")
  }
  
  # When the value is NA and was not NA before, then there is information loss while transforming the cell.
  index <- !is.na(oldValues) & is.na(values)
  addErrorCell(keyword, paste0("Can't convert '", oldValues[index], "' into ", ifelse(type == "Numeric", "a real number", "an integer")), rows = rows[index], columns = column)
  return(values)
}
