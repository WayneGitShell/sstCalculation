# Process the error log to add metadata
getCompleteErrorLog <- function(inputErrorLog, Table){
  # Make a copy otherwise we would modify the log table by reference
  errorLog <- copy(inputErrorLog)
  
  # Add metadata from the configuration Table
  errorLog[unique(Table, by = "keyword"), ':='(description = i.description, sheet = i.sheet, rowConfig = as.integer(i.row), col = as.integer(i.col), isValue = i.isValue, company = i.company), on = "keyword"]
  errorLog[Table, col := as.integer(i.col), on = c("keyword", "column"="name")]
  
  # If the error concerns one cell, use the row from this cell
  errorLog[isValue == TRUE, row := rowConfig]
  
  # If now row is specified, use the header row of the table (first row minus one)
  errorLog[is.na(row), row := rowConfig - 1L]
  
  # Fill missing values
  errorLog[is.na(keyword), keyword := ""]
  errorLog[is.na(company), company := ""]
  outputErrorLog <- errorLog[, .(keyword, type, row, col, sheet, description, message, company)]
  
  return(outputErrorLog)
}


# Evaluate the expression and catch any unexpected error/warning
safeEvaluation <- function(expr){
  # The R-code should always return an error using the errorHandling class.
  # If an error or a warning is thrown, this is not expected and will be caught. In this case, an error with errorHandling class is thrown with the content of the error/warning.
  result <- tryCatch({
    tryCatch(expr,
             error = function(e){addError(NA, paste0("An unexpected error was thrown: ", e$message), debugMode = FALSE)},
             warning = function(e){addError(NA, paste0("An unexpected warning was thrown: ", e$message), debugMode = FALSE)}
    )
  }, errorHandling = function(e){}
  )
  return(result)
}


# Evaluate the expression in a clean environment
process <- function(expr, main = TRUE, resultsForShinyDashboard = FALSE, debugMode = identical(getOption("sstCalculation.debugMode"), TRUE)){
  on.exit(closePb())
  if(!main | debugMode){
    return(expr)
  }
  cleanup()
  on.exit(cleanup())
  result <- safeEvaluation(expr)
  
  resultFinal <- finalizeProzess(result, errorLog = getConfig("errorLog"), resultsForShinyDashboard = resultsForShinyDashboard)
  return(resultFinal)
}


# Return the error message
finalizeProzess <- function(result, errorLog, resultsForShinyDashboard){
  messages <- messageTableToMessage(errorLog)
  
  if(resultsForShinyDashboard){
    return(list(x = result, messages = gsub("\n", "<br>", messages), errorLog = errorLog, messageOriginal = messages))
  }
  
  if(nrow(errorLog) > 0){
    if(any(errorLog$type == "Error")){
      stop(messages, call. = FALSE)
    }else{
      warning(messages, call. = FALSE)
    }
  }
  return(result)
}


# Catch error while doing a lapply
lapplyCatch <- function(X, FUN, ...){
  numberError <- sum(packageEnv$errorLog$type == "Error")
  FUN1 <- function(...){
    tryCatch(FUN(...), errorHandling = function(e){NULL})  
  }
  results <- lapply(X = X, FUN = FUN1, ...)
  numberErrorNew <- sum(packageEnv$errorLog$type == "Error")
  
  
  if(numberErrorNew > numberError){
    stop(caughtError()) 
  }
  return(results)
}


# Class for error/warnings caught within the package
caughtError <- function() {
  structure(class = c("errorHandling", "condition"), list(message = "The error was added to the error log.", call = NULL))
}


# Throw an error or warning message
addError <- function(keyword, message, columns = NA, condition = length(keyword) > 0 && length(message) > 0, type = "Error", ignore = FALSE, debugMode = identical(getOption("sstCalculation.debugMode"), TRUE)){
  if(length(condition) != 1 || is.na(condition) || !is.logical(condition) || is.null(condition)){
    stop("incorrect error trigger")
  }
  
  if(condition && length(columns) > 0){
    errorLog <- data.table(keyword = as.character(keyword), row = as.integer(NA), column = as.character(columns), type = as.character(type), message = as.character(message))
    errorLog <- getCompleteErrorLog(errorLog, Table = packageEnv$excelStructure)
    
    sendErrorsIfDebugMode(errorLog, debugMode)
    
    packageEnv$errorLog <- rbind(packageEnv$errorLog, errorLog)
    
    if(any(errorLog$type == "Error") && !ignore){
      stop(caughtError())
    }
  }
}


# Error handling for debug mode
sendErrorsIfDebugMode <- function(errorLog, debugMode){
  if(debugMode){
    for(i in seq_len(nrow(errorLog))){
      u <- errorLog[i]
      if(u$type == "Error"){
        stop(u$message)
      }else{
        warning(u$message)
      }
    }
  }
  
}


# Throw an error or warning message for a specific cell
addErrorCell <- function(keyword, message, rows, columns, type = "Error", debugMode = identical(getOption("sstCalculation.debugMode"), TRUE)){
  if(length(rows) != 0 && length(columns) != 0){
    errorLog <- data.table(keyword = as.character(keyword), row = as.integer(rows), column = as.character(columns), type = as.character(type), message = as.character(message))
    
    errorLog <- getCompleteErrorLog(errorLog, Table = packageEnv$excelStructure)
    
    sendErrorsIfDebugMode(errorLog, debugMode = debugMode)
    
    packageEnv$errorLog <- rbind(packageEnv$errorLog, errorLog)
    
    if(any(errorLog$type == "Error")){
      stop(caughtError())
    }
  }
}


# Create an error message based on a message table
messageTableToMessage <- function(errorLog){
  if(nrow(errorLog) == 0){
    return("")
  }
  
  errorLog <- copy(packageEnv$errorLog)
  
  errorLog[!is.na(description), ':=' (header = paste0(type," in sheet '", sheet, "' with the ", description, " (company '", company, "')"), prefix = paste0(" - In cell ",openxlsx::int2col(col), row," : "))]
  errorLog[is.na(description), ':=' (header = type, prefix = " - ")]
  errorLog <- unique(errorLog, by = c("sheet", "row", "col", "message"))
  setorder(errorLog, "sheet", "col", "row")
  
  errors <- sapply(split(errorLog, by = c("keyword", "type")), getMessages)
  prefix <- ifelse(any(errorLog$type == "Error"), "Some errors occurred.\n\n", "")
  errors <- paste0(prefix, paste(errors, collapse = "\n\n"))
  return(errors)
}


# Format the error message in an user-friendly way 
getMessages <- function(A){
  errors <- unique(A[, paste0(prefix, message)])
  n <- length(errors)
  if(n>5){errors <- c(errors[1:5], paste0(" - (and ", n-5, " additional entries)"))}
  errors <- paste(errors, collapse = "\n")
  paste(c(A$header[1], errors), collapse = "\n")
}
