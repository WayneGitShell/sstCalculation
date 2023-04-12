# Update the workbook with the specified values
updateWorkbook <- function(wb, config, values){
  textContent = list()
  numContent = list()
  FX_to_CHF <- getValue(values, "FX_to_CHF")
  
  # New worksheet with a specified number of rows and columns
  for(i in which(config$action == "addWorksheet")){
    u <- config[i]
    textContent[[u$sheet]] <- matrix(NA_character_, nrow = u$row1, ncol = u$col1)
    numContent[[u$sheet]] <- matrix(NA_real_, nrow = u$row1, ncol = u$col1)
    openxlsx::addWorksheet(wb, u$sheet, gridLines = FALSE)
  }
  
  # Font for the worksheet
  for(i in which(config$action == "addBaseFont")){
    u <- config[i]
    openxlsx::modifyBaseFont(wb, fontSize = as.integer(u$numericValue), fontName = u$textValue)
  }
  
  # Add text values specified in the configuration file and from the SST-Template
  for(i in which(config$action %in% c("addText", "addTextFromExcel"))){
    u <- config[i]
    textContent[[u$sheet]][u$row1, u$col1] <- u$textValue
  }
  
  # Add numeric values from specified cells from the SST-Template
  for(i in which(config$action == "addNumericFromExcel")){
    u <- config[i]
    data <- suppressWarnings(as.numeric(u$textValue)) * ifelse(u$withFX == 1, FX_to_CHF, 1)
    value <- numContent[[u$sheet]][u$row1, u$col1]
    if(is.na(data)){
      data <- 0
    }
    if(is.na(value)){
      value <- 0
    }
    numContent[[u$sheet]][u$row1, u$col1] <- value + data
  }
  
  # Add numeric values which were calculated by this R-Tool
  for(i in which(config$action == "addNumericFromKeyword")){
    u <- config[i]
    numContent[[u$sheet]][u$row1, u$col1] <- as.numeric(getValue(values, u$textValue)) * ifelse(u$withFX == 1, FX_to_CHF, 1)
  }
  
  # Write the text and numeric values separately
  for(i in which(config$action == "writeValues")){
    u <- config[i]
    if(u$style == "textContent"){
      data <- textContent[[u$sheet]][u$row1:u$row2, u$col1:u$col2]
    }else{
      data <- numContent[[u$sheet]][u$row1:u$row2, u$col1:u$col2]
    }
    openxlsx::writeData(wb = wb, sheet = u$sheet, x = data, colNames = FALSE, rowNames = FALSE, startRow = u$row1, startCol = u$col1)
  }
  
  # Apply styles
  for(i in which(config$action == "addStyle")){
    u <- config[i]
    openxlsx::addStyle(wb, sheet = u$sheet, style = styles[[u$style]], rows = u$row1:u$row2, cols = u$col1:u$col2, gridExpand = TRUE, stack = u$withFX == 1)
  }
  
  # Column width
  for(i in which(config$action == "addColWidth")){
    u <- config[i]
    openxlsx::setColWidths(wb, sheet = u$sheet, cols = u$col1, u$numericValue)
  }
  
  # Formats
  for(i in which(config$action == "addFormat")){
    u <- config[i]
    style <- openxlsx::createStyle(numFmt = u$textValue)
    openxlsx::addStyle(wb, sheet = u$sheet, style = style, rows = u$row1:u$row2, cols = u$col1:u$col2, gridExpand = TRUE, stack = TRUE)
  }
  return(NULL)
}
