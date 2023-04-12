# Get a table from the template
getTable <- function(configTable, template){
  sheet <- getValue(configTable$value, "sheet")
  row <- getValue(configTable$value, "row")
  col <- getValue(configTable$value, "col")
  name <- getValue(configTable$columns, "Name")
  isValue <- getValue(configTable$indicator, "isValue")
  
  A <- getValue(template, sheet, objectCategory = "sheet")
  
  if(isValue){
    A <- extractValue(A, row, col, name)
    
  }else{
    A <- extractTable(A, row, col, name)
  }
  return(A)
}

extractValue <- function(mat, row, col, name){
  a <- ifelse(row > nrow(mat) || col > ncol(mat), NA_character_, mat[row, col])
  A <- data.table(a)
  names(A) <- name
  A[, rowNumber := 1]
  return(A)
}

extractTable <- function(mat, row, col, name){
  nbColumnAdd <- max(max(col) - ncol(mat), 0)
  A <- extendMatrix(mat, nbColumnAdd)
  A <- data.table(A[, col, drop = FALSE])
  
  names(A) <- name
  A[, rowNumber := seq_len(.N)]
  A <- A[rowNumber >= unique(row)]
  return(A)
}

extendMatrix <- function(mat, nbColumnAdd){
  matExtension <- matrix(NA_character_, nrow = nrow(mat), ncol = nbColumnAdd)
  matExtended <- cbind(mat, matExtension)
  return(matExtended)
}

# Get the structure of the Excel file. It specified the coordinates of each table and the column types.
getStructure <- function(excelStructure, colRange){
  
  column <- list(
    AutoComplete = excelStructure$name[excelStructure$characteristic == "AutoComplete"],
    Skip = excelStructure$name[excelStructure$dataSelection == "Skip"],
    Split = excelStructure$name[excelStructure$characteristic == "Split"],
    NamedVectorValue = excelStructure$name[excelStructure$characteristic == "Named vector"],
    NamedVectorName = excelStructure$name[excelStructure$characteristic == "Primary key"]
  )
  
  columns <- list(
    FilterValue = excelStructure$filterValue[excelStructure$filterValue != ""],
    FilterName = excelStructure$name[excelStructure$filterValue != ""],
    ValueRequired = excelStructure$name[excelStructure$isValueRequired == TRUE],
    ReplacementZero = excelStructure$name[excelStructure$isReplacementZero == TRUE],
    PrimaryKey = excelStructure$name[excelStructure$characteristic == "Primary key"],
    Name = excelStructure$name,
    Transpose = excelStructure$name[excelStructure$characteristic == "Transpose"]
  )
  
  value <- list(
    Filter = excelStructure$filterValue[!is.na(excelStructure$filterValue)],
    sheet = excelStructure$sheet[1],
    description = excelStructure$description[1],
    row = excelStructure$row[1],
    col = excelStructure$col
  )
  
  indicator <- list(
    isEmptyAllowed = excelStructure$isEmptyAllowed[1],
    isDropEmptyRows = excelStructure$isDropEmptyRows[1],
    isValue = excelStructure$isValue[1],
    isCorrelation = excelStructure$isCorrelation[1]
  )
  
  keyword <- excelStructure$keyword[1]
  
  range <- colRange[[keyword]]
  if(is.null(range)){
    ranges <- list()
  }else{
    ranges <- lapply(seq_len(nrow(range)), function(i){
      as.list(range[i])
    })
  }
  
  colTypes <- lapply(1:nrow(excelStructure), function(i){list(name = excelStructure$name[i], type = excelStructure$type[i])})
  
  x <- list(keyword = keyword, column = column, columns = columns, value = value, indicator = indicator, ranges = ranges, colTypes = colTypes)
  x <- namedObject(x, "Structure of the Excel file", "Element")
  return(x)
}

# Access to a cell from the SST-Template, which will be  used for the production of the FDS.
getTemplateValue <- function(excelTemplate, sheet, row, col){
  A <- getValue(excelTemplate, sheet, objectCategory = "worksheet")
  if(row > nrow(A) | col > ncol(A)){
    addError(NA, paste0("Can't access to the row ", row, " and column ", col, " from worksheet '", sheet, "'."), type = "Warning")
    return(NA_character_)
  }else{
    return(A[row, col])
  }
}

# Identify the sheets associated to the variant
getSheetsMapping <- function(sheets, variant){
  sheetsMapping <- data.table(rawName = sheets, sheetName = sheets)
  sheetsMapping[, isVariant := paste0("__", variant) == substr(rawName, nchar(rawName) - 3, nchar(rawName))]
  sheetsMapping[isVariant == TRUE , sheetName := substr(rawName, 1, nchar(rawName)-4)]
  sheetsRemove <- sheetsMapping[isVariant == TRUE, sheetName]
  sheetsMapping[!rawName %in% sheetsRemove, .(sheet = sheetName, newSheet = rawName)]
}

# Initialization phase. The configuration from the Excel file is loaded
getExcelStructure <- function(configurationTable, sheetsMapping, companyName){
  # Prevent modification by reference
  excelStructure <- copy(configurationTable)
  
  excelStructure[sheetsMapping, sheet := i.newSheet, on = "sheet"]
  
  # Add company name
  excelStructure[, company := companyName]
  
  return(excelStructure[])
}

# Get a table with one entry per row for checking that the values in a column are in the allowed range
getColRange <- function(excelStructure){
  colRange <- melt(excelStructure, id.vars = c("keyword", "name"), measure.vars = c("Warning", "Error"), variable.name = "type", na.rm = TRUE, variable.factor = FALSE)
  colRange <- colRange[, .(range = as.character(unlist(tstrsplit(value, ",")))), by = c("keyword", "name", "type")]
  colRange <- split(colRange, by = "keyword")
  return(colRange)
}