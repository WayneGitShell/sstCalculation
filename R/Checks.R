# Ensure that risk factors are consistent
checkConsistencyRF <- function(correlationMarket, marketVolatility, scenarioMacroEconomic){
  vectorRF <- rownames(correlationMarket)
  
  # The correlation matrix should match the volatility vector
  vector <-   names(marketVolatility)
  checkRF("marketVolatility", vector, vectorRF)
  
  # The macroeconomic scenarios need to have a participation risk factor and the other names should match
  vector <- scenarioMacroEconomic$name
  addError("scenarioMacroEconomic", "The participation shocks are missing", condition = ! "participation" %in% vector)
  checkRF("scenarioMacroEconomic", setdiff(vector, "participation"), vectorRF)
}


# Check the consistency of the MVM cashflows
checkConsistencyCashFlowsMVM <- function(MVM_LifeIndicator, volatility){
  rowsWithError <- MVM_LifeIndicator[name %in% names(volatility[volatility != 0]) != anyValue, rowNumber]
  addErrorCell("MVM_CashFlowsLife", "Expected cash-flows should be provided exactly when the sensitivity to the risk factor is non-zero", rows = rowsWithError, columns = "name")  
}


# Identify missing names
checkNames <- function(namesA, namesB, keyword, tableName){
  missingNames <- setdiff(namesA, namesB)
  addError(keyword, paste0("The risk factor '", missingNames, "' defined here is missing in the ", tableName, " table"), condition = length(missingNames)>0)
}


# Get all model inputs
getModelInputs <- function(path){
  pathTemplate <- getPathTemplate(path)
  pathConfig <- getPathConfig(path)
  pathConcatenated <- c(pathTemplate, pathConfig)
  
  # Identify the variants
  variants <- getVariant(pathConcatenated)
  
  # Total number of times that a template will be loaded
  nbTemplates <- length(pathConcatenated) * length(variants)
  
  # Load the model input for each variant
  modelInputs <- lapply(variants, getModelInput, pathTemplate = pathTemplate, pathConfig = pathConfig, nbTemplates = nbTemplates)
  return(modelInputs)
}


# Get the model inputs from a variant
getModelInput <- function(variant, pathTemplate, pathConfig, nbTemplates){
  
  # Load templates
  templates <- lapply(pathTemplate, importTemplate, nbTemplates = nbTemplates)
  configs <- lapply(pathConfig, importTemplate, nbTemplates = nbTemplates)
  
  # Merge the configuration templates with the templates
  consolidatedTemplates <- lapply(templates, getConsolidatedTemplate, configs = configs)
  rm("templates", "configs")
  
  # The sub-model inputs are named according to the name specified by the company in the SST template
  subModelInputs <- lapply(consolidatedTemplates, getSubModelInput, variant = variant)
  names(subModelInputs) <- unlist(getSubValue(subModelInputs, "companyName"))
  
  # Some processing is required
  processedSubModelInputs <- getProcessedObjectsAllTemplates(subModelInputs)
  
  return(processedSubModelInputs)
}


# Extract the configuration inputs
getConfigurationData <- function(configurationDataInput, pathConfiguration){
  if(is.null(pathConfiguration)){
    addError(NA, "Incorrect configuration table, please ensure that you are using a valid SST-Template", condition = ncol(configurationDataInput) < 18)
    configurationData <- configurationDataInput[, 1:18]
    
  }else{
    # Used by FINMA only to use a different configuration path
    configurationData <- as.matrix(fread(pathConfiguration, header = F, na.strings = ""))
  }
  
  return(as.data.table(configurationData))
  
}


# Process a SST-Template to extract all relevant inputs
getSubModelInput <- function(consolidatedTemplate, variant, pathConfiguration = getOption("sstCalculation.pathConfiguration")){
  # Ensure that the template is compatible with the R Tool
  if(is.null(pathConfiguration)){
    checkCompatibility(consolidatedTemplate)
  }
  
  # Extract two worksheets
  introData <- getValue(consolidatedTemplate, "Intro")
  configurationDataInput <- getValue(consolidatedTemplate, "Configuration")
  
  # We load the company name. The tool displays the name of the company for which an error was produced.
  # This is useful to distinguish between an error from the mother and daughter company
  companyName <- getCellValue(introData, 9, 5, FALSE, "(no company name)")
  
  # Configuration used to generate error messages
  configurationData <- getConfigurationData(configurationDataInput, pathConfiguration)
  configurationTable <- getConfiguration(configurationData)
  sheetsMapping <- getSheetsMapping(sheets = names(consolidatedTemplate), variant = variant)
  excelStructure <- getExcelStructure(configurationTable, sheetsMapping, companyName)
  
  # Save structure in the package environment, which is used to generate meaningful error messages
  packageEnv$excelStructure <- excelStructure
  
  # List of structures
  excelStructures <- split(excelStructure, by = "keyword") 
  objectsStructure <- lapply(excelStructures, getStructure, colRange = getColRange(excelStructure))
  
  # List of tables
  Tables <- lapplyCatch(objectsStructure, getTable, template = consolidatedTemplate)
  
  # List of processed tables
  excelInputs <- lapplyCatch(objectsStructure, getExcelInputs, Tables = Tables)
  
  # Apply some post-processing
  excelInputs <- processExcelInputs(excelInputs, sheetsMapping, excelStructure, companyName, consolidatedTemplate, variant)
  excelInputs <- namedObject(excelInputs, "Excel file inputs", "Input")
  
  return(excelInputs)
}

# Parse the configuration file for the FDS
getExternalConfigurationFDS <- function(pathFDS){
  # The first line is not relevant
  configurationFDS <- fread(pathFDS, encoding = "UTF-8")[-1]
  configurationFDS[is.na(numericValue), numericValue := 0]
  names(configurationFDS)[names(configurationFDS) == 'content'] <- 'action'
  configurationFDS[, rowNumber := seq(.N)]
  return(configurationFDS)
}

# Apply post-processing
processExcelInputs <- function(excelInputs, sheetsMapping, excelStructure, companyName, consolidatedTemplate, variant, pathFDS = getOption("sstCalculation.pathConfigurationFDS")){
  
  # Use the user-specified FDS configuration (FINMA only)
  if(!is.null(pathFDS)){
    excelInputs$configurationFDS <- getExternalConfigurationFDS(pathFDS)
  }
  
  # Extract data (for the FDS) from the correct variant of the worksheet
  excelInputs$configurationFDS[sheetsMapping, sheetSource := i.newSheet, on = c("sheetSource" = "sheet")]
  excelInputs$configurationFDS[action %in% c("addTextFromExcel", "addNumericFromExcel"), textValue := getTemplateValue(consolidatedTemplate, sheet = sheetSource, row = row2, col = col2), by = "rowNumber"]
  
  # Prepare data export for the FDS in the right FDS variant
  variantSheetName <- ifelse(variant == "", "", paste0("__", variant))
  excelInputs$configurationFDS[, sheet := paste0(sheet, variantSheetName)]
  
  # Add the suffix to the sheet name only if it is "schattenrechnung" also only add 15 first because else sheet name is too long
  suffixSheetName <- excelInputs$fileNameSuffix
  if(grepl("_sr_[0-9]{4}", tolower(suffixSheetName))){
    excelInputs$configurationFDS[, sheet := paste0(sheet, substr(suffixSheetName, 1, 15))]
  }
  
  # We sometimes need to use the name of the variant later on for analysis
  excelInputs$variant <- variant
  excelInputs$excelStructure <- excelStructure
  excelInputs$companyName <- companyName
  return(excelInputs)
}


# Main function to check that the provided table is correct
getExcelInputs <- function(configTable, Tables, pb = NULL, ...){
  
  riskFactors <- getValue(Tables, "marketVolatility")$name[!is.na(Tables$marketVolatility$name)]
  currencies <- unique(c(getValue(Tables, "initialFX")$from, getValue(Tables, "initialFX")$to))
  LGDClass <- getValue(Tables, "mappingLGD")$class
  
  keyword <- configTable$keyword
  
  Table <- getValue(Tables, keyword)
  
  Table <- copy(Table)
  
  # Get parameters from the configuration list
  isValue <- configTable$indicator$isValue
  columnSkip <- configTable$column$Skip
  columnAutoComplete <- configTable$column$AutoComplete
  isDropEmptyRows <- configTable$indicator$isDropEmptyRows
  columnsFilterName <- configTable$columns$FilterName
  columnsFilterValue <- configTable$columns$FilterValue
  columnsValueRequired <- configTable$columns$ValueRequired
  columnsReplacementZero <- configTable$columns$ReplacementZero
  columnsPrimaryKey <- configTable$columns$PrimaryKey
  isEmptyAllowed <- configTable$indicator$isEmptyAllowed
  columnsNames <- configTable$columns$Name
  columnSplit <- configTable$column$Split
  columnsTranspose <- configTable$columns$Transpose
  isCorrelation <- configTable$indicator$isCorrelation
  columnNamedVectorName <- configTable$column$NamedVectorName
  columnNamedVectorValue <- configTable$column$NamedVectorValue
  ranges <- configTable$ranges
  colTypes <- configTable$colTypes
  
  # Remove unnecessary rows (only for tables, not for values)
  if(!isValue){
    Table <- transformRemoveTrailingRows(Table, columns = columnSkip)
    Table <- transformRemoveEmptyRows(Table, indicator = isDropEmptyRows)
    Table <- transformAutoComplete(Table, columns = columnAutoComplete)
    Table <- transformFilter(Table, columns = columnsFilterName, filters = columnsFilterValue)
  }
  
  # Processing for the correlation matrix
  if(isCorrelation){
    Table <- getResizedTable(Table, columns = columnAutoComplete)
    columnsValueRequired <- intersect(columnsValueRequired, names(Table))
    columnsReplacementZero <- intersect(columnsValueRequired, names(columnsReplacementZero))
    colTypes <- colTypes[seq_len(ncol(Table)-1)]
  }
  
  checkMissingValues(Table, columns = c(columnSplit, columnsValueRequired), keyword = keyword)
  
  # Replace NA by 0
  Table <- transformReplaceNA(Table, columns = columnsReplacementZero)
  
  # Check that no duplicate value are provided
  checkPrimaryKey(Table, columns = columnsPrimaryKey, keyword = keyword)
  
  # Return an error if the table is empty
  checkEmptyTable(Table, emptyAllowed = isEmptyAllowed, keyword = keyword)
  
  # Enforce the correct column type (character/numeric/integer) to all columns
  Table <- transformColumnTypes(Table, colTypes = colTypes, keyword = keyword)
  
  # A cell containing a comma-separated list of value is transformed in multiple cells with each one value
  Table <- transformSplitRows(Table, columns = columnSplit)
  
  # Check that the values are valid
  lapply(ranges, checkRange, Table = Table, keyword = keyword, riskFactors = riskFactors, currencies = currencies, LGDClass = LGDClass)
  
  # Apply an unpivot operation to the table
  Table <- transformTranspose(Table, columns = columnsTranspose)
  
  # Create a correlation matrix from the table
  if(isCorrelation){
    # We exclude any column for which an autocompletion has been performed.
    # This is useful if we need to extract a correlation matrix where the rows are filtered after an autocompletion has been performed.
    Table <- transformCorrelation(Table, keyword = keyword, columnsToExclude = columnAutoComplete)
  }
  
  # Create a named vector from the table
  Table <- transformNamedVector(Table, columnValue = columnNamedVectorValue, columnLabel = columnNamedVectorName)
  if(isValue){
    Table <- Table$none[1]
  }
  return(Table)
}


# Remove unnecessary columns to the table
getResizedTable <- function(Table, columns){
  
  # The columns either contain labels or correlation values
  labelColumns <- unique(c("name", "rowNumber", columns))
  correlationColumns <- setdiff(names(Table), labelColumns)
  
  # The maximal number of column is specified in the configuration table
  # If there are too many rows, the correlation matrix can't be loaded.
  correlationSize <- nrow(Table)
  addError(keyword, "There are too many columns in the correlation matrix", condition = length(correlationColumns) < correlationSize)
  
  # Remove all unused columns to have a square correlation matrix
  columnsKeep <- c(labelColumns, correlationColumns[seq_len(correlationSize)])
  columnsRemove <- setdiff(names(Table), columnsKeep)
  
  # The columns removed should all be empty
  for(column in columnsRemove){
    index <- !is.na(Table[[column]])
    addErrorCell(keyword, "The cell should be empty", columns = column, rows = Table$rowNumber[index])
  }
  
  # Square correlation matrix with labels
  Table <- Table[, columnsKeep, with = FALSE]
  return(Table)
}


# Check that the path is correct
checkPath <- function(path){
  addError(NA, "Please provide a path", condition = is.null(path))
  addError(NA, "Invalid path", condition = !is.character(path) || length(path) != 1 || anyNA(path))
  addError(NA, paste0("The path '", path, "' does not exist"), condition = !file.exists(path))
  addError(NA, "Incorrect file extension, should be .xlsx", condition = substr(path, nchar(path) - 4, nchar(path)) != ".xlsx")
  return(as.character(path))
}


# Check that the number of simulation is valid
checkSim <- function(nsim){
  addError(NA, "Please provide the number of simulation", condition = is.null(nsim))
  addError(NA, "Invalid number of simulations", condition = !is.numeric(nsim) || length(nsim) != 1 || anyNA(nsim))
  
  # Do this before casting as integer in case nsim is too large
  addError(NA, "The number of simulations for one run should be at most 10 mio.", condition = nsim > 1e7 )
  addError(NA, "The number of simulations should be an integer", condition = as.integer(nsim) != nsim)
  addError(NA, "The number of simulations should be at least 1000", condition = nsim < 1000)
  addError(NA, "The number of simulations should be a mulitple of 100", condition = nsim %% 100 != 0)
  
  return(as.integer(nsim))
}


# Check that the seed is valid
checkSeed <- function(seed){
  addError(NA, "Invalid seed", condition = !is.numeric(seed) || length(seed) != 1 || anyNA(seed) || as.integer(seed) != seed || seed < 0)
  return(as.integer(seed))
}


# Check that each instrument is well defined
checkReference <- function(market, targetCurrency, A, colNames, removeTargetCurrency = FALSE){
  index <- rowSums(is.na(A[, colNames, with = FALSE])) == 0 & (A$currency != targetCurrency | removeTargetCurrency == FALSE)
  U <- market[A[index], on = colNames, .(error = rowNumber, rows = i.rowNumber, keyword)][is.na(error), .(keyword, rows)]
  addErrorCell(U$keyword, "no risk factor is associated to this instrument", columns = NA, rows = U$rows)
}


# Check that the values from the specified column from the input Table belong to the correct range
checkRange <- function(ranges, Table, keyword, currencies, riskFactors, LGDClass){
  column <- ranges$name
  rows <- Table$rowNumber
  range <- ranges$range
  type <- ranges$type
  
  values <- Table[[column]]
  indexKeep <- !(is.na(values) | values == "")
  
  values <- values[indexKeep]
  rows <- rows[indexKeep]
  n <- length(values)
  
  if(range == "Negative"){
    index <- values < 0
    if(identical(keyword, "instrumentLiability")){
      additionalMessage <- ". Note that negative liability cash-flows are considered as gains"
    }else if(identical(keyword, "portfolioCreditRisk") | identical(keyword, "portfolioCreditRiskCheck")){
      additionalMessage <- ". Note that cash-flows are only required when the instrument is considered in the credit risk model and when migration risk is enabled. Please refer to the credit risk technical documentation for more details"
    }else{
      additionalMessage <- ""
    }
    addErrorCell(keyword, paste0(messageNegation(type, "negative"), additionalMessage), rows = rows[index], columns = column, type = type)
    
  }else if(range == "Zero"){
    index <- values == 0
    addErrorCell(keyword, messageNegation(type, "zero"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "Positive"){
    index <- values > 0
    addErrorCell(keyword, messageNegation(type, "positive"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "LargerOne"){
    index <- values >= 1
    addErrorCell(keyword, messageNegation(type, "larger than 100% or equal to 100%"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "Larger50"){
    index <- values > 50
    addErrorCell(keyword, messageNegation(type, "larger than 50"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notUnit"){
    index <- !values %in% c("bp", "%")
    addErrorCell(keyword, messageRequirement(type, "either 'bp' or '%'"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notLongShort"){
    index <- !tolower(values) %in% c("short", "long")
    addErrorCell(keyword, messageRequirement(type, "either 'short' or 'long'"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notIncreasing"){
    if(n == 0){return()}
    index <- diff(values) < 0
    addErrorCell(keyword, "The values in the column 'F(x)' need to be increasing", rows = rows[-1][index], columns = column, type = type)
    
  }else if(range == "notStrictlyIncreasing"){
    if(n == 0){return()}
    index <- diff(values) <= 0
    addErrorCell(keyword, "The values in the column 'x' need to be strictly increasing", rows = rows[-1][index], columns = column, type = type)
    
  }else if(range == "notContainOne"){
    if(n == 0){return()}
    index <- values[n] != 1
    addErrorCell(keyword, "The last value in the column 'F(x)' should be equal to 1", rows = rows[n][index], columns = column, type = type)
    
  }else if(range == "notSmallValue"){
    index <- values > 20
    message <- paste0("The expected number of losses is high. This indicates that the application of the captive standard model may not be appropriate in this case. ", 
                      "For more details, please refer to the technical documentation for the captive standard model. ",
                      "It is technically possible to override this error message, please contact FINMA for further details. Please note that you may use your own simulation engine.")
    addErrorCell(keyword, message, rows = rows[index], columns = column, type = type)
    
  }else if(range == "notCurrency"){
    index <- !values %in% currencies
    addErrorCell(keyword, paste0("No initial exchange rate is defined for the currency '", strtrim(values[index], 15), "'"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notLGDClass"){
    index <- !values %in% LGDClass
    addErrorCell(keyword, paste0("The value '", strtrim(values[index], 15), "' is not defined as a Basel III class"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notRiskFactor"){
    index <- !values %in% c(riskFactors, "participation")
    addErrorCell(keyword, paste0("'", strtrim(values[index], 15), "' is not defined as a risk factor"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notEmpty"){
    
    # Any entry means that we have a non-empty value
    addErrorCell(keyword, messageRequirement(type, "empty"), rows = rows, columns = column, type = type)
    
  }else if(range == "notHorizon"){
    index <- !values %in% c("k", "m", "l")
    addErrorCell(keyword, messageRequirement(type, "either 'k', 'm' or 'l'"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notStandalone"){
    index <- !values %in% getParam("standaloneTypes")
    addErrorCell(keyword, paste0("'", strtrim(values[index], 15),"' is not a valid standalone"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notNonLifeType"){
    index <- !values %in% getParam("nonLifeType")
    addErrorCell(keyword, paste0("'", strtrim(values[index], 30),"' is not a valid selection for the non-life simulation type"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notCaptiveBranchType"){
    index <- !values %in% getParam("notCaptiveBranchType")
    addErrorCell(keyword, paste0("'", strtrim(values[index], 30),"' is not a valid type of line of business"), rows = rows[index], columns = column, type = type)
    
  }else if(range == "notRating"){
    index <- !values %in% getParam("notRating")
    addErrorCell(keyword, "The rating should be either 1, 2, ...,  8", rows = rows[index], columns = column, type = type)
    
  }else{
    addError(NA, paste0("Internal error: unknown range '", range, "'"))
  }
}


# Check that the data set contains no duplicated values
checkPrimaryKey <- function(A, columns, keyword){
  if(length(columns) > 0){
    index <- duplicated(A[, columns, with = F])
    rows <- A$rowNumber[index]
    addErrorCell(keyword, "Duplicate values are not allowed", rows = rows, columns = columns[1])
  }
}


# Check that the table is not empty
checkEmptyTable <- function(A, emptyAllowed, keyword){
  addError(keyword, "The table should not be empty", condition = !emptyAllowed && (nrow(A) == 0))
}


# Ensure that the data.table has no missing values in the selected columns
checkMissingValues <- function(A, columns, keyword){
  naRow <- function(x){data.table(rows = A$rowNumber[A[[x]] == "" | is.na(A[[x]])])}
  U <- rbindlist(lapply(stats::setNames(nm = columns), naRow), idcol = "column")
  addErrorCell(keyword, "The cell should be non-empty", rows = U$rows, columns = U$column)
}


# Check that the risk factors are valid
checkRF <- function(keyword, vector, vectorRF, correlationMatrixName = "market"){
  missingValues <- setdiff(vectorRF, vector)
  addError(keyword, paste0("The risk factor '", missingValues, "' is defined in the ", correlationMatrixName, " risk correlation matrix but could not be found in this table"), condition = length(missingValues) > 0)
  additionalValues <- setdiff(vector, vectorRF)
  addError(keyword, paste0("The risk factor '", additionalValues, "' could not be found in the ", correlationMatrixName, " risk correlation matrix"), condition = length(additionalValues) > 0)
  
  addError(keyword, paste0("The number of risk factors does not match the size of the correlation matrix"), condition = length(vectorRF) != length(vector))
  addError(keyword, paste0("Please ensure that the risk factors are in the same order as the correlation matrix names"), condition = any(vectorRF != vector))
}