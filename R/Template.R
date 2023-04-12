
# Consolidate mutiples templates into one
consolidateTemplates <- function(templates){
  consolidatedTemplate <- list()
  for(template in templates){
    # Identify the sheets which are not in the consolidated template and add them to this consolidated template
    newSheets <- setdiff(names(template), names(consolidatedTemplate))
    consolidatedTemplate <- c(consolidatedTemplate, template[newSheets])
  }
  consolidatedTemplate <- namedObject(consolidatedTemplate, "SST-Template", "Worksheet")
  return(consolidatedTemplate)
}


# Get the path to the SST Template
getPathTemplate <- function(path){
  if(isSpecialPath(path)){
    pathTemplate <- getValue(path, "Template")
  }else{
    pathTemplate <- path
  }
  addError(NA, "Please provide at least one SST-Template to process", condition = length(pathTemplate) == 0)
  return(pathTemplate)
}

# Get the path to the configuration file
getPathConfig <- function(path){
  if(isSpecialPath(path)){
    pathConfig <- getValue(path, "Config")
  }else{
    pathConfig <- NULL
  }
  return(pathConfig)
}


# A special path is a path containing both a SST Template and a configuration file.
# This functionality is only used by FINMA
isSpecialPath <- function(path){
  return(is.list(path) && names(path) == c("Template", "Config") && length(path) == 2)
}


loadSheet <- function(sheet, path, progressBarIncrease){
  # suppressMessages is used to avoid any unwanted message regarding automatic change of column names by tibble
  worksheet <- unname(suppressMessages(as.matrix(readxl::read_excel(path, sheet, range = readxl::cell_limits(c(1, 1), c(NA, NA)), col_names = FALSE, col_types = "text"))))
  
  # Increase the progress bar
  addProgress(step = progressBarIncrease, message = "Loading") 
  return(worksheet)
}

# Combine the config templates with the template
getConsolidatedTemplate <- function(template, configs){
  combinedTemplates <- c(configs, list(template))
  consolidateTemplates(combinedTemplates)
}


# Extract the configuration
getConfiguration <- function(configurationData){
  
  # Extract the headers from the first row
  A <- configurationData[-1,]
  names(A) <- unlist(configurationData[1,])
  
  # Remove trailing rows filled with NA
  A <- A[!is.na(name)]
  
  # Cast the columns in the correct type
  for(x in c("row", "col")){
    A[, (x) := as.integer(A[[x]])]
  }
  for(x in c("isValueRequired", "isReplacementZero", "isValue", "isEmptyAllowed", "isDropEmptyRows", "isCorrelation")){
    A[, (x) := as.logical(A[[x]])]
  }
  for(x in c("characteristic", "dataSelection", "filterValue")){
    A[is.na(A[[x]]), (x) := ""]
  }
  return(A[])
}


# Extract the value from a cell
getCellValue <- function(worksheet, row, column, isInteger, replacement){
  if(nrow(worksheet) < row || ncol(worksheet) < column){
    return(replacement)
  }
  cellValue <- worksheet[row, column]
  if(isInteger){
    cellValue <- suppressMessages(as.integer(cellValue))
  }
  if(is.na(cellValue)){
    return(replacement)
  }
  return(cellValue)
}


# Check that the correct template version is used
checkCompatibility <- function(consolidatedTemplate){
  configurationData <- getValue(consolidatedTemplate, "Configuration")
  versionNumber <- getCellValue(configurationData, 2, 26, TRUE, 0)
  checkTemplateVersion(versionNumber)
}


# Load the template with caching
importTemplate <- function(path, nbTemplates, pathCache = getOption("sstCalculation.pathCache")){
  
  # This is the standard case. The file caching is usually only used by FINMA 
  if(is.null(pathCache)){
    excelTemplate <- loadTemplate(path, nbTemplates)
    return(excelTemplate)
  }
  
  # We need to normalize the path to compute the checksum
  md5 <- tools::md5sum(normalizePath(path))
  if(!dir.exists(pathCache)){
    stop("Output directory for caching does not exist")
  }
  pathNew <- paste0(pathCache, "Cache__", md5, ".Rdata")
  if(file.exists(pathNew)){
    # The template with this checksum was already processed
    excelTemplate <- readRDS(pathNew)
    addProgress(step = 1/nbTemplates, message = "Loading") 
  }else{
    # New template, saving a cached version
    excelTemplate <- loadTemplate(path, nbTemplates)
    saveRDS(excelTemplate, file = pathNew)
  }
  return(excelTemplate)
}

# Load the template
loadTemplate <- function(path, nbTemplates){
  sheets <- stats::setNames(nm = readxl::excel_sheets(path))
  template <- lapply(sheets, loadSheet, path = path, progressBarIncrease = 1/(nbTemplates*length(sheets)))
  template <- namedObject(template, "Excel template", "Worksheet")
  return(template)
}

checkTemplateVersion <- function(versionNumber){
  versionId <- floor(versionNumber/1e5)
  
  messageDetails <- "More details and guidance about the R-Tool are available on FINMA's website."
  
  if(versionId < 15912){
    addError(NA, paste0("A newer version of the SST-Template is required to be processed by the R-Tool. Please use the latest version of the SST-Template provided by FINMA.", messageDetails), type = "Error")
    
  }else if(versionId == 15912){
    # The version which is backward compatible.
    addError(NA, "You are using last year's SST template. This version of the R-tool is backward compatible with the older template for illustration purposes. For the actual SST reporting use the newer SST template.", type = "Warning")
    
  }else if(versionId == 15913){
    # Correct version
    
  }else if(versionId > 15913){
    addError(NA, paste0("A newer version of the R-Tool is required to process this SST-Template. Please use the latest version of the R-Tool provided by FINMA.", messageDetails))
    
  }else{
    addError(NA, paste0("The template provided is not a valid SST-Template. ", messageDetails))
    
  }
}