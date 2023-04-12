# Identify all variants of the SST-Template. A variant is a copy of a sheet having the suffix "__xx", for example "Life" and "Life__S1"
getVariant <- function(path){
  
  path <- lapply(path, checkPath)
  
  sheetNames <- lapply(path, readxl::excel_sheets)
  variant <- getVariantFromSheets(sheetNames)
  return(variant)
}


# Identify the variants from the sheet names
getVariantFromSheets <- function(sheetsList){
  sheets <- unlist(sheetsList)
  suffix <- substr(sheets, nchar(sheets)-3, nchar(sheets))
  variantIdentified <- sort(unique(substr(suffix[substr(suffix, 1, 2) == "__"], 3, 4)))
  
  variantName <- c("Base case", paste0(rep("Variant ", length(variantIdentified)), variantIdentified))
  variant <- stats::setNames(c("", variantIdentified), variantName)
  return(variant) # Output example: "Base case", "Variant S1", "Variant V2", etc...
}