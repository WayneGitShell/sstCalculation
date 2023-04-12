#' @import data.table

# Global variables
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("i.rowNumber", "i.scale", "i.type", "i.value", "id", "instrument", "instrumentId",
                           "insurance.all", #"insurance_market.all", "insurance_market_scenario.all",
                           "insurance_market_credit.all", "insurance_market_credit_scenario.all", "credit.all", "credit",
                           "isValue", "keyword", "label", "life.all", "market", "market.all", "market.participation",
                           "marketValue", "name", "nominal", "nonlife.all", "participation", "position", "price",
                           "probability", "progress", "rate", "rating", "reference", "rowError", "rowNumber", "rows",
                           "scenario", "scenario.all", "sheet", "sigma", "simulation", "simulation_id", "spread",
                           "standaloneTypes", "standalones", "target", "time", "to", "totalValue", "type", "value",
                           "weightedExposure", "x",  ".rs.invokeShinyWindowViewer", "MainResults", "Template", "allPositive",
                           "all_beforeScenario", "alpha_value", "anyValue", "colError", "column", "constant",
                           "currency", "description", "discount", "effect", "error", "exponentialValuation",
                           "exposure", "factorId", "foreign", "from", "global", "health.all", "i.coef", "i.col",
                           "i.column", "i.discount", "i.factorId", "i.fx", "i.message", "i.name", "i.newName", "i.row", ".",
                           "normalLossNumber", "normalLossSigma", "normalLossSize", "prefix", "sheetName", "textValue", "threshold", "sd", "cov",
                           "counterparty", "counterpartyId", "default", "deltaRBC", "deltaRBC_beforeLLPO", "expectedLoss", "fx", "fxRate",
                           "i.company", "i.description", "i.isValue", "i.newSheet", "i.scalingLGD", "i.sheet", "insurance_market_credit_scenario_LLPO.all",
                           "isVariant", "largeLossNumber", "largeLossShape", "largeLossThreshold", "migration",
                           "AAL", "Basel.all", "CY", "EEL", "LGD", "Merton.all", "PY", "QS", "action", "market.marketInstruments.all", "marketunchanged.all",
                           "maximumPossibleLoss", "mu", "nb", "rawName", "reserve", "row2", "rowConfig", "scalingCF", "scalingLGD",
                           "anyExposure", "anyMarketValue", "anyMissingSpread", "col2", "company", "constant.all", "sheetSource",
                           "flooringThreshold", "withFX"), package = "sstCalculation", add=F)
  
  
}

# Styles used for the generation of the FDS
styles <- list(commentStyle = openxlsx::createStyle(fgFill = "#fde2ce", border =  c("top", "bottom"),  borderColour = "#bfbfbf"),
               headerStyle = openxlsx::createStyle(fgFill = "#d4ecf9", textDecoration = "bold"),
               headerStyleCentering = openxlsx::createStyle(fgFill = "#d4ecf9", textDecoration = "bold", halign = "center"),
               boldStyle = openxlsx::createStyle(textDecoration = "bold"),
               italicStyle = openxlsx::createStyle(textDecoration = "italic"),
               whiteStyle = openxlsx::createStyle(fontColour = "#ffffff"),
               topHeaderStyle = openxlsx::createStyle(fontSize = 16, fontColour = "#002d64", textDecoration = "bold"),
               lightGridStyle = openxlsx::createStyle(border = c("top", "bottom"), borderColour = "#bfbfbf"),
               whiteGridStyle = openxlsx::createStyle(border = c("top", "bottom"), borderColour = "#ffffff")
)
