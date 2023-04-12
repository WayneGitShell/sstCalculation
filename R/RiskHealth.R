# Health risks
getHealthRisks <- function(correlation, sensitivityHealth, scenarioHealth){
  checkRF("sensitivityHealth", vector = names(sensitivityHealth), vectorRF = rownames(correlation), correlationMatrixName = "health")
  
  scenario <- getCompleteScenariosSet(keyword = "scenarioHealth", scenarioHealth)
  x <- list(correlation = correlation, volatility = sensitivityHealth, scenario = scenario)
  retClass(x, "healthModule")
}