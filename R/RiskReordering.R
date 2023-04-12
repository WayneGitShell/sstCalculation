# Dependency structure for all main risks
getDependencyRisks <- function(correlation){
  dependencyRisks <- getDependency(correlation = correlation, labelIndependentComponents = c("scenario", "constant"))
  
  retClass(dependencyRisks, "dependencyRisks")
}


# Dependency within credit risk
getDependencyCredit <- function(corr){
  correlation <- getCorrelation(corr = corr, labels = c("creditBasel", "creditMerton"))
  dependencyCredit <- getDependency(correlation)
  
  retClass(dependencyCredit, "dependencyCredit")
}


# Dependency within market risk
getDependencyMarket <- function(){
  correlation <- getCorrelation(corr = 1, labels = c("marketInstruments", "marketParticipation"))
  dependencyMarket <- getDependency(correlation)
  
  retClass(dependencyMarket, "dependencyMarket")
}


# Dependency structure
getDependency <- function(correlation, labelIndependentComponents = character(0)){
  # The dependency consists of a correlation matrix and additional components that are independent of all risk factors
  x <- list(correlation = correlation, labelIndependentComponents = labelIndependentComponents)
  retClass(x, "Reordering")
}


# Transform a correlation parameter into a 2x2 correlation matrix
getCorrelation <- function(corr, labels){
  correlation <- matrix(c(1, corr, corr, 1), nrow = 2, dimnames = list(labels, labels))
  return(correlation)
}
