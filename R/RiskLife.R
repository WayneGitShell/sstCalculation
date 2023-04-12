# Life risks
getLifeRisks <- function(correlation, exposureLife){
  quantile <- getParam("lifeQuantile")
  volatility <- exposureLife / stats::qnorm(quantile)
  
  checkRF("exposureLife", vector = names(volatility), vectorRF = rownames(correlation), correlationMatrixName = "life")
  
  x <- list(correlation = correlation, volatility = volatility)
  retClass(x, "lifeModule")
}