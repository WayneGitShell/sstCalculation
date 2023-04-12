context("Reordering")

test_that("simulate.Reordering",{
  nsim <- 10
  seed <- 1
  
  # We need to add class attributes to use simulate function
  # Test "dependencyRisks"
  correlation <- matrix(c(1, 0.99, 0.99, 1), ncol = 2)
  object <- list(correlation = correlation, labelIndependentComponents = NULL)
  attr(object, "class") <- c("dependencyRisks", "Reordering", "list") 
  
  set.seed(8229) # seed obtained getSeed(1, "dependencyRisks") used in simulate reordering
  A <- data.table(MASS::mvrnorm(n = nsim, mu = rep(0, ncol(correlation)), Sigma = correlation))
  
  for (j in names(A)){
    set(A, j = j, value = frank(A[[j]], ties.method = "first"))
  }
  
  B <- simulate.Reordering(object, nsim, seed)
  
  expect_identical(A, B)
  
  # Test "dependencyMarket"
  correlation <- matrix(c(1, 1, 1, 1), ncol = 2)
  object <- list(correlation = correlation, labelIndependentComponents = NULL)
  attr(object, "class") <- c("dependencyMarket", "Reordering", "list") 
  
  set.seed(90597) # seed obtained getSeed(1, "dependencyMarket") used in simulate reordering
  A <- data.table(MASS::mvrnorm(n = nsim, mu = rep(0, ncol(correlation)), Sigma = correlation))
  
  for (j in names(A)){
    set(A, j = j, value = frank(A[[j]], ties.method = "first"))
  }
  
  B <- simulate.Reordering(object, nsim, seed)
  
  expect_identical(A, B)
  
  # Test "dependencyCredit"
  correlation <- matrix(c(1, 0.95, 0.95, 1), ncol = 2)
  object <- list(correlation = correlation, labelIndependentComponents = NULL)
  attr(object, "class") <- c("dependencyCredit", "Reordering", "list") 
  
  set.seed(25305) # seed obtained getSeed(1, "dependencyCredit") used in simulate reordering
  A <- data.table(MASS::mvrnorm(n = nsim, mu = rep(0, ncol(correlation)), Sigma = correlation))
  
  for (j in names(A)){
    set(A, j = j, value = frank(A[[j]], ties.method = "first"))
  }
  
  B <- simulate.Reordering(object, nsim, seed)
  
  expect_identical(A, B)
  
})
