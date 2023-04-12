context("Simulate")

addNullRF <-function(sim){
  list(sim = sim, RF = NULL)
}

test_that("Life simulation", {
  # Life and health
  nm <- c("RF1", "RF2")
  object <- list(correlation = matrix(c(1, 0.2, 0.2, 1), 2, dimnames = list(nm, nm)), volatility = c(RF1 = 1, RF2 = 1.5))

  seed <- getSeed(1, "Life")

  sim <- simulate.lifeModule(object = object, nsim = 10, seed = 1)
  set.seed(seed)
  sim_result <- simulationMultivariate(object$correlation, object$volatility, 10)
  expect_identical(sim, sim_result)
})

