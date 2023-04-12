context("Participation")
ObjectsList <- list(list(companyName = "A", daughterValue = NA_real_, daughterScaling = NA_real_, daughterApplyLLPO= FALSE, riskAsParticipation = FALSE),
                    list(companyName = "B", daughterValue = 10, daughterScaling = 0.3, daughterApplyLLPO= FALSE, riskAsParticipation = TRUE),
                    list(companyName = "C", daughterValue = 10, daughterScaling = 0.3, daughterApplyLLPO= TRUE, riskAsParticipation = FALSE),
                    list(companyName = "C", daughterValue = 10, daughterScaling = 0.5, daughterApplyLLPO= FALSE, riskAsParticipation = FALSE),
                    list(companyName = "D", daughterValue = NA_real_, daughterScaling = 5, daughterApplyLLPO= FALSE, riskAsParticipation = FALSE),
                    list(companyName = "A", daughterValue = NA_real_, daughterScaling = NA_real_, daughterApplyLLPO= TRUE, riskAsParticipation = FALSE),
                    list(companyName = "A", daughterValue = NA_real_, daughterScaling = NA_real_, daughterApplyLLPO= FALSE, riskAsParticipation = TRUE),
                    list(companyName = "A", daughterValue = NA_real_, daughterScaling = NA_real_, daughterApplyLLPO= TRUE, riskAsParticipation = TRUE),
                    list(companyName = "A", daughterValue = 10, daughterScaling = 10, daughterApplyLLPO= FALSE, riskAsParticipation = TRUE),
                    list(companyName = "AA", daughterValue = NA_real_, daughterScaling = NA_real_, daughterApplyLLPO= FALSE, riskAsParticipation = FALSE),
                    list(companyName = "DaughterCompany", daughterValue = 10, daughterScaling = NA_real_, daughterApplyLLPO= FALSE, riskAsParticipation = FALSE)
                    
)

test_that("checkStandaloneObjectsList", {
  # Correct case
  expect_silent(checkStandaloneObjectsList(ObjectsList[1]))
  expect_warning(checkStandaloneObjectsList(ObjectsList[1:2]), "Mother company identified: 'A'")
  expect_warning(checkStandaloneObjectsList(ObjectsList[1:3]), "Mother company identified: 'A'")
  
  # Error from duplicated company names
  expect_error(checkStandaloneObjectsList(ObjectsList[1:4]), "Please use distinct company names. The company name 'C' was provided twice")
  expect_error(checkStandaloneObjectsList(ObjectsList[3:4]), "Please use distinct company names. The company name 'C' was provided twice")

  # Incorrect standalones
  expect_error(checkStandaloneObjectsList(ObjectsList[2]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[3]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[4]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[5]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[6]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[7]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[8]), "This cell should be left empty")
  expect_error(checkStandaloneObjectsList(ObjectsList[9]), "This cell should be left empty")
  
  # Incorrect participation inputs
  expect_error(checkStandaloneObjectsList(ObjectsList[c(1, 10)]), "Multiple mother companies were found")
  expect_warning(expect_error(checkStandaloneObjectsList(ObjectsList[2:3]), "No mother company was found"), "Daughter company identified: 'B'")
  expect_warning(expect_error(checkStandaloneObjectsList(ObjectsList[4:5]), "The scaling factor needs to be provided if and only if a daughter value has been provided (Template 'D')", fixed = TRUE), "Mother company identified: 'D'")
  expect_warning(expect_error(checkStandaloneObjectsList(ObjectsList[c(1, 11)]), "The scaling factor needs to be provided if and only if a daughter value has been provided (Template 'DaughterCompany')", fixed = TRUE), "Mother company identified: 'A'")
  
  
  expect_warning(expect_error(checkStandaloneObjectsList(ObjectsList[c(2,6)]), "This cell should be left empty for the mother company (Template 'A')", fixed = TRUE))
  expect_warning(expect_error(checkStandaloneObjectsList(ObjectsList[c(2,7)]), "This cell should be left empty for the mother company (Template 'A')", fixed = TRUE))
  
})

test_that("getOrderingFromSim", {
  sim <- list(CA = data.table(all = 1:3, cost = 3:1), CB = data.table(all = rep(0, 3), cost = 3:1), CC = data.table(all = c(10, 0, 20), cost = 3:1))
  
  # One company
  x <- getOrderingFromSim(sim[1])
  y <- 1:3
  expect_identical(x, y)
  
  # Two companies
  x <- getOrderingFromSim(sim[1:2])
  y <- 1:3
  expect_identical(x, y)
  
  # Two companies
  x <- getOrderingFromSim(sim[c(1, 3)])
  y <- as.integer(c(2, 1, 3))
  expect_identical(x, y)
  
  # Three companies
  x <- getOrderingFromSim(sim[1:3])
  y <- as.integer(c(2, 1, 3))
  expect_identical(x, y)
})
