## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------
x <- list("Standalones_Simulations" = NA,
          "Risk_factors_simulations" = NA,
          "Target_capital_decomposition" = NA,
          "Open_FDS" = NA,
          "Internal_parameters" = NA)
x <- sstCalculation:::namedObject(x, "SST simulation results", "Object")
result <- sstCalculation:::retClass(x, "sstSimulation")

# base_case <- list("Standalones_Simulations" = NA,
#                   "Risk_factors_simulations" = NA,
#                   "Target_capital_decomposition" = NA,
#                   "Internal_parameters" = NA)
# 
# base_case <- sstCalculation:::namedObject(base_case, "SST simulation results", "Object")
# result <- list(`Base case` = sstCalculation:::retClass(base_case, "sstSimulation"), 
#                open_FDS = function(){openxlsx::openXL(wb)})

## ---- results = FALSE, warning = FALSE, eval = FALSE--------------------------
#  result <- sstCalculation(path = c("~/SST-Template_daughter.xlsx", "~/SST-Template_mother.xlsx"),
#                           nsim = 1000, seed = 123
#  )

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
cat("Loading\n")
cat("|======================================================================================| 100%")
cat("Performing simulations\n")
cat("|======================================================================================| 100%")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
cat("--------------------------------------------------\n")
cat("               Simulations results\n")
cat("--------------------------------------------------\n")
cat("- Element `Base case`\n")
cat("- Element `Open_FDS`\n")

## ---- eval = TRUE, echo = FALSE-----------------------------------------------
cat("--------------------------------------------------\n")
cat("               SST simulation results\n")
cat("--------------------------------------------------\n")
cat("- Object `Standalones_Simulations`\n")
cat("- Object `Risk_factors_simulations`\n")
cat("- Object `Target_capital_decomposition`\n")
cat("- Object `Internal_parameters`\n")

## ---- eval = FALSE------------------------------------------------------------
#  result$`Base case`$Standalones_Simulations

## ---- results = FALSE, warning = FALSE, eval = FALSE--------------------------
#  model <- excelToModelSST(path = c("~/SST-Template_daughter.xlsx", "~/SST-Template_mother.xlsx"))

## ---- results = FALSE, warning = FALSE, eval = FALSE--------------------------
#  result <- simulate(model, nsim = 1000)

