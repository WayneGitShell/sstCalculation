#' @importFrom stats simulate

# Main simulation step. Simulate all versions (variant) of the SST-Template
#' @export
simulate.sstCalculationVariant <- function(object, nsim = NULL, seed = 1, openFDS = FALSE, ...){
  process({
    createProgressBar(message = "Performing simulations", size = 1)
    
    # Create an empty Excel template for the FDS 
    wb <- openxlsx::createWorkbook(creator = "SST-Tool")
    
    # Simulate each variant of the SST-Template(s).
    results <- lapply(object, simulate.sstCalculation, nsim = nsim, seed = seed, wb = wb, ...)
    
    # Open/save the FDS
    if(identical(openFDS, TRUE)){
      openxlsx::openXL(wb)
    }else if(length(openFDS) == 1 & is.character(openFDS)){
      openxlsx::saveWorkbook(wb = wb, file = openFDS, overwrite = TRUE)
    }
    
    results[["Open_FDS"]] <- function(){openxlsx::openXL(wb)}
    
    results <- namedObject(results, "Simulations results", "Element")
    
    return(results)
    
  }, ...)
}

# Get the required ordering
getOrderingFromSim <- function(sim){
  dt <- getSubValue(sim, "all")
  all <- rowSums(as.data.table(dt))
  ordering <- order(all)
  return(ordering)
}


# Simulate all elements from the module
simulateModule <- function(module, nsim, seed){
  
  ModuleSimulaton <- lapply(module, simulate, nsim = nsim, seed = seed)
  
  sim <- getSubValue(ModuleSimulaton, "sim")
  RF <- getSubValue(ModuleSimulaton, "RF")
  
  ordering <- getOrderingFromSim(sim)
  
  
  # Sanity check
  stopifnot(isIdentical(RF))
  RF <- RF[[1]]
  
  list(sim = sim, RF = RF, ordering = ordering)
}

# Apply the reordering to simulations
getReorderedSimulations <- function(module, company, simulations, rank){
  newOrder <- simulations[[module]]$ordering[rank[[module]]]
  reorderedSimulations <- simulations[[module]]$sim[[company]][newOrder]
  return(reorderedSimulations)
}

# Apply the reordering to the risk factors
getReorderedRF <- function(module, simulations, rank){
  newOrder <- simulations[[module]]$ordering[rank[[module]]]
  RF <- getValue(simulations[[module]], "RF")
  if(is.null(RF)){
    return(NULL)
  }else{
    return(RF[newOrder])
  }
}

# Consolidate the simulations
getConsolidatedSimulations <- function(sim){
  totalTable <- as.data.table(lapply(sim, getValue, keyword = "all"))
  total <- rowSums(totalTable)
  
  simConsolidated <- do.call("cbind", sim)
  simConsolidated[, all := total]
  setcolorder(simConsolidated, c("all", names(simConsolidated)[-length(simConsolidated)]))
  return(simConsolidated[])
}

# Apply the reordering to simulations
getReorderedConsolidatedSimulations <- function(company, rank, simulations){
  modules <- stats::setNames(nm = names(rank))
  sim <- lapply(modules, getReorderedSimulations, company = company, simulations = simulations, rank = rank)
  simConsolidated <- getConsolidatedSimulations(sim)
  return(simConsolidated)
}

# Apply the reordering  to risk factors
getReorderedConsolidatedRF <- function(rank, simulations){
  modules <- stats::setNames(nm = names(rank))
  
  RF <- lapply(modules, getReorderedRF, simulations = simulations, rank = rank)
  RF <- RF[!sapply(RF, is.null)]
  if(length(RF) == 0){
    return(NULL)
  }else{
    return(do.call("cbind", RF))
  }
}

# Get results after reordering
getReorderedResults <- function(simulations, rank, companies){
  sim <- lapply(companies, getReorderedConsolidatedSimulations, simulations = simulations, rank = rank)
  RF <- getReorderedConsolidatedRF(simulations = simulations, rank = rank)
  ordering <- getOrderingFromSim(sim)
  
  list(sim = sim, RF = RF, ordering = ordering)
}


# Simulate a combination of mother and daughter
#' @export
simulate.sstCalculation <- function(object, nsim = NULL, seed = 1, wb = openxlsx::createWorkbook(creator = "SST-Tool"), ...){
  # Before we begin with simulation, we check that the parameters are valid
  nsim <- checkSim(nsim)
  seed <- checkSeed(seed)
  
  # Companies to be processed
  companies <- stats::setNames(nm = names(object))
  
  # Get a list containing the models and the modules
  Models <- getPermutate(object)
  Modules <- getPermutate(Models$Modules)
  
  # Get the list of all objects
  Objects <- Models$Objects
  ObjectsMother <- Objects[[1]]
  
  # Get the dependency structure (from the mother template)
  dependency <- Models$Dependency[[1]]
  
  # Simulate the dependency and the risks
  ranks <- lapply(dependency, simulate, nsim = nsim, seed = seed)
  simulations <- lapply(Modules, simulateModule, nsim = nsim, seed = seed)
  
  # Reordering
  for(rankCategory in c("market", "credit", "risks")){
    rank <- ranks[[rankCategory]]
    simulations[[rankCategory]] <- getReorderedResults(simulations, rank, companies)
    simulations[names(rank)] <- NULL
  }

  sim <- copy(simulations$risks$sim)
  RF <- simulations$risks$RF
  
  # Apply the adjustments to RF
  RF[, simulation_id := 1:.N]
  setnames(RF, names(RF), gsub(".marketInstruments.", ".", names(RF), fixed = TRUE))
  
  # Apply the adjustments to the sim
  for(company in companies){
    setSimulationAdjustments(sim = sim[[company]],
                             flooringDeltaRBC = getValue(Objects[[company]], "flooringDeltaRBC"),
                             riskAsParticipation = getValue(Objects[[company]], "riskAsParticipation")
    )
  }
  
  # Consolidate the simulations
  simConcatenated <- getConsolidatedLosses(sim)
  
  
  ES <- getShortfallValues(simulations = simConcatenated[MainResults == TRUE])
  scenariosAggregated <- getScenariosAggregated(getValue(Modules, "scenario"))
  values <- calculateValues(ES = ES, Objects = ObjectsMother, scenarios = scenariosAggregated)
  
  # Remove temporary column
  simConcatenated[, marketunchanged.all := NULL]
  
  # FDS configuration
  configurationFDS <- getValue(ObjectsMother, "configurationFDS")
  updateWorkbook(wb, config = configurationFDS, values = values)
  
  # Table with key results
  keyResults <- getKeyResults(values = values)
  
  Internal_parameters <- list(Model = Models, OutputValues = values, FDS_WorkBook = wb)
  
  x <- list("Standalones_Simulations" = simConcatenated[],
            "Risk_factors_simulations" = RF[],
            "Target_capital_decomposition" = keyResults[],
            "Internal_parameters" = Internal_parameters)
  x <- namedObject(x, "SST simulation results", "Object")
  retClass(x, "sstSimulation")
}

# Take the sum of the scenarios from the macroeconomic part and insurance loss part
getScenariosAggregated <- function(scenarios){
  scenarioConcatenated <- rbindlist(getSubValue(scenarios, "scenario"))
  scenariosAggregated <- scenarioConcatenated[, .(effect = sum(effect)), by = "label"]
  scenariosAggregated <- scenariosAggregated[label != "none"]
  scenariosAggregated[, label := paste0("scenario.", label)]
  scenariosList <- as.list(stats::setNames(scenariosAggregated$effect, scenariosAggregated$label))
  return(scenariosList)
}


# Simulate a mother or daughter
#' @export
simulate.sstModel <- function(object, nsim, seed, ...){
  Modules <- getValue(object, "Modules")
  
  # Perform simulation of each risk component
  simulationResult <- lapply(Modules, simulate, nsim = nsim, seed = seed)
  
  # Separate simulations from risk factors
  sim <- getValue(simulationResult, "sim", subLevel = TRUE)
  RF <- getValue(simulationResult, "RF", subLevel = TRUE)
  
  # Total risks from each risk component, this is used later for the reordering
  main <- as.data.table(getValue(sim, "all", subLevel = TRUE))
  return(list(sim = sim, RF = RF, main = main))
}


# Apply adjustments
setSimulationAdjustments <- function(sim, flooringDeltaRBC, riskAsParticipation){
  sim[, market.marketInstruments.all := NULL]
  
  # Rename the names of the simulation table to have user-friendly names
  setnames(sim, names(sim), gsub(".marketInstruments.", ".", names(sim), fixed = TRUE))
  setnames(sim, names(sim), gsub("credit.credit", "credit.", names(sim), fixed = TRUE))
  setnames(sim, "market.marketParticipation.all", "market.participation")
  
  # The total risk obtained is the delta RBC before the LLPO ajdustment
  setnames(sim, "all", "deltaRBC_beforeLLPO")
  
  # Apply the capping for the LLPO
  sim[, deltaRBC := pmax(deltaRBC_beforeLLPO, flooringDeltaRBC)]
  
  # One-year change of the RBC from the risk components
  sim[, insurance_market_credit_scenario_LLPO.all := deltaRBC - constant.all]
  
  # Market risk before risk as participation
  sim[, marketunchanged.all := market.all]
  
  # Display the one-year change of the RBC from risk components as a market risk participation
  if(riskAsParticipation){
    for(field in grep("^(life|nonlife|health|market|credit)\\.", names(sim), value = TRUE)){
      sim[, (field) := 0]  
    }
    sim[, market.participation := insurance_market_credit_scenario_LLPO.all]
    sim[, market.all := insurance_market_credit_scenario_LLPO.all]
  }
  
  # Risk aggregation
  sim[, insurance.all := life.all + nonlife.all + health.all]
  sim[, insurance_market_credit.all := deltaRBC_beforeLLPO - constant.all - scenario.all]
  sim[, insurance_market_credit_scenario.all := deltaRBC_beforeLLPO - constant.all]
  
  # Put delta RBC columns at the end
  cols <- c("insurance_market_credit_scenario_LLPO.all", "deltaRBC_beforeLLPO", "deltaRBC")
  setcolorder(sim, c(setdiff(names(sim),cols), cols))
  sim[, simulation_id := 1:.N][]
  
  return(NULL)
}

# Consolidate the losses from the daughter and mother companies
getConsolidatedLosses <- function(simulationsLosses){
  if(length(simulationsLosses) == 1){
    # Classical SST calculation
    sim_total <- simulationsLosses[[1]]
    sim_total[, Template := names(simulationsLosses)]
    sim_total[, MainResults := TRUE][]
    
    return(sim_total)
  }else{
    # SST calculation with mother and daughter
    
    # Standalone simulations
    sim_total <- sumListDataTable(simulationsLosses)
    sim_total[, simulation_id := 1:.N]
    sim_total[, Template:= "Consolidated SST (Mother + Daughter)"]
    sim_total[, MainResults := TRUE]
    
    # Consolidated simulations
    sim_decomposition <- rbindlist(simulationsLosses, idcol = "Template")
    sim_decomposition <- sim_decomposition[, MainResults := FALSE]
    
    # Simulation table with standalone and consolidated simulations
    sim_total <- rbind(sim_total, sim_decomposition)
    
    return(sim_total)
  }
}
