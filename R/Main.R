packageEnv <- new.env(parent = emptyenv())
cleanup()
#' Load SST template
#' 
#' @description Load the SST template(s) into a model. During this step, the content of the SST template is checked to ensure that the model is valid.
#' 
#' This function covers both the standard SST calculation (one SST-Template) and the case where the participation model is used (two or more SST-Templates).
#' 
#' For the look-through calculation, one needs to provide exactly one mother template and one (or multiple) daughter templates.
#' The order of the provided templates is irrelevant, as the mother and daughter templates are identified automatically.
#' 
#' Note that when the only provided template is a daughter template, this template will be considederd as a standard SST-Template (the additional daughter parameters are ignored).
#' 
#' @param path Either the path to the SST Template (standard SST calculation) or the vector of paths to the SST Templates (look-through calculation).
#' @param ... Optional arguments passed to function.
#'
#' @return A model, of class sstCalculation.
#'
#' @export
excelToModelSST <- function(path = NULL, ...){
  process({
    createProgressBar(message = "Loading", size = 1)
    
    # Load each input file
    modelInputs <- getModelInputs(path)
    
    # Produce the sst models
    models <- lapplyCatch(modelInputs, getModel)
    
    retClass(models, "sstCalculationVariant")
    
  }, ...)
}



#' Perform SST calculations
#' 
#' @description Main function to perform SST calculations.
#' 
#' This function covers both the standard SST calculation (one SST-Template) and the case where the participation model is used (two or more SST-Templates).
#' 
#' For the look-through calculation, one needs to provide exactly one mother template and one (or multiple) daughter templates.
#' The order of the provided templates is irrelevant, as the mother and daughter templates are identified automatically.
#' 
#' Note that when the only provided template is a daughter template, this template will be considered as a standard SST-Template (the additional daughter parameters are ignored).
#' 
#' @param path Either the path to the SST Template (standard SST calculation) or the vector of paths to the SST Templates (look-through calculation). The mother and daughter are identified automatically.
#' @param nsim Number of simulations to perform.
#' @param seed Initial seed.
#' @param openFDS Either a logical value indicating if the resulting Excel template (FDS) should be opened right away in Excel, or a string containing the path where the template will be exported.
#' @param ... Optional arguments passed to function.
#'
#' @return A list containing RBC simulations, market risk simulations, key figures.
#'
#' @export
sstCalculation <- function(path = NULL, nsim = NULL, seed = 1, openFDS = FALSE, ...){
  
  process({
    nsim <- checkSim(nsim)
    seed <- checkSeed(seed)
    
    models <- excelToModelSST(path = path, main = FALSE)
    Result <- simulate.sstCalculationVariant(models, nsim = nsim, seed = seed, main = FALSE, openFDS = openFDS)
    Result
  }, ...)
}

#' Introduction to the sstCalculation package
#' 
#' @description Open vignette describing the package.
#' 
#' @return Nothing.
#'
#' @export
sstIntroduction <- function(){
  utils::RShowDoc("Introduction", package = "sstCalculation")
}

#' News from the sstCalculation package
#' 
#' @description Open the vignette describing the package changes in time.
#' 
#' @return Nothing.
#'
#' @export
sstNews <- function(){
  utils::news(package = "sstCalculation")
}


#' Launching The Dashboard In A Browser
#'
#' @description This function launches an interactive dashboard for SST computations.
#'   
#' @param workingDirectory Directory where the calculation results will be exported.
#'
#' @return None.
#'
#' @export
sstDashboard <- function(workingDirectory = "~") {
  if(is.null(workingDirectory) || is.na(workingDirectory)){
    stop("Please provide a working directory")
  }
  workingDirectory <- normalizePath(workingDirectory, mustWork = FALSE)
  if(!dir.exists(workingDirectory)){
    stop(paste0("The directory '", workingDirectory, "' does not exist, please provide a valid directory that will be used to export the results from the SST calculation."))
  }else{
    cat(paste0("The results will be exported to the following directory: ", workingDirectory), fill = TRUE)
  }
  options(sstCalculation.execMode = FALSE)
  options(sstCalculation.workingDirectory = workingDirectory)
  browser <- TRUE
  
  tryCatch({
    browser <- .rs.invokeShinyWindowViewer
  }, error = function(e) {
    warning("You are using the sstCalculation R-package without R-Studio, the
            dashboard will use an external browser and issues might
            occur. \n Please install RStudio and use the dashboard
            from it.")
  })
  
  shiny::runApp(appDir = system.file("shiny", package = "sstCalculation"), launch.browser = browser, quiet = TRUE)
}