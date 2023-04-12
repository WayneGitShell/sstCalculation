#' @title Implementation of the Swiss Solvency Test (SST) Standard Models.
#'
#' @description Framework for performing solvency related computations based on
#'  standard models for the Swiss Solvency Test (SST), a risk-based capital standard for Swiss
#'  insurance companies.
#'
#'
#' @docType package
#' @name sstCalculation-package
NULL

.onAttach <- function(libname, pkgname) {
  
  # retrieve version number
  PACKver <- read.dcf(file=system.file("DESCRIPTION", package = pkgname),
                      fields = "Version")
  
  cp_notice <- paste(readLines(system.file("COPYRIGHT/COPYRIGHT_SHORT", package = pkgname)),
                     collapse = "\n")
  
  # start-up messages
  packageStartupMessage(paste(pkgname, PACKver, "\n"))
  packageStartupMessage(parse(text = paste0("'", paste(cp_notice, "\n"), "'"))) # Trick to display © correctly when using ASCII chars.
  packageStartupMessage("Type sstDashboard() and go to the 'Legal Notices' Tab for more details about the license.")
  packageStartupMessage("Type sstIntroduction() to open an introduction to the sstCalculation package.")
  packageStartupMessage("Type sstNews() to open an overview of the changes to the sstCalculation package.\n")
}
