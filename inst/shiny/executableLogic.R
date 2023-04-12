# library(shinydashboard)


# Changing this file could break the executable
# version of the Tool.

execMode <- getOption("sstCalculation.execMode")
if(is.null(execMode)) execMode <- FALSE
quitOnClose <- execMode

#' Handles the logic to close the shiny app when a session terminates
#' Terminates the shiny app when the option \code{sstModel.execMode} is
#' set to true.
#'
#' @param input The input argument from the \code{shiny::server} function.
#' Used to observe the \code{input$reload} variable to reload the session
#' when this input is triggered. This input can be triggered via javascript.
#' See : js/preventQuitOnReload.js
#'
#' @param session The session parameter from \code{shiny::server} function.
#' Used to observe a session end
#'
#' @return None (intended for side effects)
#'
executable.QuitOnEnd <- function(input, session) {
  
  observeEvent(input$reload, {
    if(quitOnClose) {
      quitOnClose <<- FALSE
    }
    session$reload()
  })
  
  session$onSessionEnded(function() {
    if(quitOnClose) {
      stopApp()
    }
    if (execMode) {
      quitOnClose <- TRUE
    }
  })
}
