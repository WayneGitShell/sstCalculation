selectLegal <- function(input, output) {
  output$copyrightsText <- shiny::renderUI({HTML(readPackage(input$copyrights))})
  output$licenseText <- shiny::renderUI({HTML(readLicense(input$licenses))})
}

readPackage <- function(pkgName) {
  if(identical(pkgName, "SST Dashboard")) {
    paste(
      gsub(">", "&gt;", gsub("<", "&lt;", readLines(system.file("COPYRIGHT/COPYRIGHT", package = "sstCalculation")))),
      collapse = "<br>"
    )
  } else {
    paste(
      gsub(">", "&gt;", gsub("<", "&lt;", readLines(system.file(paste0("COPYRIGHT/notices/", gsub(" ", "_", pkgName)), package = "sstCalculation")))),
      collapse = "<br>"
    )
  }
}

readLicense <- function(licenseName) {
  paste(
    gsub(">", "&gt;", gsub("<", "&lt;", readLines(system.file(paste0("COPYRIGHT/licenses/", gsub(" ", "_", licenseName)), package = "sstCalculation")))),
    collapse = "<br>"
  )
}

drawPlot <- function(output, simulations, alpha){
  output$deltaRBC <- shiny::renderPlot(execOnResize = T, {
    plotDensity(data=simulations[["insurance_market_credit_scenario.all"]], xlab="Market, insurance, credit and scenarios risk simulations", qq=alpha)
  })
  output$market_insurance_credit <- shiny::renderPlot(execOnResize = T, {
    plotDensity(data=simulations[["insurance_market_credit.all"]], xlab="Market, insurance and credit risk simulations", qq=alpha)
  })
  output$market <- shiny::renderPlot(execOnResize = T, {
    plotDensity(data=simulations[["market.all"]], xlab="Market risk simulations", qq=alpha)
  })
  output$insurance <- shiny::renderPlot(execOnResize = T, {
    plotDensity(data=simulations[["insurance.all"]], xlab="Insurance risk simulations", qq=alpha)
  })
  output$credit <- shiny::renderPlot(execOnResize = T, {
    plotCredit(data=simulations[["credit.all"]], xlab="Credit risk simulations", qq=alpha)
  })
}

plotDensity <- function(data, xlab, qq=0.01, polyColor="darkorange2") {
  polyColor = makeTransparent(polyColor)
  data_trimmed_for_plot <- data[data >= quantile(data, probs = 0.001) & data <= quantile(data, probs = 1 - 0.001)]
  
  if(min(data) == max(data)){
    main <- "No risks to display"
  }else{
    main <- NA
  }
  
  hist(data_trimmed_for_plot, freq = FALSE, breaks = 100, main = main, xlab = xlab)
  if(min(data) != max(data)){
    q <- quantile(data, probs = qq)
    d <- density(data_trimmed_for_plot)
    
    x_lim = max(which(d$x <= q))
    lines(d, col = "darkred")
    abline(v = q, col = "darkgoldenrod1")
    with(d, polygon(x=c(x[c(0:x_lim, x_lim)]), y=c(y[0:x_lim], 0), col=polyColor, border = NA)) #highlights a portion of the plot
    legend("topleft", inset = .01, legend = c("Value at risk 99%", "Expected shortfall"), col = c("orange", polyColor),
           lty = c(1, 0), pch = c(NA, 21), pt.bg = c(NA, polyColor), pt.cex = 2)
  }
}

plotCredit <- function(data, xlab, qq=0.01) {
  if(min(data) == max(data)){
    main <- "No risks to display"
  }else{
    main <- NA
  }
  hist(data, freq = FALSE, breaks = 100, main = main, xlab = xlab)
  if(min(data) != max(data)){
    q <- quantile(data, probs = qq)
    d <- density(data)
    x_lim = max(which(d$x <= q))
    lines(d, col = "darkred")
    abline(v = q, col = "darkgoldenrod1")
    legend("topleft", inset = .01, legend = "Value at risk 99%", lty = 1, col = "orange", pt.cex = 2)
  }
}

makeTransparent <- function(someColor, alpha=100){
  newColor<-col2rgb(someColor)
  apply(newColor, 2, function(curcoldata){
    rgb(red=curcoldata[1], green=curcoldata[2], blue=curcoldata[3],alpha=alpha, maxColorValue=255)
  })
}

computexLimits <- function(data){
  q1 <- quantile(data, probs = 0.001)
  q2 <- quantile(data, probs = 1-0.001)
  distance <- abs(q2-q1)
  q1 <- q1-0.1*distance
  q2 <- q2+0.1*distance
  return(as.list(stats::setNames(nm=c("q1","q2"),c(q1,q2))))
}

makeTable <- function(output, table){
  output$tableKeyFigures <- shiny::renderTable(table, hover = TRUE, align = 'lr', digits = 2, spacing = "xs")
}

# Waterfall chart
plotWaterfallChart <- function(u, main){
  if(u[length(u)] <= 0){
    return()
  }
  U <- data.table::data.table(label = gsub("0-", "", names(u), fixed = TRUE), value = 100 * u/u[length(u)], group = cumsum(grepl("0-",names(u), fixed = TRUE)))
  U[, base := cumsum(c(0, value[-.N])), by = "group"]
  
  graphics::par(mar=c(8,4,4,4))
  v <- pretty(1.05*c(U$base,U$base+U$value))
  a = graphics::barplot(rbind(U$base, U$value), col = c("white", "lightskyblue3"), border = c(NA, TRUE), main = main, space = 0.1, yaxt = "n", ylim = range(v))
  graphics::axis(2, at=v, lab=paste0(v, "%"), las=TRUE)
  
  graphics::axis(side = 1, at = a, labels = FALSE)
  graphics::box()
  graphics::text(a, -8, srt = 30, adj= 1, xpd = TRUE, labels = U$label , cex=1)
  graphics::text(a, U$base + pmax(0, U$value) + 4, labels = paste0(round(U$value) , "%"), xpd = TRUE)
}

getWaterfallPlot <- function(content, file){
  pdf(file = file, width = 10, height =7)
  vec <- content
  u <-c("0-Market risk" = vec$market.all,
        "Exp. financial result" = -vec$expectedFinancialResult,
        "Credit risk" = vec$credit.all,
        "Insurance risk" = vec$insurance.all,
        "Exp. technical result" = -vec$expectedInsuranceResult,
        "Scenarios" = vec$scenarioImpact,
        "Other" = vec$additionalEffectOnTC,
        "Diversification" = vec$diversificationMarketInsuranceCredit,
        "0-One-year capital\n requirement" = vec$SCR,
        "MVM" = vec$MVM,
        "0-Target capital" = vec$targetCapital)
  plotWaterfallChart(u, "Target capital decomposition")
  
  u <- c("0-Interest rate risk" = vec$`market.interest rate`,
         "Spread risk" = vec$market.spread,
         "Currency risk" = vec$market.currency,
         "Equity risk" = vec$market.equity,
         "Property risk" = vec$`market.real estate`,
         "Hedge funds risk" = vec$`market.hedge fund`,
         "Private equity risk" = vec$`market.private equity`,
         "Participation risk" = vec$market.participation,
         "Other" = vec$market.other + vec$market.companySpecific1 + vec$market.companySpecific2,
         "Diversifiation" = vec$market.diversification,
         "0-Market risk" = vec$market.all)
  plotWaterfallChart(u, "Market risk decomposition")
  
  u <- c("0-CHF interest rate risk" = vec$`market.CHF rate`,
         "EUR interest rate risk" = vec$`market.EUR rate`,
         "USD interest rate risk" = vec$`market.USD rate`,
         "GBP interest rate risk" = vec$`market.GBP rate`,
         "Interest rates diversification" = 0,
         "0-Interest rates risk" = vec$`market.interest rate`)
  u[5] <- u[6] - sum(u[1:4])
  plotWaterfallChart(u, "Interest rate analysis")
  dev.off()
}

tryDownload <- function(fileName, fileExtension, content, suffix){
  dir <- getOption("sstCalculation.workingDirectory")
  
  if(is.null(suffix) || is.na(suffix)){
    suffix <- ""
  }
  file <- gsub("/", "\\", file.path(dir, paste0(fileName, suffix, ".", fileExtension)), fixed = TRUE)
  tryCatch({
    if(fileExtension == "csv"){
      result <- data.table::fwrite(x = content, file = file, row.names = FALSE)
    }else if(fileExtension == "xlsx"){
      result <- openxlsx::saveWorkbook(wb = content, file = file, overwrite = TRUE)
    }else if(fileExtension == "pdf"){
      getWaterfallPlot(content = content , file = file)
    }
    warning(paste0("The file has been saved to: ", gsub("/", "\\", path.expand(file), fixed = TRUE)))
  },
  error = function(e){infoMessage(e$message)},
  warning = function(e){infoMessage(e$message)}
  )
}

infoMessage <- function(message){
  shiny::showModal(
    shiny::modalDialog(
      title = "Info",
      tags$div(HTML(message)),
      style = "color: black;", easyClose = T, footer = tagList(modalButton("Ok"))
    )
  )
}

exportDirect <- function(output, input, tableSimulation, tableMarketSimulation, workBook, outputValues, suffix){
  shiny::observeEvent(input$downloadSimulations, {
    tryDownload(fileName = "Standalones_Simulations", fileExtension = "csv", content = tableSimulation, suffix = suffix)
  })
  shiny::observeEvent(input$downloadMarketSimulations, {
    tryDownload(fileName = "Risk_factors_simulations", fileExtension = "csv", content = tableMarketSimulation, suffix = suffix)
  })
  shiny::observeEvent(input$downloadExcel, {
    tryDownload(fileName = "Fundamental_Data", fileExtension = "xlsx", content = workBook, suffix = suffix)
  })
  shiny::observeEvent(input$downloadPDF, {
    tryDownload(fileName = "Waterfall", fileExtension = "pdf", content = outputValues, suffix = suffix)
  })
}

exportExcel <- function(output, wb, input, suffix){
  output$Fundamental_Data.xlsx <- downloadHandler(
    filename = paste0("Fundamental_Data", suffix, ".xlsx"),
    content = function(file) {
      openxlsx::saveWorkbook(wb = wb, file = file, overwrite = FALSE)
    }
  )
}

exportSimulations <- function(output, table, input, suffix){
  output$Standalones_Simulations.csv <- downloadHandler(
    filename = paste0("Standalones_Simulations", suffix, ".csv"),
    content = function(file) {
      data.table::fwrite(x = table, file = file, row.names = FALSE)
    }
  )
}

exportMarketSimulations <- function(output, table, input, suffix){
  output$Risk_factors_simulations.csv <- downloadHandler(
    filename = paste0("Risk_factors_simulations", suffix, ".csv"),
    content = function(file) {
      data.table::fwrite(x = table, file = file, row.names = FALSE)
    }
  )
}

exportWaterfallPlots <- function(output, values, input, suffix){
  output$Waterfall.pdf <- downloadHandler(
    filename = paste0("Waterfall", suffix, ".pdf"),
    content = function(file) {
      getWaterfallPlot(content = values , file = file)
    }
  )
}

get_new_simulations <- function(input){
  shiny::observeEvent(input$newSim, {
    shiny::showModal(
      shiny::modalDialog(
        title = "Do you want to run a new simulation?",
        "Be aware that every data produced during simulation will be lost.",
        easyClose = F,
        footer = tagList(
          shiny::modalButton("Cancel"),
          shiny::actionButton("reload", "Reload")
        )
      )
    )
  })
}


server <- function(input, output, session) {
  source("executableLogic.R", local = TRUE)
  source("ui.R", local = TRUE)
  options(shiny.maxRequestSize=100*1024^2)
  
  
  rv <- reactiveValues()
  
  shiny::observeEvent(input$excel_file,{
    paths <- input$excel_file$datapath
    initialSeed <- getInitialValue(paths = paths, sheet = "Intro", row = 19, col = 5, 1)
    initialNsim <- getInitialValue(paths = paths, sheet = "Intro", row = 17, col = 5, 1000000)
    updateTextInput(session, "seed", value = initialSeed)
    updateTextInput(session, "sim_number", value = initialNsim)
  })
  
  shiny::observeEvent(input$button, {
    if(is.null(rv$output) & !is.null(input$excel_file$datapath)){
      seed <- suppressWarnings(as.numeric(input$seed))
      nsim <- suppressWarnings(as.numeric(input$sim_number))
      withProgress(message = "", detail = NULL, max = 2, value = 0, {
        result <- sstCalculation::sstCalculation(path = input$excel_file$datapath, nsim = nsim, seed = seed, openFDS = FALSE, resultsForShinyDashboard = TRUE)
      })
      rv$output <- result$x[[1]]
      
      if(!is.null(result$errorLog) & nrow(result$errorLog)>0){
        isError <- any(result$errorLog$type == "Error")
        
        if (isError){
          
          shiny::showModal(
            shiny::modalDialog(
              title = "Error",
              tags$div(HTML(result$messages)),
              style = "color: black;",
              easyClose = FALSE,  # Obliges to read
              footer = tagList(actionButton("reload", "New simulation"))
            )
          )
        }else{
          shiny::showModal(
            shiny::modalDialog(
              title = "Warning",
              tags$div(HTML(result$messages)),
              style = "color: black;",
              easyClose = TRUE,
              footer = tagList(modalButton("Ok"))
            )
          )
        }
      }
      
      if(!is.null(rv$output)){
        modifyBody(input, output, data=rv$output$Internal_parameters$OutputValues)
        modifySideBar(input)
        makeTable(output, table=rv$output$Target_capital_decomposition)
      }
      
      
    }
  })
  
  selectLegal(input, output)
  drawPlot(output, simulations = rv$output$Standalones_Simulations[MainResults == TRUE], alpha=0.01) #rv$output$Internal_parameters$OutputValues[["alpha"]])
  
  get_new_simulations(input)
  exportExcel(output, wb = rv$output$Internal_parameters$FDS_WorkBook, input, suffix = rv$output$Internal_parameters$Model$Objects[[1]]$fileNameSuffix)
  exportSimulations(output, table = rv$output$Standalones_Simulations, input, suffix = rv$output$Internal_parameters$Model$Objects[[1]]$fileNameSuffix)
  exportMarketSimulations(output, table = rv$output$Risk_factors_simulations, input, suffix = rv$output$Internal_parameters$Model$Objects[[1]]$fileNameSuffix)
  exportWaterfallPlots(output, values = rv$output$Internal_parameters$OutputValues, input, suffix = rv$output$Internal_parameters$Model$Objects[[1]]$fileNameSuffix)
  
  exportDirect(input = input,
               output = output,
               tableSimulation = rv$output$Standalones_Simulations,
               tableMarketSimulation = rv$output$Risk_factors_simulations,
               workBook = rv$output$Internal_parameters$FDS_WorkBook,
               suffix = rv$output$Internal_parameters$Model$Objects[[1]]$fileNameSuffix,
               outputValues = rv$output$Internal_parameters$OutputValues)
  executable.QuitOnEnd(input, session)
  
}
