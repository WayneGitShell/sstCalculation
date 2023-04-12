execMode <- getOption("sstCalculation.execMode")

preventQuitOnReload <- if(is.null(execMode) || !execMode) {
  NULL
} else {
  includeScript("js/preventQuitOnReload.js")
}

getInitialValue <- function(paths, sheet, row, col, replacement){
  tryCatch({
    range <- readxl::cell_limits(c(row, col), c(row, col))
    for(path in paths){
      a <- suppressMessages(suppressWarnings(as.integer(readxl::read_excel(path = path, sheet = sheet, range = range, col_names = FALSE))))
      if(length(a)==1 && !is.null(a) && !is.na(a)){
        return(a)
      }
    }
    stop()
  },
  error = function(e){return(replacement)})
}

# Displays the legal notices for the tool
legalPage <- function() {
  packages <- list.files(system.file("COPYRIGHT/notices", package = "sstCalculation"))
  licenses <- list.files(system.file("COPYRIGHT/licenses", package = "sstCalculation"))
  
  # The license of the data.table has changed. The executable version is packaged with the old license, therefore we show in this case the old license.
  if(is.null(execMode) || !execMode){
    packages <- setdiff(packages, "data.table_")
  }else{
    packages <- setdiff(packages, "data.table")
  }
  
  # Replace filename '_' chars by spaces for nicer display.
  packages <- c("SST Dashboard", gsub("_", " ", packages))
  licenses <- gsub("_", " ", licenses)
  
  shiny::fluidPage(
    h2("Legal notices"),
    shiny::fluidRow(
      shiny::column(width = 6,
                    shinydashboard::box(
                      solidHeader = TRUE, title = "Packages - Copyright specifications", status = "primary", width = "100%", align = "center", collapsible = TRUE,
                      shiny::selectInput(inputId = "copyrights", label = "Select package:", choices = packages, selected = "SST Dashboard"), br(),
                      shiny::htmlOutput("copyrightsText")
                    )
      ),
      shiny::column(width = 6,
                    shinydashboard::box(
                      solidHeader = TRUE, title = "Licenses", status = "primary", width = "100%", align = "center", collapsible = TRUE,
                      shiny::selectInput(inputId = "licenses", label = "Select license: ", choices = licenses, selected = "GPL-3"), br(),
                      shiny::htmlOutput("licenseText")
                    )
      )
    )
  )
}

modifyBody <- function(input, output, data){
  shiny::removeUI(selector = "#input_page")
  shiny::insertUI(
    selector = "#sstTool",
    where = "afterEnd",
    shiny::fluidPage(
      id = "results",
      shiny::fluidRow(
        shiny::column(width = 12,tags$h2("Simulations results"))
      ),
      shiny::fluidRow(
        id = "plotBox",
        shinydashboard::box(
          title = "Plots",
          width = "100%",
          shinydashboard::tabBox(
            width = "100%",
            shiny::tabPanel("\U0394RBC", plotOutput("deltaRBC")), # \Delta RBC
            shiny::tabPanel("Market, Insurance and Credit Risk", plotOutput("market_insurance_credit")),
            shiny::tabPanel("Market Risk", plotOutput("market")),
            shiny::tabPanel("Insurance Risk", plotOutput("insurance")),
            shiny::tabPanel("Credit Risk", plotOutput("credit"))
          )
        )
      ),
      shiny::fluidRow(
        id = "boxKeyFigures",
        shiny::column(4, addInfoPercentageBox(title="SST Ratio", val=data[["SSTRatio"]], icon=icon("percent"), color=sstRatioColor(data[["SSTRatio"]]))),
        shiny::column(4, addInfoMoneyBox(title="RBC", val=data[["RTK"]], currency=data[["referenceCurrency"]], icon=icon("fas fa-chart-line"), color="blue")),
        shiny::column(4, addInfoMoneyBox(title="Target Capital", val=data[["targetCapital"]], currency=data[["referenceCurrency"]], icon=icon("fas fa-chart-line"), color="blue"))
      ),
      shiny::fluidRow(
        id = "table",
        shiny::column(width = 8,
                      offset = 2,
                      align="center",
                      shinydashboard::box(title = NULL,
                                          shiny::tableOutput("tableKeyFigures"),
                                          width = "100%",
                                          # tags$style(type="text/css", "#tableKeyFigures tr:last-child {font-weight:bold;}"))
                                          tags$style(type="text/css", "#tableKeyFigures tr:nth-child(4) {font-style:italic;}"),
                                          tags$style(type="text/css", "#tableKeyFigures tr:nth-child(6) {font-style:italic;}"),
                                          tags$style(type="text/css", "#tableKeyFigures tr:nth-child(8) {font-style:italic;}"),
                                          tags$style(type="text/css", "#tableKeyFigures tr:nth-child(11) {font-style:italic;}"),
                                          tags$style(HTML("thead:first-child > tr:first-child > th {border-top: 1; font-size: 120%;}"))
                      )
        )
      )
    )
  )
}

sstRatioColor <- function(sstRatio) {
  if(!is.finite(sstRatio) || is.null(sstRatio)){
    return("teal")
  }
  if(sstRatio < 0.33) "red" else if(sstRatio < 0.80) "orange" else if (sstRatio < 1) "yellow" else "green"
}

addInfoMoneyBox <- function(title, val, currency, icon, color, width="50%"){
  shinydashboard::infoBox(
    # value = paste0("Mio. ", currency, " ", prettyNum(val, format="f", digits=2, big.mark="'", decimal.mark=".")),
    value = paste0("Mio. ", currency, " ", prettyNum(val, digits=4, big.mark="'", decimal.mark=".")),
    title = title,
    icon = icon,
    color = color,
    fill = TRUE,
    width = width
  )
}

addInfoPercentageBox <- function(title, val, icon, color, width="50%"){
  val = 100*val
  shinydashboard::infoBox(
    value = paste0(prettyNum(val, digits=5, big.mark = "'", scientific = FALSE), " %"),
    title = title,
    icon = icon,
    color = color,
    fill = TRUE,
    width = width
  )
}

modifySideBar <- function(input){
  shiny::insertUI(selector = "#sideMenu", where = "beforeEnd", shinydashboard::sidebarMenuOutput("Menu"))
  if(is.null(execMode) || !execMode){
    shiny::insertUI(selector = ".sidebar-menu", where = "afterEnd", addSidebarButtonDownload1())
  }else{
    shiny::insertUI(selector = ".sidebar-menu", where = "afterEnd", addSidebarButtonDownload())
  }
}

# Run from web browser
addSidebarButtonDownload <- function(){
  tags$div(
    style = "padding: 20px; color: white;",
    tags$hr(),
    tags$div(
      shiny::downloadButton("Fundamental_Data.xlsx", "Export FDS", style = "color: black !important; display: inline;")
    ),
    tags$hr(),
    tags$div(
      shiny::downloadButton("Standalones_Simulations.csv", "Export Standalones Sim.", style = "color: black !important; display: inline;")
    ),
    tags$br(),
    tags$div(
      shiny::downloadButton("Risk_factors_simulations.csv", "Export Risk Factors Sim.", style = "color: black !important; display: inline;")
    ),
    # tags$br(),
    # tags$div(
    #   shiny::downloadButton("Waterfall.pdf", "Export Waterfall Chart", style = "color: black !important; display: inline;")
    # ),
    tags$hr(),
    tags$div(
      shiny::actionButton("newSim", "New simulation")
    )
  )
}

# Run from RStudio window
addSidebarButtonDownload1 <- function(){
  tags$div(
    style = "padding: 20px; color: white;",
    tags$hr(),
    tags$div(
      shiny::actionButton("downloadExcel", "Export FDS")
    ),
    tags$hr(),
    tags$div(
      shiny::actionButton("downloadSimulations", "Export Standalones Sim.")
    ),
    tags$br(),
    tags$div(
      shiny::actionButton("downloadMarketSimulations", "Export Risk Factors Sim.")
    ),
    # tags$br(),
    # tags$div(
    #   shiny::actionButton("downloadPDF", "Export Waterfall Chart")
    # ),
    tags$hr(),
    tags$div(
      shiny::actionButton("newSim", "New simulation")
    )
  )
}

ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "SST Dashboard"),
  sidebar = shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "sideMenu",
      shinydashboard::menuItem("Dashboard", tabName = "sstTool", icon = icon("fas fa-gauge-high")),
      shinydashboard::menuItem("Legal Notices", tabName = "legal", icon = icon("fas fa-scale-balanced"))
    )
  ),
  body = shinydashboard::dashboardBody(
    preventQuitOnReload,
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "sstTool",
        div(id="sstTool"),
        shiny::fluidPage(
          tags$style(".shiny-file-input-progress {display: none}"),
          id = "input_page",
          shiny::fluidRow(
            shinydashboard::box(title = h3("Input SST Template(s)"), fileInput("excel_file", label = NULL, placeholder = "No portfolio selected", multiple = TRUE), status = "primary", width = "100%")
          ),
          shiny::fluidRow(
            shinydashboard::box(title = h3("Seed"), textInput("seed", label = NULL, value = 1), status = "primary"),
            shinydashboard::box(title = h3("Number of simulations"), textInput("sim_number", label = NULL, value = 1000000L), status = "primary")
          ),
          shiny::fluidRow(
            align="center",
            shiny::actionButton("button", h4("Run tool"), width = "50%")
          )
        )
      ),
      shinydashboard::tabItem(tabName = "legal", legalPage())
    )
  )
)
