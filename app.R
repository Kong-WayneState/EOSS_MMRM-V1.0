#' The application User-Interface and Server-Side

library(rsconnect)
library(shiny)
library(shinydashboard)
library(parallel)


#'Set number of cores for parallel calculation
# num.core = 6
num.core = detectCores()-1 # at most

source("data_sim.R")
source("test_sim.R")
source("mmrm_mod.R")


app_ui <- function(request) {
  tagList(
    fluidPage(theme = shinythemes::shinytheme("flatly"), collapsible = TRUE,
              # add this to change color of head
              tags$head(tags$style(HTML('.navbar-static-top {background-color: #0C5449;}',
                                        '.navbar-default .navbar-nav>.active>a {background-color: #0C5449;}'))),
              titlePanel(""),
              navbarPage(title = "EOSS_MMRM V1.0", id = "navbar",
                         
                         tabPanel("Welcome!", icon = icon("home", lib = "glyphicon"),
                                  suppressWarnings(htmltools::includeHTML("home.html"))),
                         tabPanel("Instruction", suppressWarnings(htmltools::includeHTML("instruction.html"))),
                         tabPanel("Shiny App", (mod_ui("mod_fu"))),
              )
    )
  )
  
}



app_server <- function(input, output, session) {
  mod_server("mod_fu")
}




shinyApp(
  #options = list(launch.browser = TRUE),
  ui = app_ui, 
  server = app_server)

