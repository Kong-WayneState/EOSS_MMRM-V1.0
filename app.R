#'The application User-Interface and Server-Side

#'Step 1: In the R Studio console, install the necessary packages 
#'(only needed for the first time)
# install.packages(c("shiny", "shinyjs", shinydashboard", "shinytheme", DT", "dplyr", "mmrm"))

#'Step 2: Set the number of cores for parallel calculation. 
#'The default setup is with 6 cores.
num.core = 6     #select and run the code in the console
#'You can adjust this number based on your system capabilities. 
#'For example, if your computer has 32 cores, you may set it to 32 or a lower value.


#'Step 3: Click "Run App" button in the script editor toolbar to launch the Shiny app.

#library(rsconnect)
library(shiny)

source("data_sim.R")
source("test_sim.R")
source("mmrm_mod.R")


app_ui <- function(request) {
  tagList(
    fluidPage(theme = shinythemes::shinytheme("flatly"), collapsible = TRUE,
              # add this to change color of head
              tags$head(tags$style(HTML('.navbar-static-top {background-color: #0C5449;}',
                                        '.navbar-default .navbar-nav>.active>a 
                                        {background-color: #0C5449;}'))),
              titlePanel(""),
              navbarPage(title = "EOSS_MMRM V1.0", id = "navbar",
                         
                         tabPanel("Welcome!", icon = icon("home", lib = "glyphicon"),
                                  suppressWarnings(htmltools::includeHTML("home.html"))),
                         tabPanel("Instruction", 
                                  suppressWarnings(htmltools::includeHTML("instruction.html"))),
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

