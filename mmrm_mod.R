#' Mod for ui and server
#' 


mod_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        br(),
        numericInput(ns("seed"), "Random Seed", 1234),
        numericInput(ns("n"), "Number of Simulations", 1000),
        numericInput(ns("alpha"), "Significant Level", 0.05, 
                     min = 0.01, max = 0.2, step = 0.01),
        br(),
        radioButtons(ns("nfollow.up"), "Number of Post Baseline Visits",
                     choices = list("TWO" = 2, "THREE" = 3), selected = 2),
        br(),
        actionButton(ns("runSim"), "Simulate", #icon= icon("greater-than-equal"),
                     style = "width:80%; background-color:#0C5449; 
                     color:White;font-size: 20px;font-weight:bold;"),
        br()
      ),
      mainPanel(
        width = 8,
        tags$style(".small-box.bg-yellow { background-color: #FFCC33 !important; color: #0C5449 !important; }"),
        tabsetPanel(
          tabPanel(title = "Experimental Design", 
                   tags$head(tags$script(HTML(js))), 
                   useShinydashboard(),
                   fluidRow(
                     br(),
                     column(12, 
                            column(3, numericInput(ns("ss"), "Sample Size", 160)),
                            column(3, numericInput(ns("trt.rate"), "TRT Rate", 
                                                   0.5, min = 0, max = 1, step = 0.05)),
                            column(5, align="left", 
                                   shinydashboard::valueBoxOutput(ns("simulationPower"), width = 8))
                     ),
                     br(),
                     column(12, 
                            column(4, numericInput(ns("mu0"), "Baseline Value", 38, min = 0)),
                            column(4, numericInput(ns("sd0"), "Common SD", 10.5, min = 0)),
                            column(4, numericInput(ns("corr0"),"Corr Among Visits", 
                                                   0.5, min = -1, max = 1))),
                     
                     br(),
                     uiOutput(ns("fu"))
                   )
          ),
          tabPanel("Simulated Data",
                   tags$head(tags$style(".butt{background:#FFCC33;} .butt{color:#0C5449;} 
                                        .butt{font-size: 18px;} .butt{font-weight:bold;}")),
                   fluidRow(
                     br(),
                     br(),
                     column(12, align="center", tags$p("A Trial of Simulated Data", style = "width:50%;
                background-color:#FFCC33; color:#0C5449; font-size: 18px;font-weight:bold;")),
                     br(),
                     br(),
                     column(12, align="center", DT::dataTableOutput(ns("showdata"))),
                     br(),
                     br(),
                     column(12, align="center", downloadButton(ns("downloadData"), "Download Data",
                                                               class = "butt"))
                     
                   )
          )
        )
      )
    )
  )      
}




mod_server <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    shinyjs::useShinyjs()
    
    # Elements of post follow-ups
    output$fu <- renderUI({
      buttons <- as.list(1:input$nfollow.up)
      div(class = "dynamicSI",
          lapply(buttons, function(i)
            column(
              width = 4,
              div(h4(paste("Post Baseline Visit",i), 
                     style = "width:80%;background-color:#FFCC33; 
                                  color:#0C5449;font-size:16px;font-weight:bold;")),
              div(class = "input",
                  numericInput(inputId = ns(paste0("eff.fu",i)),
                               label = paste("Efficacy",i), value = 2*(i+1))
              ),
              div(class = "input",
                  numericInput(inputId = ns(paste0("eff.efu.fu",i)),
                               label = paste("EFU Efficacy", i),value = i+1)
              ),
              div(class = "input",
                  sliderInput(inputId = ns(paste0("et.rate.trt.fu",i)),
                              label = paste("ET Rate TRT",i),value = 0.25-(i-1)*0.05,
                              min = 0, max = 0.5, step=0.01)
              ),
              div(class = "input",
                  sliderInput(inputId = ns(paste0("et.rate.pbo.fu", i)),
                              label = paste("ET Rate PBO",i), value = 0.15-(i-1)*0.05,
                              min = 0, max = 0.5, step = 0.01)
              ),
              div(class = "input",
                  sliderInput(inputId = ns(paste0("efu.rate.fu",i)),
                              label = paste("EFU Rate", i), value = 0.2,
                              min = 0, max = 1, step = 0.05)
              ))))
      
    })
    
    # react to changes in dynamically generated select Input's
    #ACTION BUTTON
    observeEvent(input$runSim, {
      withProgress(message = "Generating Data", value=0, {
        Sys.sleep(0.1)
        
        if(input$nfollow.up == 2){
          finalData <- reactive(
            # to store the processed data
            {isolate(CAPS5.MMRM.sim.n(n=input$n, 
                                      seed=input$seed, 
                                      ss=input$ss,
                                      trt.rate=input$trt.rate,
                                      mu0=input$mu0, 
                                      sd0=input$sd0, 
                                      corr0=input$corr0,
                                      fu1 = list(eff=input$eff.fu1, 
                                                 et.rate.trt=input$et.rate.trt.fu1,
                                                 et.rate.pbo=input$et.rate.pbo.fu1,
                                                 eff.efu=input$eff.efu.fu1, 
                                                 efu.rate=input$efu.rate.fu1),
                                      fu2 = list(eff=input$eff.fu2, 
                                                 et.rate.trt=input$et.rate.trt.fu2,
                                                 et.rate.pbo=input$et.rate.pbo.fu2,
                                                 eff.efu=input$eff.efu.fu2, 
                                                 efu.rate=input$efu.rate.fu2)))}
          )}else{
            finalData <- reactive(
              # to store the processed data
              {isolate(CAPS5.MMRM.sim.n(n=input$n, 
                                        seed=input$seed, 
                                        ss=input$ss,
                                        trt.rate=input$trt.rate,
                                        mu0=input$mu0, 
                                        sd0=input$sd0, 
                                        corr0=input$corr0,
                                        fu1 = list(eff=input$eff.fu1, 
                                                   et.rate.trt=input$et.rate.trt.fu1,
                                                   et.rate.pbo=input$et.rate.pbo.fu1,
                                                   eff.efu=input$eff.efu.fu1, 
                                                   efu.rate=input$efu.rate.fu1),
                                        fu2 = list(eff=input$eff.fu2, 
                                                   et.rate.trt=input$et.rate.trt.fu2,
                                                   et.rate.pbo=input$et.rate.pbo.fu2,
                                                   eff.efu=input$eff.efu.fu2, 
                                                   efu.rate=input$efu.rate.fu2),
                                        fu3 = list(eff=input$eff.fu3, 
                                                   et.rate.trt=input$et.rate.trt.fu3,
                                                   et.rate.pbo=input$et.rate.pbo.fu3,
                                                   eff.efu=input$eff.efu.fu3, 
                                                   efu.rate=input$efu.rate.fu3)))}
            )}
        
        sampleData = finalData()[[1]]
        output$showdata = DT::renderDT(sampleData , rownames = FALSE, 
                                       class = "display nowrap compact", filter = "top",
                                       options = list(
                                         scrollX = TRUE,
                                         scrollY = TRUE,
                                         autoWidth = FALSE))
        
        
        
        incProgress(1, message = "Calculating Power")
        
        Sys.sleep(0.1)
        power =  CAPS5.MMRM.test.n(finalData(), alpha =input$alpha)
        
        output$simulationPower =  shinydashboard::renderValueBox({
          shinydashboard::valueBox(value = tags$b(paste0(power*100, "%"),
                                                  style = "font-size: 80%;"),
                                   subtitle = tags$b("Simulated Power", style="font-size: 16px;"), 
                                   color = 'yellow'
          )}
        )
        
        
        incProgress(1, message = "COMPLETE!")
        Sys.sleep(0.1)
      })
      
      # DOWNLOAD BUTTON 
      output$downloadData <- downloadHandler(
        filename = function() { 
          paste("data-mmrm-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          write.csv(finalData(), file)
        })
      
    })
    
    
  }
  )}


##############################################################################
useShinydashboard <- function() {
  if (!requireNamespace(package = "shinydashboard"))
    message("Package 'shinydashboard' is required to run this function")
  deps <- htmltools::findDependencies(shinydashboard::dashboardPage(
    header = shinydashboard::dashboardHeader(),
    sidebar = shinydashboard::dashboardSidebar(),
    body = shinydashboard::dashboardBody()
  ))
  htmltools::attachDependencies(tags$div(class = "main-sidebar", 
                                         style = "display: none;"), 
                                value = deps)
}



#' for dynamic input
js <- "
$(document).on('change', '.dynamicSI .input input', function(){
  Shiny.setInputValue('lastSelectId', this.id, {priority: 'event'});
});
"


