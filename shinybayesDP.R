library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)

ui <- dashboardPage(title = "bayesDP",
      dashboardHeader(title = "bayesDP"),
      dashboardSidebar(
        useShinyjs(),
        tags$head(tags$style(HTML(".sidebar{height:100vh;overflow-y:auto;}"))),
        tags$div(class = "header", checked = NA,
                 tags$a(href = "https://cran.r-project.org/package=bayesDP",
                        "View help files and download the package from CRAN")),
        br(),
        
        checkboxInput("funccheck", "Use your own function"),
        textInput("anyfunc","Write in your function name"),
        
        conditionalPanel(
          condition = "input.funccheck == FALSE",
          selectInput("func",
                      "Select Function",
                      choices = c("bdpnormal", "bdpbinomial", "bdpsurvival"),
                      selected = "bdpnormal")),
        
        conditionalPanel(
          condition = "input.funccheck == TRUE",
          checkboxInput("formulacheck","formulacheck"),
          checkboxInput("datacheck","datacheck"),
          tags$style(type='text/css', "button#example_button { margin-left: 12px; }"),
          actionButton("example_button", label = "Use Example Data"),
          fileInput("file1", "Upload .csv File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          )),
        
        conditionalPanel(
          condition = "input.func == 'bdpsurvival'",
          uiOutput("colchoose"),
          textInput("Formula",
                    label = "Formula",
                    value = "Surv(time, status) ~ historical + treatment")),
        
        uiOutput("params"),
        HTML("<br><br><br>")
      ),
      dashboardBody(
        fluidPage(
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          box(width = "100%",
            uiOutput("plottabs")
          )#,
          #box(verbatimTextOutput("summary")),
          #tags$head(tags$style(HTML("#summary {font-size: 8px;}")))
        ),
        hr(),
        conditionalPanel(
          condition = "input.func == 'bdpsurvival'",
          dataTableOutput("contents"))
      )
)

server <- function(input, output, enableBookmarking = "url"){
  
  params <- reactive({
    
    if(input$funccheck == TRUE){
      params <- as.list(args(input$anyfunc))
    }
    else{
      params <- as.list(args(input$func))
    }
    params <- params[-length(params)]
    params
  })

  params_names <- reactive({
    names(params())
  })
  
  updata <- reactiveValues(x = NULL)
  
  observe({
    input$example_button
    # Two-arm trial (OPC) example
    # Simulate survival data for a two-arm trial
    time   <- c(rexp(50, rate=1/20), # Current treatment
                rexp(50, rate=1/10), # Current control
                rexp(50, rate=1/30), # Historical treatment
                rexp(50, rate=1/5))  # Historical control
    status <- rexp(200, rate=1/40)
    status <- ifelse(time < status, 1, 0)
    
    # Collect data into a dataframe
    updata$x <- data.frame(status     = status,
               time       = time,
               historical = c(rep(0,100),rep(1,100)),
               treatment  = c(rep(1,50),
                              rep(0,50),
                              rep(1,50),
                              rep(0,50)))
  })
  
  observe({
    inFile <- input$file1
    if (is.null(inFile)){
      updata$x <- NULL
    }
    else{
      updata$x <- read.csv(inFile$datapath,header=TRUE)
    }
  })

  output$colchoose <- renderUI({
    
    survcols <- c("status", "time", "historical", "treatment")

    survnames <- names(updata$x)
    
    lapply(survcols,function(x){
      do.call(
        selectInput,list(x,
                    paste0("Select ",x),
                    choices = survnames,
                    selected = x)
      )
    })
  })
  
  survchosen <- reactive({
    data.frame(status     = updata$x[[input$status]],
               time       = updata$x[[input$time]],
               historical = updata$x[[input$historical]],
               treatment  = updata$x[[input$treatment]])
  })

  output$params <- renderUI({
    
    if(input$funccheck == FALSE){
      if(input$func == "bdpsurvival"){
        omit <- c("formula", "data")
      }
      else{
        omit <- c()
      }
    }
    else{
      omit <- c()
      if(input$formulacheck == TRUE){
        omit <- c(omit, "formula")
      }
      if(input$formulacheck == TRUE){
        omit <- c(omit, "data")
      }
    }
      
    lapply(setdiff(params_names(),omit),function(x){
      if(class(params()[[x]]) == "logical"){
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
      if(class(params()[[x]]) == "numeric"){
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
      if(class(params()[[x]]) == "NULL"){
        do.call(textInput,list(x, label = x, value = 100))
      }
      else{
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
    })
  })

  final <- reactive({
    
    final <- c()
    for(i in params_names()){
      final <- c(final, input[[i]])
    }
    
    if(input$funccheck == TRUE){
      
      if(input$formulacheck == TRUE){
        
      }
      
      if(input$datacheck == TRUE){
        
      }
      
      final <- eval(parse(text = paste0(input$anyfunc,
                                        "(",
                                        "formula = ",
                                        input$Formula,
                                        ",",
                                        "data = updata$x,",
                                        paste0(final,collapse = ","),
                                        ")",
                                        collapse = ",")))
      
    }
    if(input$funccheck == FALSE){
      if(length(final > 0)){
        if(input$func == "bdpsurvival" &&
           length(input$status) > 0 &&
           length(input$time) > 0 &&
           length(input$historical) > 0 &&
           length(input$treatment) > 0 &&
           length(input$func) > 0 &&
           length(survchosen()) > 0){
          
          final <- eval(parse(text = paste0(input$func,
                                            "(",
                                            "formula = ",
                                            input$Formula,
                                            ",",
                                            "data = survchosen(),",
                                            paste0(final,collapse = ","),
                                            ")",
                                            collapse = ",")))
        }
        if(input$func %in% c("bdpnormal","bdpbinomial")){
          final <- eval(parse(text = paste0(input$func,"(",
                                            paste0(final,collapse = ",")
                                            ,")")))
        }
      }
    }
    final
  })

  output$discount <- renderPlot({
    plot(final(), type = "discount")
  })
  output$survival <- renderPlot({
    plot(final(), type = "survival")
  })
  output$posteriors <- renderPlot({
    plot(final(), type = "posteriors")
  })
  output$density <- renderPlot({
    plot(final(), type = "density")
  })

  output$summary <- renderPrint({
    summary(final())
  })
  
  output$print <- renderPrint({
    print(final())
  })
  
  output$contents <- renderDataTable({updata$x})
  
  output$plottabs <- renderUI({
    if(input$func == "bdpsurvival"){
      tabsetPanel(
        tabPanel("print", verbatimTextOutput("print")),
        tabPanel("summary", verbatimTextOutput("summary")),
        tabPanel("discount", plotOutput("discount")),
        tabPanel("survival", plotOutput("survival"))
      )
    }
    else{
      tabsetPanel(
        tabPanel("print", verbatimTextOutput("print")),
        tabPanel("summary", verbatimTextOutput("summary")),
        tabPanel("discount", plotOutput("discount")),
        tabPanel("posteriors", plotOutput("posteriors")),
        tabPanel("density", plotOutput("density"))#,
        #tabPanel("help", uiOutput("vig"))
      )
    }
  })
  
  observeEvent(input$funccheck, {
    toggle("anyfunc")  # toggle is a shinyjs function
  })
  
  output$vig <- renderUI(tags$iframe(width="100%",{includeHTML(system.file("doc", "bdpnormal-vignette.html", package="bayesDP"))}))
  
}

shinyApp(ui, server)
