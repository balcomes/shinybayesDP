library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)

ui <- dashboardPage(title = "bayesDP",
      dashboardHeader(title = "bayesDP"),
      dashboardSidebar(
        shinyjs::useShinyjs(),
        tags$head(tags$style(HTML(".sidebar{height:100vh;overflow-y:auto;}"))),
        #hidden(
        #  checkboxGroupInput("Check1",label=h4 ("Fish:"), choices = c("Bass","Shark","Tuna"))
        #),
        tags$div(class = "header", checked = NA,
                 tags$a(href = "https://cran.r-project.org/package=bayesDP",
                        "Download package from CRAN")
        ),
        br(),
        #uiOutput('Button'),
        #bookmarkButton(),
        selectInput("func",
                    "Select Function",
                    choices = c("bdpnormal", "bdpbinomial", "bdpsurvival"),
                    selected = "bdpnormal"),
        conditionalPanel(
          condition = "input.func == 'bdpsurvival'",
          actionButton("example_button", label = "Use Example Data"),
          tags$style(type='text/css', "button#example_button { margin-left: 12px; }"),
          fileInput("file1", "Choose CSV File",
                    accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
          ),
        uiOutput("colchoose")),
        uiOutput("params"),
        HTML("<br><br><br>")
      ),
      dashboardBody(
        fluidPage(
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }"),
          box(
            uiOutput("plottabs")
          ),
          box(verbatimTextOutput("summary")),
          tags$head(tags$style(HTML("#summary {font-size: 8px;}")))
        ),
        hr(),
        conditionalPanel(
          condition = "input.func == 'bdpsurvival'",
          dataTableOutput("contents"))
      )
)

server <- function(input, output, enableBookmarking = "url"){
  
  params <- reactive({
    params <- as.list(args(input$func))
    params <- params[-length(params)]
    params
  })

  params_names <- reactive({
    names(params())
  })
  
  survdata <- reactiveValues(x = NULL)
  
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
  survdata$x <- data.frame(status     = status,
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
      survdata$x <- NULL
    }
    else{
      survdata$x <- read.csv(inFile$datapath,header=TRUE)
    }
  })

  output$colchoose <- renderUI({
    
    survcols <- c("status", "time", "historical", "treatment")

    survnames <- names(survdata$x)
    
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
    data.frame(status     = survdata$x[[input$status]],
               time       = survdata$x[[input$time]],
               historical = survdata$x[[input$historical]],
               treatment  = survdata$x[[input$treatment]])
  })

  output$params <- renderUI({
    
    if(input$func == "bdpsurvival"){
      lapply(params_names()[c(-1,-2)],function(x){
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
    }
    else{
      lapply(params_names(),function(x){
      if(class(params()[[x]]) == "logical"){
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
      if(class(params()[[x]]) == "numeric"){
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
      if(class(params()[[x]]) == "NULL"){
        do.call(textInput,list(x, label = x, value = 100))
      }
      })
    }
  })

  final <- reactive({
    
    final <- c()
    for(i in params_names()){
      final <- c(final, input[[i]])
    }
    if(length(final > 0 && length(input$func) > 0 && length(survchosen()) > 0)){
      if(input$func == "bdpsurvival" &&
         length(input$status) > 0 &&
         length(input$time) > 0 &&
         length(input$historical) > 0 &&
         length(input$treatment) > 0){
        final <- eval(parse(text = paste0("bdpsurvival",
                                          "(",
                                          "Surv(time, status) ~ historical + treatment,",
                                          "data = survchosen(),",
                                          "fix_alpha = input$fix_alpha",
                                          ")",
                                          collapse = ",")))
      }
          #else{
          #  final <- bdpsurvival(Surv(time, status) ~ historical + treatment,
          #                       data = example_surv_2arm)
          #}
      else{
        final <- eval(parse(text = paste0(input$func,"(",
                                          paste0(final,collapse = ",")
                                          ,")")))
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

  output$contents <- renderDataTable({survdata$x})
  
  output$plottabs <- renderUI({
    if(input$func == "bdpsurvival"){
      tabsetPanel(
        tabPanel("discount", plotOutput("discount")),
        tabPanel("survival", plotOutput("survival"))
      )
    }
    else{
      tabsetPanel(
        tabPanel("discount", plotOutput("discount")),
        tabPanel("posteriors", plotOutput("posteriors")),
        tabPanel("density", plotOutput("density"))
      )
    }
  })
}

shinyApp(ui, server)
