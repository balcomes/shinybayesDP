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
        #actionButton("fishButton", label = "Fish"),
        #hidden(
        #  checkboxGroupInput("Check1",label=h4 ("Fish:"), choices = c("Bass","Shark","Tuna"))
        #),
        uiOutput('Button'),
        bookmarkButton(),
        selectInput("func",
                    "Select Function",
                    choices = c("bdpnormal", "bdpbinomial", "bdpsurvival"),
                    selected = "bdpnormal"),
        uiOutput("params"),
        conditionalPanel(
          condition = "input.func == 'bdpsurvival'",
        fileInput("file1", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        )),
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
        dataTableOutput("contents")
      )
)

server <- function(input, output, enableBookmarking = "url"){

  # Two-arm trial (OPC) example
  # Simulate survival data for a two-arm trial
  time   <- c(rexp(50, rate=1/20), # Current treatment
              rexp(50, rate=1/10), # Current control
              rexp(50, rate=1/30), # Historical treatment
              rexp(50, rate=1/5))  # Historical control
  status <- rexp(200, rate=1/40)
  status <- ifelse(time < status, 1, 0)

  # Collect data into a dataframe
  example_surv_2arm <- data.frame(status     = status,
                                  time       = time,
                                  historical = c(rep(0,100),rep(1,100)),
                                  treatment  = c(rep(1,50),rep(0,50),rep(1,50),rep(0,50)))

  params <- reactive({
    params <- as.list(args(input$func))
    params <- params[-length(params)]
    #print(params)
    params
  })

  params_names <- reactive({
    names(params())
    #print(names(params()))
  })

  survdata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }
    read.csv(inFile$datapath,header=TRUE)
  })

  output$params <- renderUI({
    lapply(params_names(),function(x){
      if(class(params()[[x]]) == "logical"){
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
      if(class(params()[[x]]) == "numeric"){
        do.call(textInput,list(x, label = x, value = params()[[x]]))
      }
      if(class(params()[[x]]) == "formula"){
        do.call(textInput,
                list(paste0(x), label = paste0(x),
                     value = "Surv(time, status) ~ historical + treatment"))
      }
      if(class(params()[[x]]) == "data"){
        do.call(textInput,list(x, label = x, value = output$contents))
      }
      if(class(params()[[x]]) == "NULL"){
        do.call(textInput,list(x, label = x, value = sample(1000,1)))
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

    if(length(survdata) > 0){
      if(input$func == "bdpsurvival"){
        if(length(survdata()) > 0){
          final <- eval(parse(text = paste0("bdpsurvival",
                                            "(",
                                            "Surv(time, status) ~ historical + treatment,",
                                            "data = survdata(),",
                                            "fix_alpha = TRUE",
                                            ")",
                                            collapse = ",")))
        }
        else{
          final <- bdpsurvival(Surv(time, status) ~ historical + treatment,
                               data = example_surv_2arm)
        }
      }
      else{
        final <- eval(parse(text = paste0(input$func,"(",
                                          paste0(final,collapse = ",")
                                          ,")")))
      }
    }
    final
  })

  output$discount <- renderPlot({
    plot(final(),type = "discount")
  })

  output$posteriors <- renderPlot({
    plot(final(),type = "posteriors")
  })

  output$density <- renderPlot({
    plot(final(),type = "density")
  })

  output$survival <- renderPlot({
    plot(final(),type = "survival")
  })

  output$summary <- renderPrint({
    summary(final())
  })

  output$contents <- renderDataTable({survdata()})

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

  #observeEvent(input$fishButton,{toggle("Check1")})
}

shinyApp(ui, server)
