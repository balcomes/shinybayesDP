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
        
        box(tabsetPanel(tabPanel("discount", "discount",
                                 plotOutput("discount")),
                        tabPanel("posteriors", "posteriors",
                                 plotOutput("posteriors")),
                        tabPanel("density", "density",
                                 plotOutput("density")),
                        tabPanel("survival", "survival",
                                 plotOutput("survival"))
        )),
        box(verbatimTextOutput("summary")),

        tags$head(tags$style(HTML("#summary {font-size: 8px;}")))
      ),
        hr(),
        dataTableOutput("contents")
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
                list(x, label = x,
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
    
    if(length(final) > 0){
      if(input$func == "bdpsurvival"){
        final <- eval(parse(text = paste0("bdpsurvival",
                                          "(",
                                          "Surv(time, status) ~ historical + treatment,",
                                          "data = survdata(),",
                                          "fix_alpha = TRUE",
                                          ")",
                                          collapse = ",")))
        print(survdata())
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
    plot(final())
  })

  output$summary <- renderPrint({
    summary(final())
  })
  
  survdata <- reactive({
    inFile <- input$file1
    if (is.null(inFile)){
      return(NULL)
    }
    read.csv(inFile$datapath,header=TRUE)
  })
  
  output$contents <- renderDataTable({survdata()})
  
  #observeEvent(input$fishButton,{toggle("Check1")})
}
  
shinyApp(ui, server)