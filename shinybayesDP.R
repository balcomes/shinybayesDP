library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- function(request) {
  dashboardPage(title = "bayesDP",
    dashboardHeader(title = "bayesDP"),
    dashboardSidebar(
      tags$head(tags$style(HTML(".sidebar{height:100vh;overflow-y:auto;}"))),
      br(),
      tags$div(class = "header", checked = NA,
               tags$a(href = "https://cran.r-project.org/package=bayesDP",
                      "View help files and download the package from CRAN")),
      br(),
      tags$div(class = "header", checked = NA,
               tags$a(href = "https://github.com/balcomes/bayesDP",
                      "Development version of bayesDP")),
      br(),
      bookmarkButton(),
      downloadButton("downloadReport", "Generate Report"),
      radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      uiOutput("funcdrop"),
      uiOutput("up"),
      uiOutput("writeformula"),
      uiOutput("colchoose"),
      uiOutput("params"),
      uiOutput("dev"),
      HTML("<br><br><br>")
    ),
    dashboardBody(
      fluidPage(
        tags$head(tags$style(HTML("body {width: 100% !important;
                                  max-width: 100% !important;}"))),
        tags$script('$(document).on("keypress", function (e) {
                    Shiny.onInputChange("secret", e.which);});'),
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        box(width = "100%", uiOutput("plottabs"))),
      hr()
    )
  )
}

server <- function(input, output, enableBookmarking = "url"){
  
  updata <- reactiveValues(x = NULL)
  
  params <- reactive({
    if(!is.null(input$funccheck) && input$funccheck == TRUE){
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

  survchosen <- reactive({
    data.frame(status     = updata$x[[input$status]],
               time       = updata$x[[input$time]],
               historical = updata$x[[input$historical]],
               treatment  = updata$x[[input$treatment]])
  })

  output$params <- renderUI({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpsurvival" || input$func == "bdpregression"){
        omit <- c("formula", "data")
      }
      else{
        omit <- c()
      }
    }
    else{
      omit <- c()
      if(!is.null(input$formulacheck) || input$formulacheck == TRUE){
        omit <- c(omit, "formula")
      }
      if(!is.null(input$datacheck) || input$datacheck == TRUE){
        omit <- c(omit, "data")
      }
    }
      
    out <- lapply(setdiff(params_names(),omit),function(x){
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
    menuItem("Inputs", icon = icon("tasks"), out)
  })

  final <- reactive({
    final <- c()
    for(i in params_names()){
      final <- c(final, input[[i]])
    }
    
    if(!is.null(input$funccheck) && input$funccheck == TRUE){
      if(!is.null(input$formulacheck) && input$formulacheck == TRUE){
    
      }
      if(!is.null(input$datacheck) && input$datacheck == TRUE){
        
      }
      
      final <- eval(parse(text = paste0(input$anyfunc,"(",
                                        paste0(final,collapse = ",")
                                        ,")")))
    }
    
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(length(final > 0)){
        if(input$func == "bdpsurvival" || input$func == "bdpregression" &&
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
  
  discount <- reactive({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        plot(final(), type = "discount")
      }
    }
  })
  posteriors <- reactive({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        plot(final(), type = "posteriors")
      }
    }
  })
  density <- reactive({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        plot(final(), type = "density")
      }
    }
  })
  
  output$discount <- renderPlot({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        plot(final(), type = "discount")
      }
    }
  })
  output$posteriors <- renderPlot({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        plot(final(), type = "posteriors")
      }
    }
  })
  output$density <- renderPlot({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        plot(final(), type = "density")
      }
    }
  })
  
  survival <- reactive({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpsurvival"){
        plot(final(), type = "survival")
      }
    }
  })
  output$survival <- renderPlot({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpsurvival"){
        plot(final(), type = "survival")
      }
    }
  })

  output$summary <- renderPrint({
    summary(final())
  })
  output$print <- renderPrint({
    print(final())
  })
  
  output$contents <- renderDataTable({updata$x})
  
  output$plottabs <- renderUI({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpsurvival"){
        tabsetPanel(
          tabPanel("Print", verbatimTextOutput("print")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel(discount()$plot$labels$title, plotOutput("discount")),
          tabPanel(survival()$plot$labels$title, plotOutput("survival")),
          tabPanel("Help", uiOutput("vig")),
          tabPanel("Data", dataTableOutput("contents"))
        )
      }
    }
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpregression"){
        tabsetPanel(
          tabPanel("Print", verbatimTextOutput("print")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          #tabPanel(discount()$plot$labels$title, plotOutput("discount")),
          #tabPanel(survival()$plot$labels$title, plotOutput("survival")),
          tabPanel("Help", uiOutput("vig")),
          tabPanel("Data", dataTableOutput("contents"))
        )
      }
    }
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" || input$func == "bdpbinomial"){
        tabsetPanel(
          tabPanel("Print", verbatimTextOutput("print")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel(discount()$plot$labels$title, plotOutput("discount")),
          tabPanel(posteriors()$plot$labels$title, plotOutput("posteriors")),
          tabPanel(density()$plot$labels$title, plotOutput("density")),
          tabPanel("Help", uiOutput("vig"))
        )
      }
    }
    else{
      tabsetPanel(
        tabPanel("Print", verbatimTextOutput("print")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    }
  })
  
  output$checks <- renderUI({
    if(!is.null(input$funccheck) && input$funccheck == TRUE){
      list(checkboxInput("formulacheck", "formulacheck"),
           checkboxInput("datacheck", "datacheck"))
    }
  })
  
  output$funcdrop <- renderUI({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      selectInput("func",
                  "Select Function",
                  choices = c("bdpnormal",
                              "bdpbinomial",
                              "bdpsurvival",
                              "bdpregression"),
                  selected = "bdpnormal")
    }
  })
  
  output$vig <- renderUI({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal"){
        mdout <- do.call(includeHTML,
                         list(system.file("doc",
                                          "bdpnormal-vignette.html",
                                          package="bayesDP")))
      }
      if(input$func == "bdpbinomial"){
        mdout <- do.call(includeHTML,
                         list(system.file("doc",
                                          "bdpbinomial-vignette.html",
                                          package="bayesDP")))
      }
      if(input$func == "bdpsurvival"){
        mdout <- do.call(includeHTML,
                         list(system.file("doc",
                                          "bdpsurvival-vignette.html",
                                          package="bayesDP")))
      }
      if(input$func == "bdpregression"){
        mdout <- do.call(includeHTML,
                         list(system.file("doc",
                                          "bdpregression-vignette.html",
                                          package="bayesDP")))
      }
      else{

      }
      mdout
    }
  })
  
  output$up <- renderUI({
    if(input$func == "bdpsurvival" ||
       input$func == "bdpregression" ||
       (!is.null(input$datacheck) && input$datacheck == TRUE)){
      out <- list()
      out <- list(out,tags$style(type='text/css',
                                 "button#example_button { margin-left: 12px; }"))
      out <- list(out, actionButton("example_button",
                                    label = "Use Example Data"))
      out <- list(out, fileInput("file1", "Upload .csv File",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")))
    }
    menuItem("Data", icon = icon("table"),out)
  })
  
  output$funcname <- renderUI({
    if(!is.null(input$funccheck) && input$funccheck == TRUE){
      textInput("anyfunc","Write in your function name")
    }
  })
  
  output$writeformula <- renderUI({
    if(input$func == "bdpsurvival" ||
       input$func == "bdpregression" ||
       (!is.null(input$formulacheck) && input$formulacheck == TRUE)){
      menuItem("Formula",
               icon = icon("bar-chart-o"),
      textInput("Formula",
                label = "Formula",
                value = "Surv(time, status) ~ historical + treatment"))
    }
  })
  
  output$colchoose <- renderUI({
    if((input$func == "bdpsurvival" || input$func == "bdpregression") &&
       ((is.null(input$funccheck)  || input$funccheck == FALSE))){
      survcols <- c("status", "time", "historical", "treatment")
      
      survnames <- names(updata$x)
      
      out <- lapply(survcols,function(x){
        do.call(
          selectInput,list(x,
                           paste0("Select ",x),
                           choices = survnames,
                           selected = x)
        )
      })
      menuItem("Column Select", icon = icon("columns"), out)
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      system(paste0("touch ",src))
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)
      
      out <- rmarkdown::render('report.Rmd', switch(
        input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.rename(out, file)
    }
  )
  
  secret <- reactiveValues(x = 0)
  
  observeEvent(input$secret,{
    if(input$secret == 96){
      secret$x <- 1
    }
  })

  output$dev <- renderUI({
    conditionalPanel(
      condition = "secret == 96",
    
    if(secret$x == 1){
      menuItem("Dev Tool", icon = icon("key"),
               checkboxInput("funccheck", "Use your own function"),
               uiOutput("funcname"),
               uiOutput("checks"))
    })
  })
  
}

shinyApp(ui, server, enableBookmarking = "url")
