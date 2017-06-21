library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(highlight)
library(knitr)
library(rmarkdown)

ui <- function(request) {
  
  la <- lapply(paste0(letters,7),function(x){do.call(uiOutput,list(x))})
  ch <- lapply(paste0(letters,4),function(x){do.call(uiOutput,list(x))})
  tx <- lapply(paste0(letters,5),function(x){do.call(uiOutput,list(x))})
  df <- lapply(paste0(letters,6),function(x){do.call(uiOutput,list(x))})
  
  insert <- list()
  for(i in 1:length(ch)){
    insert <- list(insert,hr(),la[i],br(),ch[i],br(),tx[i],df[i])
  }
  
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
      br(),
      br(),
      downloadButton("downloadReport", "Generate Report"),
      radioButtons('format', 'Document format', c('HTML', 'PDF', 'Word'),
                   inline = TRUE),
      uiOutput("dev"),
      uiOutput("funcdrop"),
      uiOutput("up"),
      uiOutput("writeformula"),
      uiOutput("colchoose"),
      uiOutput("params"),
      uiOutput("togparams"),
      insert,
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
  
  ##############################################################################
  # Get function parameters and build dynamic UI elements.
  ##############################################################################
  
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
  
  output$params <- renderUI({
    if(!is.null(input$func) && (is.null(input$funccheck) || input$funccheck == FALSE)){
      if(!is.null(input$func) && (input$func == "bdpsurvival" || input$func == "bdpregression")){
        omit <- c("formula", "data")
      }
      else{
        omit <- c()
      }
    }
    else{
      omit <- c()
      if("formula" %in% params_names()){
        omit <- c(omit, "formula")
      }
      if("data" %in% params_names()){
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
  
  
  ##############################################################################
  # Upload CSV data as a data frame or use example data.
  ##############################################################################
  
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
  
  output$up <- renderUI({
    out <- list()
    if("data" %in% params_names()){
      out <- list(out,tags$style(type='text/css',
                                 "button#example_button {margin-left: 12px;}"))
      out <- list(out, actionButton("example_button",
                                    label = "Use Example Data"))
      out <- list(out, fileInput("file1", "Upload .csv File",
                                 accept = c(
                                   "text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")))
    }
    menuItem("Data", icon = icon("table"), out)
  })

  survchosen <- reactive({
    data.frame(status     = updata$x[[input$status]],
               time       = updata$x[[input$time]],
               historical = updata$x[[input$historical]],
               treatment  = updata$x[[input$treatment]])
  })

  final <- reactive({
    if(!is.null(input$func)){
      final <- c()
      
      skip <- ("data" %in% params_names()) + ("formula" %in% params_names())
      
      for(i in params_names()){
        final <- c(final, input[[i]])
        if(i %in% names(which(lapply(params(),
                                     function(x){class(x)=="character"})==TRUE))){
          j <- which(params_names()==i)
          final[j-skip]<- paste0("'",final[j-skip],"'")
        }
      }
      
      if(!is.null(input$funccheck) && input$funccheck == TRUE){
        myfunc <- input$anyfunc
      }
      else{
        myfunc <- input$func
      }
      
      if(length(final > 0)){
      
        if(!is.null(input$funccheck) && input$funccheck == TRUE){
          
          if("data" %in% params_names()  && "formula" %in% params_names()){
            return(
              eval(parse(text = paste0(myfunc,
                                       "(",
                                       "formula = ",
                                       input$Formula,
                                       ",",
                                       "data = updata$x,",
                                       paste0(final,collapse = ","),
                                       ")",
                                       collapse = ",")))
            )
          }
          else{
            return(
              eval(parse(text = paste0(myfunc,"(",
                                       paste0(final,collapse = ",")
                                       ,")")))
            )
          }
        }
        else{
    
          if(!is.null(input$func) && input$func %in% c("bdpnormal","bdpbinomial")){
            return(
              eval(parse(text = paste0(myfunc,"(",
                                       paste0(final,collapse = ",")
                                       ,")")))
            )
          }
          if(!is.null(input$func) && (input$func == "bdpsurvival" || input$func == "bdpregression") &&
             length(input$status) > 0 &&
             length(input$time) > 0 &&
             length(input$historical) > 0 &&
             length(input$treatment) > 0 &&
             length(input$func) > 0 &&
             length(survchosen()) > 0){
          #if("data" %in% params_names()  && "formula" %in% params_names()){
            return(
              eval(parse(text = paste0(myfunc,
                                              "(",
                                              "formula = ",
                                              input$Formula,
                                              ",",
                                              "data = survchosen(),",
                                              paste0(final,collapse = ","),
                                              ")",
                                              collapse = ",")))
            )
          }
        }
      }
    }
  })
  
  
  ##############################################################################
  # Produce plots with dynamic tab names.
  ##############################################################################
  
  discount <- reactive({
    if(!is.null(input$func) && (is.null(input$funccheck) || input$funccheck == FALSE)){
      if(input$func == "bdpnormal" ||
         input$func == "bdpbinomial" ||
         input$func == "bdpsurvival"){
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
  
  survival <- reactive({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpsurvival"){
        plot(final(), type = "survival")
      }
    }
  })
  
  output$discount <- renderPlot({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal" ||
         input$func == "bdpbinomial" ||
         input$func == "bdpsurvival"){
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
  
  output$survival <- renderPlot({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpsurvival"){
        plot(final(), type = "survival")
      }
    }
  })
  
  
  ##############################################################################
  # Print and Summary tabs.
  ##############################################################################
  
  output$summary <- renderPrint({
    summary(final())
  })
  output$print <- renderPrint({
    print(final())
  })
  
  output$simpleplot <- renderPlot({
    plot(final())
  })
  
  
  ##############################################################################
  # Data table tab for uploaded CSV
  ##############################################################################
  
  output$contents <- renderDataTable({updata$x})
  
  #output$devcontents <- lapply(params_names(),function(x){
  #  do.call(tabPanel,list(x,do.call(dataTableOutput,list(x, label = x, value = params()[[x]]))))
  #})
    
  
  #lapply(params_names(),function(x){
  #  do.call(renderDataTable(),list(x, label = x, value = params()[[x]]))
  #})
    
    
  output$devcontents <- renderDataTable({
    as.data.frame(eval(parse(text = input[[params_names()[1]]])))
  })
  
  
  ##############################################################################
  # Tab set panel structure for each function.
  ##############################################################################
  
  output$plottabs <- renderUI({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(!is.null(input$func) && input$func == "bdpsurvival"){
        return(
          tabsetPanel(
            tabPanel("Print", verbatimTextOutput("print")),
            tabPanel("Summary", verbatimTextOutput("summary")),
            tabPanel(discount()$plot$labels$title, plotOutput("discount")),
            tabPanel(survival()$plot$labels$title, plotOutput("survival")),
            tabPanel("Help", uiOutput("vig")),
            tabPanel("Source", uiOutput("src")),
            tabPanel("Data", dataTableOutput("contents"))
          )
        )
      }

      if(!is.null(input$func) && input$func == "bdpregression"){
        return(
          tabsetPanel(
            tabPanel("Print", verbatimTextOutput("print")),
            tabPanel("Summary", verbatimTextOutput("summary")),
            tabPanel(discount()$plot$labels$title, plotOutput("discount")),
            tabPanel(survival()$plot$labels$title, plotOutput("survival")),
            tabPanel("Help", uiOutput("vig")),
            tabPanel("Source", uiOutput("src")),
            tabPanel("Data", dataTableOutput("contents"))
          )
        )
      }

      if(!is.null(input$func) && (input$func == "bdpnormal" || input$func == "bdpbinomial")){
        return(
          tabsetPanel(
            tabPanel("Print", verbatimTextOutput("print")),
            tabPanel("Summary", verbatimTextOutput("summary")),
            tabPanel(discount()$plot$labels$title, plotOutput("discount")),
            tabPanel(posteriors()$plot$labels$title, plotOutput("posteriors")),
            tabPanel(density()$plot$labels$title, plotOutput("density")),
            tabPanel("Help", uiOutput("vig")),
            tabPanel("Source", uiOutput("src"))
          )
        )
      }
    }
    else{
  
      return(
        tabsetPanel(
          tabPanel("Print", verbatimTextOutput("print")),
          tabPanel("Summary", verbatimTextOutput("summary")),
          tabPanel("Plot", plotOutput("simpleplot")),
          tabPanel("Data", dataTableOutput("devcontents"))
        )
      )
    }
  })
  
  
  ##############################################################################
  # bayesDP function selection dropdown.
  ##############################################################################
  
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
  
  
  ##############################################################################
  # Vignette panel for each bayesDP function.
  ##############################################################################
  
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
      mdout
    }
  })
  
  
  ##############################################################################
  # Source code panel for each bayesDP function.
  ##############################################################################
  
  output$src <- renderUI({
    if(is.null(input$funccheck) || input$funccheck == FALSE){
      if(input$func == "bdpnormal"){
        rend <- highlight("https://raw.githubusercontent.com/balcomes/bayesDP/master/R/bdpnormal.R",
                          output = stdout(), renderer = renderer_html())
        rend <- rend[grep("#'", rend, invert = TRUE)]
        mdout <- do.call(HTML, list(rend))
      }
      if(input$func == "bdpbinomial"){
        rend <- highlight("https://raw.githubusercontent.com/balcomes/bayesDP/master/R/bdpbinomial.R",
                          output = stdout(), renderer = renderer_html())
        rend <- rend[grep("#'", rend, invert = TRUE)]
        mdout <- do.call(HTML, list(rend))
      }
      if(input$func == "bdpsurvival"){
        rend <- highlight("https://raw.githubusercontent.com/balcomes/bayesDP/master/R/bdpsurvival.R",
                          output = stdout(), renderer = renderer_html())
        rend <- rend[grep("#'", rend, invert = TRUE)]
        mdout <- do.call(HTML, list(rend))
      }
      if(input$func == "bdpregression"){
        rend <- highlight("https://raw.githubusercontent.com/balcomes/bayesDP/master/R/bdpregression.R",
                          output = stdout(), renderer = renderer_html())
        rend <- rend[grep("#'", rend, invert = TRUE)]
        mdout <- do.call(HTML, list(rend))
      }
      else{
        
      }
      mdout
    }
  })
  
  
  ##############################################################################
  # Textbox for user to write in any function in dev mode.
  ##############################################################################
  
  output$funcname <- renderUI({
    if(!is.null(input$funccheck) && input$funccheck == TRUE){
      textInput("anyfunc","Write in your function name")
    }
  })
  
  
  ##############################################################################
  # Textbox for user to write in a formula for functions that require it.
  ##############################################################################
  
  output$writeformula <- renderUI({
    if("formula" %in% params_names()){
      menuItem("Formula",
               icon = icon("bar-chart-o"),
      textInput("Formula",
                label = "Formula",
                value = "Surv(time, status) ~ historical + treatment"))
    }
  })
  
  
  ##############################################################################
  # bdpsurvival and bdpregression specific dropdowns for column selection. 
  ##############################################################################
  
  output$colchoose <- renderUI({
    if(!is.null(input$func) && (input$func == "bdpsurvival" || input$func == "bdpregression") &&
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
  
  
  ##############################################################################
  # Download reports.
  ##############################################################################
  
  report <- reactive({
    "Hello"
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
  
  
  ##############################################################################
  # Dev mode code.  Click dashboard body and hit "`" (tild ~ key, no shift).
  ##############################################################################
  
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
  
  pnl <- reactive({length(params_names())})
  
  reactive({print(pnl())})
  
  lapply(letters,function(x){
    output[[paste0(x,7)]] <- renderUI(
    if(which(letters == strsplit(x,1)) <= pnl()){
      h4(params_names()[which(letters == x)])
    }
  )})
  
  lapply(letters,function(x){output[[paste0(x,4)]] <- renderUI(
    if(!is.null(pnl()) && which(letters == strsplit(x,1)) <= pnl()){
      actionButton(paste0(x,1),"Toggle Value or Data Frame")
    }
  )})

  lapply(letters,function(x){
    ind <- which(letters == strsplit(x,1))
    output[[paste0(x,5)]] <- renderUI(
    if(which(letters == strsplit(x,1)) <= pnl()){
      if(is.null(input[[paste0(x,1)]]) || input[[paste0(x,1)]] %% 2 == 0){
          textInput(paste0(x,2), label = NULL, value = params()[ind])
      }
    })
  })
  
  lapply(letters,function(x){output[[paste0(x,6)]] <- renderUI(
    if(which(letters == strsplit(x,1)) <= pnl()){
      if(input[[paste0(x,1)]] %% 2 == 1){
        fileInput(paste0(x,2), "Upload .csv File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"))
    }
    })
  })
}

shinyApp(ui, server, enableBookmarking = "url")
