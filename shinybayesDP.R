library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)

ui <- dashboardPage(title = "bayesDP",
      dashboardHeader(title = "bayesDP"),
      dashboardSidebar(
        tags$head(tags$style(HTML(".sidebar{height:100vh;overflow-y:auto;}"))),
        selectInput("func", 
                    "Select Function", 
                    choices = c("bdpnormal", "bdpbinomial", "bdpsurvival"), 
                    selected = "bdpnormal"),
        uiOutput("params")),
      dashboardBody(
        tags$style(type = "text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"),
        shinythemes::themeSelector(),
        tabsetPanel(tabPanel("discount", "discount",
                             plotOutput("discount")),
                    tabPanel("posteriors", "posteriors",
                             plotOutput("posteriors")),
                    tabPanel("density", "density",
                             plotOutput("density"))),
        verbatimTextOutput("summary")))

server <- function(input, output){
  
  params <- reactive({
    params <- as.list(args(input$func))
    params[sapply(params, is.null)] <- 100
    params <- params[-length(params)]
    params})
  
  params_names <- reactive({
    names(params())})
  
  output$params <- renderUI({
    lapply(params_names(),function(x){
      do.call(textInput,list(x, label = x, value = params()[[x]]))})})
  
  final <- reactive({
    final <- c()
    for(i in params_names()){
      final <- c(final, input[[i]])}
    if(length(final) > 0){
      final <- eval(parse(text = paste0(input$func,"(",
                                       paste0(final,collapse = ",")
                                       ,")")))}
    final})
  
  output$discount <- renderPlot({
    plot(final(),type = "discount")})
  
  output$posteriors <- renderPlot({
    plot(final(),type = "posteriors")})
  
  output$density <- renderPlot({
    plot(final(),type = "density")})
  
  output$summary <- renderPrint({
    summary(final())})}
  
shinyApp(ui, server)