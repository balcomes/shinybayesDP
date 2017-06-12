library(shiny)
library(DT)

ui <- shinyUI(fluidPage(mainPanel(
  DT::dataTableOutput("test"),
  htmlOutput("html")
)))

server <- shinyServer(function(input, output, session) {
  words <- data.frame(stringsAsFactors = FALSE,
                      words = c("the", "hello", "world"))
  output$test <- DT::renderDataTable({
    words
  }, selection = list(mode = "single", target = "row"))
  
  text <- "This is the hello world example for this problem."
  
  output$html <- renderUI({
    if (is.null(input$test_rows_selected))
      return(HTML(text))
    
    HTML(gsub(
      words$words[input$test_rows_selected],
      paste0("<mark>",
             words$words[input$test_rows_selected],
             "</mark>"),text ))
  })
})

shinyApp(ui = ui, server = server)