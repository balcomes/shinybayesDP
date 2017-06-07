library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    verbatimTextOutput("results"),
    tags$script('
                $(document).on("keypress", function (e) {
                Shiny.onInputChange("mydata", e.which);
                });
                ') 
    )

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$results = renderPrint({
    input$mydata
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

