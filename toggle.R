library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(highlight)

ui <- function(request) {

  ch <- lapply(paste0(letters,4),function(x){do.call(uiOutput,list(x))})
  tx <- lapply(paste0(letters,5),function(x){do.call(uiOutput,list(x))})
  df <- lapply(paste0(letters,6),function(x){do.call(uiOutput,list(x))})
  
  insert <- list()
  for(i in 1:length(ch)){
    insert <- list(insert,ch[i],tx[i],df[i])
  }
  
  dashboardPage(title = "Test",
    dashboardHeader(title = "Test"),
    dashboardSidebar(
      tags$head(tags$style(HTML(".sidebar{height:100vh;overflow-y:auto;}"))),
      insert,
      HTML("Hello")
    ),
    dashboardBody(
      fluidPage(

      )
    )
  )
}
server <- function(input, output, enableBookmarking = "url"){
  lapply(letters,function(x){output[[paste0(x,4)]] <- renderUI(actionButton(paste0(x,1),paste0(x,1)))})
  lapply(letters,function(x){output[[paste0(x,5)]] <- renderUI(
    conditionalPanel(condition = (paste0("input.", paste0(x,1), " % 2 == 0")),{
      textInput(paste0(x,2),paste0(x,2))
    }))})
  lapply(letters,function(x){output[[paste0(x,6)]] <- renderUI(
    conditionalPanel(condition = (paste0("input.", paste0(x,1), " % 2 == 1")),{
      sliderInput(paste0(x,3),
                  "Hello",
                  min = 0,
                  max = 1000,
                  value = 500)
    }))})
}

shinyApp(ui, server, enableBookmarking = "url")