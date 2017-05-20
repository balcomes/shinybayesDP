library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)

header <- dashboardHeader()

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Inputs", icon = icon("bar-chart-o"),
             # Input directly under menuItem
             selectInput("inputTest", "Input Test",
                         choices = c("a", "b", "c", "d"), multiple=TRUE, selectize=TRUE,
                         width = '98%'),
             
             # Input inside of menuSubItem
             menuSubItem(icon = NULL,
                         sliderInput("inputTest2", "Input test 2", min=0, max=10, value=5,
                                     width = '95%')
             )
    )
  )
)

body <- dashboardBody()

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) { }
)