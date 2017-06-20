library(bayesDP)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(highlight)

ui <- function(request) {
  
  dashboardPage(title = "Test",
                dashboardHeader(title = "Test"),
                dashboardSidebar(
                  tags$head(tags$style(HTML(".sidebar{height:100vh;overflow-y:auto;}"))),
                  tags$head(tags$script('src="http://d3js.org/d3.v3.min.js"')),
                  HTML("Hello")
                ),
                dashboardBody(
                  fluidPage(
                    tags$head(tags$style(HTML("body {width: 100% !important;
                                              max-width: 100% !important;}"))),
                    tags$script('$(document).on("keypress", function (e) {
                                Shiny.onInputChange("secret", e.which);});'),
                    tags$script('src="https://d3js.org/d3.v4.js"'),
                    tags$script("
function walk(svg, x, y, generation) {
  var x1 = x, y1 = y;
  var axis = Math.random() > 0.5;
  var delta = Math.random() > 0.5 ? 10 : -10;

  axis ? x += delta : y += delta;
  
  svg
  .append('line')
  .attr({
  x1: x1,
  y1: y1,
  x2: x,
  y2: y
  });

  requestAnimationFrame(walk.bind(null, svg, x, y, ++generation));
}

walk(d3.select('svg'), 50, 50, 1, 1000);
"),
                    tags$style(type = "text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"),
                    box(width = "100%", uiOutput("main"))
                  ),
                  hr()
                )
  )
}
server <- function(input, output, enableBookmarking = "url"){
  output$main <- renderUI({
    tags$head(tags$style(HTML('body {
                              background: black;
                            }
                            line {
                              stroke: green;
                            }')))
    HTML('<svg width="100%" height="500"></svg>')
  })
}

shinyApp(ui, server, enableBookmarking = "url")