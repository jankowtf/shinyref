## Packages //
library(shiny)

## Functions //
handleInput <- function(input) {
  UseMethod("handleInput")
}
handleInput2 <- function(input) {
  UseMethod("handleInput2")
}
handleInput.InputNum <- function(input) {
  hist(rnorm(input))
}
handleInput2.InputNum <- function(input) {
  renderPlot({
    hist(rnorm(input))
  })
}

## UI //
ui <- fluidPage(
  sliderInput(inputId = "num",
    label = "Choose a number",
    value = 25, min = 1, max = 100),
  plotOutput("hist"),
  plotOutput("hist2")
)

## Server //
server <- function(input, output) {
#   output$hist <- renderPlot({
#     hist(rnorm(input$num))
#   })
  output$hist <- renderPlot({
    handleInput(structure(input$num, class = c("InputNum", class(input$num))))
  })
  output$hist2 <- handleInput2(
    structure(input$num, class = c("InputNum", class(input$num))))
}

shinyApp(ui = ui, server = server)
