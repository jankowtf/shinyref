library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
    label = "Choose a number",
    value = 25, min = 1, max = 100),
  plotOutput("hist")
)

server <- function(input, output) {
  handleInput.InputNum <- function(input) {
    renderPlot({
      hist(rnorm(input))
    })
  }
#   output$hist <- renderPlot({
#     input_2 <- input
#     class(input_2$num) <- c(class(input_2$num), "InputNum")
#     handleInput.InputNum(input_2$num)
#   })
  output$hist <- renderPlot({
    handleInput.InputNum(input$num)
  })
}

shinyApp(ui = ui, server = server)
