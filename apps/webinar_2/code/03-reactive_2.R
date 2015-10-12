handleInput <- function(x) UseMethod("handleInput")
handleInput.Num <- function(x) {
  renderPlot({
    hist(x())
  })
}
# 03-reactive

library(shiny)

ui <- fluidPage(
  sliderInput(inputId = "num",
    label = "Choose a number",
    value = 25, min = 1, max = 100),
  plotOutput("hist"),
  verbatimTextOutput("stats")
)

server <- function(input, output) {

  data <- reactive({
    rnorm(input$num)
  })
  class(data) <- c("Num", class(data))
  print(data)
  print(class(data))

#   output$hist <- renderPlot({
#     hist(data())
#   })
  output$hist <- handleInput.Num(data)
  output$stats <- renderPrint({
    summary(data())
  })
}

shinyApp(ui = ui, server = server)
