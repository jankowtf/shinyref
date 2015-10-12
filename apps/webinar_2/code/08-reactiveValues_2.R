# 08-reactiveValues

library(shiny)

ui <- fluidPage(
  actionButton(inputId = "norm", label = "Normal"),
  actionButton(inputId = "unif", label = "Uniform"),
  actionButton(inputId = "go", label = "Update"),
  plotOutput("hist")
)

server <- function(input, output) {

  rv <- reactiveValues(data = rnorm(100))

  eventReactive(input$go, {
    observeEvent(input$norm, { rv$data <- rnorm(100) })
    observeEvent(input$unif, { rv$data <- runif(100) })
  })
  output$hist <- renderPlot({
    hist(rv$data)
  })
}

shinyApp(ui = ui, server = server)
