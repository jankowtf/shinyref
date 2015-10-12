shinyServer(function(input, output) {
  output$table <- renderTable({
    data.frame(Input = input$text)
  }, include.rownames = FALSE)
})
