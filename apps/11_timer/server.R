shinyServer(function(input, output, session) {
  output$currentTime <- renderText({
    invalidateLater(5000, session)
    paste("The current time is", Sys.time())
  })
})
