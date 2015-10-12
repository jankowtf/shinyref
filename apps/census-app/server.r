library(maps)
library(mapproj)
source("helpers.R")
counties <- readRDS("data/counties.rds")

shinyServer(function(input, output) {

  output$text1 <- renderText({
    paste("You have selected", input$var)
  })

  output$text2 <- renderText({
    paste("You have chosen a range that goes from",
      strong(input$range[1]), " to ", input$range[length(input$range)])
  })

  output$ui2 <- renderUI({
    p("You have chosen a range that goes from",
      strong(input$range[1]), " to ", input$range[length(input$range)])
  })

  output$ui1 <- renderUI({
    list(a = 1, b = 1:10, c = strong("Hello World!"))
    # p(strong("Hello"), "world")
  })

  getImage <- reactive({
    # print(input$image)
    # img(src = sprintf("%s.png", input$image), height = 72*0.5, width = 72*0.5)
    # sprintf("%s.png", input$image)
    list(src = file.path(getwd(), sprintf("www/%s.png", input$image)),
      alt = "This is alternate text")
  })

  output$image1 <- renderImage({
    getImage()
  }, deleteFile = FALSE)

  output$map <- renderPlot({
    args <- switch(input$var,
      "Percent White" = list(counties$white, "darkgreen", "% White"),
      "Percent Black" = list(counties$black, "black", "% Black"),
      "Percent Hispanic" = list(counties$hispanic, "darkorange", "% Hispanic"),
      "Percent Asian" = list(counties$asian, "darkviolet", "% Asian"))

    args$min <- input$range[1]
    args$max <- input$range[2]

    do.call(percent_map, args)
  })

})
