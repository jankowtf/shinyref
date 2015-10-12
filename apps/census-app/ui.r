shinyUI(fluidPage(
  titlePanel("censusVis"),

  sidebarLayout(
    sidebarPanel(
      helpText("Create demographic maps with
        information from the 2010 US Census."),

      selectInput("var",
        label = "Choose a variable to display",
        choices = c("Percent White", "Percent Black",
          "Percent Hispanic", "Percent Asian"),
        selected = "Percent White"),

      selectInput("image",
        label = "Choose an image to display",
        choices = c("bigorb", "other"),
        selected = "bigorb"),

      sliderInput("range",
        label = "Range of interest:",
        min = 0, max = 100, value = c(0, 100))
    ),

    mainPanel(
      textOutput("text1"),
      br(),
      textOutput("text2"),
      br(),
      uiOutput("ui1"),
      uiOutput("ui2"),
      br(),
      imageOutput("image1"),
      br(),
      plotOutput("map")
    )
  )
))
