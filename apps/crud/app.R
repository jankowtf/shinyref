##------------------------------------------------------------------------------
## Meta //
##------------------------------------------------------------------------------

library(methods)
library(shiny)
library(shinydashboard)

# install.packages("methods")

## App stage //
app_stage <- "v1.0"

stages <- c(
  "v1.0",
  "v1.1"
)

##------------------------------------------------------------------------------
## App stages //
##------------------------------------------------------------------------------

validateAppStage <- function(
  stage,
  .stage = stages
) {
  match.arg(stage, .stage)
}
app_stage <- validateAppStage(app_stage)

##########
## v1.0 ##
##########

if (app_stage == "v1.0") {
  # install.packages("RNeo4j")
  # devtools::install_github("nicolewhite/RNeo4j")
  # install.packages("devtools")
  library(RNeo4j)

  graph = startGraph("http://localhost:7474/db/data/",
    username = "neo4j", password = "stayagile")
  # importSample(graph, "dfw")
  summary(graph)

  ## Query //
  query = "
  MATCH (p:Place)-[:IN_CATEGORY]->(c:Category),
        (p)-[:AT_GATE]->(g:Gate),
        (g)-[:IN_TERMINAL]->(t:Terminal)
  WHERE c.name IN {categories} AND t.name = {terminal}
  WITH c, p, g, t, ABS(g.gate - {gate}) AS dist
  ORDER BY dist
  RETURN p.name AS Name,
  c.name AS Category,
  g.gate AS Gate,
  t.name AS Terminal
  "

  # Get categories and terminals //
  categories <- getLabeledNodes(graph, "Category")
  categories <- sapply(categories, function(c) c$name)

  terminals <- getLabeledNodes(graph, "Terminal")
  terminals <- sapply(terminals, function(t) t$name)

  gates <- getLabeledNodes(graph, "Gate")
  gates <- sapply(gates, function(t) t$gate)

  query = "
  MATCH (p:Place)-[:IN_CATEGORY]->(c:Category),
        (p)-[:AT_GATE]->(g:Gate),
        (g)-[:IN_TERMINAL]->(t:Terminal)
  WHERE c.name IN {categories} AND t.name = {terminal}
  WITH c, p, g, t, ABS(g.gate - {gate}) AS dist
  ORDER BY dist
  RETURN dist
  "
  query_res = cypher(graph,
    query,
    categories = as.list(categories),
    terminal = terminals[[1]],
    gate = gates[[1]]
  )

  ui <- fluidPage(
    titlePanel("DFW Food & Drink Finder"),
    sidebarLayout(
      sidebarPanel(
        strong("Show me food & drink places in the following categories"),
        checkboxGroupInput("categories",
          label = "",
          choices = categories,
          selected = sample(categories, 3)),
        strong("closest to gate"),
        numericInput("gate",
          label = "",
          value = sample(1:30, 1)),
        br(),
        strong("in terminal"),
        selectInput("terminal",
          label = "",
          choices = terminals,
          selected = sample(terminals, 1)),
        "Powered by", a("Neo4j",
          href = "http://www.neo4j.org/",
          target = "_blank")
      ),
      mainPanel(
        tableOutput("restaurants")
      )
    )
  )

  server <- function(input, output, session) {

    output$restaurants <- renderTable({
      data = cypher(graph,
        query,
        categories = as.list(input$categories),
        terminal = input$terminal,
        gate = input$gate)
      return(data)
    })
  }
}

##########
## v1.1 ##
##########

if (app_stage == "v1.1") {
  ui <- fluidPage(
    #use shiny js to disable the ID field
    shinyjs::useShinyjs(),

    #data table
    DT::dataTableOutput("responses", width = 300),

    #input fields
    tags$hr(),
    shinyjs::disabled(textInput("id", "Id", "0")),
    textInput("name", "Name", ""),
    checkboxInput("used_shiny", "Used Shiny", FALSE),
    sliderInput("r_num_years", "R Years", 0, 25, 2, ticks = FALSE),

    #action buttons
    actionButton("submit", "Submit"),
    actionButton("new", "New"),
    actionButton("delete", "Delete")
  )

  server <- function(input, output, session) {

    # input fields are treated as a group
    formData <- reactive({
      sapply(names(GetTableMetadata()$fields), function(x) input[[x]])
    })

    # Click "Submit" button -> save data
    observeEvent(input$submit, {
      if (input$id != "0") {
        UpdateData(formData())
      } else {
        CreateData(formData())
        UpdateInputs(CreateDefaultRecord(), session)
      }
    }, priority = 1)

    # Press "New" button -> display empty record
    observeEvent(input$new, {
      UpdateInputs(CreateDefaultRecord(), session)
    })

    # Press "Delete" button -> delete from data
    observeEvent(input$delete, {
      DeleteData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }, priority = 1)

    # Select row in table -> show details in inputs
    observeEvent(input$responses_rows_selected, {
      if (length(input$responses_rows_selected) > 0) {
        data <- ReadData()[input$responses_rows_selected, ]
        UpdateInputs(data, session)
      }

    })

    # display table
    output$responses <- DT::renderDataTable({
      #update after submit is clicked
      input$submit
      #update after delete is clicked
      input$delete
      ReadData()
    }, server = FALSE, selection = "single",
      colnames = unname(GetTableMetadata()$fields)[-1]
    )

  }
}

##------------------------------------------------------------------------------
## Start app //
##------------------------------------------------------------------------------

shinyApp(ui, server)
