##------------------------------------------------------------------------------
## Meta //
##------------------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)

## App stage //
app_stage <- "v1.0"
app_stage <- "v1.1"
app_stage <- "neo4j"

app_stages <- c(
  "v1.0",  ## Initial, based on global list,
  "v1.1",  ## based on reading from file system
  "neo4j"  ## based on neo4j
)

##------------------------------------------------------------------------------
## App stages //
##------------------------------------------------------------------------------

validateAppStage <- function(
  stage,
  .stages
) {
  match.arg(stage, .stages)
}
app_stage <- validateAppStage(app_stage, .stages = app_stages)

################
## Stage v1.0 ##
################

if (app_stage == "v1.0") {

  ##----------------------------------------------------------------------------
  ## Global //
  ##----------------------------------------------------------------------------

  GLOBALS <- list(
    namespaces = list(
      ns_1 = list(
        timeseries = list(
          A = list(
            projects = list(
              v1.0 = list(
                variants = list(
                  A_v1.0.1 = "v1.0.1.txt",
                  A_v1.0.2 = "v1.0.2.txt",
                  A_v1.0.3 = "v1.0.3.txt"
                )
              ),
              v1.1 = list(
                variants = list(
                  A_v1.1.1 = "v1.1.1.txt",
                  A_v1.1.2 = "v1.1.2.txt"
                )
              )
            )
          ),
          B = list(
            projects = list(
              v2.0 = list(
                variants = list(
                  B_v2.0.1 = "v2.0.1.txt",
                  B_v2.0.2 = "v2.0.2.txt",
                  B_v2.0.3 = "v2.0.3.txt"
                )
              ),
              v2.1 = list(
                variants = list(
                  B_v2.1.1 = "v2.1.1.txt",
                  B_v2.1.2 = "v2.1.2.txt"
                )
              )
            )
          )
        )
      ),
      ns_2 = list(
        timeseries = list(
          X = list(
            projects = list(
              v1.0 = list(
                variants = list(
                  X_v1.0.1 = "v1.0.1.txt",
                  X_v1.0.2 = "v1.0.2.txt",
                  X_v1.0.3 = "v1.0.3.txt"
                )
              ),
              v1.1 = list(
                variants = list(
                  X_v1.1.1 = "v1.1.1.txt",
                  X_v1.1.2 = "v1.1.2.txt"
                )
              )
            )
          ),
          Y = list(
            projects = list(
              v2.0 = list(
                variants = list(
                  Y_v2.0.1 = "v2.0.1.txt",
                  Y_v2.0.2 = "v2.0.2.txt",
                  Y_v2.0.3 = "v2.0.3.txt"
                )
              ),
              v2.1 = list(
                variants = list(
                  Y_v2.1.1 = "v2.1.1.txt",
                  Y_v2.1.2 = "v2.1.2.txt"
                )
              )
            )
          )
        )
      )
    )
  )

  ns_values <- GLOBALS$namespaces
  ns_default <- names(ns_values)[1]

  ts_values <- GLOBALS$namespaces[[ns_default]]$timeseries
  ts_default <- names(ts_values)[1]

  project_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects
  project_default <- names(project_values)[1]

  variant_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects[[project_default]]$variants
  variant_default <- names(variant_values)[1]

  ##----------------------------------------------------------------------------
  ## UI //
  ##----------------------------------------------------------------------------

  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = "Nested inputs"),


    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        selectInput("ns", "Select namespace",
          choices = names(ns_values),
          selected = ns_default
        ),
        menuItem("Variants meta", tabName = "variants", icon = icon("th"))
      )
    ),

    ## Body content
    dashboardBody(
      tabItems(
        # Option 1 //
        tabItem(
          tabName = "variants",
          fluidRow(
            ## Time series //
            box(
              selectInput("ts", "Time series",
                choices = names(ts_values),
                selected = ts_default
              ),
              width = 2
            ),

            ## Select IF project //
            box(
              selectInput("project", "Project",
                choices = names(project_values),
                selected = project_default
                # width = "300px"
              ),
              width = 4
            ),

            ## Select variant specification //
            box(
              selectInput("variant", "Model Variant",
                choices = names(variant_values),
                selected = variant_default
              ),
              width = 4
            ),

            box(actionButton("go", "Go"), status = "primary", width = 2,
              height = "100px")
          )
        )
      ),

      tabItem(
        tabName = "Path",
        h2("Path"),
        fluidRow(
          tabBox(
            width = 12,
            height = "500px",
            title = "Path",
            id = "path",
            tabPanel("Result", textOutput("path"))
          )
        )
      )
    )
  )

  ##----------------------------------------------------------------------------
  ## Server //
  ##----------------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({
      # input$update
      input_ns <- input$ns
      # print(input_ns)
      updateSelectInput(session, "ts",
        choices = names(GLOBALS$namespaces[[input_ns]]$timeseries)
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # print(input_ts)
      updateSelectInput(session, "project",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects
        )
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # input_ts <- isolate(input$ts)
      input_project <- input$project
      #       print(input_project)
      updateSelectInput(session, "variant",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects[[input_project]]$variants
        )
      )
    })

    path <- eventReactive(input$go, {
      #       GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$
      #         projects[[input$projects]]$variants[[input$variant]]
      paste0(input$ns, "/", input$ts, "/", input$project, "/", input$variant, "/"
      )
    })

    output$path <- renderText({
      path()
    })

  }
}

################
## Stage v1.1 ##
################

if (app_stage == "v1.1") {

  ##----------------------------------------------------------------------------
  ## Global //
  ##----------------------------------------------------------------------------

  # setwd("apps/conditional_ui")
  dir_structure <- mapDirectory(dir = "data", include_root = FALSE)
  # dir_structure <- mapDirectory(dir = "data", drop_root = FALSE)
  print(dir_structure)

  GLOBALS <- dir_structure

  ns_values <- GLOBALS$namespaces
  ns_default <- names(ns_values)[1]

  ts_values <- GLOBALS$namespaces[[ns_default]]$timeseries
  ts_default <- names(ts_values)[1]

  project_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects
  project_default <- names(project_values)[1]

  variant_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects[[project_default]]$variants
  variant_default <- names(variant_values)[1]

  ##----------------------------------------------------------------------------
  ## UI //
  ##----------------------------------------------------------------------------

  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = "Nested inputs"),


    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        selectInput("ns", "Select namespace",
          choices = names(ns_values),
          selected = ns_default
        ),
        menuItem("Variants meta", tabName = "variants", icon = icon("th"))
      )
    ),

    ## Body content
    dashboardBody(
      tabItems(
        # Option 1 //
        tabItem(
          tabName = "variants",
          fluidRow(
            ## Time series //
            box(
              selectInput("ts", "Time series",
                choices = names(ts_values),
                selected = ts_default
              ),
              width = 2
            ),

            ## Select IF project //
            box(
              selectInput("project", "Project",
                choices = names(project_values),
                selected = project_default
                # width = "300px"
              ),
              width = 4
            ),

            ## Select variant specification //
            box(
              selectInput("variant", "Model Variant",
                choices = names(variant_values),
                selected = variant_default
              ),
              width = 4
            ),

            box(actionButton("go", "Go"), status = "primary", width = 2,
              height = "100px")
          )
        )
      ),

      tabItem(
        tabName = "Path",
        h2("Path"),
        fluidRow(
          tabBox(
            width = 12,
            height = "500px",
            title = "Path",
            id = "path",
            tabPanel("Result", textOutput("path"))
          )
        )
      )
    )
  )

  ##----------------------------------------------------------------------------
  ## Server //
  ##----------------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({
      # input$update
      input_ns <- input$ns
      # print(input_ns)
      updateSelectInput(session, "ts",
        choices = names(GLOBALS$namespaces[[input_ns]]$timeseries)
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # print(input_ts)
      updateSelectInput(session, "project",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects
        )
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # input_ts <- isolate(input$ts)
      input_project <- input$project
      #       print(input_project)
      updateSelectInput(session, "variant",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects[[input_project]]$variants
        )
      )
    })

    path <- eventReactive(input$go, {
      # paste0(input$ns, "/", input$ts, "/", input$project, "/", input$variant, "/")
      # print(capture.output(GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]][[1]]))
      value <- GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]]
      if (!length(value)) {
        value <- ""
      }

      paste(
        c(
          dir,
          "namespaces",
          input$ns,
          "timeseries",
          input$ts,
          "projects",
          input$project,
          "variants",
          input$variant,
          value
        ),
        collapse = "/"
      )
    })

    output$path <- renderText({
      path <- path()
      print(file.exists(path))
      path
    })

  }
}

#################
## Stage neo4j ##
#################

if (app_stage == "neo4j") {

  # install.packages("RNeo4j")
  library(RNeo4j)
  graph <- startGraph("http://localhost:7474/db/data", username = "neo4j",
    password = "stayagile")

  ## Only needed once //
  if (FALSE) {
    clear(graph)

    ## Constraints //
    addConstraint(graph, "Namespace", "id")
    addConstraint(graph, "Timeseries", "id")
    addConstraint(graph, "Project", "id")
    addConstraint(graph, "Variant", "id")

    ## Nodes //
    createDb <- function(x) {
      ns <- getOrCreateNode(graph, "Namespace", id = x$ns_id)
      ts <- getOrCreateNode(graph, "Timeseries", id = x$ts_id)
      project <- getOrCreateNode(graph, "Project", id = x$project_id)
      variant <- getOrCreateNode(graph, "Variant", id = x$variant_id)

#       rels <- getRels(graph, "MATCH (:Namespace {id:{id_ns}})-[h:HAS_TS]->(:Timeseries {id:{id_ts}}) RETURN h",
#         id_ns = "ns_1", id_ts = "a")
#       rels <- getRels(graph, "MATCH (:Namespace {id:{id_ns}})-[h:HAS_TS]->(:Timeseries {id:{id_ts}}) RETURN h",
#         id_ns = ns, id_ts = ts)

      query <- sprintf("
        MATCH (ns:Namespace)-[h:HAS_TS]->(ts:Timeseries)
        WHERE ns.id = '%s' AND ts.id = '%s'
        RETURN count(h) AS count
      ", x$ns_id, x$ts_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(ns, "HAS_TS", ts)
      }

      query <- sprintf("
        MATCH (ts:Timeseries)-[h:HAS_PROJECT]->(project:Project)
        WHERE ts.id = '%s' AND project.id = '%s'
        RETURN count(h) AS count
      ", x$ts_id, x$project_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(ts, "HAS_PROJECT", project)
      }

      query <- sprintf("
        MATCH (project:Project)-[h:HAS_VARIANT]->(variant:Variant)
        WHERE project.id = '%s' AND variant.id = '%s'
        RETURN count(h) AS count
      ", x$project_id, x$variant_id)
      count <- unlist(cypher(graph, query))

      if(count == 0) {
        createRel(project, "HAS_VARIANT", variant)
      }

      TRUE
    }

    createDb(x = list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
      variant_id = "a_v1.0.1"))
    createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
      variant_id = "a_v1.0.2"))
    createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.0",
      variant_id = "a_v1.0.3"))

    createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
      variant_id = "a_v1.1.1"))
    createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
      variant_id = "a_v1.1.2"))
    createDb(list(ns_id = "ns_1", ts_id = "a", project_id = "a_v1.1",
      variant_id = "a_v1.1.3"))

    createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
      variant_id = "b_v1.0.1"))
    createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
      variant_id = "b_v1.0.2"))
    createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.0",
      variant_id = "b_v1.0.3"))

    createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
      variant_id = "b_v1.1.1"))
    createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
      variant_id = "b_v1.1.2"))
    createDb(list(ns_id = "ns_1", ts_id = "b", project_id = "b_v1.1",
      variant_id = "b_v1.1.3"))
  }

  ##----------------------------------------------------------------------------
  ## Global //
  ##----------------------------------------------------------------------------

  dir_structure <- mapDatabase(con = graph)
  # dir_structure <- mapDirectory(dir = "data", drop_root = FALSE)
  print(dir_structure)

  GLOBALS <- dir_structure

  ns_values <- GLOBALS$namespaces
  ns_default <- names(ns_values)[1]

  ts_values <- GLOBALS$namespaces[[ns_default]]$timeseries
  ts_default <- names(ts_values)[1]

  project_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects
  project_default <- names(project_values)[1]

  variant_values <- GLOBALS$namespaces[[ns_default]]$timeseries[[
    ts_default]]$projects[[project_default]]$variants
  variant_default <- names(variant_values)[1]

  ##----------------------------------------------------------------------------
  ## UI //
  ##----------------------------------------------------------------------------

  ui <- dashboardPage(
    ## Header //
    dashboardHeader(title = "Nested inputs"),


    ## Sidebar content //
    dashboardSidebar(
      sidebarMenu(
        selectInput("ns", "Select namespace",
          choices = names(ns_values),
          selected = ns_default
        ),
        menuItem("Variants meta", tabName = "variants", icon = icon("th"))
      )
    ),

    ## Body content
    dashboardBody(
      tabItems(
        # Option 1 //
        tabItem(
          tabName = "variants",
          fluidRow(
            ## Time series //
            box(
              selectInput("ts", "Time series",
                choices = names(ts_values),
                selected = ts_default
              ),
              width = 2
            ),

            ## Select IF project //
            box(
              selectInput("project", "Project",
                choices = names(project_values),
                selected = project_default
                # width = "300px"
              ),
              width = 4
            ),

            ## Select variant specification //
            box(
              selectInput("variant", "Model Variant",
                choices = names(variant_values),
                selected = variant_default
              ),
              width = 4
            ),

            box(actionButton("go", "Go"), status = "primary", width = 2,
              height = "100px")
          )
        )
      ),

      tabItem(
        tabName = "Path",
        h2("Path"),
        fluidRow(
          tabBox(
            width = 12,
            height = "500px",
            title = "Path",
            id = "path",
            tabPanel("Result", textOutput("path"))
          )
        )
      )
    )
  )

  ##----------------------------------------------------------------------------
  ## Server //
  ##----------------------------------------------------------------------------

  server <- function(input, output, session) {

    observe({
      # input$update
      input_ns <- input$ns
      # print(input_ns)
      updateSelectInput(session, "ts",
        choices = names(GLOBALS$namespaces[[input_ns]]$timeseries)
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # print(input_ts)
      updateSelectInput(session, "project",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects
        )
      )
    })

    observe({
      input_ns <- input$ns
      # input_ns <- isolate(input$ns)
      input_ts <- input$ts
      # input_ts <- isolate(input$ts)
      input_project <- input$project
      #       print(input_project)
      updateSelectInput(session, "variant",
        choices = names(GLOBALS$
            namespaces[[input_ns]]$
            timeseries[[input_ts]]$
            projects[[input_project]]$variants
        )
      )
    })

    path <- eventReactive(input$go, {
      # paste0(input$ns, "/", input$ts, "/", input$project, "/", input$variant, "/")
      # print(capture.output(GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]][[1]]))
      value <- GLOBALS$namespaces[[input$ns]]$timeseries[[input$ts]]$projects[[input$project]]$variants[[input$variant]]
      if (!length(value)) {
        value <- ""
      }

      paste(
        c(
          dir,
          "namespaces",
          input$ns,
          "timeseries",
          input$ts,
          "projects",
          input$project,
          "variants",
          input$variant,
          value
        ),
        collapse = "/"
      )
    })

    output$path <- renderText({
      path <- path()
      print(file.exists(path))
      path
    })

  }
}

##------------------------------------------------------------------------------
## Start app //
##------------------------------------------------------------------------------

shinyApp(ui, server)
