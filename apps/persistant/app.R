##------------------------------------------------------------------------------
## Meta //
##------------------------------------------------------------------------------

options(CRAN = c(getOption("CRAN"), "http://www.omegahat.org/R"))

# install.packages("rdrop2")
# install.packages("RAmazonS3")
# install.packages("RSQLite")
# install.packages("rmongodb")
# install.packages("mongolite")

library(methods)
library(shiny)
# library(shinydashboard)
library(rdrop2)
library(RSQLite)

## App stage //
app_stage <- "no storage"
app_stage <- "file system"
app_stage <- "sqlite"
app_stage <- "mongodb"

stages <- c(
  "no storage",
  "file system",
  "sqlite",
  "mongodb"
)

fields <- c("name", "used_shiny", "r_num_years")

########
## UI ##
########

ui <- fluidPage(
  DT::dataTableOutput("responses", width = 300), tags$hr(),
  textInput("name", "Name", ""),
  checkboxInput("used_shiny", "I've built a Shiny app in R before", FALSE),
  sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
  actionButton("submit", "Submit")
)

############
## Server ##
############

server <- function(input, output, session) {

  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })

  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(formData())
  })

  # Show the previous responses
  # (update with current response when Submit is clicked)
  output$responses <- DT::renderDataTable({
    input$submit
    loadData()
  })
}

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

################
## No storage ##
################

if (app_stage == "no storage") {
  saveData <- function(data) {
    data <- as.data.frame(t(data))
    if (exists("responses")) {
      responses <<- rbind(responses, data)
    } else {
      responses <<- data
    }
  }

  loadData <- function() {
    if (exists("responses")) {
      responses
    }
  }


}

#################
## File system ##
#################

if (app_stage == "file system") {
  outputDir <- "responses"
  dir.create(outputDir, recursive = TRUE, showWarnings = FALSE)

  saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(outputDir, fileName),
      row.names = FALSE, quote = TRUE
    )
  }

  loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  }
}

#############
## Dropbox ##
#############

if (app_stage == "dropbox") {
  library(rdrop2)
  outputDir <- "responses"

  saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), fileName)
    write.csv(data, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, dest = outputDir)
  }

  loadData <- function() {
    # Read all the files into a list
    filesInfo <- drop_dir(outputDir)
    filePaths <- filesInfo$path
    data <- lapply(filePaths, drop_read_csv, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
  }
}

###############
## Amazon S3 ##
###############

if (app_stage == "amazon") {
}

############
## SQLite ##
############

if (app_stage == "sqlite") {
  library(RSQLite)
  sqlitePath <- "sqlite/database"
  dir.create(dirname(sqlitePath), recursive = TRUE, showWarnings = FALSE)

  table <- "responses"
  data <- data.frame(name = "Janko", stringsAsFactors = FALSE)
  saveData <- function(data) {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    # Construct the update query by looping over the data fields
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table,
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
  }

  loadData <- function() {
    # Connect to the database
    db <- dbConnect(SQLite(), sqlitePath)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
  }
}

##############
## Mongo DB ##
##############

if (app_stage == "mongodb") {
  library(rmongodb)

  options(mongodb = list(
    "host" = "ds012345.mongolab.com:61631",
    "username" = "myuser",
    "password" = "mypassword"
  ))
  databaseName <- "myshinydatabase"
  collectionName <- "myshinydatabase.responses"

  saveData <- function(data) {
    # Connect to the database
    db <- mongo.create(db = databaseName, host = options()$mongodb$host,
      username = options()$mongodb$username, password = options()$mongodb$password)
    # Convert the data to BSON (Binary JSON)
    data <- mongo.bson.from.list(as.list(data))
    # Insert the data into the mongo collection and disconnect
    mongo.insert(db, collectionName, data)
    mongo.disconnect(db)
  }

  loadData <- function() {
    # Connect to the database
    db <- mongo.create(db = databaseName, host = options()$mongodb$host,
      username = options()$mongodb$username, password = options()$mongodb$password)
    # Get a list of all entries
    data <- mongo.find.all(db, collectionName)
    # Read all entries into a list
    data <- lapply(data, data.frame, stringsAsFactors = FALSE)
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    # Remove the ID variable
    data <- data[ , -1, drop = FALSE]
    # Disconnect
    mongo.disconnect(db)
    data
  }
}

##------------------------------------------------------------------------------
## Start app //
##------------------------------------------------------------------------------

shinyApp(ui, server)

