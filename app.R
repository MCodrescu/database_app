library(shiny)
library(shinyjs)
library(readr)
library(RPostgres)
library(dplyr)
library(glue)
library(DT)
library(teradatasql)
library(stringr)

ui <- bootstrapPage(
  theme = bslib::bs_theme(version = 5),
  
  # Initiate shinyjs
  useShinyjs(),
  
  #######################################################
  
  # Navbar
  tags$nav(class = "navbar navbar-expand-lg navbar-light bg-light",
           div(class = "container-fluid",
               tags$a(class = "navbar-brand",
                      "Database App"),
               tags$button(class = "navbar-toggler",
                           type = "button",
                           "data-bs-toggle" = "collapse",
                           "data-bs-target" = "#navbarSupportedContent",
                           tags$span(class = "navbar-toggler-icon")
               ),
               div(class = "collapse navbar-collapse",
                   id = "navbarSupportedContent",
                   tags$ul(
                     class = "navbar-nav me-auto mb-2 mb-lg-0",
                     tags$li(
                       class = "nav-item",
                       tags$a(class = "nav-link",
                              id = "viewNav",
                              role = "button",
                              "data-bs-toggle" = "dropdown",
                              "View"
                       ),
                     ),
                     tags$li(
                       class = "nav-item",
                       tags$a(class = "nav-link",
                              id = "connectionNav",
                              role = "button",
                              "data-bs-toggle" = "dropdown",
                              "Connection"
                       )
                       
                     ),
                     
                   )
               )
           )
  ),
  
  #########################################################
  
  # Main container
  div(
    class = "container py-3",
    div(
      class = "row justify-content-center",
      div(
        class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 bordered rounded shadow",
        
        # Connection options
        div(id = "connectionDiv",
            # Header
            tags$h3(class = "text-center", "Select a Connection"),
            
            # Select a connection
            selectInput("connectionSelect", "", choices = NULL, width = "100%"),
            
            # Connect button
            tags$button(class = "btn btn-outline-success w-100 mt-2 mb-3", id = "connectButton", "Connect"),
            
            # Add connection button
            tags$button(class = "btn btn-outline-secondary w-100 mt-2 mb-3", id = "manageConnectionsButton", "Manage Connections"),
            
            # Connection status
            p(id = "connectionStatus")
        ),
        
        # View tables
        div(id = "viewDiv",
            style = "display: none;",
            # Header
            tags$h3(class = "text-center", "View Tables"),
            
            # Select schema
            selectizeInput("schema", "Schema", choices = "Loading...", width = "100%"),
            
            # Select table
            selectizeInput("tables", "Table", choices = "Loading...", width = "100%"),
            div(
              class = "row justify-content-between py-3",
              
              # View Table
              div(
                class = "col-6",
                tags$button(id = "viewTable",
                            class = "btn btn-outline-primary w-100",
                            "View")
                
              ),
              
              # Delete Table
              div(
                class = "col-6",
                tags$button(id = "deleteTable",
                            class = "btn btn-outline-danger w-100",
                            "Delete")
              )
              
            ),
            
            # File upload to database
            fileInput("newTableUpload", "Upload CSV", accept = ".csv", width = "100%"),
            
            # Send query to database
            textAreaInput("query", "Query", width = "100%", height = "250px"),
            tags$button(id = "submitQuery", class = "btn btn-outline-success", "Submit Query")
        ),
      ),
    )
  )
)

#############################################################
#############################################################
#############################################################

server <- function(input, output, session) {
  
  # Set initial connection status
  connectionStatus <- FALSE
  
  # Create a place to store DB credentials
  dbc_path <- glue("{Sys.getenv(\"USERPROFILE\")}\\AppData\\Local\\Programs\\Database_App\\")
  if(file.exists(glue("{dbc_path}\\database_credentials.csv"))){
    connections <- read_csv(glue("{dbc_path}\\database_credentials.csv"))
  } else {
    dir.create(dbc_path)
    file.create(glue("{dbc_path}\\database_credentials.csv"))
    connections <- as.data.frame(matrix(nrow = 1, ncol = 7))
    colnames(connections) <- c("name", "host","username", "password", "port", "database", "driver")
    write_csv(connections, glue("{dbc_path}\\database_credentials.csv"))
  }
  
  
  ###########################################################
  
  # Switch views
  onclick("viewNav", {
    if(connectionStatus){
      hideElement("connectionDiv")
      showElement("viewDiv")
    } else {
      showNotification("Please connect to a database first")
    }
  })
  
  onclick("connectionNav", {
    showElement("connectionDiv")
    hideElement("viewDiv")
  })
  
  ###################################################
  
  # Update the connection select
  updateSelectInput(inputId = "connectionSelect", choices = connections$name)
  
  # Connect to DB
  onclick("connectButton", {
    read_csv(glue("{dbc_path}\\database_credentials.csv")) %>%
      filter(name == input$connectionSelect) ->
      database_credentials
    
    # Get user credentials
    host <- database_credentials$host
    username <- database_credentials$username
    password <- database_credentials$password
    port <- database_credentials$port
    database <- database_credentials$database
    driver <- database_credentials$driver
    
    # Try connecting to DB
    result <- tryCatch({
      # Postgres driver
      if (driver == "postgres"){
        con <- dbConnect(Postgres(),
                         host = host,
                         user = username,
                         password = password,
                         port = port,
                         dbname = database
        )
        result <- "Success"
      } 
      
      # Teradata driver
      if (driver == "teradata"){
        con <- dbConnect(TeradataDriver(),
                         host = host,
                         user = username,
                         password = password
        )
        result <- "Success"
      } else{
        showNotification("Driver must be one of: postgres, teradata")
      }
    },
    error = function(error) {
      result <- error$message
    })
    
    # Notify of connection result
    showNotification(result)
    
    if(result == "Success"){
      connectionStatus <- TRUE
      html("connectionStatus", glue("Connected to: {database_credentials$name}"))
      hideElement("connectionDiv")
      showElement("viewDiv")
    }
    
    # List all schemas
    schemas <- 
      str_remove_all(
        str_split(
          sapply(
            dbListObjects(con)$table,
            dbQuoteIdentifier,
            conn = con),
          " "
        ),
        "\""
      )
    
    # Update schema list
    updateSelectizeInput(
      session,
      "schema",
      choices = schemas,
      selected = schemas[1],
      server = TRUE
    )
    
    # Set initial table options
    get_tables <- function(schema){
      tables <- 
        str_remove_all(
          sapply(
            dbListObjects(con, schema)$table,
            dbQuoteIdentifier,
            conn = con),
          glue("\"|{schema}|\\.")
        )
    }
    
    # Set initial tables
    current_tables <- get_tables(schemas[1])
    updateSelectizeInput(
      session,
      "tables",
      choices = current_tables,
      selected = current_tables[1],
      server = TRUE
    )
    
    # Update table select on schema change
    onevent("change", "schema", {
      current_tables <- get_tables(input$schema)
      updateSelectizeInput(
        session,
        "tables",
        choices = current_tables,
        selected = current_tables[1],
        server = TRUE
      )
    })
    
  })
  
  
  ######################################################
  
  # View tables on click view button
  onclick("viewTable", {
    
    # Get the number of rows
    dbSendQuery(con, glue("CREATE TEMP VIEW temp_view_1234 AS (SELECT * FROM \"{input$schema}\".\"{input$tables}\")"))
    n_rows <- dbGetQuery(con, "SELECT COUNT(*) FROM temp_view_1234")$count
    dbSendQuery(con, "DROP VIEW temp_view_1234")
    
    # Show the modal
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "xl",
        h3(glue("Preview {input$tables} in {input$schema}")),
        p(glue("{n_rows} rows")),
        div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          renderDataTable(
            options = list(dom = 't', paging = FALSE, ordering = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              dbGetQuery(con, glue("SELECT * FROM \"{input$schema}\".\"{input$tables}\" ORDER BY RANDOM() LIMIT 100"))
            }
          )
        ),
        footer = tagList(
          tags$button(class = "btn btn-outline-secondary", id = "downloadPreview", style = "display: none;", "Download"),
          modalButton("Dismiss")
        )
      )
    )
    
    # Don't allow downloading if query result too big
    if(n_rows < 50000 & n_rows != 0){showElement("downloadPreview")} else{hideElement("downloadPreview")}
    
    # Download the query result
    onclick("downloadPreview", {
      result <- tryCatch({
        
        # Get query and write to csv
        write_csv(dbGetQuery(con, glue("SELECT * FROM \"{input$schema}\".\"{input$tables}\" ")), glue("{Sys.getenv(\"USERPROFILE\")}\\Downloads\\query_result_{format(Sys.time(), \"%Y-%m-%d-%H%M%S\")}.csv"))
        result <- glue("Downloaded Successfully to {Sys.getenv(\"USERPROFILE\")}\\Downloads")
        
      }, error = function(error){
        result <- error$message
      })
      showNotification(result)
    })
  })
  
  # Allow deleting a table
  onclick("deleteTable", {
    table <- input$tables
    showModal(
      modalDialog(
        easyClose = TRUE,
        h3("Confirm Deletion"),
        p(glue("Are you sure you want to delete the table: {table}?")),
        footer = tags$button(id = "confirmDelete", class = "btn btn-outline-danger", "Confirm")
      )
    )
    
    # Confirm delete
    onclick("confirmDelete", asis = TRUE, {
      removeModal()
      
      result <- tryCatch({
        dbSendQuery(con, glue("DROP TABLE \"{input$schema}\".\"{table}\""))
        result <- "Success"
      }, error = function(error){
        result <- error$message
      })
      
      # Notify success
      showNotification(result)
      
      # Update select input
      updateSelectInput(inputId = "tables", choices = get_tables(input$schema))
    })
  })
  
  ###########################################
  
  # Increase file upload limit
  options(shiny.maxRequestSize=2000*1024^2)
  
  # Upload file to DB
  observeEvent(input$newTableUpload, {
    # Read csv
    file <- input$newTableUpload
    req(file)
    new_table <- read_csv(file$datapath, show_col_types = FALSE)
    
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "s",
        textInput("newTableName", "Confirm Table Name", value = gsub(".csv", "", file$name)),
        footer = tagList(
          tags$button(id = "confirmNewTableName", class = "btn btn-outline-primary","data-bs-dismiss" = "modal", "Confirm")
        )
      )
    )
    
    onclick("confirmNewTableName", {
      # Write data frame to DB
      result <- tryCatch({
        dbWriteTable(con,
                     name = Id(table = input$newTableName, schema = input$schema),
                     value = data.frame(new_table),
                     overwrite = TRUE
        )
        result <- "Success"
      },
      error = function(error) {
        result <- error$message
      }
      )
      
      # Show result
      showNotification(result, duration = 3)
      
      # Update select input
      updateSelectInput(inputId = "tables", choices = get_tables(input$schema), selected = input$newTableName)
    })
    
    
  })
  
  ###########################################
  
  # Manage connections modal
  onclick("manageConnectionsButton", {
    
    # Get most updated connections file
    connections <- read_csv(glue("{dbc_path}\\database_credentials.csv"), show_col_types = FALSE)
    
    # Set table proxy
    proxy = dataTableProxy('dbConnectionsTable')
    
    # Show the modal
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "xl",
        h3("Manage Connections"),
        div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          DTOutput("dbConnectionsTable")
        ),
        footer = tagList(
          tags$button(id = "addNewConnection", class = "btn btn-outline-success", "Add New"),
          tags$button(id = "deleteConnection", class = "btn btn-outline-danger", "Delete"),
          tags$button(id = "manageConfirm", class = "btn btn-outline-secondary", "Confirm", "data-bs-dismiss" = "modal")
        )
      )
    )
    
    # Create an editable datatable
    output$dbConnectionsTable <- renderDT({
      datatable(connections,
                options = list(dom = 't'),
                editable = TRUE)
    })
    
    # # Save changes to csv file
    observeEvent(input$dbConnectionsTable_cell_edit, {
      connections <<- editData(connections, input$dbConnectionsTable_cell_edit, proxy)
    })
    
    # Add new row button
    onclick("addNewConnection", {
      connections[nrow(connections) + 1, ] <- data.frame(matrix(nrow = 1, ncol = 7))
      replaceData(proxy, connections)
    })
    
    # Delete row button
    onclick("deleteConnection", {
      row_n <- input$dbConnectionsTable_rows_selected
      connections <- connections[-c(row_n), ]
      replaceData(proxy, connections)
      
      # Replace the connections file
      file.remove(glue("{dbc_path}\\database_credentials.csv"))
      write_csv(connections, glue("{dbc_path}\\database_credentials.csv"))
      
    })
    
    # Refresh the page
    onclick("manageConfirm", {
      # Replace the connections file
      file.remove(glue("{dbc_path}\\database_credentials.csv"))
      write_csv(connections, glue("{dbc_path}\\database_credentials.csv"))
      
      updateSelectInput(inputId = "connectionSelect", choices = connections$name)
      output$dbConnectionsTable <- NULL
    })
    
    
  })
  
  #########################################################
  
  # Allow submitting queries
  onclick("submitQuery", {
    
    # Get the query
    query <- input$query
    
    # Initialize number of rows
    n_rows <- 0
    
    # Check if it is a select statement
    if (grepl("SELECT|Select|select", query)){
      
      # Add Limit if needed
      if (!grepl("LIMIT|Limit|limit", query)){
        query <-  glue("{query} LIMIT 100")
      }
      
      result <- tryCatch({
        # Set search path
        dbSendQuery(con, glue("SET search_path TO public, {input$schema}"))
        
        # Get the number of rows
        dbSendQuery(con, glue("CREATE TEMP VIEW temp_view_1234 AS ({input$query})"))
        n_rows <- dbGetQuery(con, "SELECT COUNT(*) FROM temp_view_1234")$count
        dbSendQuery(con, "DROP VIEW temp_view_1234")
        
        # Get the result
        dbGetQuery(con, query)
      },
      error = function(error) {
        result <- data.frame(result = error$message)
      })
      
    } else {
      result <- tryCatch({
        # Set search path
        dbSendQuery(con, glue("SET search_path TO public, {input$schema}"))
        
        # Send query
        dbSendQuery(con, query)
        result <- data.frame(result = "Success")
      },
      error = function(error) {
        result <- data.frame(result = error$message)
      })
    }
    
    # Show query result
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "xl",
        h3("Query Preview"),
        p(glue("{n_rows} rows")),
        div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          renderDataTable(
            options = list(dom = 't', paging = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              result
            }
          )
        ),
        footer = tagList(
          tags$button(class = "btn btn-outline-secondary", id = "downloadQuery", style = "display: none;", "Download"),
          modalButton("Dismiss")
        )
      )
    )
    
    # Don't allow downloading if query result too big
    if(n_rows < 50000 & n_rows != 0){showElement("downloadQuery")}else{hide("downloadQuery")}
    
    # Download the query result
    onclick("downloadQuery", {
      result <- tryCatch({
        
        # Set search path
        dbSendQuery(con, glue("SET search_path TO public, {input$schema}"))
        
        # Get query and write to csv
        write_csv(dbGetQuery(con, input$query), glue("{Sys.getenv(\"USERPROFILE\")}\\Downloads\\query_result_{format(Sys.time(), \"%Y-%m-%d-%H%M%S\")}.csv"))
        result <- glue("Downloaded Successfully to {Sys.getenv(\"USERPROFILE\")}\\Downloads")
        
      }, error = function(error){
        result <- error$message
      })
      
      showNotification(result)
    })
    
    # Update the select input
    updateSelectInput(inputId = "tables", choices = get_tables(input$schema))
  })
  
  ########################################################
  
  
  # Disconnect from DB
  session$onSessionEnded(function(){
    try(dbDisconnect(con))
    stopApp()
  })
  
  
}

shinyApp(ui, server)