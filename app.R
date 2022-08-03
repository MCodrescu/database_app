library(shiny)
library(shinyjs)
library(readr)
library(RPostgres)
library(dplyr)
library(glue)
library(DT)

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
            selectInput("schema", "Schema", choices = NULL, width = "100%"),
            
            # Select table
            selectInput("tables", "Table", choices = NULL, width = "100%"),
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
  dbc_path <- glue("{Sys.getenv(\"USERPROFILE\")}\\AppData\\Local\\Programs\\Globys_App\\")
  if(file.exists(glue("{dbc_path}\\database_credentials.csv"))){
    connections <- read_csv(glue("{dbc_path}\\database_credentials.csv"))
  } else {
    dir.create(dbc_path)
    file.create(glue("{dbc_path}\\database_credentials.csv"))
    initial_credentials <- as.data.frame(matrix(nrow = 1, ncol = 6))
    colnames(initial_credentials) <- c("connection", "host","username", "password", "port", "database")
    write_csv(initial_credentials, glue("{dbc_path}\\database_credentials.csv"))
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
  updateSelectInput(inputId = "connectionSelect", choices = connections$connection)
  
  # Connect to DB
  onclick("connectButton", {
   read_csv(glue("{dbc_path}\\database_credentials.csv")) %>%
      filter(connection == input$connectionSelect) ->
      database_credentials
    
    # Get user credentials
    host <- database_credentials$host
    username <- database_credentials$username
    password <- database_credentials$password
    port <- database_credentials$port
    database <- database_credentials$database
    
    # Try connecting to DB
    result <- tryCatch({
      pg_con <- dbConnect(Postgres(),
                          host = host,
                          user = username,
                          password = password,
                          port = port,
                          dbname = database
      )
      result <- "Success"
    },
    error = function(error) {
      result <- error$message
    })
    
    # Notify of connection result
    showNotification(result)
    
    if(result == "Success"){
      connectionStatus <- TRUE
      html("connectionStatus", glue("Connected to: {database_credentials$connection}"))
      hideElement("connectionDiv")
      showElement("viewDiv")
    }
    
    # Update table select
    schema <- dbGetQuery(pg_con, "SELECT table_schema FROM information_schema.tables") %>%
      distinct(table_schema) %>%
      arrange(table_schema)
    
    updateSelectInput(session, "schema", choices = schema, selected = "public")
    
    # Set initial table options
    get_tables <- function(schema){
      query <- glue("SELECT table_name FROM information_schema.tables WHERE table_schema = '{schema}'")
      dbGetQuery(pg_con, query) %>%
        distinct(table_name) %>%
        arrange(table_name) %>%
        pull(table_name)
    }
    
    updateSelectInput(session, "tables", choices = get_tables("public"))
    
    # Update table select on schema change
    onevent("change", "schema", {
      updateSelectInput(session, "tables", choices = get_tables(input$schema))
    })
    
  })

    
   ######################################################
    
  # View tables on click view button
  onclick("viewTable", {
    # Show the modal
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "xl",
        h3(glue("Preview {input$tables} in {input$schema}")),
        div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          renderDataTable(
            options = list(dom = 't', paging = FALSE, ordering = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              dbGetQuery(pg_con, glue("SELECT * FROM \"{input$schema}\".\"{input$tables}\" ORDER BY RANDOM() LIMIT 100"))
            }
          )
        )
      )
    )
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
        dbSendQuery(pg_con, glue("DROP TABLE \"{input$schema}\".\"{table}\""))
        result <- "Success"
      }, error = function(error){
        result <- error$message
      })
      
      # Notify success
      showNotification(result)
      
      # Update select input
      updateSelectInput(inputId = "tables", choices = get_tables("public"))
    })
  })
  
  ###########################################
  
  # Upload file to DB
  observeEvent(input$newTableUpload, {
    # Read csv
    file <- input$newTableUpload
    req(file)
    new_table <- read_csv(file$datapath, show_col_types = FALSE)
    
    # Write data frame to DB
    result <- tryCatch({
      dbWriteTable(pg_con,
                   name = gsub(".csv", "", file$name),
                   value = data.frame(new_table)
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
    updateSelectInput(inputId = "tables", choices = get_tables("public"))
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
      connections[nrow(connections) + 1, ] <- data.frame(matrix(nrow = 1, ncol = 6))
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
      
      updateSelectInput(inputId = "connectionSelect", choices = connections$connection)
      output$dbConnectionsTable <- NULL
    })
    
    
  })
  
  #########################################################
  
  # Allow submitting queries
  onclick("submitQuery", {
    
    # Get the query
    query <- input$query
    
    # Check if it is a select statement
    if (grepl("SELECT|Select|select", query)){
      
      # Add Limit if needed
      if (!grepl("LIMIT|Limit|limit", query)){
        query <-  glue("{query} LIMIT 100")
      }
      
      result <- tryCatch({
        dbGetQuery(pg_con, query)
      },
      error = function(error) {
        result <- data.frame(result = error$message)
      })
      
    } else {
      result <- tryCatch({
        dbSendQuery(pg_con, query)
        result <- data.frame(result = "Success")
      },
      error = function(error) {
        print(error$message)
        result <- data.frame(result = error$message)
      })
    }
    
    
    # Show query result
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "xl",
        h3("Query Preview"),
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
        )
      )
    )
    
    # Update the select input
    updateSelectInput(inputId = "tables", choices = get_tables("public"))
  })
  
  ########################################################
  
  
  # Disconnect from DB
  onStop(function(){
    dbDisconnect(pg_con)
  })
  
  
}

shinyApp(ui, server)