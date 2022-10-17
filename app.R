library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(glue)
library(DT)
library(janitor)
library(shinyAce)
library(stringr)
library(purrr)
library(DBI)

ui <- bootstrapPage(
  theme = bslib::bs_theme(version = 5),

  # Initiate shinyjs
  useShinyjs(),
 

  #######################################################

  # Navbar
  tags$div(
    class = "container-fluid pt-2",
    tags$nav(
      class = "navbar navbar-expand-sm navbar-light bg-light border rounded shadow-sm",
      div(
        class = "container-fluid",
        tags$a(
          class = "navbar-brand",
          "Database App"
        ),
        tags$button(
          class = "navbar-toggler",
          type = "button",
          "data-bs-toggle" = "collapse",
          "data-bs-target" = "#navbarSupportedContent",
          tags$span(class = "navbar-toggler-icon")
        ),
        div(
          class = "collapse navbar-collapse",
          id = "navbarSupportedContent",
          tags$ul(
            class = "navbar-nav me-auto mb-2 mb-sm-0",
            tags$li(
              class = "nav-item",
              tags$a(
                class = "nav-link",
                id = "connectionNav",
                role = "button",
                "data-bs-toggle" = "dropdown",
                "Connection"
              )
            ),
            tags$li(
              class = "nav-item",
              tags$a(
                class = "nav-link",
                id = "viewNav",
                role = "button",
                "data-bs-toggle" = "dropdown",
                "View"
              ),
            ),
            tags$li(
              class = "nav-item",
              tags$a(
                class = "nav-link",
                id = "queryNav",
                role = "button",
                "data-bs-toggle" = "dropdown",
                "Query"
              )
            ),
          )
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
        class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 border rounded shadow",
        id = "connectionDiv",

        
        
        div(
          class = "row",
          div(
            class = "col",
            # Header
            tags$h3(class = "text-start", "Connect"),
          ),
          
          div(
            class = "col",
            # Add new connection button
            tags$button(
              class = "btn btn-light mt-3 float-right",
              id = "addConnectionButton",
              tags$img(
                src = "plus-square.svg",
                style = "width: 25px; height: 25px;"
              )
            ),
            # Edit connections button
            tags$button(
              class = "btn btn-light mt-3 float-right",
              id = "manageConnectionsButton",
              tags$img(
                src = "gear.svg",
                style = "width: 25px; height: 25px;"
              )
            ),
          ),
        ),
        
        # Select a connection
        selectInput("connectionSelect", "", choices = NULL, width = "100%"),
        
        # Connect button
        tags$button(
          class = "btn btn-outline-success w-100 mt-2 mb-3",
          id = "connectButton",
          "Connect"
        ),

        # Connection status
        p(id = "connectionStatus")
      ),
      div(
        class = "col-10 col-md-9 col-lg-6 bg-light py-3 px-5 border rounded shadow",
        id = "viewDiv",
        style = "display: none;",

        # Header
        tags$h3(class = "text-center", "View Tables"),

        # Select schema
        selectInput("schema", "Schema", choices = c("Loading..."), width = "100%", ),

        # Select table
        selectInput("tables", "Table", choices = c("Loading..."), width = "100%"),
        div(
          class = "row justify-content-between py-3",

          # View Table
          div(
            class = "col-6",
            tags$button(
              id = "viewTable",
              class = "btn btn-outline-primary w-100",
              "View"
            )
          ),

          # Delete Table
          div(
            class = "col-6",
            tags$button(
              id = "deleteTable",
              class = "btn btn-outline-danger w-100",
              "Delete"
            )
          )
        ),

        # File upload to database
        fileInput("newTableUpload", "Upload CSV", accept = ".csv", width = "100%"),
      ),
      div(
        class = "col-10 col-lg-8 bg-light pb-2 pt-1 border rounded shadow",
        id = "queryDiv",
        style = "display: none; height: 80vh;",
        
        div(
          class = "row mt-0 pt-0",
          div(
            class = "col",
            tags$button(
              id = "formatQuery",
              class = "btn btn-sm btn-outline-none float-right",
              tags$img(
                src = "info-square.svg",
                style = "width: 20px; height: 20px;"
              ),
              " Format "
            ),
            tags$button(
              id = "submitQuery",
              class = "btn btn-sm btn-outline-none float-right",
              tags$img(
                src = "caret-right-square.svg",
                style = "width: 20px; height: 20px;"
              ),
              " Run "
            ),
          )
        ),
        
        div(
          class = "row h-100 pt-1",
          div(
            class = "col",
            # Send query to database
            aceEditor(
              "query",
              mode = "pgsql",
              height = "95%",
              value = "",
              showPrintMargin = FALSE,
              fontSize = 16,
              highlightActiveLine = FALSE
            ),
          )
        )
        
        
        
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
  dbc_path <-
    glue(
      "{Sys.getenv(\"USERPROFILE\")}\\AppData\\Local\\Programs\\Database_App\\"
    )
  if (file.exists(glue("{dbc_path}\\database_connections.csv"))) {
    connections_df <-
      read_csv(
        glue("{dbc_path}\\database_connections.csv"),
        show_col_types = FALSE
      )
    connections <- connections_df$connection_id
    names(connections) <- connections_df$connection_name
  } else {
    dir.create(dbc_path)
    file.create(
      glue(
        "{dbc_path}\\database_connections.csv"
        )
      )
    connections_df <- data.frame(
      connection_id = integer(),
      connection_name = character()
    )
    write_csv(
      connections_df,
      glue(
        "{dbc_path}\\database_connections.csv"
        )
      )
    connections <- connections_df$connection_id
    names(connections) <- connections_df$connection_name
  }


  ###########################################################

  onclick("viewNav", {
    if (connectionStatus) {
      hideElement("connectionDiv")
      hideElement("queryDiv")
      showElement("viewDiv")
    } else {
      showNotification("Please connect to a database first")
    }
  })

  onclick("queryNav", {
    if (connectionStatus) {
      hideElement("connectionDiv")
      hideElement("viewDiv")
      showElement("queryDiv")
    } else {
      showNotification("Please connect to a database first")
    }
  })

  onclick("connectionNav", {
    hideElement("viewDiv")
    hideElement("queryDiv")
    showElement("connectionDiv")
  })


  ###################################################

  # Update the connection select
  updateSelectInput(
    inputId = "connectionSelect",
    choices = connections
  )

  # Connect to DB
  onclick("connectButton", {
    database_credentials <-
      read_csv(
        glue("{dbc_path}\\connection_{input$connectionSelect}.csv"),
        show_col_types = FALSE
      )
    
    driver <-
      database_credentials$driver
    
    # Try connecting to DB
    result <- tryCatch(
      {
        if (driver == "postgres"){
          if (require("RPostgres")){
            con <- DBI::dbConnect(
              RPostgres::Postgres(),
              host = database_credentials$host,
              user = database_credentials$user,
              password = database_credentials$password,
              port = database_credentials$port,
              dbname = database_credentials$dbname
            )
            
            # Get all schemas
            schemas <-
              dbGetQuery(
                con,
                "
                SELECT schema_name
                FROM information_schema.schemata
                ORDER BY schema_name;
                "
              ) |>
              pull(
                schema_name
              )
            
            # Create a unique get tables function
            get_tables <- function(schema) {
              dbGetQuery(
                con,
                glue(
                  "
                  SELECT *
                  FROM information_schema.tables
                  WHERE table_schema = '{schema}'
                  ORDER BY table_name;
                  "
                )
              ) |>
                pull(
                  table_name
                )
            }
            
            # Create a unique get row count function
            get_n_rows <- function(schema, table){
              dbGetQuery(
                con,
                glue(
                  "
                  WITH cte1 AS (
                    SELECT * 
                    FROM \"{schema}\".\"{table}\"
                  )
                  SELECT
                    COUNT(*)
                  FROM cte1
                  "
                )
              ) |>
                pull(
                  count
                )
            }
            
            # Create a unique preview function
            get_preview <- function(schema, table){
              dbGetQuery(
                con,
                glue(
                  "
                  SELECT * 
                  FROM \"{schema}\".\"{table}\"
                  LIMIT 100;
                  "
                )
              )
            }
            
            result <- "Success"
          } else {
            showNotification("Please install RPostgres package.")
          }
          
        } else if (driver == "teradata"){
            if(require(teradatasql)){
              con <- DBI::dbConnect(
                teradatasql::TeradataDriver(),
                host = database_credentials$host,
                user = database_credentials$user,
                password = database_credentials$password
              )
              
              # Get all schemas
              schemas <-
                dbListObjects(con) |>
                filter(
                  is_prefix
                ) |>
                mutate(
                  schema = map_chr(
                    table,
                    ~ DBI::dbUnquoteIdentifier(con, .x)[[1]]@name
                  )
                ) |>
                arrange(
                  schema
                ) |>
                pull(
                  schema
                )

              
              # Create a unique get tables function
              get_tables <- function(schema) {
                dbListTables(con, schema = schema)
              }
              
              # Create a unique get row count function
              get_n_rows <- function(schema, table){
                
                dbGetQuery(
                  con,
                  glue(
                    "
                    SELECT COUNT(*) AS count
                    FROM {table_sql}
                    "
                  )
                ) |>
                  pull(
                    count
                  )
              }
              
              # Create a unique preview function
              get_preview <- function(schema, table){
                table_sql <-
                  dbQuoteIdentifier(
                    con,
                    Id(
                      schema = schema,
                      table = tables
                    )
                  )
                
                dbGetQuery(
                  con,
                  glue(
                    "
                  SELECT TOP 100 
                  FROM {table_sql};
                  "
                  )
                )
              }
              
              result <- "Success"
            } else {
              showNotification("Please install teradatasql package.")
            }
            
        } else if (driver == "snowflake"){
            if(require(odbc)){
              if ("SnowflakeDSIIDriver" %in% pull(odbc::odbcListDrivers(), name)){
                con <- DBI::dbConnect(
                  odbc::odbc(),
                  driver = "SnowflakeDSIIDriver",
                  uid = database_credentials$user,
                  pwd = database_credentials$password,
                  server = database_credentials$server,
                  database = database_credentials$database,
                  schema = database_credentials$schema,
                  warehouse = database_credentials$warehouse,
                  role = database_credentials$role
                )
                
                schemas <-
                  dbGetQuery(
                    con,
                    "
                    SELECT schema_name
                    FROM information_schema.schemata
                    ORDER BY schema_name;
                    "
                  ) |>
                  pull(
                    SCHEMA_NAME
                  )
                
                # Create a unique get tables function
                get_tables <- function(schema) {
                  dbGetQuery(
                    con,
                    glue(
                      "
                      SELECT table_name
                      FROM information_schema.tables
                      WHERE table_schema = '{schema}'
                      ORDER BY table_name;
                      " 
                    )
                  ) |>
                    pull(
                      TABLE_NAME
                    )
                }
                
                
                # Create a unique get row count function
                get_n_rows <- function(schema, table){
                  
                  dbGetQuery(
                    con,
                    glue(
                      "
                      SELECT COUNT(*) AS count
                      FROM \"{schema}\".\"{table}\"
                      "
                    )
                  ) |>
                    pull(
                      COUNT
                    )
                }
                
                # Create a unique preview function
                get_preview <- function(schema, table){
                  dbGetQuery(
                    con,
                    glue(
                      "
                      SELECT * 
                      FROM \"{schema}\".\"{table}\"
                      LIMIT 100
                      "
                    )
                  )
                }
                
                result <- "Success"
              } else {
                showNotification("Please install the Snowflake 64 bit driver")
              }
              
            } else {
              showNotification("Please install odbc package.")
            } 
        } else if (driver == "vertica"){
            if(require(odbc)){
              if ("Vertica" %in% pull(odbc::odbcListDrivers(), name)){
                con <- DBI::dbConnect(
                  odbc::odbc(),
                  driver = "Vertica",
                  username = database_credentials$username,
                  password = database_credentials$password,
                  server = database_credentials$server,
                  database = database_credentials$database,
                  port = database_credentials$port
                )
                
                # Get all schemas
                schemas <-
                  dbGetQuery(
                    con,
                    "
                    SELECT
                      schema_id, 
                      schema_name,
                      u.user_name as owner,
                      create_time,
                      is_system_schema
                    FROM v_catalog.schemata s
                    JOIN v_catalog.users u
                      ON s.schema_owner_id = u.user_id
                    ORDER BY schema_name;
                    "
                  ) |>
                  pull(
                    schema_name
                  )
                
                # Create a unique get tables function
                get_tables <- function(schema) {
                  dbGetQuery(
                    con,
                    glue(
                      "
                      SELECT 
                        table_schema,
                        table_name,
                        create_time
                      FROM v_catalog.tables
                      WHERE table_schema = '{schema}'
                      ORDER BY table_name;
                      "
                    )
                  ) |>
                    pull(
                      table_name
                    )
                }
                
                # Create a unique get row count function
                get_n_rows <- function(schema, table){
                  dbGetQuery(
                    con,
                    glue(
                      "
                      SELECT COUNT(*) AS COUNT
                      FROM \"{schema}\".\"{table}\"
                      "
                    )
                  ) |>
                    pull(
                      COUNT
                    )
                }
                
                # Create a unique get preview function
                get_preview <- function(schema, table){
                  dbGetQuery(
                    con,
                    glue(
                      "
                      SELECT * 
                      FROM \"{schema}\".\"{table}\"
                      LIMIT 100;
                      "
                    )
                  )
                }
                
                result <- "Success"
              } else {
                showNotification("Please install the Vertica 64 bit driver")
              }
            } else {
            showNotification("Please install odbc package.")
            }
        } else {
          result <- "Driver must be one of: postgres, teradatasql, vertica, or snowflake"
        }
      },
      error = function(error) {
        result <- error$message
      }
    )

    # Notify of connection result
    showNotification(result)

    if (result == "Success") {
      connectionStatus <- TRUE
      html(
        "connectionStatus",
        glue("Connected to: {input$connectionSelect}")
      )
      hideElement("connectionDiv")
      showElement("viewDiv")
      

      # Update schema list
      updateSelectizeInput(
        session,
        "schema",
        choices = schemas,
        selected = schemas[1],
        server = TRUE
      )
      
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
    }
  })


  ######################################################

  # View tables on click view button
  onclick("viewTable", {


    # Get the number of rows
    n_rows <- tryCatch({
      get_n_rows(
        input$schema,
        input$tables
      )
    }, error = function(error){
      print(error)
    })
      

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
            options = list(dom = "t", paging = FALSE, ordering = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              result <- tryCatch({
                get_preview(input$schema, input$tables)
              }, error = function(error){
                data.frame(
                  error = error$message
                )
              })
              result
            }
          )
        ),
        footer = tagList(
          tags$button(
            class = "btn btn-outline-secondary",
            id = "downloadPreview",
            style = "display: none;",
            "Download"
          ),
          modalButton("Dismiss")
        )
      )
    )

    # Don't allow downloading if query result too big
    if (n_rows < 50000 & n_rows != 0) {
      showElement("downloadPreview")
    } else {
      hideElement("downloadPreview")
    }

    # Download the query result
    onclick("downloadPreview", {
      result <- tryCatch(
        {

          # Get query and write to csv
          write_csv(
            dbGetQuery(
              con,
              glue(
                "SELECT * FROM {table_sql}"
              )
            ),
            glue(
              "{Sys.getenv(\"USERPROFILE\")}\\Downloads\\query_result_{format(Sys.time(), \"%Y-%m-%d-%H%M%S\")}.csv"
            )
          )
          result <-
            glue(
              "Downloaded Successfully to {Sys.getenv(\"USERPROFILE\")}\\Downloads"
            )
        },
        error = function(error) {
          result <- error$message
        }
      )
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
        footer = tags$button(
          id = "confirmDelete",
          class = "btn btn-outline-danger",
          "Confirm"
        )
      )
    )

    # Confirm delete
    onclick("confirmDelete", asis = TRUE, {
      removeModal()

      result <- tryCatch(
        {
          dbSendQuery(con, glue("DROP TABLE {table_sql}"))
          result <- "Success"
        },
        error = function(error) {
          result <- error$message
        }
      )

      # Notify success
      showNotification(result)

      # Update select input
      updateSelectInput(inputId = "tables", choices = get_tables(input$schema))
    })
  })

  ###########################################

  # Increase file upload limit
  options(shiny.maxRequestSize = 2000 * 1024^2)

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
        tagList(
          textInput(
            "newTableName",
            "Confirm Table Name",
            value = gsub(".csv", "", file$name)
          ),
          selectInput(
            "cleanColumnNames",
            "Clean column names?",
            choices = c("Yes", "No"),
            selected = "Yes"
          )
        ),
        footer = tagList(
          tags$button(
            id = "confirmNewTableName",
            class = "btn btn-outline-primary",
            "data-bs-dismiss" = "modal",
            "Confirm"
          )
        )
      )
    )

    onclick("confirmNewTableName", {
      if (input$cleanColumnNames == "Yes") {
        new_table <- clean_names(new_table)
      }

      # Write data frame to DB
      result <- tryCatch(
        {
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
      updateSelectInput(
        inputId = "tables",
        choices = get_tables(input$schema),
        selected = input$newTableName
      )
    })
  })

  ###########################################

  # Manage connections modal
  onclick("manageConnectionsButton", {

    # Get most updated connections file
    database_credentials <-
      read_csv(
        glue("{dbc_path}\\connection_{input$connectionSelect}.csv"),
        show_col_types = FALSE
      )

    # Set table proxy
    proxy <- dataTableProxy("dbConnectionsTable")

    # Show the modal
    showModal(
      modalDialog(
        easyClose = TRUE,
        size = "xl",
        title = "Manage Connection",
        div(
          class = "table-responsive",
          style = "max-height: 70vh;",
          DTOutput("dbConnectionsTable")
        ),
        footer = tagList(
          tags$button(
            id = "deleteConnection",
            class = "btn btn-outline-danger",
            "Delete"
          ),
          tags$button(
            id = "manageConfirm",
            class = "btn btn-outline-secondary",
            style = "display: none;",
            "Confirm Changes",
            "data-bs-dismiss" = "modal"
          )
        )
      )
    )
    
  
    # Create an editable datatable
    output$dbConnectionsTable <- renderDT({
      datatable(
        database_credentials,
        options = list(dom = "t"),
        editable = TRUE
      )
    })

    # # Save changes to csv file
    observeEvent(input$dbConnectionsTable_cell_edit, {
      showElement("manageConfirm")
      database_credentials <<- editData(
        database_credentials,
        input$dbConnectionsTable_cell_edit,
        proxy
      )
    })

    # Delete row button
    onclick("deleteConnection", {
      connections_df <-
        read_csv(
          glue("{dbc_path}\\database_connections.csv"),
          show_col_types = FALSE
        )
      
      connections_df <-
        connections_df |>
        filter(
          connection_id != input$connectionSelect
        ) |>
        mutate(
          old_id = connection_id,
          new_id = row_number()
        )
        
      connections_df |>
        select(
          old_id,
          new_id
        ) |>
        pmap(
          function(old_id, new_id){
            file.rename(
              glue(
                "{dbc_path}\\connection_{old_id}.csv",
              ),
              glue(
                "{dbc_path}\\connection_{new_id}.csv",
              )
            )
          }
        )
      
      connections_df |>
        select(
          connection_id = new_id,
          connection_name
        ) |>
        write_csv(
          glue(
            "{dbc_path}\\database_connections.csv"
          )
        )
      
      connections <- connections_df$connection_id
      names(connections) <- connections_df$connection_name
      
      updateSelectInput(
        inputId = "connectionSelect",
        choices = connections
      )
      
      removeModal()
      showNotification("Done!")
    })

    # Refresh the page
    onclick("manageConfirm", {
      # Replace the connections file
      file.remove(glue("{dbc_path}\\connection_{input$connectionSelect}.csv"))
      write_csv(database_credentials, glue("{dbc_path}\\connection_{input$connectionSelect}.csv"))

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
    if (grepl("SELECT|Select|select", query) & !grepl("CREATE", query)) {

      # Add Limit if needed
      if (!grepl("LIMIT|Limit|limit", query)) {
        if (driver == "teradatasql"){
          query <- gsub("SELECT", "SELECT TOP 100", {query})
        } else {
          query <- glue("{query} LIMIT 100")
        }
      }

      result <- tryCatch(
        {
          # Set search path
          if (driver %in% c("postgres", "vertica")){
            dbSendQuery(con, glue("SET search_path TO public, {input$schema}"))
          }

          # Get the number of rows
          n_rows <-
            get_n_rows()

          # Get the result
          dbGetQuery(con, query)
        },
        error = function(error) {
          result <- data.frame(result = error$message)
        }
      )
    } else {
      result <- tryCatch(
        {
          # Set search path
          if (driver %in% c("postgres", "vertica")){
            dbSendQuery(con, glue("SET search_path TO public, {input$schema}"))
          }

          # Send query
          dbSendQuery(con, query)
          result <- data.frame(result = "Success")
        },
        error = function(error) {
          result <- data.frame(result = error$message)
        }
      )
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
            options = list(dom = "t", paging = FALSE),
            server = TRUE,
            rownames = FALSE,
            {
              result
            }
          )
        ),
        footer = tagList(
          tags$button(
            class = "btn btn-outline-secondary",
            id = "downloadQuery",
            style = "display: none;",
            "Download"
          ),
          modalButton("Dismiss")
        )
      )
    )

    # Don't allow downloading if query result too big
    if (n_rows < 50000 & n_rows != 0) {
      showElement("downloadQuery")
    } else {
      hide("downloadQuery")
    }

    # Download the query result
    onclick("downloadQuery", {
      result <- tryCatch(
        {

          # Set search path
          if (driver %in% c("postgres", "vertica")){
            dbSendQuery(con, glue("SET search_path TO public, {input$schema}"))
          }

          # Get query and write to csv
          write_csv(
            dbGetQuery(con, input$query),
            glue("{Sys.getenv(\"USERPROFILE\")}\\Downloads\\query_result_{format(Sys.time(), \"%Y-%m-%d-%H%M%S\")}.csv")
          )
          result <-
            glue("Downloaded Successfully to {Sys.getenv(\"USERPROFILE\")}\\Downloads")
        },
        error = function(error) {
          result <- error$message
        }
      )

      showNotification(result)
    })

    # Update the select input
    updateSelectInput(inputId = "tables", choices = get_tables(input$schema))
  })

  ########################################################

  # Reformat SQL code
  onclick("formatQuery", {
    original_query <- input$query
    if (require(httr)) {
      tryCatch(
        {
          response <-
            GET(
              glue(
                "https://sqlformat.org/api/v1/format",
                "?reindent=1",
                "&keyword_case=upper",
                "&sql={URLencode(original_query)}"
              ),
              use_proxy(
                Sys.getenv("https_proxy")
              )
            )
          updateAceEditor(
            session = session,
            editorId = "query",
            value = content(response, as = "parsed")$result
          )
        },
        error = function(error) {
          showNotification(error$message)
        }
      )
    } else {
      showNotification("You must have httr package installed to format.")
    }
  })

  #######################################################
  
  onclick(
    "addConnectionButton",
    {
      # Show the modal
      showModal(
        modalDialog(
          easyClose = TRUE,
          size = "m",
          title = "Add Connection",
          selectInput(
            "newConnectionType",
            "Select new connection type.",
            choices = c(
              "postgres",
              "teradata",
              "vertica",
              "snowflake"
            ),
            width = "100%"
          ),
          div(
            id = "postgresConnectionForm",
            style = "display: none;",
            textInput("newPostgresConnectionName", "Connection Name", width = "100%"),
            textInput("newPostgresHost", "Host", width = "100%"),
            textInput("newPostgresUser", "User", width = "100%"),
            textInput("newPostgresPassword", "Password", width = "100%"),
            textInput("newPostgresPort", "Port", value = 5432, width = "100%"),
            textInput("newPostgresDbname", "Database Name", width = "100%"),
          ),
          div(
            id = "teradataConnectionForm",
            style = "display: none;",
            textInput("newTeradataConnectionName", "Connection Name", width = "100%"),
            textInput("newTeradataServer", "Server", width = "100%"),
            textInput("newTeradataUsername", "Username", width = "100%"),
            textInput("newTeradataPassword", "Password", width = "100%"),
          ),
          div(
            id = "verticaConnectionForm",
            style = "display: none;",
            textInput("newVerticaConnectionName", "Connection Name", width = "100%"),
            textInput("newVerticaServer", "Server", width = "100%"),
            textInput("newVerticaUsername", "Username", width = "100%"),
            textInput("newVerticaPassword", "Password", width = "100%"),
            textInput("newVerticaPort", "Port", value = 5433, width = "100%"),
            textInput("newVerticaDatabase", "Database Name", width = "100%"),
          ),
          div(
            id = "snowflakeConnectionForm",
            style = "display: none;",
            textInput("newSnowflakeConnectionName", "Connection Name", width = "100%"),
            textInput("newSnowflakeServer", "Server", width = "100%"),
            textInput("newSnowflakeUser", "User", width = "100%"),
            textInput("newSnowflakePassword", "Password", width = "100%"),
            textInput("newSnowflakeDatabase", "Database", width = "100%"),
            textInput("newSnowflakeRole", "Role", width = "100%"),
            textInput("newSnowflakeWarehouse", "Warehouse", width = "100%"),
          ),
          footer = tagList(
            tags$button(
              id = "createConnection",
              class = "btn btn-outline-secondary",
              "Create"
            ),
            tags$button(
              class = "btn btn-outline-primary",
              id = "confirmNewConnection",
              style = "display: none;",
              "Confirm"
            )
          )
        )
      )
      
      onclick(
        "createConnection",
        {
          hideElement("newConnectionType")
          showElement(
            id = glue(
              "{input$newConnectionType}ConnectionForm"
            ),
          )
          hideElement("createConnection")
          showElement("confirmNewConnection")
        }
      )
      
      onclick("confirmNewConnection", {
        if (input$newConnectionType == "postgres"){
          new_name <- input$newPostgresConnectionName
          new_host <- input$newPostgresHost
          new_user <- input$newPostgresUser
          new_password <- input$newPostgresPassword
          new_port <- input$newPostgresPort
          new_dbname <- input$newPostgresDbname
          
          connections_df <-
            read_csv(
              glue("{dbc_path}\\database_connections.csv"),
              show_col_types = FALSE
            )
          
          connections_df <-
            connections_df |>
            bind_rows(
              data.frame(
                connection_id = nrow(connections_df) + 1,
                connection_name = new_name
              )
            )
          
          write_csv(
            data.frame(
              driver = "postgres",
              host = new_host,
              user = new_user,
              password = new_password,
              port = new_port,
              dbname = new_dbname
            ), 
            glue(
              "{dbc_path}\\connection_{nrow(connections_df)}.csv"
            )
          )
        } else if(input$newConnectionType == "teradata"){
          new_name <- input$newTeradataConnectionName
          new_server <- input$newTeradataServer
          new_username <- input$newTeradataUsername
          new_password <- input$newTeradataPassword
          
          connections_df <-
            read_csv(
              glue("{dbc_path}\\database_connections.csv"),
              show_col_types = FALSE
            )
          
          connections_df <-
            connections_df |>
            bind_rows(
              data.frame(
                connection_id = nrow(connections_df) + 1,
                connection_name = new_name
              )
            )
          
          write_csv(
            data.frame(
              driver = "teradata",
              server = new_server,
              username = new_username,
              password = new_password
            ), 
            glue(
              "{dbc_path}\\connection_{nrow(connections_df)}.csv"
            )
          )
        } else if(input$newConnectionType == "vertica"){
          new_name <- input$newVerticaConnectionName
          new_server <- input$newVerticaServer
          new_username <- input$newVerticaUsername
          new_password <- input$newVerticaPassword
          new_port <- input$newVerticaPort
          new_database <- input$newVerticaDatabase
          
          connections_df <-
            read_csv(
              glue("{dbc_path}\\database_connections.csv"),
              show_col_types = FALSE
            )
          
          connections_df <-
            connections_df |>
            bind_rows(
              data.frame(
                connection_id = nrow(connections_df) + 1,
                connection_name = new_name
              )
            )
          
          write_csv(
            data.frame(
              driver = "vertica",
              server = new_server,
              username = new_username,
              password = new_password,
              port = new_port,
              database = new_database
            ), 
            glue(
              "{dbc_path}\\connection_{nrow(connections_df)}.csv"
            )
          )
          
        } else if(input$newConnectionType == "snowflake"){
          new_name <- input$newSnowflakeConnectionName
          new_server <- input$newSnowflakeServer
          new_user <- input$newSnowflakeUser
          new_password <- input$newSnowflakePassword
          new_database <- input$newSnowflakeDatabase
          new_role <- input$newSnowflakeRole
          new_warehouse <- input$newSnowflakeWarehouse
          
          connections_df <-
            read_csv(
              glue("{dbc_path}\\database_connections.csv"),
              show_col_types = FALSE
            )
          
          connections_df <-
            connections_df |>
            bind_rows(
              data.frame(
                connection_id = nrow(connections_df) + 1,
                connection_name = new_name
              )
            )
          
          write_csv(
            data.frame(
              driver = "snowflake",
              server = new_server,
              user = new_user,
              password = new_password,
              database = new_database,
              role = new_role,
              warehouse = new_warehouse
            ), 
            glue(
              "{dbc_path}\\connection_{nrow(connections_df)}.csv"
            )
          )
        }
        
        
        # Replace the connections file
        file.remove(
          glue("{dbc_path}\\database_connections.csv")
        )
        write_csv(
          connections_df,
          glue("{dbc_path}\\database_connections.csv")
        )
        
        connections <- connections_df$connection_id
        names(connections) <- connections_df$connection_name
        
        updateSelectInput(
          inputId = "connectionSelect",
          choices = connections,
          selected = connections[-1]
        )
        
        removeModal()
      }
      )
    }
  )
  
  #######################################################


  # Disconnect from DB
  session$onSessionEnded(function() {
    try(dbDisconnect(con))
    stopApp()
  })
}

shinyApp(ui, server)
