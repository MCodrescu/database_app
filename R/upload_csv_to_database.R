
upload_csv_to_database_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Main container
    div(
      class = "container py-3",
      div(
        class = "row justify-content-center",
        div(
          class = "col-8 col-md-6 bg-light p-5 bordered rounded shadow",
          
          # Connection message
          p(id = ns("connectionMessage"), "Trying to connect ..."),
          
          div(
            id = ns("inputTable"), style = "display: none;",
            
            # Title
            h3("Upload Data", class = "text-center fw-bold"),
            
            # View tables
            selectInput(ns("tables"), "Tables", choices = NULL, width = "100%"),
            div(
              class = "row justify-content-between pb-3",
              
              # View Table
              div(
                class = "col-6",
                tags$button(id = ns("viewTable"),
                            class = "btn btn-outline-primary w-100",
                            "View")
                
              ),
              
              # Delete Table
              div(
                class = "col-6",
                tags$button(id = ns("deleteTable"),
                            class = "btn btn-outline-danger w-100",
                            "Delete")
              )
              
            ),
            p(id = ns("deletionResult"), style = "display: none; color: green;", "Done!"),
            
            # File upload to database
            fileInput(ns("newTableUpload"), "Upload New Table", accept = ".csv", width = "100%"),
            
            # Send query to database
            textAreaInput(ns("query"), "Query", width = "100%", height = "100px"),
            tags$button(id = ns("submitQuery"), class = "btn btn-outline-success", "Submit Query")
          )
        )
      )
    )
  )
}

upload_csv_to_database_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
    
      # Get user credentials
      host <- "135.170.241.70"
      username <- "GlobysUser@attebiz-eastus2-poc-pgs.postgres.database.azure.com"
      password <- "Gl0b7su5er"
      port <- "5432"
      database <- "Globys"
      
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
      }
      )
      
      # Show the connection result
      html("connectionMessage", result)
      
      # Hide the result if success
      if (result == "Success") {
        delay(1000, {
          hideElement("connectionMessage", animType = "fade")
          hideElement("connectionResult", animType = "fade")
          showElement("inputTable")
          
          # Show tables in the database
          dbGetQuery(pg_con, "SELECT * FROM information_schema.tables WHERE table_schema = 'public'") %>%
            select(table_name) %>%
            arrange(table_name) ->
            tables
          
          # Update the select input
          updateSelectInput(inputId = "tables", choices = tables)
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
            
            dbSendQuery(pg_con, glue("DROP TABLE \"public\".\"{table}\""))
            
            # Update select input
            dbGetQuery(pg_con, "SELECT * FROM information_schema.tables WHERE table_schema = 'public'") %>%
              select(table_name) %>%
              arrange(table_name) ->
              tables
            
            # Notify success
            showNotification("Done!", duration = 3)
            
            # Update select input
            updateSelectInput(inputId = "tables", choices = tables)
          })
        })
        
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
          
          # Update select input
          dbGetQuery(pg_con, "SELECT * FROM information_schema.tables WHERE table_schema = 'public'") %>%
            select(table_name) %>%
            arrange(table_name) ->
            tables
          
          # Show result
          showNotification(result, duration = 3)
          
          # Update select input
          updateSelectInput(inputId = "tables", choices = tables)
        })
        
        # Allow viewing the tables
        onclick("viewTable", {
          table <- input$tables
          dbGetQuery(pg_con, glue("SELECT * FROM \"public\".\"{table}\" LIMIT 100")) %>%
            mutate(across(.fns = as.character)) ->
            result
          
          showModal(
            modalDialog(
              easyClose = TRUE,
              size = "xl",
              h3(glue("Table Preview: {input$tables}")),
              div(
                class = "table-responsive",
                style = "max-height: 70vh;",
                renderTable(
                  striped = TRUE,
                  hover = TRUE,
                  bordered = TRUE,
                  spacing = c("xs"),
                  width = "100%", {
                    data.frame(result)
                  }
                )
              )
            )
          )
        })
        
        # Allow submitting queries
        onclick("submitQuery", {
          
          # Get the query
          query <- input$query
          
          result <- tryCatch({
            dbSendQuery(pg_con, query)
            result <- "Success"
          },
          error = function(error) {
            result <- error$message
          }
          )
          
          # Show query result
          showNotification(result, duration = 5)
          
          # Update tables in the database
          dbGetQuery(pg_con, "SELECT * FROM information_schema.tables WHERE table_schema = 'public'") %>%
            select(table_name) %>%
            arrange(table_name) ->
            tables
          
          # Update the select input
          updateSelectInput(inputId = "tables", choices = tables)
        })
        
      }
    })
}

