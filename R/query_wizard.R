
query_wizard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container",
        div(class = "row",
            div(class = "col-3",
                selectInput(ns("queryWizardAction"), "Select an Action",
                            choices = c("View Table", "Join Tables"),
                            width = "100%"),
                div(id = ns("queryWizardOptions"), style = "display: none;",
                    selectInput(ns("joinTable1"), "Select First Table",
                                choice = c("Table 1", "Table 2", "Table 3"),
                                width = "100%"),
                    selectInput(ns("joinTable2"), "Select Second Table",
                                choice = c("Table 1", "Table 2", "Table 3"),
                                width = "100%"),
                    selectInput(ns("columnsToKeep"), "Select Columns to Keep",
                                choice = c('column1', 'column3'),
                                width = "100%",
                                multiple = TRUE),
                    selectInput(ns("joinLogic"), "Select Columns to Join",
                                choice = c('column1 = column2', 'column3 = column4'),
                                width = "100%",
                                multiple = TRUE),
                    tags$button(class = "btn btn-outline-primary",
                                id = ns("submitJoinQuery"),
                                "Submit")
                ),
            ),
            div(class = "col-9",
                div(
                  class = "table-responsive",
                  style = "max-height: 80vh;",
                  tableOutput("tableOutputWizard")
                )
            )
        )
    )
  )
}

query_wizard_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # Query Wizard Join Tables Option
      onevent("change", "queryWizardAction", {
        if (input$queryWizardAction == "Join Tables") {
          
          # Set the options to the tables
          dbGetQuery(pg_con, "SELECT * FROM information_schema.tables WHERE table_schema = 'public'") %>%
            select(table_name) %>%
            arrange(table_name) ->
            tables
          
          # Update the select input
          updateSelectInput(inputId = "joinTable1", choices = tables)
          updateSelectInput(inputId = "joinTable2", choices = tables)
          
          # Update column options by table selected
          onevent("change", "joinTable1", {
            dbGetQuery(pg_con, glue("SELECT * FROM {input$joinTable1} LIMIT 1")) %>%
              colnames() ->
              table_1_columns
            
            dbGetQuery(pg_con, glue("SELECT * FROM {input$joinTable2} LIMIT 1")) %>%
              colnames() ->
              table_2_columns
            
            # Update the columns to keep input
            updateSelectInput(
              inputId = "columnsToKeep",
              choices = c(table_1_columns, table_2_columns)
            )
            
            # Get all the possible joins
            joinLogic <- c()
            for (column1 in table_1_columns) {
              for (column2 in table_2_columns) {
                joinLogic <- c(joinLogic, glue('"{column1}" = "{column2}"'))
              }
            }
            
            # Update the join logic input
            updateSelectInput(
              inputId = "joinLogic",
              choices = joinLogic
            )
          })
          
          # Again to catch other change event
          onevent("change", "joinTable2", {
            dbGetQuery(pg_con, glue("SELECT * FROM {input$joinTable1} LIMIT 1")) %>%
              colnames() ->
              table_1_columns
            
            dbGetQuery(pg_con, glue("SELECT * FROM {input$joinTable2} LIMIT 1")) %>%
              colnames() ->
              table_2_columns
            
            # Update the columns to keep input
            updateSelectInput(
              inputId = "columnsToKeep",
              choices = c(table_1_columns, table_2_columns)
            )
            
            # Get all the possible joins
            joinLogic <- c()
            for (column1 in table_1_columns) {
              for (column2 in table_2_columns) {
                joinLogic <- c(joinLogic, glue('"{column1}" = "{column2}"'))
              }
            }
            
            # Update the join logic input
            updateSelectInput(
              inputId = "joinLogic",
              choices = joinLogic
            )
          })
          
          # Show join options
          toggle("queryWizardOptions")
          
          # Show result table
          onclick("submitJoinQuery", {
            collapse_string <- "\", \""
            
            # Get the query
            query <- glue(
              '
              SELECT "{str_c(input$columnsToKeep, collapse = collapse_string)}"
              FROM "{input$joinTable1}"
              INNER JOIN "{input$joinTable2}"
              ON {str_c(input$joinLogic, collapse = ", ")}
              LIMIT 1000
              '
            )
            
            output$tableOutputWizard <-
              renderTable(
                striped = TRUE,
                hover = TRUE,
                bordered = TRUE,
                spacing = c("xs"),
                width = "100%", {
                  dbGetQuery(pg_con, query)
                }
              )
          })
        }
      })
    }
  )
}

