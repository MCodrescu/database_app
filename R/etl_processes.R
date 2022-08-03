etl_processes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "container",
        div(
          class = "row justify-content-center",
          div(
            class = "col-8 col-md-6 bg-light p-5 bordered rounded shadow",
        
            # Page Title
            div(class = "row text-center pb-3 fw-b",
                div(class = "col",
                    h3("Run ETL Process Scripts")
                )
            ),
            
            # LDAP Data Button
            div(class = "row justify-content-center pb-3",
                div(class = "col-8",
                    tags$button(id = ns("ldapLoad"),
                                class = "btn btn-outline-primary w-100",
                                "Load LDAP Data"),
                )
            ),
            
            # eBill Data Button
            div(class = "row justify-content-center pb-3",
                div(class = "col-8",
                    tags$button(id = ns("ebillDataLoad"),
                                class = "btn btn-outline-success w-100",
                                "Load eBill Data")
                )
            ),
            
            # ETL Process Result
            div(class = "row justify-content center pb-3",
                div(class = "col-8",
                    p(id = ns("etlProcessResult"))
                    )
                )
            
          )
        )
    )
  )
}

etl_processes_server <- function(id, oneDriveFile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # Upload ebill Companies and Users on click
      onclick("ebillDataLoad", {
        
        # Connect to One Drive
        one_drive <- get_business_onedrive()
        
        # Download eBill Companies and Users
        one_drive$list_shared_files() %>%
          filter(name == 'eBill_Companies&users') %>%
          pull(remoteItem) ->
          eBill_folder
        
        # Show modal to select files
        showModal(
          modalDialog(
            title = "One Drive File Upload",
            size = "l",
            easyClose = TRUE,
            selectInput("oneDriveFile", 
                        label = "Select Files to Upload",
                        width = "100%",
                        multiple = TRUE,
                        choices = eBill_folder[[1]]$list_files()$name),
            footer = tagList(
              tags$button(id = "submitOneDriveFileUpload",
                          class = "btn btn-primary",
                          "data-bs-dismiss" = "modal",
                          "Upload"),
              modalButton("Dismiss")
            )
          )
        )
        
        onclick("submitOneDriveFileUpload", asis = TRUE, {
          
          # Try uploading files to DB
          result <- tryCatch({
            
            # Keep track of the progress
            withProgress(min = 0, max = 3, expr = {
              
              incProgress(1, "Downloading Files")
              
              # On selecting files begin uploading
              for(f in oneDriveFile()){
                file <- eBill_folder[[1]]$get_item(f)
                file$download()
              }
              
              # Connect to DB
              pg_con <- dbConnect(Postgres(),
                                  host = "135.170.241.70",
                                  user = "GlobysUser@attebiz-eastus2-poc-pgs.postgres.database.azure.com",
                                  password = "Gl0b7su5er",
                                  port = 5432,
                                  dbname = "Globys")
              
              # Delete old eBill files
              try({
                dbSendQuery(pg_con, "DROP TABLE ebill_companies")
                dbSendQuery(pg_con, "DROP TABLE ebill_users")
              })
              
              
              incProgress(1, "Reading New Files to DB")
              
              # Read new files into DB
              dbWriteTable(pg_con, "ebill_companies", read_csv(oneDriveFile()[1]))
              dbWriteTable(pg_con, "ebill_users", read_csv(oneDriveFile()[2]))
              
              # Remove the csv files
              file.remove(oneDriveFile()[1])
              file.remove(oneDriveFile()[2])
              
              incProgress(1, "Updating Data Dictionary")
              
              # Update the data dictionary table
              dbSendQuery(pg_con, glue(
                'UPDATE "data_dictionary"
                SET "last_updated" = \'{Sys.Date()}\'::DATE 
                WHERE "table_name" = \'ebill_companies\'
                OR table_name = \'ebill_users\''
              ))
              
              # Disconnect from DB
              dbDisconnect(pg_con)
              
              # Finalize result
              result <- "Success"
              
            })
          },
          error = function(error) {
            result <- error$message
          })
          
          # Show the ETL Process Result
          html("etlProcessResult", result)
          delay(5000, hide("etlProcessResult"))
          
        })
      })
      
      
      
      # Upload LDAP file on button click
      onclick("ldapLoad", {
        
        # Try to load the LDAP data
        result <- tryCatch({
          # Keep track of the progress
          withProgress(min = 0, max = 5, expr = {
            
            # Get the date of the most recent sunday
            if(wday(today()) != 1){
              distance <- abs(1 - wday(today()))
              sunday <- today() - distance
            } else {
              sunday <- today()
            }
            
            # Format the date 
            sunday_formatted <- format(sunday, "%Y%m%d")
            
            # INC progress
            incProgress(1, "Downloading LDAP File")
            
            # Download file
            url <- glue("https://businessdirectreports.it.att.com:8443/rpts/bd-ldap-data-extract/svar/data/transfer/bd-ldap-data-extract/BD_DATA_EXTRACT_{sunday_formatted}.tar")
            options(timeout = 120)
            download.file(url, destfile = "ldap.tar", mode = "wb", )
            
            # Untar the file
            untar("ldap.tar")
            
            incProgress(1, "Reading File")
            
            # Find the user and company files
            files <- list.files()
            user_file <- files[grepl("^UserObjectClass.*", files)]
            company_file <- files[grepl("^CompanyObjectClass.*", files)]
            
            # Read the user and company files
            bd_users <- read_delim(user_file)
            bd_companies <- read_delim(company_file)
            
            # Fix the column names of company file
            bd_companies_col_names <-
              c(
                "CompanyID",
                "CompanyName",
                "CompanyAcceptedTnC",
                "CompanyApplication",
                "CompanyCPMigrationInd",
                "CompanyCustOfInd",
                "CompanyDescription",
                "CompanyLastUpdateTime",
                "CompanyLookAndFeelInd",
                "CompanyOffer",
                "CompanyParent",
                "CompanyRegistrationInd",
                "CompanyStrataInd",
                "CompanySubsidiary",
                "CompanySubStrataInd",
                "CompanyTypeInd",
                "CompanyURL",
                "DeletionDate",
                "CompanyACSSOU",
                "companySMAcct",
                "peindicator"
              )
            #colnames(bd_companies) <- colnames(bd_companies_col_names)
            
            incProgress(1, "Connecting to DB")
            
            # Connect to DB
            pg_con <- dbConnect(Postgres(),
                                host = "135.170.241.70",
                                user = "GlobysUser@attebiz-eastus2-poc-pgs.postgres.database.azure.com",
                                password = "Gl0b7su5er",
                                port = 5432,
                                dbname = "Globys")
            
            # Drop the old tables
            dbSendQuery(pg_con, "DROP TABLE bd_users")
            dbSendQuery(pg_con, "DROP TABLE bd_companies")
            
            incProgress(1, "Uploading Tables")
            
            # Write the new tables
            dbWriteTable(pg_con, "bd_users", bd_users)
            dbWriteTable(pg_con, "bd_companies", bd_companies)
            
            # Remove the files
            files_to_remove <- files[grepl(".*.txt", files)]
            sapply(files_to_remove, file.remove)
            file.remove("ldap.tar")
            rm(bd_users)
            rm(bd_companies)
            
            incProgress(1, "Updating Data Dictionary")
            
            # Update the data dictionary table
            dbSendQuery(
              pg_con,
              glue(
                'UPDATE "data_dictionary"
                  SET "last_updated" = \'{sunday}\'::DATE
                  WHERE "table_name" = \'bd_users\'
                  OR table_name = \'bd_companies\''
              )
            )
            
            # Disconnect from DB
            dbDisconnect(pg_con)
            
          })
          result <- "Success"
        },
        error = function(error) {
          result <- error$message
        })
        
        # Print the result of the ETL process
        show("etlProcessResult")
        html("etlProcessResult", result)
        delay(5000, hide("etlProcessResult"))
        
      })
    }
  )
}
