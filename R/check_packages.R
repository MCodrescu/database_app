check_packages <- function(){
  # Load libraries
  packages <-
    c(
      "shiny",
      "shinyjs",
      "RPostgres",
      "dplyr",
      "tidyr",
      "readr",
      "glue",
      "stringr",
      "lubridate",
      "shinythemes",
      "Microsoft365R"
    )
  library_location <- "C:/Program Files/R/R-4.2.0/library"
  
  # Load/install all packages
  package.check <- lapply(
    packages,
    FUN = function(x) {
      if (!require(x, lib.loc = library_location, character.only = TRUE)) {
        install.packages(x, lib = library_location, repos = "https://cloud.r-project.org/", dependencies = TRUE)
        library(x, lib.loc = library_location, character.only = TRUE)
      }
    }
  )
}