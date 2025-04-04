#' Import AMR data
#'
#' Import antimicrobial resistance data from various file formats
#'
#' @param file Path to the file containing AMR data
#' @param format File format. If NULL, it will be guessed from file extension. 
#'   Options: "csv", "excel", "rdata"
#' @param sheet Sheet name or number if importing from Excel file
#' @param ... Additional parameters passed to the import function 
#'   (e.g., readr::read_csv, readxl::read_excel)
#'
#' @return A data frame containing the imported AMR data
#' @export
#'
#' @examples
#' \dontrun{
#' # Import from CSV
#' amr_data <- import_amr_data("amr_data.csv")
#' 
#' # Import from Excel
#' amr_data <- import_amr_data("amr_data.xlsx", sheet = "Resistance_Data")
#' }
import_amr_data <- function(file, format = NULL, sheet = NULL, ...) {
  if (!file.exists(file)) {
    stop("File does not exist: ", file)
  }
  
  # Determine format from file extension if not provided
  if (is.null(format)) {
    ext <- tools::file_ext(file)
    format <- switch(tolower(ext),
                    csv = "csv",
                    txt = "csv",
                    tsv = "csv",
                    xlsx = "excel",
                    xls = "excel",
                    rdata = "rdata",
                    rda = "rdata",
                    stop("Unsupported file format: ", ext, ". Please specify format manually."))
  }
  
  # Import data based on format
  data <- switch(tolower(format),
                csv = {
                  if (!requireNamespace("readr", quietly = TRUE)) {
                    stop("Package 'readr' is required to import CSV files. Please install it.")
                  }
                  readr::read_csv(file, ...)
                },
                excel = {
                  if (!requireNamespace("readxl", quietly = TRUE)) {
                    stop("Package 'readxl' is required to import Excel files. Please install it.")
                  }
                  readxl::read_excel(file, sheet = sheet, ...)
                },
                rdata = {
                  env <- new.env()
                  load(file, envir = env)
                  # Get the first data frame object in the environment
                  obj_names <- ls(env)
                  data_obj <- NULL
                  for (name in obj_names) {
                    if (is.data.frame(env[[name]])) {
                      data_obj <- env[[name]]
                      break
                    }
                  }
                  if (is.null(data_obj)) {
                    stop("No data frame found in RData file.")
                  }
                  data_obj
                },
                stop("Unsupported format: ", format))
  
  # Basic checks
  if (nrow(data) == 0) {
    warning("Imported data contains 0 rows.")
  }
  
  # Convert to data frame if not already
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  # Add source file attribute
  attr(data, "source_file") <- file
  attr(data, "import_time") <- Sys.time()
  
  return(data)
}

#' Import local AMR data
#'
#' Import local research data to compare with meta-analysis results
#'
#' @param file Path to the file containing local AMR data
#' @param format File format. If NULL, it will be guessed from file extension
#' @param study_info List containing study metadata
#' @param ... Additional parameters passed to import_amr_data
#'
#' @return A data frame containing the imported local AMR data with study metadata
#' @export
#'
#' @examples
#' \dontrun{
#' # Import local data with study information
#' local_data <- import_local_data(
#'   "local_hospital_data.csv",
#'   study_info = list(
#'     study_name = "Local Hospital Study 2022",
#'     institution = "General Hospital",
#'     location = "New York, USA",
#'     collection_period = "Jan-Dec 2022"
#'   )
#' )
#' }
import_local_data <- function(file, format = NULL, study_info = list(), ...) {
  # Import data using the generic import function
  data <- import_amr_data(file, format = format, ...)
  
  # Add study info as attributes
  for (info_name in names(study_info)) {
    attr(data, info_name) <- study_info[[info_name]]
  }
  
  # Mark as local data
  attr(data, "data_type") <- "local"
  
  # Add timestamp
  if (is.null(attr(data, "collection_date"))) {
    attr(data, "collection_date") <- Sys.Date()
  }
  
  return(data)
}

#' Connect to external AMR database
#'
#' Establish a connection to an external AMR database
#'
#' @param database The name of the database to connect to
#' @param credentials A list containing credential information
#' @param ... Additional parameters specific to the database connection
#'
#' @return A connection object or NULL if connection failed
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect to a hypothetical external database
#' conn <- connect_to_database(
#'   "example_db",
#'   credentials = list(
#'     username = "user",
#'     password = "pass",
#'     host = "db.example.org"
#'   )
#' )
#' }
connect_to_database <- function(database, credentials = list(), ...) {
  # Currently supported databases
  supported_dbs <- c("demo", "local")
  
  if (!database %in% supported_dbs) {
    warning("Database '", database, "' is not officially supported. ",
            "Supported databases: ", paste(supported_dbs, collapse = ", "))
  }
  
  # Handle different database types
  conn <- switch(database,
                demo = {
                  # Return a demo connection (just a named list with sample data)
                  message("Connected to demo database. This contains sample AMR data for testing.")
                  list(
                    name = "demo",
                    data = data.frame(
                      study_id = paste0("demo", 1:5),
                      pathogen = sample(c("E. coli", "K. pneumoniae", "S. aureus"), 5, replace = TRUE),
                      antibiotic = sample(c("ciprofloxacin", "amoxicillin", "ceftriaxone"), 5, replace = TRUE),
                      resistance_rate = runif(5, 0.1, 0.9),
                      sample_size = sample(50:500, 5)
                    ),
                    connected = TRUE,
                    fetch = function(query = NULL) {
                      # Simple query function that returns the demo data
                      if (is.null(query)) {
                        return(data)
                      } else {
                        # Very simple filtering
                        if ("pathogen" %in% names(query)) {
                          return(data[data$pathogen == query$pathogen, ])
                        } else {
                          return(data)
                        }
                      }
                    }
                  )
                },
                local = {
                  # Connect to a local SQLite database
                  if (!requireNamespace("RSQLite", quietly = TRUE)) {
                    stop("Package 'RSQLite' is required to connect to a local database. Please install it.")
                  }
                  
                  db_path <- credentials$path
                  if (is.null(db_path)) {
                    stop("Path to local database must be provided in credentials$path")
                  }
                  
                  if (!file.exists(db_path)) {
                    stop("Database file does not exist: ", db_path)
                  }
                  
                  conn <- try(RSQLite::dbConnect(RSQLite::SQLite(), db_path), silent = TRUE)
                  if (inherits(conn, "try-error")) {
                    stop("Failed to connect to local database: ", db_path)
                  }
                  
                  conn
                },
                {
                  # Default case for unsupported databases
                  stop("Connection to database '", database, "' is not implemented.")
                })
  
  return(conn)
}

#' Import data from published paper
#'
#' A helper function to guide the process of extracting and entering AMR data from published papers
#'
#' @param interactive Logical; if TRUE, launch interactive data entry
#'
#' @return A data frame containing the extracted AMR data
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch interactive data entry
#' paper_data <- import_from_paper(interactive = TRUE)
#' }
import_from_paper <- function(interactive = TRUE) {
  if (interactive) {
    if (!requireNamespace("shiny", quietly = TRUE)) {
      stop("Package 'shiny' is required for interactive data entry. Please install it.")
    }
    
    # A minimal shiny app for data entry
    app <- shiny::shinyApp(
      ui = shiny::fluidPage(
        shiny::titlePanel("METEOR - Paper Data Entry"),
        
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::textInput("paper_id", "Paper ID/DOI", ""),
            shiny::textInput("first_author", "First Author", ""),
            shiny::numericInput("year", "Publication Year", value = as.integer(format(Sys.Date(), "%Y"))),
            shiny::textInput("location", "Study Location", ""),
            shiny::selectInput("study_type", "Study Type", 
                             choices = c("Cross-sectional", "Case-control", "Cohort", "Other")),
            shiny::textInput("population", "Population", ""),
            shiny::numericInput("sample_size", "Sample Size", value = 0),
            shiny::actionButton("add_record", "Add Resistance Record"),
            shiny::hr(),
            shiny::actionButton("save_data", "Save Data")
          ),
          
          shiny::mainPanel(
            shiny::tabsetPanel(
              shiny::tabPanel("Study Records", shiny::dataTableOutput("records_table")),
              shiny::tabPanel("About", 
                            shiny::p("This tool helps you enter AMR data from published papers."),
                            shiny::p("Fill in the study details on the left, then add resistance records."),
                            shiny::p("When finished, click 'Save Data' to export the data as CSV."))
            )
          )
        )
      ),
      
      server = function(input, output, session) {
        # Initialize data frame to store records
        records <- shiny::reactiveVal(data.frame(
          paper_id = character(),
          first_author = character(),
          year = integer(),
          location = character(),
          study_type = character(),
          population = character(),
          sample_size = integer(),
          pathogen = character(),
          antibiotic = character(),
          n_resistant = integer(),
          n_total = integer(),
          resistance_rate = numeric(),
          stringsAsFactors = FALSE
        ))
        
        # Show records table
        output$records_table <- shiny::renderDataTable({
          records()
        })
        
        # Add record modal
        shiny::observeEvent(input$add_record, {
          shiny::showModal(shiny::modalDialog(
            title = "Add Resistance Record",
            shiny::selectInput("pathogen", "Pathogen", 
                             choices = c("E. coli", "K. pneumoniae", "S. aureus", "P. aeruginosa", "Other")),
            shiny::conditionalPanel(
              condition = "input.pathogen == 'Other'",
              shiny::textInput("other_pathogen", "Specify Pathogen", "")
            ),
            shiny::selectInput("antibiotic", "Antibiotic", 
                             choices = c("Ciprofloxacin", "Amoxicillin", "Ceftriaxone", "Meropenem", "Other")),
            shiny::conditionalPanel(
              condition = "input.antibiotic == 'Other'",
              shiny::textInput("other_antibiotic", "Specify Antibiotic", "")
            ),
            shiny::numericInput("n_resistant", "Number of Resistant Isolates", value = 0),
            shiny::numericInput("n_total", "Total Number of Isolates", value = 0),
            footer = shiny::tagList(
              shiny::actionButton("confirm_add", "Add"),
              shiny::modalButton("Cancel")
            )
          ))
        })
        
        # Add confirmed record
        shiny::observeEvent(input$confirm_add, {
          # Get pathogen and antibiotic (handle "Other" selection)
          pathogen <- ifelse(input$pathogen == "Other", input$other_pathogen, input$pathogen)
          antibiotic <- ifelse(input$antibiotic == "Other", input$other_antibiotic, input$antibiotic)
          
          # Calculate resistance rate
          resistance_rate <- ifelse(input$n_total > 0, 
                                   input$n_resistant / input$n_total,
                                   NA)
          
          # Create new record
          new_record <- data.frame(
            paper_id = input$paper_id,
            first_author = input$first_author,
            year = input$year,
            location = input$location,
            study_type = input$study_type,
            population = input$population,
            sample_size = input$sample_size,
            pathogen = pathogen,
            antibiotic = antibiotic,
            n_resistant = input$n_resistant,
            n_total = input$n_total,
            resistance_rate = resistance_rate,
            stringsAsFactors = FALSE
          )
          
          # Append to records
          records(rbind(records(), new_record))
          
          # Close modal
          shiny::removeModal()
        })
        
        # Save data
        shiny::observeEvent(input$save_data, {
          if (nrow(records()) == 0) {
            shiny::showNotification("No records to save!", type = "warning")
            return()
          }
          
          # Generate a default filename
          default_filename <- paste0("paper_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
          
          # Create a modal to ask for filename
          shiny::showModal(shiny::modalDialog(
            title = "Save Data",
            shiny::textInput("save_filename", "Filename", default_filename),
            footer = shiny::tagList(
              shiny::downloadButton("do_save", "Save"),
              shiny::modalButton("Cancel")
            )
          ))
        })
        
        # Handle download
        output$do_save <- shiny::downloadHandler(
          filename = function() {
            input$save_filename
          },
          content = function(file) {
            utils::write.csv(records(), file, row.names = FALSE)
            shiny::removeModal()
          }
        )
      }
    )
    
    # Run the app and wait for it to finish
    data <- shiny::runApp(app)
    
    # Return the entered data
    return(data)
  } else {
    # Non-interactive mode: return an empty template
    template <- data.frame(
      paper_id = character(),
      first_author = character(),
      year = integer(),
      location = character(),
      study_type = character(),
      population = character(),
      sample_size = integer(),
      pathogen = character(),
      antibiotic = character(),
      n_resistant = integer(),
      n_total = integer(),
      resistance_rate = numeric(),
      stringsAsFactors = FALSE
    )
    
    return(template)
  }
} 