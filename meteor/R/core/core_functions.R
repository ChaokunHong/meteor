#' Get METEOR package version
#'
#' @return Character string with version information
#' @export
#'
#' @examples
#' meteor_version()
meteor_version <- function() {
  packageVersion("meteor")
}

#' Initialize a new METEOR project
#'
#' Creates a new directory structure for a METEOR project
#'
#' @param project_name Character string with the name of the project
#' @param path Path where the project should be created. Default is current directory.
#' @param overwrite Logical indicating whether to overwrite existing project. Default is FALSE.
#'
#' @return Invisibly returns the path to the created project
#' @export
#'
#' @examples
#' \dontrun{
#' initialize_meteor_project("my_amr_analysis")
#' }
initialize_meteor_project <- function(project_name, path = getwd(), overwrite = FALSE) {
  # Check if the project name is valid
  if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", project_name)) {
    stop("Project name must start with a letter and can only contain letters, numbers, and underscores.")
  }
  
  # Create full path
  project_dir <- file.path(path, project_name)
  
  # Check if directory exists
  if (dir.exists(project_dir)) {
    if (!overwrite) {
      stop("Project directory already exists. Use overwrite = TRUE to replace it.")
    } else {
      # Remove directory and contents
      unlink(project_dir, recursive = TRUE)
    }
  }
  
  # Create main directory
  dir.create(project_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Create subdirectories
  dirs <- c(
    "data", 
    "data/raw", 
    "data/processed", 
    "analysis", 
    "results",
    "results/figures",
    "results/tables", 
    "reports"
  )
  
  for (d in dirs) {
    dir.create(file.path(project_dir, d), showWarnings = FALSE, recursive = TRUE)
  }
  
  # Create README file
  readme_content <- paste0(
    "# ", project_name, "\n\n",
    "This is a METEOR project for antimicrobial resistance analysis.\n\n",
    "## Project Structure\n\n",
    "- data/raw: Raw data files\n",
    "- data/processed: Processed data files\n",
    "- analysis: R scripts for analysis\n",
    "- results/figures: Generated figures\n",
    "- results/tables: Generated tables\n",
    "- reports: Generated reports\n\n",
    "Created with [METEOR package](https://github.com/username/meteor) version ", packageVersion("meteor"), "\n"
  )
  
  writeLines(readme_content, file.path(project_dir, "README.md"))
  
  # Create sample script
  script_content <- paste0(
    "# ", project_name, " Analysis\n\n",
    "# Load packages\n",
    "library(meteor)\n\n",
    "# Import data\n",
    "# amr_data <- import_amr_data('data/raw/your_data_file.csv')\n\n",
    "# Validate data\n",
    "# validated_data <- validate_data(amr_data)\n\n",
    "# Perform analysis\n",
    "# results <- calculate_pooled_rate(validated_data)\n\n",
    "# Create visualizations\n",
    "# forest_plot <- create_forest_plot(results)\n\n",
    "# Save results\n",
    "# export_plot(forest_plot, 'results/figures/forest_plot.png')\n\n",
    "# Generate report\n",
    "# generate_summary_report(results, output_file = 'reports/summary_report.html')\n"
  )
  
  writeLines(script_content, file.path(project_dir, "analysis", paste0(project_name, "_analysis.R")))
  
  message("METEOR project '", project_name, "' created at ", project_dir)
  return(invisible(project_dir))
}

#' METEOR package options
#'
#' A list containing global options for the METEOR package
#'
#' @export
#'
#' @examples
#' # Access current options
#' meteor_options()
meteor_options <- new.env(parent = emptyenv())

# Initialize default options
meteor_options$default_meta_method <- "REML"  # Default meta-analysis method
meteor_options$confidence_level <- 0.95  # Default confidence level
meteor_options$color_palette <- "viridis"  # Default color palette
meteor_options$na_handling <- "omit"  # Default NA handling strategy
meteor_options$significant_digits <- 3  # Default number of significant digits for reporting
meteor_options$show_warnings <- TRUE  # Show warnings by default

#' Set global options for METEOR package
#'
#' @param ... Named options to set
#' @param reset Logical; if TRUE, reset all options to defaults
#'
#' @return Invisibly returns the updated options
#' @export
#'
#' @examples
#' # Set confidence level
#' set_meteor_options(confidence_level = 0.99)
#'
#' # Reset to defaults
#' set_meteor_options(reset = TRUE)
set_meteor_options <- function(..., reset = FALSE) {
  if (reset) {
    meteor_options$default_meta_method <- "REML"
    meteor_options$confidence_level <- 0.95
    meteor_options$color_palette <- "viridis"
    meteor_options$na_handling <- "omit"
    meteor_options$significant_digits <- 3
    meteor_options$show_warnings <- TRUE
  } else {
    args <- list(...)
    if (length(args) == 0) {
      warning("No options provided. Use get_meteor_options() to see current options.")
    } else {
      for (option_name in names(args)) {
        meteor_options[[option_name]] <- args[[option_name]]
      }
    }
  }
  
  return(invisible(as.list(meteor_options)))
}

#' Get current global options for METEOR package
#'
#' @return A list of current options
#' @export
#'
#' @examples
#' # Get all options
#' get_meteor_options()
get_meteor_options <- function() {
  as.list(meteor_options)
}

#' Display help information for METEOR package
#'
#' @param topic Optional topic to get help on (function name or category)
#'
#' @return Invisibly returns NULL
#' @export
#'
#' @examples
#' # General help
#' meteor_help()
#'
#' # Help on a specific topic
#' meteor_help("import_amr_data")
meteor_help <- function(topic = NULL) {
  if (is.null(topic)) {
    cat("METEOR: Meta-analysis Tool for Exploring antimicrobial resistance Outcomes across Realms\n\n")
    cat("Main function categories:\n")
    cat("  - Data import: import_amr_data(), import_local_data()\n")
    cat("  - Data validation: validate_data(), check_data_quality()\n")
    cat("  - Analysis: calculate_pooled_rate(), analyze_heterogeneity()\n")
    cat("  - Visualization: create_forest_plot(), create_geo_map()\n")
    cat("  - Reporting: generate_summary_report()\n\n")
    cat("For more detailed help on a specific topic, use meteor_help(\"topic\")\n")
    cat("or use the standard R help system: ?function_name\n")
  } else {
    if (topic %in% c("import", "data", "import_data")) {
      cat("Data Import Functions:\n\n")
      cat("import_amr_data() - Import AMR data from various file formats\n")
      cat("import_local_data() - Import local research data\n")
      cat("connect_to_database() - Connect to external AMR databases\n\n")
      cat("For detailed function help, use: ?function_name\n")
    } else if (topic %in% c("validate", "validation", "data_validation")) {
      cat("Data Validation Functions:\n\n")
      cat("validate_data() - Validate the structure and content of AMR data\n")
      cat("check_data_quality() - Check for data quality issues\n")
      cat("standardize_amr_data() - Standardize data fields to METEOR format\n\n")
      cat("For detailed function help, use: ?function_name\n")
    } else if (topic %in% c("analysis", "meta", "meta_analysis")) {
      cat("Meta-Analysis Functions:\n\n")
      cat("calculate_pooled_rate() - Calculate pooled resistance rates\n")
      cat("analyze_heterogeneity() - Analyze heterogeneity among studies\n")
      cat("perform_subgroup_analysis() - Perform subgroup meta-analysis\n\n")
      cat("For detailed function help, use: ?function_name\n")
    } else if (topic %in% c("visualization", "plots", "figures")) {
      cat("Visualization Functions:\n\n")
      cat("create_forest_plot() - Create forest plots from meta-analysis results\n")
      cat("create_geo_map() - Create geographic maps of resistance data\n")
      cat("create_resistance_heatmap() - Create heatmaps of resistance patterns\n\n")
      cat("For detailed function help, use: ?function_name\n")
    } else if (topic %in% c("report", "reporting")) {
      cat("Reporting Functions:\n\n")
      cat("generate_summary_report() - Generate a comprehensive summary report\n")
      cat("generate_region_report() - Generate region-specific reports\n")
      cat("generate_pathogen_report() - Generate pathogen-specific reports\n\n")
      cat("For detailed function help, use: ?function_name\n")
    } else {
      # Try to find the function and show its help
      if (exists(topic, mode = "function")) {
        utils::help(topic, package = "meteor")
      } else {
        warning("Topic '", topic, "' not found in METEOR package.")
        cat("Available topics: import, validation, analysis, visualization, reporting\n")
      }
    }
  }
  
  return(invisible(NULL))
}

#' Check required package dependencies
#'
#' @param quietly Logical; if TRUE, suppress messages
#'
#' @return Logical indicating whether all dependencies are available
#' @export
#'
#' @examples
#' check_dependencies()
check_dependencies <- function(quietly = FALSE) {
  # Core dependencies
  core_packages <- c("dplyr", "tidyr", "ggplot2", "meta", "metafor", "shiny")
  
  # Optional dependencies for advanced features
  optional_packages <- c("leaflet", "DT", "plotly", "shinydashboard", "sf", "countrycode")
  
  # Check core packages
  missing_core <- core_packages[!sapply(core_packages, requireNamespace, quietly = TRUE)]
  
  # Check optional packages
  missing_optional <- optional_packages[!sapply(optional_packages, requireNamespace, quietly = TRUE)]
  
  # Report status
  if (!quietly) {
    if (length(missing_core) == 0) {
      message("All required dependencies are installed.")
    } else {
      warning("Missing required dependencies: ", paste(missing_core, collapse = ", "), 
              "\nPlease install these packages with: install.packages(c('", 
              paste(missing_core, collapse = "', '"), "'))")
    }
    
    if (length(missing_optional) > 0) {
      message("Optional dependencies not installed: ", paste(missing_optional, collapse = ", "), 
              "\nSome advanced features may not be available.")
    }
  }
  
  return(length(missing_core) == 0)
}

#' Launch the main METEOR Shiny application
#'
#' @param ... Additional parameters to pass to shiny::runApp()
#'
#' @return Shiny application object
#' @export
#'
#' @examples
#' \dontrun{
#' launch_meteor()
#' }
launch_meteor <- function(...) {
  # Check if shiny is installed
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the METEOR application. Please install it.")
  }
  
  # Get the path to the main app
  app_dir <- system.file("shiny-apps", "meteor-app", package = "meteor")
  
  if (app_dir == "") {
    stop("Could not find Shiny application. Please reinstall 'meteor' package.")
  }
  
  # Run the app
  shiny::runApp(app_dir, ...)
} 